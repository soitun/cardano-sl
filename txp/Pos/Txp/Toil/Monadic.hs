{-# LANGUAGE DeriveFunctor #-}

-- | Some monads used in Toil and primitive actions.

module Pos.Txp.Toil.Monadic
       (
         -- * Monadic Utxo
         UtxoM
       , runUtxoM
       , evalUtxoM
       , execUtxoM
       , utxoGet
       , utxoPut
       , utxoDel

         -- * Monadic local Toil
       , LocalToilState (..)
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , LocalToilM
       , hasTx
       , memPoolSize
       , putTxWithUndo

         -- * Monadic global Toil
       , GlobalToilState (..)
       , gtsUtxoModifier
       , gtsStakesView
       , defGlobalToilState
       , GlobalToilEnv (..)
       , GlobalToilM
       , runGlobalToilM
       , getStake
       , getTotalStake
       , setStake
       , setTotalStake

         -- * Conversions
       , utxoMToLocalToilM
       , utxoMToGlobalToilM
       ) where

import           Universum

import           Control.Lens (at, magnify, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Free (Free (..), foldFree)
import           Control.Monad.Reader (mapReaderT)
import           Data.Default (def)
import           Fmt ((+|), (|+))
import           System.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import           Pos.Txp.Toil.Types (MemPool, StakesView, UndoMap, UtxoLookup, UtxoModifier,
                                     mpLocalTxs, mpSize, svStakes, svTotal)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Monadic actions with Utxo.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup values in UTXO and modify it.
type UtxoM = ReaderT UtxoLookup (State UtxoModifier)

-- | Run 'UtxoM' action using 'UtxoLookup' and 'UtxoModifier'.
runUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> (a, UtxoModifier)
runUtxoM modifier getter = usingState modifier . usingReaderT getter

-- | Version of 'runUtxoM' which discards final state.
evalUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> a
evalUtxoM = fst ... runUtxoM

-- | Version of 'runUtxoM' which discards action's result.
execUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> UtxoModifier
execUtxoM = snd ... runUtxoM

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGet :: TxIn -> UtxoM (Maybe TxOutAux)
utxoGet txIn = do
    utxoLookup <- ask
    MM.lookup utxoLookup txIn <$> use identity

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: TxIn -> TxOutAux -> UtxoM ()
utxoPut id txOut = utxoGet id >>= \case
    Nothing -> identity %= MM.insert id txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|id|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: TxIn -> UtxoM ()
utxoDel id = utxoGet id >>= \case
    Just _  -> identity %= MM.delete id
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|id|+" is not in the utxo")

----------------------------------------------------------------------------
-- Monad used for local Toil and some actions.
----------------------------------------------------------------------------

data LocalToilState = LocalToilState
    { _ltsMemPool      :: !MemPool
    , _ltsUtxoModifier :: !UtxoModifier
    , _ltsUndos        :: !UndoMap
    }

makeLenses ''LocalToilState

type LocalToilM = ReaderT UtxoLookup (State LocalToilState)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: TxId -> LocalToilM Bool
hasTx id = isJust <$> use (ltsMemPool . mpLocalTxs . at id)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: TxId -> TxAux -> TxUndo -> LocalToilM ()
putTxWithUndo id tx undo =
    unlessM (hasTx id) $ do
        ltsMemPool . mpLocalTxs . at id .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at id .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: LocalToilM Int
memPoolSize = use $ ltsMemPool . mpSize

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

-- | Type which parameterizes free monad with access to Stakes.
data StakesLookupF a =
    StakesLookupF StakeholderId
                  (Maybe Coin -> a)
    deriving (Functor)

data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier :: !UtxoModifier
    , _gtsStakesView   :: !StakesView
    }

-- | Default 'GlobalToilState'.
defGlobalToilState :: GlobalToilState
defGlobalToilState =
    GlobalToilState {_gtsUtxoModifier = mempty, _gtsStakesView = def}

makeLenses ''GlobalToilState

data GlobalToilEnv = GlobalToilEnv
    { _gteUtxo       :: !UtxoLookup
    , _gteTotalStake :: !Coin
    }

makeLenses ''GlobalToilEnv

type GlobalToilM
     = ReaderT GlobalToilEnv (StateT GlobalToilState (NamedPureLogger (Free StakesLookupF)))

-- | Run 'GlobalToilM' action in some monad capable of getting
-- stakeholders' stakes and logging.
runGlobalToilM ::
       forall m a. (WithLogger m)
    => GlobalToilEnv
    -> GlobalToilState
    -> (StakeholderId -> m (Maybe Coin))
    -> GlobalToilM a
    -> m (a, GlobalToilState)
runGlobalToilM env gts stakeGetter =
    launchNamedPureLog foldFree' . usingStateT gts . usingReaderT env
  where
    foldFree' :: forall x. Free StakesLookupF x -> m x
    foldFree' =
        foldFree $ \case
            StakesLookupF sId f -> f <$> stakeGetter sId

getStake :: StakeholderId -> GlobalToilM (Maybe Coin)
getStake id =
    (<|>) <$> lift (use (gtsStakesView . svStakes . at id)) <*> baseLookup id
  where
    baseLookup :: StakeholderId -> GlobalToilM (Maybe Coin)
    baseLookup i = lift $ lift $ lift $ Free $ StakesLookupF i pure

getTotalStake :: GlobalToilM Coin
getTotalStake =
    maybe (view gteTotalStake) pure =<< use (gtsStakesView . svTotal)

setStake :: StakeholderId -> Coin -> GlobalToilM ()
setStake id c = gtsStakesView . svStakes . at id .= Just c

setTotalStake :: Coin -> GlobalToilM ()
setTotalStake c = gtsStakesView . svTotal .= Just c

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Lift 'UtxoM' action to 'LocalToilM'.
utxoMToLocalToilM :: UtxoM a -> LocalToilM a
utxoMToLocalToilM = mapReaderT f
  where
    f :: forall a. State UtxoModifier a -> State LocalToilState a
    f = zoom ltsUtxoModifier

-- | Lift 'UtxoM' action to 'GlobalToilM'.
utxoMToGlobalToilM :: UtxoM a -> GlobalToilM a
utxoMToGlobalToilM = mapReaderT f . magnify gteUtxo
  where
    f :: forall a.
         State UtxoModifier a
      -> StateT GlobalToilState (NamedPureLogger (Free StakesLookupF)) a
    f = state . runState . zoom gtsUtxoModifier
