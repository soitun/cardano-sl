-- | Pure Toss.

module Pos.Ssc.GodTossing.Toss.Pure
       ( PureToss (..)
       , MultiRichmenStake
       , MultiRichmenSet
       , runPureToss
       , runPureTossWithLogger
       , evalPureTossWithLogger
       , execPureTossWithLogger
       ) where

import           Control.Lens                   (at, (%=), (.=))
import           Control.Monad.RWS.Strict       (RWS, runRWS)
import           Control.Monad.Trans.Identity   (mapIdentityT)
import qualified Data.HashMap.Strict            as HM
import           System.Wlog                    (CanLog, HasLoggerName, LogEvent,
                                                 LoggerName, NamedPureLogger (..),
                                                 WithLogger, launchNamedPureLog,
                                                 runPureLog, usingLoggerName)
import           Universum

import           Pos.Lrc.Types                  (RichmenSet, RichmenStake)
import           Pos.Ssc.GodTossing.Core        (deleteSignedCommitment,
                                                 insertSignedCommitment)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Toss.Class  (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Types       (GtGlobalState, gsCommitments, gsOpenings,
                                                 gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (EpochIndex, crucialSlot)

type MultiRichmenStake = HashMap EpochIndex RichmenStake
type MultiRichmenSet = HashMap EpochIndex RichmenSet

newtype PureToss a = PureToss
    { getPureToss :: NamedPureLogger (RWS MultiRichmenStake () GtGlobalState) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName)

instance (Monad m, HasLoggerName m) => HasLoggerName (IdentityT a m) where
    getLoggerName = lift getLoggerName

    modifyLoggerName = mapIdentityT . modifyLoggerName

instance MonadTossRead PureToss where
    getCommitments = PureToss $ use gsCommitments
    getOpenings = PureToss $ use gsOpenings
    getShares = PureToss $ use gsShares
    getVssCertificates = VCD.certs <$> getVssCertData
    getVssCertData = PureToss $ use $ gsVssCertificates
    getStableCertificates epoch
        | epoch == 0 = pure genesisCertificates
        | otherwise =
            PureToss $
            VCD.certs . VCD.setLastKnownSlot (crucialSlot epoch) <$>
            use gsVssCertificates
    getRichmen epoch = PureToss $ asks (HM.lookup epoch)

instance MonadToss PureToss where
    putCommitment signedComm =
        PureToss $ gsCommitments %= insertSignedCommitment signedComm
    putOpening id op = PureToss $ gsOpenings . at id .= Just op
    putShares id sh = PureToss $ gsShares . at id .= Just sh
    putCertificate cert =
        PureToss $ gsVssCertificates %= VCD.insert cert
    delCommitment id =
        PureToss $ gsCommitments %= deleteSignedCommitment id
    delOpening id = PureToss $ gsOpenings . at id .= Nothing
    delShares id = PureToss $ gsShares . at id .= Nothing
    resetCO = PureToss $ do
        gsCommitments .= mempty
        gsOpenings .= mempty
    resetShares = PureToss $ gsShares .= mempty
    setEpochOrSlot eos = PureToss $ gsVssCertificates %= VCD.setLastKnownEoS eos

runPureToss
    :: LoggerName
    -> MultiRichmenStake
    -> GtGlobalState
    -> PureToss a
    -> (a, GtGlobalState, [LogEvent])
runPureToss loggerName richmenData gs =
    convertRes .
    (\a -> runRWS a richmenData gs) .
    usingLoggerName loggerName . runPureLog . runNamedPureLogger . getPureToss
  where
    convertRes :: ((a, [LogEvent]), GtGlobalState, ())
               -> (a, GtGlobalState, [LogEvent])
    convertRes ((res, logEvents), newGs, ()) = (res, newGs, logEvents)

runPureTossWithLogger
    :: WithLogger m
    => MultiRichmenStake
    -> GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState)
runPureTossWithLogger richmenData gs action = do
    let traverse' (fa, b, c) = (, b, c) <$> fa
    let unwrapLower a = return $ traverse' $ runRWS a richmenData gs
    (res, newGS, ()) <- launchNamedPureLog unwrapLower $ getPureToss action
    return (res, newGS)

evalPureTossWithLogger
    :: WithLogger m
    => MultiRichmenStake -> GtGlobalState -> PureToss a -> m a
evalPureTossWithLogger r g = fmap fst . runPureTossWithLogger r g

execPureTossWithLogger
    :: WithLogger m
    => MultiRichmenStake -> GtGlobalState -> PureToss a -> m GtGlobalState
execPureTossWithLogger r g = fmap snd . runPureTossWithLogger r g
