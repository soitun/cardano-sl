{-# LANGUAGE TypeFamilies #-}

-- | Wallets, accounts and addresses management logic

module Pos.Wallet.Web.Methods.Logic
       ( getWallet
       , getWallets
       , getAccount
       , getAccounts

       , createWalletSafe
       , newAccount
       , newAccountIncludeUnready
       , newAddress
       , newAddress_

       , deleteWallet
       , deleteAccount

       , updateWallet
       , updateAccount
       , changeWalletPassphrase
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import           Data.List                  (findIndex)
import qualified Data.Set                   as S
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, sformat, (%), shown, int)
import           System.Wlog                (logInfo)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Core                   (Address, Coin, mkCoin, sumCoins,
                                             unsafeIntegerToCoin)
import           Pos.Crypto                 (PassPhrase, changeEncPassphrase,
                                             checkPassMatches, emptyPassphrase)
import           Pos.Txp                    (GenericTxpLocalData, TxAux, TxId, UndoMap,
                                             applyUtxoModToAddrCoinMap, getLocalTxs,
                                             getLocalUndos, withTxpLocalData)
import           Pos.Util                   (maybeThrow)
import qualified Pos.Util.Modifier          as MM
import           Pos.Util.Servant           (encodeCType)
import           Pos.Wallet.KeyStorage      (addSecretKey, deleteSecretKey,
                                             getSecretKeysPlain)
import           Pos.Wallet.Web.Account     (AddrGenSeed, findKey, genUniqueAccountId,
                                             genUniqueAddress, getAddrIdx, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..),
                                             CAccountInit (..), CAccountMeta (..),
                                             CAddress (..), CCoin, CId,
                                             CWAddressMeta (..), CWallet (..),
                                             CWalletMeta (..), Wal, addrMetaToAccount,
                                             encToCId, mkCCoin)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode, convertCIdTOAddr)
import           Pos.Wallet.Web.State       (AddressInfo (..),
                                             AddressLookupMode (Existing),
                                             CustomAddressType (ChangeAddr, UsedAddr),
                                             WalletSnapshot, addWAddress, askWalletDB,
                                             askWalletSnapshot,
                                             createWallet, createAccountWithAddress,
                                             doesAccountExist,
                                             getAccountIds, getWalletAddresses,
                                             getWalletBalancesAndUtxo,
                                             getWalletMetaIncludeUnready, getWalletPassLU,
                                             getWalletSnapshot, isCustomAddress,
                                             removeAccount, removeWallet, setAccountMeta,
                                             setWalletMeta, setWalletPassLU)
import           Pos.Wallet.Web.Tracking    (CAccModifier (..), CachedCAccModifier,
                                             sortedInsertions, txMempoolToModifier)
import           Pos.Wallet.Web.Util        (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                             getAccountMetaOrThrow, getWalletAccountIds)


----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------


sumCCoin :: MonadThrow m => [CCoin] -> m CCoin
sumCCoin ccoins = mkCCoin . unsafeIntegerToCoin . sumCoins <$> mapM decodeCTypeOrFail ccoins

getBalanceWithMod :: WalletSnapshot -> CachedCAccModifier -> Address -> Coin
getBalanceWithMod ws accMod addr =
    fromMaybe (mkCoin 0) .
    HM.lookup addr $
    flip applyUtxoModToAddrCoinMap balancesAndUtxo (camUtxo accMod)
  where
    balancesAndUtxo = getWalletBalancesAndUtxo ws

getWAddressBalanceWithMod
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> CachedCAccModifier
    -> CWAddressMeta
    -> m Coin
getWAddressBalanceWithMod ws accMod addr =
    getBalanceWithMod ws accMod
        <$> convertCIdTOAddr (cwamId addr)

-- BE CAREFUL: this function has complexity O(number of used and change addresses)
getWAddress
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> CachedCAccModifier -> CWAddressMeta -> m CAddress
getWAddress ws cachedAccModifier cAddr = do
    let aId = cwamId cAddr
    balance <- getWAddressBalanceWithMod ws cachedAccModifier cAddr

    let getFlag customType accessMod =
            let checkDB = isCustomAddress ws customType (cwamId cAddr)
                checkMempool = elem aId . map (fst . fst) . toList $
                               MM.insertions $ accessMod cachedAccModifier
             in checkDB || checkMempool
        isUsed   = getFlag UsedAddr camUsed
        isChange = getFlag ChangeAddr camChange
    return $ CAddress aId (mkCCoin balance) isUsed isChange

getAccountMod
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> CachedCAccModifier
    -> AccountId
    -> m CAccount
getAccountMod ws accMod accId = do
    logInfo $ sformat ("getAccountMod: Account " % build % " has accMod: " % build) accId accMod
    dbAddrs    <- map adiCWAddressMeta . sortOn adiSortingKey <$> getAccountAddrsOrThrow ws Existing accId
    logInfo $ sformat ("getAccountMod: dbAddrs count: " % build) $ length dbAddrs
    let allAddrIds = gatherAddresses (camAddresses accMod) dbAddrs
    logInfo $ sformat ("getAccountMod: allAddrIds (dbAddrd + unknownMemAddrs) count: " % build)
            $ length allAddrIds
    allAddrs <- mapM (getWAddress ws accMod) allAddrIds
    balance <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
               mapM (decodeCTypeOrFail . cadAmount) allAddrs
    logInfo $ sformat ("getAccountMod: Account " % build % " has balance: " % build) accId balance
    meta <- getAccountMetaOrThrow ws accId
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    gatherAddresses addrModifier dbAddrs = do
        let memAddrs = sortedInsertions addrModifier
            dbAddrsSet = S.fromList dbAddrs
            relatedMemAddrs = filter ((== accId) . addrMetaToAccount) memAddrs
            unknownMemAddrs = filter (`S.notMember` dbAddrsSet) relatedMemAddrs
        dbAddrs <> unknownMemAddrs

getAccount :: MonadWalletWebMode m => AccountId -> m CAccount
getAccount accId = do
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    accMod <- txMempoolToModifier ws mps =<< findKey accId
    getAccountMod ws accMod accId

getAccountsIncludeUnready
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
    -> Bool -> Maybe (CId Wal) -> m [CAccount]
getAccountsIncludeUnready ws mps includeUnready mCAddr = do
    whenJust mCAddr $ \cAddr ->
      void $ maybeThrow (noWallet cAddr) $
        getWalletMetaIncludeUnready ws includeUnready cAddr
    let accIds = maybe (getAccountIds ws) (getWalletAccountIds ws) mCAddr
    logInfo $ sformat ("getAccountsIncludeUnready: Computing balance for " % shown) accIds
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    logInfo $ sformat ("getAccountsIncludeUnready: Grouped AccountId(s) " % shown) groupedAccIds
    cAccounts <- concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) -> do
        accMod <- txMempoolToModifier ws mps =<< findKey wid
        mapM (getAccountMod ws accMod) walAccIds
    forM_ cAccounts $ \CAccount{..} -> logInfo $ sformat ("Account " % build % " has balance " % build) caId caAmount
    pure cAccounts
  where
    noWallet cAddr = RequestError $
        -- TODO No WALLET with id ...
        -- dunno whether I can fix and not break compatible w/ daedalus
        sformat ("No account with id "%build%" found") cAddr

getAccounts
    :: MonadWalletWebMode m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts mCAddr = do
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    getAccountsIncludeUnready ws mps False mCAddr

getWalletIncludeUnready :: MonadWalletWebMode m
                        => WalletSnapshot
                        -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
                        -> Bool -> CId Wal -> m CWallet
getWalletIncludeUnready ws mps includeUnready cAddr = do
    meta       <- maybeThrow noWallet $ getWalletMetaIncludeUnready ws includeUnready cAddr
    accounts   <- getAccountsIncludeUnready ws mps includeUnready (Just cAddr)
    let accountsNum = length accounts
    logInfo $ sformat ("getWalletIncludeUnready: Computing balance out of " % int % " accounts..") accountsNum
    balance    <- sumCCoin (map caAmount accounts)
    logInfo $ sformat ("getWalletIncludeUnready: Balance is " % build % ".") balance
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKById cAddr
    passLU     <- maybeThrow noWallet (getWalletPassLU ws cAddr)
    pure $ CWallet cAddr meta accountsNum balance hasPass passLU
  where
    noWallet = RequestError $
        sformat ("No wallet with address "%build%" found") cAddr

getWallet :: MonadWalletWebMode m => CId Wal -> m CWallet
getWallet wid = do
    logInfo "getWallet: Starting.."
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    logInfo $ sformat ("getWallet: mempool snapshot is " % shown) mps
    getWalletIncludeUnready ws mps False wid

getWallets :: MonadWalletWebMode m => m [CWallet]
getWallets = do
    ws <- askWalletSnapshot
    mps <- withTxpLocalData getMempoolSnapshot
    mapM (getWalletIncludeUnready ws mps False) (getWalletAddresses ws)

----------------------------------------------------------------------------
-- Creators
----------------------------------------------------------------------------

newAddress_
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CWAddressMeta
newAddress_ ws addGenSeed passphrase accId = do
    -- check whether account exists
    let parentExists = doesAccountExist ws accId
    unless parentExists $ throwM noAccount

    -- XXX Transaction
    -- Make 'newAddress' generate a unique name internally
    cAccAddr <- genUniqueAddress ws addGenSeed passphrase accId
    db <- askWalletDB
    addWAddress db cAccAddr
    return cAccAddr
  where
    noAccount =
        RequestError $ sformat ("No account with id "%build%" found") accId

newAddress
    :: MonadWalletWebMode m
    => WalletSnapshot
    -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
    -> AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress ws mps addGenSeed passphrase accId = do
    cwAddrMeta <- newAddress_ ws addGenSeed passphrase accId
    accMod <- txMempoolToModifier ws mps =<< findKey accId
    getWAddress ws accMod cwAddrMeta

newAccountIncludeUnready
    :: MonadWalletWebMode m
    => Bool -> AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccountIncludeUnready includeUnready addGenSeed passphrase CAccountInit {..} = do
    mps <- withTxpLocalData getMempoolSnapshot
    db <- askWalletDB
    ws <- getWalletSnapshot db
    -- TODO nclarke We read the mempool at this point to be consistent with the previous
    -- behaviour, but we may want to consider whether we should read it _after_ the
    -- account is created, since it's not used until we call 'getAccountMod'
    accMod <- txMempoolToModifier ws mps =<< findKey caInitWId
    -- check wallet exists
    _ <- getWalletIncludeUnready ws mps includeUnready caInitWId

    cAddr <- genUniqueAccountId ws addGenSeed caInitWId
    cAddrMeta <- genUniqueAddress ws addGenSeed passphrase cAddr

    createAccountWithAddress db cAddr caInitMeta cAddrMeta

    ws' <- askWalletSnapshot

    -- Re-read DB after the update.
    getAccountMod ws' accMod cAddr

newAccount
    :: MonadWalletWebMode m
    => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount = newAccountIncludeUnready False

createWalletSafe
    :: MonadWalletWebMode m
    => CId Wal -> CWalletMeta -> Bool -> m CWallet
createWalletSafe cid wsMeta isReady = do
    -- Disallow duplicate wallets (including unready wallets)
    db <- askWalletDB
    ws <- getWalletSnapshot db
    mps <- withTxpLocalData getMempoolSnapshot
    let wSetExists = isJust $ getWalletMetaIncludeUnready ws True cid
    when wSetExists $
        throwM $ RequestError "Wallet with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWallet db cid wsMeta isReady curTime
    -- Return the newly created wallet irrespective of whether it's ready yet
    ws' <- getWalletSnapshot db
    getWalletIncludeUnready ws' mps True cid


----------------------------------------------------------------------------
-- Deleters
----------------------------------------------------------------------------

deleteWallet :: MonadWalletWebMode m => CId Wal -> m ()
deleteWallet wid = do
    db <- askWalletDB
    removeWallet db wid
    deleteSecretKey . fromIntegral =<< getAddrIdx wid

deleteAccount :: MonadWalletWebMode m => AccountId -> m ()
deleteAccount accId = do
  db <- askWalletDB
  removeAccount db accId

----------------------------------------------------------------------------
-- Modifiers
----------------------------------------------------------------------------

updateWallet :: MonadWalletWebMode m => CId Wal -> CWalletMeta -> m CWallet
updateWallet wId wMeta = do
    db <- askWalletDB
    setWalletMeta db wId wMeta
    getWallet wId

updateAccount :: MonadWalletWebMode m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    db <- askWalletDB
    setAccountMeta db accId wMeta
    getAccount accId

changeWalletPassphrase
    :: MonadWalletWebMode m
    => CId Wal -> PassPhrase -> PassPhrase -> m ()
changeWalletPassphrase wid oldPass newPass = do
    oldSK <- getSKById wid

    unless (isJust $ checkPassMatches newPass oldSK) $ do
        newSK <- maybeThrow badPass =<< changeEncPassphrase oldPass newPass oldSK
        deleteSK oldPass
        addSecretKey newSK
        db <- askWalletDB
        setWalletPassLU db wid =<< liftIO getPOSIXTime
  where
    badPass = RequestError "Invalid old passphrase given"
    deleteSK passphrase = do
        let nice k = encToCId k == wid && isJust (checkPassMatches passphrase k)
        midx <- findIndex nice <$> getSecretKeysPlain
        idx  <- RequestError "No key with such address and pass found"
                `maybeThrow` midx
        deleteSecretKey (fromIntegral idx)

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | Get local transactions and undos from the mempool.
--   We define this function here rather than in 'Pos.Txp.MemState.Class'
--   because it is less composable than the functions defined there - it
--   obfuscates the underlying structure. But hlint complains if we refuse
--   to unroll each of the uses in this module.
getMempoolSnapshot :: GenericTxpLocalData e -> STM ([(TxId, TxAux)], UndoMap)
getMempoolSnapshot txpData =  (,)
    <$> getLocalTxs txpData
    <*> getLocalUndos txpData
