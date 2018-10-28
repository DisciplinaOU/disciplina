-- | Block-related getters.

module Dscp.Witness.Logic.Getters
    ( getTipHash
    , getTipBlock
    , getTipHeader

    , getBlockMaybe
    , getBlock
    , getHeaderMaybe
    , getHeader

    , resolvePrevious
    , resolveNext

    , getAccountMaybe
    , getMempoolAccountMaybe
    , getAccountTxs

    , getTxMaybe
    , getTx
    , getTxWithBlock

    , getPrivateTipHash
    ) where

import Data.Coerce (coerce)
import qualified Snowdrop.Block as SD
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Logic.Exceptions
import Dscp.Witness.Mempool ()
import Dscp.Witness.SDLock

----------------------------------------------------------------------------
-- Block/Header/Tip getters
----------------------------------------------------------------------------

-- | Retrieves current tip.
getTipHash :: HasWitnessConfig => SdM_ chgacc HeaderHash
getTipHash = do
    x <- SD.unTipValue <$>
        (SD.queryOne SD.TipKey >>=
         maybe (SD.throwLocalError @(SD.BlockStateException Ids) SD.TipNotFound) pure)
    pure $ fromMaybe genesisHash x

-- | Retrieves current tip block.
getTipBlock :: HasWitnessConfig => SdM_ chgacc Block
getTipBlock = do
    tipHash <- getTipHash
    if tipHash == genesisHash
    then pure genesisBlock
    else sBlockReconstruct <$>
         (maybe (SD.throwLocalError $ LEMalformed "Tip block is absent") pure =<<
          SD.queryOne (SD.BlockRef tipHash))

-- | Retrieves current tip header.
getTipHeader
    :: HasWitnessConfig
    => SdM Header
getTipHeader = bHeader <$> getTipBlock

-- | Safely get block.
getBlockMaybe
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM_ chgacc (Maybe Block)
getBlockMaybe (headerHash -> h)
    | h == genesisHash = pure $ Just genesisBlock
    | otherwise =
          sBlockReconstruct <<$>>
          SD.queryOne (SD.BlockRef h)

-- | Resolves block, throws exception if it's absent.
getBlock
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM_ chgacc Block
getBlock o = do
    bM <- getBlockMaybe o
    maybe (SD.throwLocalError $ LEBlockAbsent $
              "Can't get block with hash " <> pretty (headerHash o))
          pure
          bM

-- | Safely resolve header.
getHeaderMaybe
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM (Maybe Header)
getHeaderMaybe = fmap (fmap bHeader) . getBlockMaybe

-- | Resolves header, throws exception if it's absent.
getHeader
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM_ chgacc Header
getHeader = fmap bHeader . getBlock

-- | Given the element, get the previous one. If the element itself
-- doesn't exist, this method will throw.
resolvePrevious
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM_ chacc (Maybe HeaderHash)
resolvePrevious o = do
    header <- getHeader o
    if headerHash header == genesisHash
    then pure Nothing
    else pure $ Just $ hPrevHash header

-- | Given the element, get the next one.
resolveNext
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM_ chgacc (Maybe HeaderHash)
resolveNext = SD.queryOne . NextBlockOf . headerHash >=> pure . map unNextBlock

----------------------------------------------------------------------------
-- Account getters
----------------------------------------------------------------------------

-- TODO [DSCP-367] Unite these two below
-- | Safely get an account.
getAccountMaybe
    :: (WitnessWorkMode ctx m, WithinReadSDLock)
    => Address -> SdReadM 'ChainOnly m (Maybe Account)
getAccountMaybe =
    lift . runStateSdM . SD.queryOne . AccountId

-- | Safely get an account taking mempool into consideration.
getMempoolAccountMaybe
    :: (WitnessWorkMode ctx m, WithinReadSDLock)
    => Address -> SdReadM 'ChainAndMempool m (Maybe Account)
getMempoolAccountMaybe addr = liftSdM $ SD.queryOne (AccountId addr)

-- | Get a list of all transactions for a given account
getAccountTxs
    :: HasWitnessConfig
    => Address -> SdM [WithBlock GTxWitnessed]
getAccountTxs address = loadTxs >>= mapM getTxWithBlock
  where
    loadTxs =
        SD.queryOne (TxsOf address) >>=
        loadNextTx [] . map unLastTx
    loadNextTx !res = \case
        Nothing -> return res
        Just gTxId ->
            SD.queryOne (TxHead address gTxId) >>=
            loadNextTx (gTxId : res) . map unTxNext

----------------------------------------------------------------------------
-- Transaction getters
----------------------------------------------------------------------------

-- | Get a block where transaction lies in.
-- Return 'Nothing' for mempool transactions.
getTxBlock :: HasWitnessConfig => GTxId -> SdM_ chgacc (Maybe Block)
getTxBlock gTxId =
    SD.queryOne (TxBlockRefId gTxId) >>= mapM (getBlock . tbrBlockRef)

-- | Safely get transaction.
getTxMaybe
    :: HasWitnessConfig
    => GTxId -> SdM_ chgacc (Maybe GTxWitnessed)
getTxMaybe gTxId = runMaybeT $ asum
    [ fmap (GMoneyTxWitnessed . tiTw) . MaybeT $
          SD.queryOne (coerce @_ @TxId gTxId)
    , fmap (GPublicationTxWitnessed . piTw) . MaybeT $
          SD.queryOne (coerce @_ @PublicationTxId gTxId)
    ]

-- | Resolves transaction, throws exception if it's absent.
getTx
    :: HasWitnessConfig
    => GTxId -> SdM_ chgacc GTxWitnessed
getTx gTxId = do
    tM <- getTxMaybe gTxId
    maybe (SD.throwLocalError $ LETxAbsent $
              "Can't get transaction with id " <> pretty gTxId)
          pure
          tM

-- | Get a transaction with block it is contained in, throw an error if that transaction
-- is not found.
getTxWithBlock :: HasWitnessConfig => GTxId -> SdM_ chgacc (WithBlock GTxWitnessed)
getTxWithBlock gTxId = WithBlock <$> getTxBlock gTxId <*> getTx gTxId

----------------------------------------------------------------------------
-- Publication getters
----------------------------------------------------------------------------

getPrivateTipHash
    :: HasWitnessConfig
    => Address -> SdM_ chgacc PrivateHeaderHash
getPrivateTipHash educator =
    maybe (genesisHeaderHash educator) unLastPublication <$>
    SD.queryOne (PublicationsOf educator)
