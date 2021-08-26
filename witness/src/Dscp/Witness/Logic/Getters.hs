-- | Block-related getters.

module Dscp.Witness.Logic.Getters
    ( runSdDual

    , getTipHash
    , getTipBlock
    , getTipHeader

    , getBlockMaybe
    , getBlock
    , getHeaderMaybe
    , getHeader

    , resolvePrevious
    , resolveNext

    , getAccountMaybe
    , getAccountTxs

    , getTxMaybe
    , getTx
    , getTxWithBlock

    , getPrivateTipHash
    , getPublicationByHeaderHash
    ) where

import Control.Monad.Except (MonadError)
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
import Dscp.Witness.Web.Types (BlocksOrMempool (..))

-- | Run an action both in chain-only and chain+mempool modes.
runSdDual
    :: WitnessWorkMode ctx m
    => (forall mode. KnownSdReadMode mode => SdReadM mode m a)
    -> m (BlocksOrMempool a)
runSdDual action =
    sequence BlocksOrMempool
    { bmConfirmed = runSdReadM @'ChainOnly action
    , bmTotal = runSdReadM @'ChainAndMempool action
    }

-- | Util function similar to `nothingToThrow`, but SD-specific
nothingToThrowSD
    :: forall e1 e m a. (SD.HasReview e e1, MonadError e m)
    => e1 -> Maybe a -> m a
nothingToThrowSD e = maybe (SD.throwLocalError e) pure

----------------------------------------------------------------------------
-- Block/Header/Tip getters
----------------------------------------------------------------------------

-- | Retrieves current tip.
getTipHash :: HasWitnessConfig => SdM HeaderHash
getTipHash =
    fmap (fromMaybe genesisHash . SD.unTipValue) $
    SD.queryOne SD.TipKey >>=
    nothingToThrowSD @(SD.BlockStateException Ids) SD.TipNotFound

-- | Retrieves current tip block.
getTipBlock :: HasWitnessConfig => SdM Block
getTipBlock = do
    tipHash <- getTipHash
    if tipHash == genesisHash
    then pure genesisBlock
    else sBlockReconstruct <$>
         (nothingToThrowSD (LEMalformed "Tip block is absent") =<<
          SD.queryOne (SD.BlockRef tipHash))

-- | Retrieves current tip header.
getTipHeader
    :: HasWitnessConfig
    => SdM Header
getTipHeader = bHeader <$> getTipBlock

-- | Safely get block.
getBlockMaybe
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM (Maybe Block)
getBlockMaybe (headerHash -> h)
    | h == genesisHash = pure $ Just genesisBlock
    | otherwise =
          sBlockReconstruct <<$>>
          SD.queryOne (SD.BlockRef h)

-- | Resolves block, throws exception if it's absent.
getBlock
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM Block
getBlock o = do
    bM <- getBlockMaybe o
    nothingToThrowSD
        (LEBlockAbsent $ "Can't get block with hash " <> pretty (headerHash o))
        bM

-- | Safely resolve header.
getHeaderMaybe
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM (Maybe Header)
getHeaderMaybe = fmap (fmap bHeader) . getBlockMaybe

-- | Resolves header, throws exception if it's absent.
getHeader
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM Header
getHeader = fmap bHeader . getBlock

-- | Given the element, get the previous one. If the element itself
-- doesn't exist, this method will throw.
resolvePrevious
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM (Maybe HeaderHash)
resolvePrevious o = do
    header <- getHeader o
    if headerHash header == genesisHash
    then pure Nothing
    else pure $ Just $ hPrevHash header

-- | Given the element, get the next one.
resolveNext
    :: (HasWitnessConfig, HasHeaderHash x)
    => x -> SdM (Maybe HeaderHash)
resolveNext = SD.queryOne . NextBlockOf . headerHash >=> pure . map unNextBlock

----------------------------------------------------------------------------
-- Account getters
----------------------------------------------------------------------------

-- | Safely get an account taking mempool into consideration.
getAccountMaybe
    :: (WitnessWorkMode ctx m, KnownSdReadMode mode, WithinReadSDLock)
    => Address -> SdReadM mode m (Maybe Account)
getAccountMaybe addr = liftSdM $ SD.queryOne (AccountId addr)

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
getTxBlock :: HasWitnessConfig => GTxId -> SdM (Maybe Block)
getTxBlock gTxId =
    SD.queryOne (TxBlockRefId gTxId) >>= mapM (getBlock . tbrBlockRef)

-- | Safely get transaction.
getTxMaybe
    :: HasWitnessConfig
    => GTxId -> SdM (Maybe GTxWitnessed)
getTxMaybe gTxId = runMaybeT $ asum
    [ fmap (GMoneyTxWitnessed . tiTw) . MaybeT $
          SD.queryOne (coerce @_ @TxId gTxId)
    , fmap (GPublicationTxWitnessed . piTw) . MaybeT $
          SD.queryOne (coerce @_ @PublicationTxId gTxId)
    ]
  where
    _completenessCheck = \case
        GMoneyTxWitnessed{} -> ()
        GPublicationTxWitnessed{} -> ()
        -- NB: when compiler forces you to add a pattern match, update
        -- the code above as well.
        -- :pled: tactics to evolve the code in bug-less way

-- | Resolves transaction, throws exception if it's absent.
getTx
    :: HasWitnessConfig
    => GTxId -> SdM GTxWitnessed
getTx gTxId = do
    tM <- getTxMaybe gTxId
    nothingToThrowSD
        (LETxAbsent $ "Can't get transaction with id " <> pretty gTxId)
        tM

-- | Get a transaction with block it is contained in, throw an error if that transaction
-- is not found.
getTxWithBlock :: HasWitnessConfig => GTxId -> SdM (WithBlock GTxWitnessed)
getTxWithBlock gTxId = WithBlock <$> getTxBlock gTxId <*> getTx gTxId

----------------------------------------------------------------------------
-- Publication getters
----------------------------------------------------------------------------

getPrivateTipHash
    :: HasWitnessConfig
    => Address -> SdM PrivateHeaderHash
getPrivateTipHash educator =
    maybe (genesisHeaderHash educator) unLastPublication <$>
    SD.queryOne (PublicationsOf educator)

getPublicationByHeaderHash
    :: PrivateHeaderHash
    -> SdM (Maybe PublicationTxWitnessed)
getPublicationByHeaderHash hhash = runMaybeT . fmap piTw $ do
    pTxId <- MaybeT $ SD.queryOne hhash
    mPubW <- lift $ SD.queryOne pTxId
    nothingToThrowSD
        (LEMalformed "Private header hash and publication indices are inconsistent")
        mPubW
