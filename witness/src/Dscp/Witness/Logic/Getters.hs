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

    , getAccountMaybe
    , getAccountTxs

    , getTxMaybe
    , getTx
    ) where

import qualified Data.Set as Set
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Logic.Exceptions

----------------------------------------------------------------------------
-- Block/Header/Tip getters
----------------------------------------------------------------------------

-- | Retrieves current tip.
getTipHash :: HasWitnessConfig => SdM HeaderHash
getTipHash = do
    x <- SD.unTipValue <$>
        (SD.queryOne SD.TipKey >>=
         maybe (SD.throwLocalError @(SD.BlockStateException Ids) SD.TipNotFound) pure)
    pure $ fromMaybe genesisHash x

-- | Retrieves current tip block.
getTipBlock :: HasWitnessConfig => SdM Block
getTipBlock = do
    tipHash <- getTipHash
    if tipHash == genesisHash
    then pure genesisBlock
    else sBlockReconstruct . SD.buBlock <$>
         (maybe (SD.throwLocalError $ LEMalformed "Tip block is absent") pure =<<
          SD.queryOne (SD.BlockRef tipHash))

-- | Retrieves current tip header.
getTipHeader :: HasWitnessConfig => SdM Header
getTipHeader = bHeader <$> getTipBlock

-- | Safely get block.
getBlockMaybe :: (HasWitnessConfig, HasHeaderHash x) => x -> SdM (Maybe Block)
getBlockMaybe (headerHash -> h)
    | h == genesisHash = pure $ Just genesisBlock
    | otherwise =
          fmap (sBlockReconstruct . SD.buBlock) <$>
          SD.queryOne (SD.BlockRef h)

-- | Resolves block, throws exception if it's absent.
getBlock :: (HasWitnessConfig, HasHeaderHash x) => x -> SdM Block
getBlock o = do
    bM <- getBlockMaybe o
    maybe (SD.throwLocalError $ LEBlockAbsent $
              "Can't get block with hash " <> pretty (headerHash o))
          pure
          bM

-- | Safely resolve header.
getHeaderMaybe :: (HasWitnessConfig, HasHeaderHash x) => x -> SdM (Maybe Header)
getHeaderMaybe = fmap (fmap bHeader) . getBlockMaybe

-- | Resolves header, throws exception if it's absent.
getHeader :: (HasWitnessConfig, HasHeaderHash x) => x -> SdM Header
getHeader = fmap bHeader . getBlock

-- | Given the element, get the previous one. If the element itself
-- doesn't exist, this method will throw.
resolvePrevious ::
       (HasWitnessConfig, HasHeaderHash x) => x -> SdM (Maybe HeaderHash)
resolvePrevious o = do
    header <- getHeader o
    if headerHash header == genesisHash
    then pure Nothing
    else pure $ Just $ hPrevHash header

----------------------------------------------------------------------------
-- Account getters
----------------------------------------------------------------------------

-- | Safely get an account.
getAccountMaybe :: WitnessWorkMode ctx m => Address -> m (Maybe Account)
getAccountMaybe = runStateSdMRead (RememberForProof False) . SD.queryOne . AccountId

-- | Get a list of all transactions for a given account
getAccountTxs :: WitnessWorkMode ctx m => Address -> m [GTx]
getAccountTxs address =
    runStateSdMRead (RememberForProof False) loadTxs >>=
    runSdMRead . SD.querySet . Set.fromList >>=
    return . map unGTxWitnessed . elems
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

-- | Safely get transaction.
getTxMaybe :: GTxId -> SdM (Maybe GTxWitnessed)
getTxMaybe = SD.queryOne

-- | Resolves transaction, throws exception if it's absent.
getTx :: GTxId -> SdM GTxWitnessed
getTx gTxId = do
    tM <- getTxMaybe gTxId
    maybe (SD.throwLocalError $ LETxAbsent $
              "Can't get transaction with id " <> pretty gTxId)
          pure
          tM
