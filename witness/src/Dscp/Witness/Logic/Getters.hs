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
    ) where

import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.State.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Witness.Config
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
getBlockMaybe :: HasHeaderHash x => x -> SdM (Maybe Block)
getBlockMaybe o =
    fmap (sBlockReconstruct . SD.buBlock) <$>
    SD.queryOne (SD.BlockRef $ headerHash o)

-- | Resolves block, throws exception if it's absent.
getBlock :: HasHeaderHash x => x -> SdM Block
getBlock o = do
    bM <- getBlockMaybe o
    maybe (SD.throwLocalError $ LEBlockAbsent $
              "Can't get block with hash " <> pretty (headerHash o))
          pure
          bM

-- | Safely resolve header.
getHeaderMaybe :: HasHeaderHash x => x -> SdM (Maybe Header)
getHeaderMaybe = fmap (fmap bHeader) . getBlockMaybe

-- | Resolves header, throws exception if it's absent.
getHeader :: HasHeaderHash x => x -> SdM Header
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
