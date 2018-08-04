-- | Block-related getters.

module Dscp.Witness.Logic.Getters where

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
getTipHash :: HasWitnessConfig => SdM_ c HeaderHash
getTipHash = do
    x <- SD.unTipValue <$>
        (SD.queryOne SD.TipKey >>=
         maybe (SD.throwLocalError @(SD.BlockStateException Ids) SD.TipNotFound) pure)
    pure $ fromMaybe genesisHash x

-- | Retrieves current tip block.
getTipBlock :: HasWitnessConfig => SdM_ c Block
getTipBlock = do
    tipHash <- getTipHash
    if tipHash == genesisHash
    then pure genesisBlock
    else sBlockReconstruct . SD.buBlock <$>
         (maybe (SD.throwLocalError $ LEMalformed "Tip block is absent") pure =<<
          SD.queryOne (SD.BlockRef tipHash))

-- | Retrieves current tip header.
getTipHeader :: HasWitnessConfig => SdM_ c Header
getTipHeader = rbHeader <$> getTipBlock

-- | Resolves block, throws exception if it's absent.
getBlock :: HasHeaderHash x => x -> SdM Block
getBlock o = do
    block <-
        maybe (SD.throwLocalError $ LEBlockAbsent $
               "Can't get block with hash " <> pretty h) pure =<<
        SD.queryOne (SD.BlockRef h)
    pure $ sBlockReconstruct $ SD.buBlock block
  where
    h = headerHash o

-- | Resolves header, throws exception if it's absent.
getHeader :: HasHeaderHash x => x -> SdM Header
getHeader = fmap rbHeader . getBlock
