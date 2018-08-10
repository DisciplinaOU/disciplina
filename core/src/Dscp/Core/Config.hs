{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Core configuration.

module Dscp.Core.Config
    (
      SlotDuration (..)

    , CoreConfig
    , CoreConfigRec
    , HasCoreConfig

    , coreConfig
    , withCoreConfig
    , fillCoreConfig

    , genesisInfo
    , genesisBlock
    , genesisHeader
    , genesisHash

    , giveL
    , giveLC
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given (..), give)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, option, sub)

import Dscp.Config (giveL, giveLC)
import Dscp.Config.AppDir (AppDirectoryParam (..), prepareAppDir)
import Dscp.Core.Foundation
import Dscp.Core.Genesis
import Dscp.Crypto (hash)

----------------------------------------------------------------------------
-- Wrappers and parameters
----------------------------------------------------------------------------

-- | Slot duration in milliseconds.
newtype SlotDuration = SlotDuration { unSlotDuration :: Word64 }
    deriving (Eq, Ord, Num, Generic)

---------------------------------------------------------------------
-- Reading config
---------------------------------------------------------------------

type CoreConfig =
   '[ "core" ::<
       '[ "homeParam" ::: AppDirectoryParam
        , "genesis" ::: GenesisConfig
        , "slotDuration" ::: SlotDuration

        , "generated" ::<
            '[ "genesisInfo" ::: GenesisInfo
             , "home" ::: FilePath
             ]
        ]
    ]

type CoreConfigRecP = ConfigRec 'Partial CoreConfig
type CoreConfigRec = ConfigRec 'Final CoreConfig

type HasCoreConfig = Given CoreConfigRec

---------------------------------------------------------------------
-- Config itself
---------------------------------------------------------------------

coreConfig :: HasCoreConfig => CoreConfigRec
coreConfig = given

withCoreConfig :: CoreConfigRec -> (HasCoreConfig => a) -> a
withCoreConfig = give

-- | Adds auto-generated fields.
fillCoreConfig :: CoreConfigRecP -> IO CoreConfigRecP
fillCoreConfig conf = do
    -- TODO make deep lenses that will fail automatically
    dirPath <-
        prepareAppDir $
        fromMaybe (error "'core.homeParam' is absent") $
        conf ^. sub #core . option #homeParam
    pure $
        conf & sub #core . sub #generated . option #genesisInfo ?~ ourGenesisInfo
             & sub #core . sub #generated . option #home ?~ dirPath
  where
    ourGenesisInfo :: GenesisInfo
    ourGenesisInfo =
        formGenesisInfo $ fromMaybe (error "'genesis' is absent")
                                    (conf ^. sub #core . option #genesis)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

genesisInfo :: HasCoreConfig => GenesisInfo
genesisInfo = giveL @CoreConfig @GenesisInfo

genesisBlock :: HasCoreConfig => Block
genesisBlock = giGenesisBlock genesisInfo

genesisHeader :: HasCoreConfig => Header
genesisHeader = bHeader genesisBlock

genesisHash :: HasCoreConfig => HeaderHash
genesisHash = hash genesisHeader
