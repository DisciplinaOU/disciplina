module Dscp.Educator.Web.Bot.Params
    ( EducatorBotParams (..)
    , EducatorBotSwitch (..)
    ) where

import Data.Aeson (FromJSON (..))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Time.Units (Microsecond, Time)

import Dscp.Util.Aeson ()

-- | Which params to use when launching bot.
data EducatorBotParams
    = EducatorBotParams
    { ebpSeed            :: !Text
      -- ^ Seed to generate initial data (assignments, ...).
    , ebpOperationsDelay :: !(Time Microsecond)
      -- ^ Artificial delay in bot operations.
    } deriving (Show, Eq)

data EducatorBotSwitch
    = EducatorBotOff
    | EducatorBotOn EducatorBotParams
    deriving (Show, Eq)

deriveFromJSON defaultOptions ''EducatorBotParams

maybeToEBotSwitch :: Maybe EducatorBotParams -> EducatorBotSwitch
maybeToEBotSwitch Nothing       = EducatorBotOff
maybeToEBotSwitch (Just params) = EducatorBotOn params

instance FromJSON EducatorBotSwitch where
    parseJSON = fmap maybeToEBotSwitch . parseJSON
