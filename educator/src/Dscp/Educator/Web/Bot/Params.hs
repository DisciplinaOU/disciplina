module Dscp.Educator.Web.Bot.Params
    ( EducatorBotParams (..)
    , EducatorBotSwitch (..)
    ) where

import Time.Units (Microsecond, Time)

-- | Which params to use when launching bot.
data EducatorBotParams
    = EducatorBotParams
    { ebpSeed            :: !Text
      -- ^ Seed to generate initial data (assignments, ...).
    , ebpOperationsDelay :: !(Time Microsecond)
      -- ^ Artificial delay in bot operations.
    }

data EducatorBotSwitch
    = EducatorBotOff
    | EducatorBotOn EducatorBotParams
