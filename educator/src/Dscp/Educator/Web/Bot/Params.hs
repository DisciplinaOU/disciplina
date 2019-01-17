module Dscp.Educator.Web.Bot.Params
    ( EducatorBotParams
    , EducatorBotParamsRec
    , EducatorBotParamsRecP

    , EducatorBotConfig
    , EducatorBotConfigRec
    , EducatorBotConfigRecP
    ) where

import Loot.Config ((::+), (::-), (:::), Config, PartialConfig)
import Time.Units (Microsecond, Time)

-- | Which params to use when launching bot.
type EducatorBotParams =
   '[ "seed" ::: Text
      -- Seed to generate initial data (assignments, ...).
    , "operationsDelay" ::: Time Microsecond
      -- Artificial delay in bot operations.
    ]

type EducatorBotParamsRec = Config EducatorBotParams
type EducatorBotParamsRecP = PartialConfig EducatorBotParams

-- | Configuration to lauch a bot (or not)
-- Note, there is a reason this contains the whole tree and not just its content
-- see 'ConfigMaybe' for an explanation.
type EducatorBotConfig =
   '[ "params" ::+
       '[ "disabled" ::- '[]
        , "enabled"  ::- EducatorBotParams
        ]
    ]

type EducatorBotConfigRec = Config EducatorBotConfig
type EducatorBotConfigRecP = PartialConfig EducatorBotConfig
