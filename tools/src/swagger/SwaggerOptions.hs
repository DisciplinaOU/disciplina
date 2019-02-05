{-# LANGUAGE ApplicativeDo #-}

module SwaggerOptions
       ( SwaggerAPI (..)
       , SwaggerOptions (..)
       , swaggerOptionsParser
       ) where

import qualified Data.Text as T
import Options.Applicative (Parser, ReadM, eitherReader, help, long, metavar, option, short,
                            strOption)

import Dscp.Util.Constructors

-- | All our APIs.
data SwaggerAPI
    = StudentAPI
    deriving (Generic)

-- | APIs names, should correspond to 'swaggerAPIReadM'.
swaggerAPINames :: Text
swaggerAPINames =
    T.intercalate ", " $ map show $
    enlistConstructors @UnsafeFiller <&> \case
        StudentAPI -> "student" :: Text

swaggerAPIReadM :: ReadM SwaggerAPI
swaggerAPIReadM = eitherReader $ \case
    "student" -> pure StudentAPI
    other -> fail $ "Unknown API name " <> show other <>
                    ", allowed values: " <> toString swaggerAPINames

data SwaggerOptions = SwaggerOptions
    { soSwaggerAPI :: SwaggerAPI
    , soOutput     :: Maybe FilePath
    , soHost       :: Maybe String
    }

swaggerOptionsParser :: Parser SwaggerOptions
swaggerOptionsParser = do
    soSwaggerAPI <- option swaggerAPIReadM $ mconcat
        [ long "api"
        , metavar "NAME"
        , help $ "Name of API to produce. Allowed values: " <>
                 toString swaggerAPINames
        ]
    soOutput <- optional . strOption $ mconcat
        [ short 'o'
        , long "output"
        , metavar "FILEPATH"
        , help "Print the swagger specification to the given file. \
               \If not specified, the specification will be printed to stdout"
        ]
    soHost <- optional . strOption $ mconcat
        [ long "host"
        , metavar "ADDR:PORT"
        , help "Override the default value for 'host' field. \
               \Useful when documenation is going to be served in Swagger UI \
               \and requests via it should refer to a given server."
        ]
    return SwaggerOptions{..}
