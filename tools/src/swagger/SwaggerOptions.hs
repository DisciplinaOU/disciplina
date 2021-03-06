{-# LANGUAGE ApplicativeDo #-}

module SwaggerOptions
       ( SwaggerAPI (..)
       , SwaggerOptions (..)
       , swaggerOptionsParser
       ) where

import Data.Default (Default)
import qualified Data.Map as M
import qualified Data.Text as T
import Options.Applicative (Parser, ReadM, eitherReader, help, long, metavar, option, short,
                            strOption)

import Dscp.Util
import Dscp.Util.Constructors
import Dscp.Web.Types

-- | All our APIs.
data SwaggerAPI
    = StudentAPI
    | EducatorAPI
    | MultiEducatorAPI
    | MultiStudentAPI
    | CertificatesAPI
    deriving (Generic)

allSwaggerAPIs :: Map Text SwaggerAPI
allSwaggerAPIs =
    M.fromList . map (apiName &&& id) $ enlistConstructors @Default
  where
    apiName = \case
        StudentAPI -> "student"
        EducatorAPI -> "educator"
        MultiEducatorAPI -> "multi-educator"
        MultiStudentAPI -> "multi-student"
        CertificatesAPI -> "certificates"

-- | APIs names, should correspond to 'swaggerAPIReadM'.
swaggerAPINames :: Text
swaggerAPINames = T.intercalate ", " . map show $ M.keys allSwaggerAPIs

swaggerAPIReadM :: ReadM SwaggerAPI
swaggerAPIReadM = eitherReader $ \name ->
    M.lookup (toText name) allSwaggerAPIs
        & nothingToFail ("Unknown API name " <> show name <> ", " <>
                         "allowed values: " <> swaggerAPINames)

data SwaggerOptions = SwaggerOptions
    { soSwaggerAPI :: SwaggerAPI
    , soOutput     :: Maybe FilePath
    , soHost       :: Maybe NetworkAddress
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
    soHost <- optional . option (eitherReader parseNetAddr) $ mconcat
        [ long "host"
        , metavar "HOST:PORT"
        , help "Override the default value for 'host' field. \
               \Useful when documenation is going to be served in Swagger UI \
               \and requests via it should refer to a given server."
        ]
    return SwaggerOptions{..}
