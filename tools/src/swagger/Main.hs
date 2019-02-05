import SwaggerOptions

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Swagger as S
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI
import Dscp.Educator.Web.Student.Swagger
import Dscp.Web.Swagger

main :: IO ()
main = do
    options <- getSwaggerOptions

    let swagger = case soSwaggerAPI options of
            StudentAPI -> studentAPISwagger

    let modSwagger = case soHost options of
            Nothing   -> id
            Just host -> S.host .~ Just (fromString host)

    let swagger' = modSwagger swagger
    let encoded = encodeSwagger swagger'

    case soOutput options of
        Nothing     -> LBS.hPut stdout encoded
        Just output -> LBS.writeFile output encoded

getSwaggerOptions :: IO SwaggerOptions
getSwaggerOptions =
    execParser $
        info (helper <*> versionOption <*> swaggerOptionsParser) $
        fullDesc <>
        progDesc "Disciplina swagger documentation generator."
