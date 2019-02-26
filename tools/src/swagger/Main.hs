import SwaggerOptions

import qualified Data.ByteString.Lazy as LBS
import Data.Tagged (untag)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI
import Dscp.Educator.Web.Educator.Swagger
import Dscp.Educator.Web.Student.Swagger
import Dscp.MultiEducator.Web.Swagger
import Dscp.Web.Swagger

main :: IO ()
main = do
    options <- getSwaggerOptions
    let mhost = soHost options

    let swagger = case soSwaggerAPI options of
            EducatorAPI      -> untag $ educatorAPISwagger mhost
            StudentAPI       -> untag $ studentAPISwagger mhost
            MultiEducatorAPI -> untag $ multiEducatorAPISwagger mhost
            MultiStudentAPI  -> untag $ multiStudentAPISwagger mhost
            CertificatesAPI  -> untag $ certificatesAPISwagger mhost

    let encoded = encodeSwagger swagger

    case soOutput options of
        Nothing     -> LBS.hPut stdout encoded
        Just output -> LBS.writeFile output encoded

getSwaggerOptions :: IO SwaggerOptions
getSwaggerOptions =
    execParser $
        info (helper <*> versionOption <*> swaggerOptionsParser) $
        fullDesc <>
        progDesc "Disciplina swagger documentation generator."
