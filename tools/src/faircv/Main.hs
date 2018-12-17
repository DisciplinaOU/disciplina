
import Servant.Client

import Dscp.Educator.Web.Student.Client.Logic
-- import Dscp.Witness.Web.Client

import Client

main :: IO ()
main = do
    -- wClient <- createWitnessClient =<< parseBaseUrl "127.0.0.1:8013"
    -- fairCV  <- getFairCV
    -- res     <- checkFairCV wClient fairCV
    -- print res

    sClient <- createStudentApiClient =<< parseBaseUrl "127.0.0.1:8090"
    asses   <- getAssignments sClient
    print asses
