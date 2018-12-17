
import Servant.Client

import Dscp.Witness.Web.Client

import Client

main :: IO ()
main = do
    wClient <- createWitnessClient =<< parseBaseUrl "127.0.0.1"
    fairCV  <- getFairCV
    res     <- checkFairCV wClient fairCV
    print res
