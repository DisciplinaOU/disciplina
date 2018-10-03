module Test.Dscp.Witness.SerialiseSpec where

import Dscp.Util.Test
import Dscp.Witness

spec :: Spec
spec = do
    describe "Witness datatypes JSON serialisation" $ do
        aesonRoundtripProp @(BlocksOrMempool ())
        aesonRoundtripProp @TxInfo
        aesonRoundtripProp @PrivateBlockInfoPart
        aesonRoundtripProp @(PaginatedList "mem" ())
        aesonRoundtripProp @TxList
        aesonRoundtripProp @PrivateBlockList
        aesonRoundtripProp @HashIs
        aesonRoundtripProp @BlockList
        aesonRoundtripProp @BlockInfo
        aesonRoundtripProp @AccountInfo
        aesonRoundtripProp @ATGChange
        aesonRoundtripProp @ATGSubjectChange
    describe "Witness datatypes HttpApiData serialisation" $ do
        httpApiRoundtripProp @TxTypeFilter
