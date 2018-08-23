module Dscp.TxPerf.Account where

import qualified Serokell.Util.Base64 as Base64

import Dscp.Core
import Dscp.Crypto
import Dscp.Witness.Web

data Account = Account
    { sk :: SecretKey
    , pk :: PublicKey
    , addr :: Address
    , nextNonce :: Integer
    , balance :: Int
    }
    deriving (Ord, Show)

instance Eq Account where
    a == b = addr a == addr b

accHasMoney :: Account -> Bool
accHasMoney = (> 0) . balance

--------------------------------------------------------------------------------

skToAcc :: WitnessClient -> SecretKey -> IO Account
skToAcc wc sk = do
    AccountInfo{..} <- wGetAccount wc addr False
    return Account
        { sk
        , pk
        , addr
        , nextNonce = aiNextNonce
        , balance = fromIntegral . unCoin $ bmTotal aiBalances
        }
  where
    pk = toPublic sk
    addr = mkAddr pk

textToSk :: Text -> SecretKey
textToSk =
    either (error . fromString) id . fromByteArray .
    either error id . Base64.decode

textToAcc :: WitnessClient -> Text -> IO Account
textToAcc wc = return . textToSk >=> skToAcc wc

genAcc :: WitnessClient -> IO Account
genAcc wc = keyGen >>= skToAcc wc . fst
