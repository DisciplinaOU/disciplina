-- | Coin type
module Dscp.Core.Foundation.Coin
    (
      Coin (..)
    , _Coin
    , unsafeMkCoin
    , coinToInteger
    , coinFromInteger
    , unsafeAddCoin
    , unsafeParseCoin
    , parseCoin
    , addCoins
    , sumCoins
    ) where

import Universum
import Control.Lens (makePrisms)

import Fmt (Buildable (..), (|+))
import Data.Fixed (Micro, Fixed(MkFixed), showFixed)

import Dscp.Util (leftToPanic)

-- | Coin amount.
newtype Coin = Coin { unCoin :: Word64 }
    deriving (Eq, Ord, Show, Generic, Hashable, Bounded)

makePrisms ''Coin

-- | Add coins.
unsafeAddCoin :: Coin -> Coin -> Coin
unsafeAddCoin a b = case coinFromInteger $ coinToInteger a + coinToInteger b of
    Left e  -> error $ "unsafeCoin failed: " <> show (a,b) <> " " <> e
    Right x -> x

-- | Safely convert coin to integer.
coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unCoin

-- | Restore coin from integer.
coinFromInteger :: Integer -> Either Text Coin
coinFromInteger i
    | i < 0
        = Left "Negative coin amount"
    | i > fromIntegral (unCoin maxBound)
        = Left "Coin amount is too high"
    | otherwise
        = Right (Coin $ fromIntegral i)

-- | Same as 'coinFromInteger', but errors if Left happens.
unsafeMkCoin :: Integral i => i -> Coin
unsafeMkCoin = either error id . coinFromInteger . fromIntegral

-- | Safely parse coin from Text.
parseCoin :: Text -> Either Text Coin
parseCoin = \coin -> case (readEither @Text @Micro $ coin) of
                     Right (MkFixed value) -> coinFromInteger value
                     Left _ -> Left "expected coin (Micro)"

-- | Same as 'parseCoin', but errors if Left happens.
unsafeParseCoin :: Text -> Coin
unsafeParseCoin = leftToPanic . parseCoin

instance Buildable Coin where
    build (Coin c) = showFixed True (MkFixed $ fromIntegral c :: Micro) |+ " DSCP"

addCoins :: Coin -> Coin -> Either Text Coin
addCoins a b = sumCoins [a, b]

sumCoins :: [Coin] -> Either Text Coin
sumCoins = coinFromInteger . sum . map coinToInteger
