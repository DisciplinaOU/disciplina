{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (State, list)

import Control.Arrow ((&&&), (***))
import Data.List ((!!))

-- | Buyer decisions.
data Input
    = Timeout
    | StartTrade
    | AcceptEncrypted
    | Handshake
    | Reject
    | Cancel
    deriving (Eq, Show)

-- | State of trade.
data Label
    = Created
    | Transmitted
    | KeySent
    | Arbitration
    | Agreed
    | SellerGetsAll
    | BuyerGetsAll
    | Cancelled
    deriving (Eq, Show)

-- | Seller decisions.
type State = [Problem]

-- | Different problems Seller can create.
data Problem
    = Garbage
    | PlaintextWasGarbage
    | WrongSecretKey
    | SecretKeyWasNotSent
    | DataNeverSent
    deriving (Eq, Show, Enum, Bounded)

-- | Generate all possible Seller decision sets.
allKindOf :: Bounded a => Enum a => [[a]]
allKindOf =
    [[]] ++ concatMap (\n -> subsequencesOfSize n [minBound.. maxBound]) [1.. 4]
  where
    -- Taken from https://stackoverflow.com/questions/21265454/subsequences-of-length-n-from-list-performance/21288092#21288092
    -- seems to work properly.
    subsequencesOfSize :: Int -> [a] -> [[a]]
    subsequencesOfSize n xs = let l = length xs
                            in if n>l then [] else subsequencesBySize xs !! (l-n)
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                                in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

-- | Scene I: test 'evilBuyer' against all possible problem sets.
evilBuyerTrades :: [(State, [Outcome])]
evilBuyerTrades =
    let
      outcomeOf = contract evilBuyer
      outcomes  = allKindOf |> map (id &&& id &&& outcomeOf)
      reports   = outcomes  |> map (id *** getMoral)
      reports'  = reports   |> map (id *** decide True)
      badOnes   = reports'  |> filter (\(_, moral) -> not $ null moral)
    in
      badOnes

-- | Scene II: test 'goodBuyer' against all possible problem sets.
goodBuyerTrades :: [(State, [Outcome])]
goodBuyerTrades =
    let
      outcomeOf = contract goodBuyer
      outcomes  = allKindOf |> map (id &&& id &&& outcomeOf)
      reports   = outcomes  |> map (id *** getMoral)
      reports'  = reports   |> map (id *** decide False)
      badOnes   = reports'  |> filter (\(_, moral) -> not $ null moral)
    in
      badOnes

-- | Derive moral choices from the outcome.
getMoral :: ([Problem], Label) -> (Bool, Bool, Bool, Bool, Bool)
getMoral (i, o) =
    (buyerGotData i, buyerLostFee o, sellerLostFee o, sellerGotMoney o, sellerWasEvil i)

-- | Moral choices phasified.
data Outcome
  = EvilSellerNotPunished
  | EvilBuyerNotPunished
  | GoodSellerPunished
  | GoodBuyerPunished
  | UnfairTrade
  deriving (Eq, Show)

-- | Get all things that are wrong.
decide :: Eq a => Bool -> (a, Bool, Bool, a, Bool) -> [Outcome]
decide evilB (dat, punB, punS, mon, evilS) =
    concat
        [ [EvilBuyerNotPunished  | evilB && not punB && dat /= mon]
        , [GoodBuyerPunished     | not evilB && punB]
        , [EvilSellerNotPunished | evilS && not punS && dat /= mon]
        , [GoodSellerPunished    | not evilS && punS]
        , [UnfairTrade           | dat /= mon]
        ]

-- | Check if Seller had evil intent.
sellerWasEvil ::
  (Element t ~ Problem, Container t,
   ElementConstraint t (Element t)) =>
  t -> Bool
sellerWasEvil list
  -- | These two terminate trade outright and can be connection failures.
  | DataNeverSent `elem` list = False
  | Garbage       `elem` list = False
  -- | Remaining are witnesses of the evil will.
  | otherwise                 = not $ null list

-- | Check if buyer got her data.
buyerGotData :: [Problem] -> Bool
buyerGotData = null  -- any problem prevens for now

-- | Check if buyer lost her fee.
buyerLostFee :: Label -> Bool
buyerLostFee SellerGetsAll = True
buyerLostFee _             = False

-- | Check if seller lost his fee.
sellerLostFee :: Label -> Bool
sellerLostFee BuyerGetsAll = True
sellerLostFee _            = False

-- | Check if seller got his money.
sellerGotMoney :: Label -> Bool
sellerGotMoney label =
    label `elem`
      [ SellerGetsAll
      , Agreed
      ]

type AI = (Label, State) -> Input

infixl 1 |>
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

-- | Test some buyer AI against given set of problems.
contract :: AI -> State -> Label
contract buyer problems = loop Created
  where
    loop st = case st of
      Created ->
        buyer (st, problems) |> \case
          StartTrade -> loop Transmitted
          _          -> error "Not implemented"

      Transmitted -> do
        buyer (st, problems) |> \case
          AcceptEncrypted
            | Garbage `elem` problems ->      Cancelled
            | otherwise               -> loop KeySent
          Timeout -> Cancelled
          _ -> error "Not implemented"

      KeySent -> do
        buyer (st, problems) |> \case
          Reject    -> loop Arbitration
          Handshake ->      Agreed
          Timeout   ->      SellerGetsAll
          _         -> error "Not implemented"

      Arbitration
        |  PlaintextWasGarbage `elem` problems
        || WrongSecretKey      `elem` problems ->
          BuyerGetsAll

        | otherwise ->
          SellerGetsAll

      _ -> error "We didn't yet think so far"

-- | An AI for good buyer.
goodBuyer :: AI
goodBuyer (input, problems) = case input of
  Created -> StartTrade
  Transmitted
    | DataNeverSent `elem` problems -> Timeout
    | Garbage       `elem` problems -> Timeout
    | otherwise                     -> AcceptEncrypted

  KeySent
    | PlaintextWasGarbage `elem` problems -> Reject
    | WrongSecretKey      `elem` problems -> Reject
    | SecretKeyWasNotSent `elem` problems -> Timeout
    | otherwise                           -> Handshake

  _ -> error "Not implemented"


-- | An AI for evil buyer.
evilBuyer :: AI
evilBuyer (input, problems) = case input of
  Created -> StartTrade
  Transmitted
    | DataNeverSent `elem` problems -> Timeout
    | Garbage       `elem` problems -> Timeout
    | otherwise                     -> AcceptEncrypted

  KeySent -> Reject
  _ -> error "Not implemented"

main :: IO ()
main = do
    putTextLn "Good buyer:"
    forM_ goodBuyerTrades $ \(problems, badThings) -> do
        when (not $ null badThings) $ do
            putStrLn (show problems ++ " -> " ++ show badThings)

    putTextLn "Evil buyer:"
    forM_ evilBuyerTrades $ \(problems, badThings) -> do
        when (not $ null badThings) $ do
            putStrLn (show problems ++ " -> " ++ show badThings)
