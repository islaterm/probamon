module Utils where

import           Prelude
import           Numeric.Probability.Distribution
import           Data.Ratio

{-| Type of number to use for probabilities -}
type Probability = Rational
{-| Probability distribution  -}
type Dist a = T Probability a
{-| Datatype representing a coin  -}
data CoinSide = H | T deriving (Eq, Ord, Show)

{-| Simulates the behaviour of a dice -}
dice :: Dist Int
dice = uniform [1 .. 6]

{-| Simulates the behaviour of throwing a coin  -}
coin :: Dist CoinSide
coin = uniform [H, T]

{-| Definition of a Bernoulli experiment  -}
bern :: Probability -> Dist Bool
bern p = choose p True False

{-| Generates a list of coin throws -}
coins :: Int -> Dist [CoinSide]
coins 0 = pure []
coins n = (:) <$> coin <*> coins (n - 1)

{-| Simulates the experiment of throwing a coin -}
resampleIfHead :: Dist CoinSide
resampleIfHead = coin >>= f where
  f T = return T
  f H = coin