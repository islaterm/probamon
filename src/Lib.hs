-- "Probamon" (c) by Ignacio Slater M.

-- "Probamon" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0.0-rc.1

module Lib where

import           Prelude
import           Numeric.Probability.Distribution
import           Data.Ratio

{-| Type of number to use for probabilities -}
type Probability = Rational
{-| Probability distribution  -}
type Dist a = T Probability a
{-| Representation of an urn as a pair with the number of participants of each university-}
type Urn = (Integer, Integer)
{-| Universities of the participants  -}
data Uni = Chile | Cato deriving (Eq, Ord, Show)

{-| Definition of a Bernoulli experiment  -}
bern :: Probability -> Dist Bool
bern p = choose p True False

{-| Selects a player at random from a university  -}
pickPlayer :: Urn -> Dist (Uni, Urn)
pickPlayer (uch, uc) = choose (uch % (uc + uch)) chile cato
 where
  chile = (Chile, (uch - 1, uc))
  cato  = (Cato, (uch, uc - 1))

{-| Calculates the probability that 2 UC students match in a single. -}
matchUC :: Urn -> Probability
matchUC (0  , uc) = if uc > 1 then 1 else 0
matchUC (uch, uc) = 2 * if uc > 1
  then
    ((== (Cato, (uch, uc - 1))) ?? pickPlayer (uch, uc))
      * ((== (Cato, (uch, uc - 2))) ?? pickPlayer (uch, uc - 1))
  else 0


{-| Calculates the probability that 2 UC students doesn't match in a single from 5 matches. -}
resultE3b :: Probability
resultE3b = 1 - repMatch (8, 2)
 where
  repMatch (uch, uc) =
    matchUC (uch, uc)
      + (if uch - 2 > 0 then repMatch (uch - 2, uc) else 0)
      + (if uch - 1 > 0 && uc - 1 > 0 then repMatch (uch - 1, uc - 1) else 0)
