-- "Probamon" (c) by Ignacio Slater M.

-- "Probamon" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0-b.1

module ProbamonTennis where

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

{-|-}
resultE3b :: Probability
resultE3b = undefined
