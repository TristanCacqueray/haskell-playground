module BCDSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Crelude hiding ((^))
import BCD
import Prelude ((^))

spec :: Spec
spec = do
  prop "roundtrip" $ prop_BCD @4
  prop "add" $ prop_add @5
  prop "sub" $ prop_sub @8

infix 4 ~=
(~=) :: forall n. (KnownNat n) => BCD n -> Integer -> Bool
x ~= y = fromBCD x == y `mod` magnitude
  where
    magnitude :: Integer
    magnitude = 10 ^ natVal (SNat @n)

prop_add :: forall n. (KnownNat n) => BCD n -> BCD n -> Bool
prop_add x y = addBCD x y ~= (fromBCD x + fromBCD y)

prop_sub :: forall n. (KnownNat n) => BCD n -> BCD n -> Bool
prop_sub x y = subBCD x y ~= (fromBCD x - fromBCD y)
