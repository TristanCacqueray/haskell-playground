-- | Binary Coded Decimal
module BCD (addBCD, subBCD, module RetroClash.BCD) where

import Crelude
import RetroClash.BCD

fromDigit :: Digit -> Unsigned 4
fromDigit = bitCoerce

addBCD :: BCD n -> BCD n -> BCD n
addBCD xs ys = snd . mapAccumR addDigit False $ zip xs ys

subBCD :: BCD n -> BCD n -> BCD n
subBCD xs ys = snd . mapAccumR subDigit False $ zip xs ys
  where
    subDigit :: Bool -> (Digit, Digit) -> (Bool, Digit)
    subDigit b (x, y) = (b', fromIntegral z')
      where
        z = sub (fromDigit x) (fromDigit y) - if b then 1 else 0
        (b', z') = if z <= 9 then (False, z) else (True, z + 10)

addDigit :: Bool -> (Digit, Digit) -> (Bool, Digit)
addDigit c (x, y) = (c', fromIntegral z')
  where
    z :: Unsigned 5
    z = add (fromDigit x) (fromDigit y) + if c then 1 else 0
    (c', z') = if z <= 9 then (False, z) else (True, z - 10)
