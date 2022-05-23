module BlinkSpec where

import Blink (blinkingCircuit)
import Crelude
import Data.List qualified as List
import Test.Hspec

-- | A 10Hz clock for simulation
createDomain vSystem {vName = "Sim", vPeriod = hzToPeriod 10}

topEntity ::
  "CLK" ::: Clock Sim ->
  "LED" ::: Signal Sim (Vec 16 Bit)
topEntity = withResetEnableGen $ blinkingCircuit

prettyVec :: Vec x Bit -> String
prettyVec = \case
  Nil -> ""
  Cons b x -> show b <> prettyVec x

spec :: Spec
spec =
  describe "Simulation" $ it "blink" $ result `shouldBe` expected
  where
    inputs = clockGen @Sim
    outputs = topEntity inputs
    -- Get two seconds of samples
    led = List.drop 1 $ sampleN 21 outputs
    result = fmap prettyVec led
    expected =
      [ "0000000000000000",
        "0000000110000000",
        "0000011111100000",
        "0000111111110000",
        "0011111111111100",
        "1111111111111111",
        "1111111001111111",
        "1111100000011111",
        "1111000000001111",
        "1100000000000011",
        "0000000000000000",
        "0000000110000000",
        "0000011111100000",
        "0000111111110000",
        "0011111111111100",
        "1111111111111111",
        "1111111001111111",
        "1111100000011111",
        "1111000000001111",
        "1100000000000011"
      ]
