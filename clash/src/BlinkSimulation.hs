{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- |
module BlinkSimulation where

import Blink (blinkingCircuit, blinkingSecond)
import Clash.Annotations.TH
import Clash.Prelude
import Clash.Signal qualified
import Data.List qualified as List
import RetroClash.Clock
import RetroClash.Utils (withResetEnableGen)

-- | A 10Hz clock for simulation
createDomain vSystem {vName = "Sim", vPeriod = hzToPeriod 10}

main :: IO ()
main = do
  _ <- traverse print led
  pure ()
  where
    inputs = clockGen @Sim
    outputs = topEntity inputs
    -- Get two seconds of samples
    led = List.drop 1 $ sampleN 21 outputs

topEntity ::
  "CLK" ::: Clock Sim ->
  "LED" ::: Signal Sim (Vec 16 Bit)
topEntity = withResetEnableGen $ blinkingCircuit
