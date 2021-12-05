{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Some of the solution from chapter 4.4
module Blink where

import Clash.Annotations.TH
import Clash.Prelude
import Data.Either
import Data.Maybe
import RetroClash.Clock
import RetroClash.Utils (succIdx, withResetEnableGen)

createDomain vSystem {vName = "Dom100", vPeriod = hzToPeriod 100_000_000}

data OnOff on off
  = On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On {} = True
isOn Off {} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On $ succIdx x
countOnOff (Off y) = maybe (On 0) Off $ succIdx y

blinkingSecond ::
  forall dom.
  (HiddenClockResetEnable dom, _) =>
  _ ->
  Signal dom Bit
blinkingSecond start = boolToBit . isOn <$> r
  where
    r ::
      Signal
        dom
        ( OnOff
            (ClockDivider dom (Milliseconds 500))
            (ClockDivider dom (Milliseconds 500))
        )
    r = register (Off start) $ countOnOff <$> r

type Ms d x = ClockDivider d (Milliseconds x)

blinkingCircuit ::
  forall dom.
  (HiddenClockResetEnable dom, _) =>
  Signal dom (Vec 16 Bit)
blinkingCircuit = bundle (half ++ reverse half)
  where
    half =
      blinkingSecond 0
        :> blinkingSecond (fromSNat $ SNat @(Ms dom 62))
        :> blinkingSecond (fromSNat $ SNat @(Ms dom 125))
        :> blinkingSecond (fromSNat $ SNat @(Ms dom 187))
        :> blinkingSecond (fromSNat $ SNat @(Ms dom 250))
        :> blinkingSecond (fromSNat $ SNat @(Ms dom 312))
        :> blinkingSecond (fromSNat $ SNat @(Ms dom 375))
        :> blinkingSecond (fromSNat $ SNat @(Ms dom 437))
        :> Nil

topEntity ::
  "CLK" ::: Clock Dom100 ->
  "LED" ::: Signal Dom100 (Vec 16 Bit)
topEntity = withResetEnableGen blinkingCircuit

makeTopEntity 'topEntity
