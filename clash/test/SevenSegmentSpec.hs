module SevenSegmentSpec where

import Clash.Annotations.TH
import Clash.Prelude
import Clash.Signal qualified
import Data.List qualified as L
import Data.List qualified as List
import RetroClash.Clock
import RetroClash.Utils
import SevenSegment
import Test.Hspec

spec :: Spec
spec =
  describe "Simulation" $ it "works" $ result `shouldBe` expected
  where
    showResult (dp, anodes, seg) =
      [ "DP: " <> show dp <> ", anodes: " <> show anodes
      ]
        <> lines (showSS $ seg)
    -- Generate inputs from 0 to 10 with the second segment set
    inputsVal :: [Vec 4 Bit]
    inputsVal = int2v <$> [0 .. 10]
    inputs = (\v -> 0 :> 0 :> 1 :> 0 :> v) <$> inputsVal
    -- Collect streams
    (anodes, segments, dps) = topEntity $ fromList inputs
    -- Collect the 5 first segment signal values
    count = 10
    segment :: [Vec 7 (Active Low)]
    segment = sampleN count segments
    dpsVal :: [Bool]
    dpsVal = fromActive <$> sampleN count dps
    anodesVal :: [Vec 4 Bool]
    anodesVal = map fromActive <$> sampleN count anodes
    segmentVal :: [Vec 7 Bool]
    segmentVal = map fromActive <$> segment

    segmentValWithDP :: [(Bool, Vec 4 Bool, Vec 7 Bool)]
    segmentValWithDP = L.zip3 dpsVal anodesVal segmentVal

    result = fmap showResult segmentValWithDP
    expected =
      [ [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          "#     #",
          "#     #",
          "#     #",
          " ...... ",
          "#     #",
          "#     #",
          "#     #",
          " ###### "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ...... ",
          ".     #",
          ".     #",
          ".     #",
          " ...... ",
          ".     #",
          ".     #",
          ".     #",
          " ...... "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          ".     #",
          ".     #",
          ".     #",
          " ###### ",
          "#     .",
          "#     .",
          "#     .",
          " ###### "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          ".     #",
          ".     #",
          ".     #",
          " ###### ",
          ".     #",
          ".     #",
          ".     #",
          " ###### "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ...... ",
          "#     #",
          "#     #",
          "#     #",
          " ###### ",
          ".     #",
          ".     #",
          ".     #",
          " ...... "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          "#     .",
          "#     .",
          "#     .",
          " ###### ",
          ".     #",
          ".     #",
          ".     #",
          " ###### "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          "#     .",
          "#     .",
          "#     .",
          " ###### ",
          "#     #",
          "#     #",
          "#     #",
          " ###### "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          ".     #",
          ".     #",
          ".     #",
          " ...... ",
          ".     #",
          ".     #",
          ".     #",
          " ...... "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          "#     #",
          "#     #",
          "#     #",
          " ###### ",
          "#     #",
          "#     #",
          "#     #",
          " ###### "
        ],
        [ "DP: False, anodes: False :> False :> True :> False :> Nil",
          " ###### ",
          "#     #",
          "#     #",
          "#     #",
          " ###### ",
          ".     #",
          ".     #",
          ".     #",
          " ###### "
        ]
      ]
