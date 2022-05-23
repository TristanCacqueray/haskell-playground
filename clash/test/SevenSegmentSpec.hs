module SevenSegmentSpec where

import Crelude
import Data.List qualified as L
import Data.List qualified as List
import SevenSegment
import Test.Hspec

showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) =
  unlines . L.concat $
    [ L.replicate 1 $ horiz a,
      L.replicate 3 $ vert f b,
      L.replicate 1 $ horiz g,
      L.replicate 3 $ vert e c,
      L.replicate 1 $ horiz d
    ]
  where
    horiz True = " ###### "
    horiz False = " ...... "
    vert b1 b2 = part b1 <> "     " <> part b2
      where
        part True = "#"
        part False = "."

-- |
-- >>> putStrLn $ showSS ss5
--  ######
-- #     .
-- #     .
-- #     .
--  ######
-- .     #
-- .     #
-- .     #
--  ######
ss5 :: Vec 7 Bool
ss5 = True :> False :> True :> True :> False :> True :> True :> Nil

spec :: Spec
spec =
  describe "Simulation" $ it "works" $ result `shouldBe` expected
  where
    showResult (dp, anodes', seg) =
      [ "DP: " <> show dp <> ", anodes: " <> show anodes'
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
    segmentValWithDP = List.zip3 dpsVal anodesVal segmentVal

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
