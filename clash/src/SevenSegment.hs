-- | Implement the first exercise solution from chapter 3.4.2
module SevenSegment where

import Clash.Annotations.TH
import Clash.Prelude
import qualified Data.List as L
import RetroClash.Utils

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

-- |
-- >>> putStrLn $ showSS $ encodeHexSS 0x6
--  ######
-- #     .
-- #     .
-- #     .
--  ######
-- #     #
-- #     #
-- #     #
--  ######
encodeHexSS :: Unsigned 4 -> Vec 7 Bool
encodeHexSS n = unpack $ case n of
  -- abcdefg
  0x0 -> 0b1111110
  0x1 -> 0b0110000
  0x2 -> 0b1101101
  0x3 -> 0b1111001
  0x4 -> 0b0110011
  0x5 -> 0b1011011
  0x6 -> 0b1011111
  0x7 -> 0b1110000
  0x8 -> 0b1111111
  0x9 -> 0b1111011
  0xa -> 0b1110111
  0xb -> 0b0011111
  0xc -> 0b1001110
  0xd -> 0b0111101
  0xe -> 0b1001111
  0xf -> 0b1000111

int2v :: Integer -> Vec 4 Bit
int2v = bv2v . fromInteger

-- |
-- >>> testOutput
-- DP: False, anodes: <False,False,True,False>
--  ######
-- #     #
-- #     #
-- #     #
--  ......
-- #     #
-- #     #
-- #     #
--  ######
--
-- DP: False, anodes: <False,False,True,False>
--  ......
-- .     #
-- .     #
-- .     #
--  ......
-- .     #
-- .     #
-- .     #
--  ......
testOutput :: IO ()
testOutput = do
  _ <- traverse showResult segmentValWithDP
  pure ()
  where
    showResult (dp, anodes, seg) = do
      putStrLn $ "DP: " <> show dp <> ", anodes: " <> show anodes
      putStrLn . showSS $ seg
    -- Generate inputs from 0 to 10 with the second segment set
    inputsVal :: [Vec 4 Bit]
    inputsVal = int2v <$> [0 .. 10]
    inputs = (\v -> 0 :> 0 :> 1 :> 0 :> v) <$> inputsVal
    -- Collect streams
    (anodes, segments, dps) = topEntity $ fromList inputs
    -- Collect the 5 first segment signal values
    count = 5
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

-- | The first 4 switch activates the anodes, the next 4 sets the value
topEntity ::
  "SWITCHES" ::: Signal System (Vec 8 Bit) ->
  "SS"
    ::: ( "AN" ::: Signal System (Vec 4 (Active High)),
          "SEG" ::: Signal System (Vec 7 (Active Low)),
          "DP" ::: Signal System (Active Low)
        )
topEntity sws =
  ( map toActive <$> anodes,
    map toActive <$> segments,
    toActive <$> dp
  )
  where
    anodes = bitCoerce . fst <$> swsSplit
    swsSplit = splitAt (SNat :: SNat 4) <$> sws
    digit = bitCoerce . snd <$> swsSplit
    segments = encodeHexSS <$> digit
    dp = pure False

makeTopEntity 'topEntity
