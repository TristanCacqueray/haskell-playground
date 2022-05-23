-- | A very simple circuit:
-- The first output is on when both input are on.
-- The second output is on when one of the input is on.
module Button where

import Clash.Annotations.TH
import Crelude hiding (either)

topEntity ::
  "BTN"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        ) ->
  "LED"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        )
topEntity (btn1, btn2) = (both, either)
  where
    both = (.&.) <$> btn1 <*> btn2
    either = (.|.) <$> btn1 <*> btn2

makeTopEntity 'topEntity
