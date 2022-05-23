-- | A prelude for the project

module Crelude (
    module Clash.Prelude,
    Witch.from,
    Witch.From,
    Debug.Trace.trace,
    -- * RetroClash.Utils
    RetroClash.Utils.withResetEnableGen,
    RetroClash.Utils.Polarity (..),
    RetroClash.Utils.Active,
    RetroClash.Utils.fromActive,
  ) where

import Clash.Prelude
import Witch qualified
import Debug.Trace qualified
import RetroClash.Utils qualified
