-- |
module Solution where

import Control.Monad.Reader
import UnliftIO

testAction :: Handle -> ReaderT Int IO ()
testAction = undefined

testApp :: ReaderT Int IO ()
testApp = withFile "/tmp/test" ReadMode testAction
