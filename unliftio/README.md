When IO is in negative position, e.g. `withFunc :: (Handle -> IO a) -> IO a` then it is not possible to
use a transformer such as ReaderT.

Unliftio provides a new type class to work around this issue
