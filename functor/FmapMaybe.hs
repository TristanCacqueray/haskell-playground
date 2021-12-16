-- | The goal is to reproduce such python code in Haskell:
--
-- ```
-- def get_builds(max_id):
--    # here max_id is either None or int
--    ...
--
-- def builds(max_id):
--    # here max_id is either None or str
--    _idx_max = idx_max is not None and int(idx_max) or idx_max
--    return get_builds(max_id)
-- ```
module FmapMaybe where

-- | A data type to model optional value (it's Maybe in base)
data Optionel a
  = Rien
  | Juste a
  deriving (Show)

-- | simple builds implementation without functor
builds :: Optionel String -> [Build]
builds max_idx =
  let _idx_max = case max_idx of
        Rien -> Rien
        Juste x -> Juste (read x)
   in get_builds _idx_max

-- | builds implementation using functor
instance Functor Optionel where
  fmap f x = case x of
    Rien -> Rien
    Juste a -> Juste (f a)

builds' :: Optionel String -> [Build]
builds' max_idx = get_builds (fmap read max_idx)

get_builds :: Optionel Int -> [Build]
get_builds _ = []

type Build = String
