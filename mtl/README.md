Monad does not compose by default.
Using 'transformers' we can wrap one monad inside another.

So instead of having: `Maybe a`, we have `MaybeT m a` where 'm' is the inner monad.

Then 'mtl' provide convenient wrapping monad
