-- Demonstrate record and functor style programing

Note: this file is a literate haskell source file, the code example are prefixed by `>`
Interactive examples are prefixed by two spaces '  '


-- Haskell record 101

> data Person
>  = Person
>      { name :: String,
>        age :: Integer
>      }

The first `Person` is a data type, use `:i` in the REPL to check what it is:

  :i Person
  data Person = Person {name :: String, age :: Integer}

The second `Person` is a data constructor, use `:t` in the REPL

  :t Person
  Person :: String -> Integer -> Person

For example, to create a person:

> aPerson :: Person           -- Person here is the type
> aPerson = Person "John" 42  -- Person here is the constructor

To access the name or the age, we don't use `person.age` notation, instead we use function application:

  :t age
  age :: Person -> Integer
  :t name
  name :: Person -> String

Each attribute of a record is actually a function. Here is an usage example:

> greet :: Person -> String
> greet person = "Hello " <> name person <> "!"

  *Main> greet aPerson
  Hello John!


Here is a function to create a record interactively:

> getPerson :: IO Person
> getPerson = do
>   putStr "name? "
>   name <- getLine
>   putStr "age? "
>   age <- getLine
>   pure $ Person name (read age)

Note: we have to use `pure` here to lift the result back in the IO.

-- Functor 101

Functor provides a convenient operator named `fmap`, or in infix notation `<$>`:

  :t fmap
  fmap :: Functor f => (a -> b) -> f a -> f b

This let us apply a function over Functor type like IO.
For example, let's say we want the list of words from the input. We could write:

> getWords :: IO [String]
> getWords = do
>   line <- getLine
>   pure $ words line

Here are the type signatures:

  :t getLine
  getLine :: IO String
  :t words
  words   :: String -> [String]

IO is a Functor, thus fmap signature is also:

  fmap    :: (a -> b) -> IO a -> IO b

Then here is what happen when using fmap:

  :t (fmap words)
  (fmap words) :: IO String -> IO [String]

Thus getWords can also be written like so:

> getWords' :: IO [String]
> getWords' = fmap words getLine

Or using infix notation:

> getWords'' :: IO [String]
> getWords'' = words <$> getLine

Using fmap, we don't have to bind intermediate result (e.g. the `line` place holder for the getLine output is no longer necessary).


-- Record and functor

Let's use our newly acquire knowledge to re-write the getPerson function using functor:

> getAge :: IO Integer
> getAge = do
>   putStr "age? "
>   read <$> getLine

> getName :: IO String
> getName = do
>   putStr "name? "
>   getLine

However we can't directly do:

  Person <$> getName <$> getAge
  * Couldn't match expected type ‘Integer -> b’
                with actual type ‘IO (Integer -> Person)’

Because (Person <$> getName) is already in the IO functor:

  :t (Person <$> getName)
  (Person <$> getName) :: IO (Integer -> Person)

Thus we need to use the Applicative operator named `<*>`:

  :t (<*>)
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b

In other words, here is what we currently have:

  Person :: String -> Integer -> Person
  (<$>) :: (a -> b) -> IO a -> IO b
  getName :: IO String

  (Person <$> getName) :: IO (Integer -> Person)
  (<*>) :: IO (a -> b) -> IO a -> IO b
  getAge :: IO Integer

Thus we can combine all of that:

> getPerson' :: IO Person
> getPerson' = Person <$> getName <*> getAge

> main :: IO ()
> main = greet <$> getPerson' >>= putStrLn
