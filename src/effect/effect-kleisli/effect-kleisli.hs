-- title       = Effect is Kleisli, Etymologically
-- pubDate     = 2025-11-26
-- tags        = effect-system, haskell, monad, 2025
-- description = It's pretty intriguing that the Effect type in this blog is exactly the traditional Kleisli type in haskell.
--               We'll further show that Monad is the constraint we need to form the valid Effect category.
--               If you are wondering about the meaning of effects in the haskell world, I believe this blog will help gain some understanding.

import           Data.Kind ( Type )

import           Prelude   hiding ( Just, Maybe, Monad, Nothing, id, return,
                             (<=<), (=<<), (>=>), (>>=) )

-- | The Effect type
--
-- When I was [perplexitying](https://www.perplexity.ai/search/explain-what-is-effect-in-hask-TS.zYSCtRdqyUfWYX1UhWw#0)
-- about effects in haskell, the result was disappointingly vague. A second attempt to
-- [define the effect type](https://www.perplexity.ai/search/define-the-effect-type-in-hask-Dc8z91r7SzOVQUjK.P011Q#0)
-- revealed that *effect is defined differently with respect to every different effect system.*
--
-- Thus speaking, the (precise) definition of effect in
-- [freer-simple](https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Control-Monad-Freer.html#g:1)
-- is different from that in [heftia](https://hackage-content.haskell.org/package/data-effects-core-0.4.2.0/docs/Data-Effect.html#t:Effect),
-- and again different from those in other effect systems.
--
-- In this post, we shall define the `Effect` type in a trivial effect system: an etymological one based on the Oxford English Dictionary.
--
-- || The Oxford definition of Effect
--
-- The Oxford Learners Dictionary [defines](https://www.oxfordlearnersdictionaries.com/definition/english/effect_1?q=effect) *effect* as:
--
-- > a change that somebody/something causes in somebody/something else
--
-- Parsing this structurally, we have:
-- - *a change*, or some transformation we'll call `m`
-- - *that something*, or an input value of type `a`
-- - *causes*, or function application `(->)`
-- - *in something else*, or an output value of type `b`
--
-- And the original definition can be rephrased to `a` causes `m` in `b`, or isomorphically in haskell: `a -> m b`.
--
-- This suggests our `Effect` type should be parametrised by three things: the transformation `m`, the input type `a`, the output type `b` & is essentially a morphism `a -> m b`.
--
-- Using the `runEffect` unwrapper/accessor, we can further make the semantics explicit:

newtype Effect m a b = Effect { runEffect :: a -> m b }

-- If you are familiar with haskell, you might have noticed that the `Effect` type here is exactly the traditional `Kleisli` type [^1] in haskell.

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

-- | The Effect Category
--
-- Having defined the Effect type, we naturally want to define the `Effect` category.
--
-- || Category Fundamentals
--
-- A category consists of objects & morphisms between objects.
--
-- To form a valid category, we need an identity morphism `id` for each object & a (binary) composition operator `.` for morphisms.
-- They must satisfy three laws: `f . id = f, id . f = f, f . (g . h) = (f . g) . h`.
--
-- For convenience, we define both the `(<=<)` & `(>=>)` operators here, where `(<=<)` is the traditional composition operator `(.)` in category theory, and `(>=>)` is the reverse composition operator.

class Category (cat :: Type -> Type -> Type) where
   id    :: cat a a
   (<=<) :: cat b c -> cat a b -> cat a c
   (>=>) :: cat a b -> cat b c -> cat a c

   (<=<) = flip (>=>)
   (>=>) = flip (<=<)

-- || The Monad Constraint
--
-- Note that it's the partially parametrised `Effect m` that matches the kind of `cat` instead of the barebones `Effect` itself.
-- Meaning we are modeling the effect of `m` as a category.
--
-- Substitute `cat` with `Effect m` & perform some further evaluations, we derive the actual type of `id` & `.`:
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- id :: Effect m a a
--     = a -> m a
--
-- (.) :: Effect m b c -> Effect m a b -> Effect m a c
--      = (b -> m c) -> (a -> m b) -> (a -> m c)
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- We'll use a figure to illustrate what's going on here:
--
-- @insert fig01.svg
--
-- There are 6 objects `a`, `b`, `c`, `m a`, `m b` & `m c`. The morphisms mentioned in `id` & `(.)` are added to the figure.
-- For convenience, we use `arr1`, `arr2`, etc. to indicate the morphisms.
--
-- It's clear that we need to somehow combine `arr2` & `arr3` to form a new morphism, then make this morphism match `arr4`.
-- However, the target of arr2 doesn't match the source of arr3 or vice versa. We need to map `arr2`/`arr3` into a continuous morphism,
-- connecting their target & source.
--
-- There are these 2 options:
-- - map `arr3` to `(a -> b)`, then compose it with `arr2` to form a new morphism, which is `arr4`.
-- - map `arr2` to `(m b -> m c)`, then compose it with `arr3` to form a new morphism, which is `arr4`.
--
-- According to the second law of thermodynamics, we prefer the second option (as we generally cannot extract a pure value from an effectful context). [^2]
--
-- The augmented figure below illustrates this mechanism. We use specific names instead of abstract `arrx` here to imply
-- they are highly related to monads.
--
-- - The `return` arrow lifts `a` to `m a`, serving as the identity morphism.
-- - The `(=<<)` arrow transforms our Effect arrow `arr2` (`b -> m c`) into the morphism `arr5` (`m b -> m c`).
-- - `arr4` is then formed by composing `arr3` and `arr5`.
--
-- @insert fig02.svg

-- And more importantly, this is precisely the Kleisli category for monad in category theory.
-- Every monad gives rise to a Kleisli category where:
-- - Objects are the same as the base category (Haskell types).
-- - Morphisms from `a` to `b` are functions of type `a -> m b`.
--
-- We then implement a typeclass as constraint for these two functions `return` & `=<<`, we call the typeclass `Monad`.

class Monad m where
   return :: a -> m a
   (=<<)  :: (a -> m b) -> (m a -> m b)
   (>>=)  :: m a -> (a -> m b) -> m b

   (>>=) = flip (=<<)
   (=<<) = flip (>>=)

instance Monad m => Category (Effect m) where
   id                        = Effect return
   (Effect f) >=> (Effect g) = Effect (\x -> g =<< f x)

-- || Proofs of Category Laws
--
-- Since we've already made `Monad` as constraint for `Effect`, we can assume the following Monad Laws as lemmas:
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- 1. Right Identity: m >>= return = m
-- 2. Left Identity: return x >>= f = f x
-- 3. Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- Here's the proof that the effect does form a valid category:
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- Right Identity: f . id = f
-- Effect f . Effect return
--    = Effect (\x -> return =<< f x)
--    = Effect (\x -> f x >>= return)           -- definition of (=<<)
--    = Effect (\x -> f x)                      -- Monad Right Identity
--    = Effect f
--
-- Left Identity: id . f = f
-- Effect return . Effect f
--    = Effect (\x -> f =<< return x)
--    = Effect (\x -> return x >>= f)            -- definition of (=<<)
--    = Effect (\x -> f x)                       -- Monad Left Identity
--    = Effect f
--
-- Associativity: f . (g . h) = (f . g) . h
-- Effect f . (Effect g . Effect h)
--    = Effect f . Effect (\y -> h =<< g y)
--    = Effect (\x -> (\y -> h =<< g y) =<< f x)
--    = Effect (\x -> f x >>= (\y -> g y >>= h))
--
-- (Effect f . Effect g) . Effect h
--    = Effect (\x -> g =<< f x) . Effect h
--    = Effect (\x -> h =<< (g =<< f x))
--    = Effect (\x -> (f x >>= g) >>= h)
--
-- These are equal by Monad Associativity: (m >>= g) >>= h = m >>= (\x -> g x >>= h)
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- In other words, Monad is the constraint for the Effect type to be a category.
--
-- And i finally understand [Gabriella's idea](https://www.haskellforall.com/2012/12/the-continuation-monad.html).
--
-- > I firmly believe that the way to a Monads heart is through its Kleisli arrows, and if you want to study a Monads "purpose" or "motivation"
--   you study what its Kleisli arrows do.

-- || Maybe, an Impractical Example
--
-- Let's define the [Maybe](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html) datatype to see how we can use the `Effect` category.

data Maybe a = Nothing | Just a deriving (Show)

instance Monad Maybe where
   return = Just

   Nothing  >>= _ = Nothing
   (Just x) >>= f = f x

-- Now we can define some effects.

safeDiv :: Int -> Effect Maybe Int Int
safeDiv 0 = Effect (\_ -> Nothing)
safeDiv y = Effect (\x -> Just (x `div` y))

-- And we can compose them using the `Effect` category:

testSafeDiv :: IO ()
testSafeDiv = do
   print $ runEffect (safeDiv 2 >=> Effect (\x -> Just (x + 1))) 10
   print $ runEffect (safeDiv 0 >=> Effect (\x -> Just (x + 1))) 10

-- | The Effect Arrow
--
-- Having established that `Effect` forms a valid Category (given a Monad), the next *natural* step is to see if
-- it forms an [Arrow](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Arrow.html).
--
-- It turns out we get this *for free*. Just as `Monad` was the key to the Category instance, the combination of `Category` and `Monad`
-- gives us everything we need to implement `Arrow`.

class Category a => Arrow a where
  arr   :: (b -> c) -> a b c
  first :: a b c -> a (b,d) (c,d)

instance Monad m => Arrow (Effect m) where
   arr f            = Effect (return . f)
   first (Effect f) = Effect $ \(a, c) -> (\x -> return (x, c)) =<< f a

-- The `arr` function allows us to lift any pure function into our Effect world. It essentially composes the pure function with `return`.
--
-- @insert fig03.svg
--
-- The `first` function handles side effects on the first component of a tuple while passing the second through unchanged.
-- This is where the monadic binding shines, allowing us to thread the context through the computation.
--
-- @insert fig04.svg

-- ^1. [Kleisli definition in Control.Arrow](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Control.Arrow.html#Kleisli)
-- ^2. If you [search](https://hoogle.haskell.org/?hoogle=%28a+-%3E+m+b%29+-%3E+%28a+-%3E+b%29&scope=set%3Astackage)
--     `(a -> m b) -> (a -> b)` on Hoogle, you won't get any result.
