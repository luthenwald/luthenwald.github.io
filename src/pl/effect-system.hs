{-
title       = An effect is a kleisli morphism, etymologically.
pubDate     = 2025-11-tbd
tags        = effect-system, haskell, monad, category-theory
description = 
-}

import Data.Kind (Type)
import Prelude hiding (Functor, Monad, id, (.), fmap, return, join)

-- | Discovering the Kleisli Category Through Etymology
-- 
-- When I was [perplexitying](https://www.perplexity.ai/search/explain-what-is-effect-in-hask-TS.zYSCtRdqyUfWYX1UhWw#0)
-- about effects in haskell, the results were disappointingly vague. A second attempt to 
-- [define the effect type](https://www.perplexity.ai/search/define-the-effect-type-in-hask-Dc8z91r7SzOVQUjK.P011Q#0)
-- revealed that "effect is defined differently with respect to every different effect system."
-- Fair enough—but what if we approach this from first principles?
--
-- This post takes an unconventional route: deriving a fundamental structure in functional
-- programming by starting with the Oxford English Dictionary.
 
-- | Define the effect type  
-- 
-- When i was [perplexitying](https://www.perplexity.ai/search/explain-what-is-effect-in-hask-TS.zYSCtRdqyUfWYX1UhWw#0)
-- about effect in haskell, it replied with no useful information as usual. The answer even got worse when i explicitly
-- prompted it to just [define the effect type](https://www.perplexity.ai/search/define-the-effect-type-in-hask-Dc8z91r7SzOVQUjK.P011Q#0).
-- It informed that effect is defined differently with respect to every different effect system nevertheless.
--
-- Thus in this post, i want to define the `Effect` type in a trivial effect system.
-- 
-- || Effect in Oxford dictionary
-- 
-- It just came to me that i should check the word `effect` in a dictionary to make sure i actually understood
-- its meaning. The [result](https://www.oxfordlearnersdictionaries.com/definition/english/effect_1?q=effect) was:
-- 
-- > a change that somebody/something causes in somebody/something else
--
-- This can actually be translated to a haskell type definition. Extracting the nouns & verbs in it, we get:
--
-- > a change (`f`) that something (`a`) causes (`->`) in something else (`b`)
--
-- An `Effect` needs to be parametrised by a change `f`, a something `a` & a something else `b`,
-- so we have the left-hand-side of it as `type Effect f a b =`. For the right-hand-side, note that we can
-- rephraze the definition as *a causes f in b*, or `a -> f b`. Putting these together, i'm deemed to write down
-- the following definition `type Effect f a b = a -> f b`.
-- 
-- Actually, a newtype is considered better here. We can also have a `runEffect` wrapper which further supports
-- the semantical derivation.

newtype Effect f a b = Effect { runEffect :: a -> f b }

-- | On the way to the Effect Category

-- || A quick glimpse of category
-- 
-- A category is composed of objects and arrows/morphisms between objects.
-- To formulate a legit category, we need a identity morphism & a compose operator:
 
class Category (cat :: Type -> Type -> Type) where
   id  :: cat a a
   (.) :: cat a b -> cat b c -> cat a c

-- Also, these 3 laws must to follow: f . id = f, id . f = f, f . (g . h) = (f . g) . h
 
-- The `Effect` is of kind `(k -> Type) -> k -> k -> Type`, and `Category` is of kind `(k -> k -> Type) -> Constraint`.
-- So the `Effect` alone can't form a category. 

class Functor (f :: Type -> Type) where
   fmap :: (a -> b) -> f a -> f b

class Functor m => Monad (m :: Type -> Type) where
   return :: a -> m a
   join   :: m (m a) -> m a

instance Monad m => Category (Effect m) where
   id                      = Effect return
   (Effect f) . (Effect g) = Effect (\x -> join (fmap g (f x)))
