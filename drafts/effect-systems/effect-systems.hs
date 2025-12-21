{-
title       = Effect is Kleisli, etymologically
pubDate     = 2025-11-26
tags        = effect-system, haskell, monad, 2026
description = 
-}

{-# LANGUAGE GADTs #-}

import           Data.Kind ( Type )

import           Prelude   hiding ( Monad, (>>=) )

{-
| Introduction

In continuation of [last blog], we will review the history of effect systems in haskell.
-}


{-
| The Operational/Free Monad

|| Effect Algebra

We can define available operations as algebraic datatypes.

In *algenraic effects*, we treat computational (side) effects as operations in an algebra.
Instead of performing the effect immediately (like executing a system call), the program constructs a data structure representing the
request for that effect.
We specifies the valid operations without specifying how they work.

Then the *free monad* is the standard mechanism to turn this effect algebra into a full programming language (a monad). It generates
the free structure required to chain these operations together sequentially.

We can have a list of effects:
-}

data Console r where
   PutStrLn    :: String -> Console ()
   GetLine     :: Console String
   ExitSuccess :: Console ()

class Monad m where
   ret   ::   a -> m a
   (>>=) :: m a -> ( a -> m b ) -> m b

-- -- The Coyoneda wrapper makes any 'f' a Functor
-- data Coyoneda f a where
--     Coyoneda :: (x -> a) -> f x -> Coyoneda f a

-- instance Functor (Coyoneda f) where
--     fmap g (Coyoneda h fx) = Coyoneda (g . h) fx

-- -- Now you can use standard Free
-- type ConsoleMonad = Free (Coyoneda Console)

-- data Program instr a where
--    Then   :: instr a -> (a -> Program instr b) -> Program instr b
--    Return :: a -> Program instr a

data Free f a where

-- data ParserInstruction a where
--    Symbol :: ParserInstruction Char
--    MZero  :: ParserInstruction a
--    MPlus  :: Parser a -> Parser a -> ParserInstruction a

-- type Parser a = Program ParserInstruction a

-- interpret :: Parser a -> String -> [a]
-- interpret (Return a)            s = if null s then [a] else []
-- interpret (Symbol    `Then` is) s = case s of
--    c:cs -> interpret (is c) cs
--    []   -> []
-- interpret (MZero     `Then` is) s = []
-- interpret (MPlus p q `Then` is) s =
--    interpret (p >>= is) s ++ interpret (q >>= is) s


{-
| References/Further Reading

- [The Operational Monad Tutorial](https://apfelmus.nfshost.com/articles/operational-monad.html)
-}



