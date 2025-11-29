{-
title       = Simply-typed λcalculus, the allmother of type system.
pubDate     = 2025-11-30
tags        = λcalculus, haskell
description = stlc is the canonical & simplest example of a typed lambda calculus.
-}

{-# LANGUAGE GADTs #-}

import           Data.Kind ( Type )

--

type TyVar = String

-- | Syntax
--
-- || Core type/term level syntax
--
-- The stlc is built on some collection of (abstract/concrete) base/atomic types: booleans, integers, strings, etc.
--
-- Besides base types, it only has one type constructor that builds function types.
--
-- So if we have base types `{a, b}`, then we can generate an infinite set of types `{a, b, a → a, b → b, a → b, b → a, a → (a → a), ...}`.
--
-- A set of term constants is also fixed for the base types. For example, it might be assumed that one of the base types
-- is nat, and its term constants could be the natural numbers.
--
-- The term syntax of stlc is essentially that of the lambda calculus itself,
-- which, in Backus–Naur form, is variable reference, abstractions, application, or constant: 𝑒 ::= 𝑥 | λ𝑥:τ.𝑒 | 𝑒𝑒 | 𝑐
--
-- where:
-- - 𝑥:τ denotes that the variable 𝑥 is of type τ,
-- - 𝑐 is a term constant (true, 1, etc.),
-- - a variable reference 𝑥 is |bound| if it is inside of an abstraction binding 𝑥,
-- - a term is |closed| if there ain't unbound variabled.
--
-- In comparison, the syntax of untyped lambda calculus has no such typing or term constants: 𝑒 ::= 𝑥 | λ𝑥.𝑒 | 𝑒𝑒
--
-- Whereas in typed lambda calculus every abstraction (i.e. function) must specify the type of its argument.

-- || Extended syntax
--
-- In this post, we'll use an [enriched syntax](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Categorical_semantics)
-- which is the internal language of Cartesian closed categories.

-- Type-level syntax (Objects in the Category)
data Ty
   = TyUnit             -- Final Object (1)
   | TyBool | TyNat     -- Base types
   | TyArr Ty Ty        -- Exponential Object (B^A) 
   | TyTup Ty Ty        -- Product Object (A × B)
   deriving (Eq)

instance Show Ty where
   show TyUnit      = "()"
   show TyBool      = "Bool"
   show TyNat       = "Nat"
   show (TyArr l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
   show (TyTup l r) = "(" ++ show l ++ ", "   ++ show r ++ ")"

-- Term-level syntax (Morphisms/Arrows)
data Tm
   = TmUnit             -- Unique element of Unit type
   | TmTrue | TmFalse
   | TmZero | TmSucc Tm | TmPred Tm | TmIsZero Tm
   | TmVar Int          -- De Bruijn index
   | TmAbs Ty Tm        -- Abstraction (Currying)
   | TmApp Tm Tm        -- Application (Evaluation)
   | TmIf Tm Tm Tm      -- Conditional
   | TmAdd Tm Tm | TmMul Tm Tm
   -- CCC Extensions:
   | TmPair Tm Tm       -- Pairing: (s, t)
   | TmFst Tm           -- Projection 1: π₁(u)
   | TmSnd Tm           -- Projection 2: π₂(u)
   deriving (Eq)

instance Show Tm where
   show TmUnit  = "()"
   show TmTrue  = "True"
   show TmFalse = "False"
   show TmZero  = "0"
   show (TmSucc TmZero) = "1"
   show (TmSucc (TmSucc a)) = show ((read @Int (show a)) + 2)
   show (TmSucc (TmPred a)) = show a

-- | Typing rules
--
-- As usual, we need to define the typing rules to bridge between types & terms.
--
-- Stlc uses these rules:
--
-- TODO: need to update the typing rules with respect to the extended syntax
--
-- ``````````````````````````````````````````````````````````````````````````````````
--   x:σ ∈ Γ
-- ----------- (1)
--   Γ ⊢ x:σ
--
--  c is a constant of type T
-- --------------------------- (2)
--         Γ ⊢ c:T
--
--         Γ, x:σ ⊢ e:τ
-- ----------------------------- (3)
--     Γ ⊢ (λx:σ. e):(σ → τ)
--
--  Γ ⊢ e1:σ → τ   Γ ⊢ e2:σ
-- -------------------------- (4)
--        Γ ⊢ e1 e2:τ
--
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- Check the [wikipedia section](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules) for an illustration
-- of these rules.

-- typeOf :: Context -> Tm -> Maybe Ty
-- eval   :: Context -> Tm -> Tm

{-
| Further reading
- [The WikiPedia page of Stlc](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)
- [Stlc: The Simply Typed Lambda-Calculus](https://jscoq.github.io/ext/sf/plf/full/Stlc.html)
-}
