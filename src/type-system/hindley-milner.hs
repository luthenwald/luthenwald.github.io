-- title       = Hindley-milner type system demystified
-- pubDate     = 2025-10-13
-- tags        = haskell, hindley-milner, type-system, 2025
-- description = a guide to hindley-milner type system: types, unification & algorithm w with haskell implementation

-- | setup
--
-- a [hindley-milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) is a
-- [type-system](https://en.wikipedia.org/wiki/Type_system) for
-- [λcalculus](https://en.wikipedia.org/wiki/Lambda_calculus) with
-- [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) (let statements).
--
-- hm type system is [complete](https://en.wikipedia.org/wiki/Completeness_(logic)) & has the ability to infer
-- the [most general type](https://en.wikipedia.org/wiki/Principal_type) of a given program without programmer-supplied
-- [type annotations](https://en.wikipedia.org/wiki/Type_signature) or other hints.
--
-- the prose is divided into 3 acts: type system, substitution/unification & algow. [^1]

import Control.Monad        ( replicateM )
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map   as Map
import           Data.Maybe ( isJust )
import qualified Data.Set   as Set

-- | hindley-milner type system
--
-- we (not so) formally describe the type system by [syntax rules](https://en.wikipedia.org/wiki/Formal_grammar)
-- in this act.
--
-- || syntax
--
-- a type variable (α) is a placeholder symbol that stands for an unknown type
-- which may later be instantiated with any concrete type (int, string, etc.) once more information is
-- available through type inference.

type TyVar = String

-- ||| expressions
--
-- the expressions (term-level language) to be typed are exactly those of the λcalculus extended with a let-expression.

data Expr
   = Var TyVar             -- variable         (x)
   | App Expr Expr         -- application      (e1e2)
   | Abs TyVar Expr        -- abstraction      (λx.e)
   | Let TyVar Expr Expr   -- let-polymorphism (let x = e1 in e2)
   deriving Show

-- the application is left-binding and binds stronger than abstraction or the let-in construct.

-- ||| monotypes
--
-- a monotype (τ) is either a type variable or an application of a type constructor to a list of monotypes.

data Monotype
   = TyVar' TyVar           -- type variable
   | TyCon' String [Mono]   -- e.g. TyCon' "Int" [], TyCon' "String" [], TyCon' "List" [Mono, Mono]

-- but for the sake of simplicity, we explicitly abstract the int/string constant types & the function type to
-- illustrate the type inference effectively:

data Mono
   = TyVar TyVar       -- variable
   | TyInt Int         -- primitive int type
   | TyStr String      -- primitive string type
   | TyFun Mono Mono   -- primitive application type
   deriving Eq         -- two monotypes are equal if they have identical terms

-- custom Show instance for readable type display
instance Show Mono where
   show (TyVar v) = v
   show (TyInt _) = "Int"
   show (TyStr _) = "String"
   show (TyFun t1 t2) = showFunArg t1 ++ " -> " ++ show t2
      where
         -- add parentheses around function types on the left of arrow
         showFunArg f@(TyFun _ _) = "(" ++ show f ++ ")"
         showFunArg t             = show t

-- ||| polytypes (type schemes)
--
-- polytypes are types containing variables bound by zero or more for-all quantifiers, e.g. `∀α.α → α`.
--
-- for instance, a function with polytype `∀α.α → α` can map any value of the
-- same type to itself. the identity function is a value for this type.
--
-- quantifiers can only appear top level. for instance, a type `∀α.α → ∀α.α` is
-- excluded by the syntax of types. also monotypes are included in the polytypes,
-- thus a type has the general form `∀α₁… ∀αₙ.τ`, where n ≥ 0 and τ is a monotype. [^2]

data Poly
   = Mono Mono
   | Forall [TyVar] Mono deriving Show

-- ||| context & typing
--
-- to meaningfully bring together the expressions & types, a third part |context| is needed.

-- syntactically, a context is a map from type variables to polytypes.
-- each item states that typevar xᵢ has the type σᵢ.
--
-- context, expression & polytypes combined give a |typing judgment| of the form `Γ ⊢ e : σ`,
-- stating that under the context (assumptions) Γ, the expression e has type σ.
--
-- later in the context of type inference, we can also interpret a typing judgment as:
-- we infer the type of expression e as σ under the assumptions Γ.

type Context = Map.Map TyVar Poly
data Judgement = Judgement Context Expr Poly deriving Show

-- ||| free type variables
--
-- free type variables are those not bound by a quantifier (∀).
--
-- intuitively, free type variables are |unknown types| that still need to be determined
-- during type inference, while bound variables are generic placeholders for polymorphism.

class Free a where
   ftv :: a -> Set.Set TyVar

-- |||| ftv for monotypes
--
-- the implementation collects |all type variables| appearing in a monotype:
--
-- - `TyVar v`: the variable itself is free → `{v}`
-- - `TyFun a b`: union of free variables from both sides → `ftv(a) ∪ ftv(b)`
-- - primitives (`TyInt`, `TyStr`): no type variables → `{}`
--
-- example: `ftv(α → (β → Int)) = {α, β}`

instance Free Mono where
   ftv (TyVar v)   = Set.singleton v
   ftv (TyFun a b) = (ftv a) <> (ftv b)
   ftv _           = mempty

-- |||| ftv for polytypes
--
-- the implementation extracts free variables by excluding those bound by ∀ quantifiers:
--
-- - `Mono m`: no quantifiers, just delegate to monotype `ftv`
-- - `Forall vars ty`: compute `ftv(ty)` then remove quantified vars → `ftv(ty) \ vars`
--
-- example: `ftv(∀α.α → β) = ftv(α → β) \ {α} = {α, β} \ {α} = {β}`
--
-- intuitively, bound variables represent generic polymorphism (known abstraction),
-- while free variables represent unknowns that need to be solved during inference.

instance Free Poly where
   ftv (Mono m)         = ftv m
   ftv (Forall vars ty) = Set.difference (ftv ty) (Set.fromList vars)

-- |||| ftv for contexts
--
-- the implementation unions all free variables from every polytype in the context.
-- context maps typevars to polytypes, so we extract values, compute `ftv` on each,
-- then union all results.
--
-- example: `ftv({x : ∀α.α, y : β → γ, z : Int}) = {} ∪ {β, γ} ∪ {} = {β, γ}`
--
-- intuitively, this tells us which type variables are |in scope| but not yet fully determined.

instance Free Context where
   ftv ctx = foldMap ftv (Map.elems ctx)

-- | substitution/unification
--
-- || substitution
--
-- a substitution is a mapping from type variables to monotypes.
-- it represents a partial solution to type constraints discovered during inference.

-- examples:
-- [α ↦ Int]               -- maps α to Int
-- [α ↦ β → γ]             -- maps α to a function type
-- [α ↦ Int, β ↦ String]   -- maps multiple variables
type Subst = Map.Map TyVar Mono

-- we define a `Substitutable` typeclass with a single function `apply` here.

class Substitutable a where
   apply :: Subst -> a -> a

-- ||| apply for monotypes
--
-- the implementation recursively replaces type variables according to the substitution:
-- - `TyVar v`: look up v in substitution s
-- -- if found → return mapped type
-- -- if not found → return original variable (identity)
-- - `TyFun l r`: apply substitution recursively to both sides
-- - primitives: unchanged (no variables to substitute)
--
-- example:
--    `apply [α ↦ Int, β ↦ String] (α → β) = Int → String`

instance Substitutable Mono where
    apply s t@(TyVar v) = Map.findWithDefault t v s
    apply s (TyFun l r) = TyFun (apply s l) (apply s r)
    apply _ t           = t

-- ||| apply for polytypes
--
-- the implementation applies substitution while respecting quantifier scope:
-- - `Mono m`: no quantifiers, just delegates to monotype `apply`
-- - `Forall vs m`: bound variables (vs) must not be substituted
-- -- remove bound variables from substitution before applying (`foldr Map.delete s vs`)
-- -- this prevents capture (shadowing bound vars)
--
-- example:
--    `apply [α ↦ Int, β ↦ String] (∀α.α → β) = ∀α.α → String`    (α not substituted, β substituted)

instance Substitutable Poly where
    apply s (Mono m)      = Mono (apply s m)
    apply s (Forall vs m) = Forall vs (apply (foldr Map.delete s vs) m)

-- ||| apply for contexts
--
-- the implementation applies substitution to every polytype in the context.
-- since context is a map from typevars to polytypes, we use `fmap/(<$>)`
-- to apply the substitution to each value.
--
-- this is crucial for propagating type information through the environment
-- as inference proceeds.
--
-- example:
--    `apply [α ↦ Int] {x : α, y : α → β} = {x : Int, y : Int → β}`

instance Substitutable Context where
    apply s = (<$>) (apply s)

-- ||| composing substitutions
--
-- we categorically want to implement composition for our substitution monoid.
--
-- with `compose :: Subst -> Subst -> Subst`,
-- we formulate a monoid for `Subst` where:
-- - identity element is the empty substitution []
-- - associative binary operation is compose (∘)
--
-- specifically, `compose s1 s2` applies s1 "after" s2, i.e. `(s1 ∘ s2)(α) = s1(s2(α))`.
-- right substitution (s2) is applied first, then left (s1) refines the result.

compose :: Subst -> Subst -> Subst
compose s1 s2 = (apply s1) <$> s2 <> s1

-- || unification
--
-- unification is the process of finding a substitution that makes two types equal.
--
-- we'll implement [robinson's unification algorithm](https://en.wikipedia.org/wiki/Unification_(computer_science)#Unification_algorithms),
-- which produces the |most general unifier| (mgu).

-- ||| occurs check
--
-- we need to check if a type variable occurs within a monotype.
-- this prevents creating infinite/recursive types like `α = α → β`.

-- examples:
-- occursCheck α α       = True   (α occurs in α)
-- occursCheck α (α → β) = True   (α occurs in function)
-- occursCheck α (β → γ) = False  (α doesn't occur)
-- occursCheck α Int     = False  (no variables in primitives)
occursCheck :: TyVar -> Mono -> Bool
occursCheck v (TyVar v')  = v == v'
occursCheck v (TyFun l r) = occursCheck v l || occursCheck v r
occursCheck _ (TyInt _)   = False
occursCheck _ (TyStr _)   = False

-- ||| the unification monad
--
-- unification can fail, so we need a monad that supports error handling:
-- `UnifyM = Except String` provides exactly this capability.
--
-- unification fails when:
-- = structural mismatch: trying to unify incompatible types (Int vs String)
-- = occurs check failure: would create infinite types (α vs α → β)
--
-- the Except monad gives us:
-- - automatic error propagation (if any step fails, whole computation fails)
-- - explicit error messages via `throwError`

type UnifyM = Except String

-- ||| variable binding
--
-- the `bind` function creates a minimal substitution (`[v ↦ t]`) from a typevar v to a monotype t, but only if it's safe.
--
-- three cases could happen:
-- - reflexive: `t == TyVar v` → return `[]` (identity, maximally general)
-- - occurs: v occurs in t → error (would create infinite type)
-- - valid: v doesn't occur in t → return `[v ↦ t]` (minimal binding)
--
-- the occurs check is essential: without it, unifying α with `(α → β)`
-- would create an infinite type: `α = α → β = (α → β) → β = ...`

-- examples:
-- bind α α             = []                 (identity)
-- bind α Int           = [α ↦ Int]          (valid binding)
-- bind α (α → β)       = error              (occurs check fails)
-- bind α (β → γ)       = [α ↦ β → γ]        (valid binding)
bind :: TyVar -> Mono -> UnifyM Subst
bind v t
  | t == TyVar v     = return mempty
  | occursCheck v t  = throwError ("Occurs check fails: " ++ v ++ " in " ++ show t)
  | otherwise        = return $ Map.singleton v t

-- ||| most general unifier (mgu)
--
-- the mgu has the property that for any other unifier σ', there exists another unifier θ such that: `σ' = θ ∘ mgu(t1, t2)`. [^3]

-- unification proceeds by structural recursion on type shapes, with four cases:
--
-- |||| case 1: type variables
--
-- the unification delegates to `bind` which performs occurs check and creates minimal substitution `[v ↦ t]`:
--
-- `unify α t → bind α t → [α ↦ t]` (or error if occurs check fails)
--
-- this case is symmetry: `unify α β` and `unify β α` both produce valid mgu's.
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example 1: unify α Int
--   result = [α ↦ Int]    (bind α to Int)
--
-- example 2: unify α β
--   result = [α ↦ β]      (bind α to β, arbitrary choice)
--
-- example 3: unify α (β → γ)
--   check: α does not occur in (β → γ) ✓
--   result = [α ↦ β → γ]
--
-- example 4: unify α (α → Int)
--   check: α occurs in (α → Int) ✗
--   result = error       (would create infinite type)
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- |||| case 2: primitives
--
-- concrete types must match exactly. no variables to solve, so:
--
-- - identical types return empty substitution `[]` (types already equal)
-- - different types return error (incompatible)
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example 1: unify Int Int
--   result = []           (already equal, no substitution needed)
--
-- example 2: unify String String
--   result = []
--
-- example 3: unify Int String
--   result = error        (incompatible primitive types)
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- |||| case 3: function types
--
-- the unification structurally decomposes and recursively unifys components:
-- - unify left sides: `s1 = unify l1 l2`
-- - apply s1 to right sides before unifying them (propagate constraints)
-- - unify rights: `s2 = unify (s1 r1) (s1 r2)`
-- - compose: return `s2 ∘ s1`
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example 1: unify (α → β) (Int → String)
--   step 1: s1 = unify α Int           = [α ↦ Int]
--   step 2: apply s1 to rights: β, String (no change, β not in s1)
--   step 3: s2 = unify β String        = [β ↦ String]
--   step 4: result = s2 ∘ s1           = [α ↦ Int, β ↦ String]
--
-- example 2: unify (α → α) (Int → β)
--   step 1: s1 = unify α Int           = [α ↦ Int]
--   step 2: apply s1 to rights: s1(α) = Int, β
--   step 3: s2 = unify Int β           = [β ↦ Int]
--   step 4: result = s2 ∘ s1           = [α ↦ Int, β ↦ Int]
--
-- example 3: unify (α → β) (γ → δ)
--   step 1: s1 = unify α γ             = [α ↦ γ]
--   step 2: apply s1 to rights: β, δ   (no change)
--   step 3: s2 = unify β δ             = [β ↦ δ]
--   step 4: result = s2 ∘ s1           = [α ↦ γ, β ↦ δ]
--
-- example 4: unify ((α → β) → γ) ((Int → String) → Bool)
--   step 1: s1 = unify (α → β) (Int → String)
--           s1a = unify α Int          = [α ↦ Int]
--           s1b = unify β String       = [β ↦ String]
--           s1 = [α ↦ Int, β ↦ String]
--   step 2: apply s1 to rights: γ, Bool (no change)
--   step 3: s2 = unify γ Bool          = [γ ↦ Bool]
--   step 4: result = s2 ∘ s1           = [α ↦ Int, β ↦ String, γ ↦ Bool]
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- |||| case 4: incompatible structures
--
-- if none of the above patterns match, types are fundamentally incompatible.
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example 1: unify Int (α → β)
--   result = error        (primitive vs function type)
--
-- example 2: unify (α → β) String
--   result = error        (function type vs primitive)
-- ``````````````````````````````````````````````````````````````````````````````````

unify :: Mono -> Mono -> UnifyM Subst
unify (TyVar v) t = bind v t
unify t (TyVar v) = bind v t
unify (TyInt n) (TyInt m)
  | n == m    = return mempty
  | otherwise = throwError ("Type mismatch: " ++ show n ++ " vs " ++ show m)
unify (TyStr x) (TyStr y)
  | x == y    = return mempty
  | otherwise = throwError ("Type mismatch: " ++ show x ++ " vs " ++ show y)
unify (TyFun l1 r1) (TyFun l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  return $ compose s2 s1
unify t1 t2 =
  throwError $ "Type mismatch: " ++ show t1 ++ " vs " ++ show t2

-- | algorithm w
--
-- [algorithm w](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W)
-- is a sound & complete type inference algorithm for the hm type system.
--
-- given a context and an expression, it either:
-- - succeeds: returns a substitution and the inferred type
-- - fails: returns an error message

-- || inference monad
--
-- type inference needs two computational effects:
-- - fresh variable generation: to create new type variables during inference
-- - error handling: to report type errors
--
-- the `InferM = ExceptT String (State Int)` monad combines both:
-- - `State Int`: maintains a counter for generating unique type variables (t0, t1, t2, ...)
-- - `ExceptT String`: wraps State to add error handling
--
-- this gives us access to:
-- - `fresh`: generates new type variables
-- - `throwError`: reports type errors

type InferM = ExceptT String (State Int)
--            ^^^^^^^^^^^^^^ ^^^^^^^^^^^
--            |              |
--            |              inner: state for fresh vars
--            outer: error handling

-- we define a function to lift unification from `UnifyM` into `InferM`.
-- this bridges the gap between the two monads so we can use `unify` inside `inferW`.

liftUnify :: Mono -> Mono -> InferM Subst
liftUnify a b = liftEither (runExcept (unify a b))

-- || fresh type variables
--
-- we need to generate unique type variables: `t0, t1, t2, ...` during a type inference process.
--
-- this is used when:
-- - inferring function application: need a fresh result type
-- - inferring abstraction: parameter type is initially unknown

fresh :: InferM TyVar
fresh = do
  n <- get
  put (n+1)
  return ("t" ++ show n)

-- generates k fresh type variables at once.
freshN :: Int -> InferM [TyVar]
freshN k = replicateM k fresh

-- || generalization
--
-- generalization turns a monotype into a polytype by quantifying free type variables,
-- where only variables |not in the context| are generalized:
--
-- `generalize ctx t = ∀(ftv(t) \ ftv(ctx)).t`
--
-- intuitively, we only generalize variables we |own| (introduced locally), not variables from outer scope (already in context).

-- examples:
-- generalize {} (α → α)      = ∀α.α → α   (α not in context)
-- generalize {x:β} (α → β)   = ∀α.α → β   (only α generalized, β in context)
-- generalize {x:α} (α → α)   = α → α      (no generalization, α in context)
generalize :: Context -> Mono -> Poly
generalize ctx t =
  let vars = Set.toList $ Set.difference (ftv t) (ftv ctx)
  in if null vars then Mono t else Forall vars t

-- || instantiation
--
-- instantiation turns a polytype into a monotype by replacing quantified variables with fresh ones.
-- this allows using polymorphic values at different types.
--
-- by the means of fresh variables, each use of a polymorphic value gets independent types.
--
-- example: `let id = λx.x in (id 5, id "hi")`
-- - first use: `instantiate (∀α.α → α) → t0 → t0`, then unify t0 with Int
-- - second use: `instantiate (∀α.α → α) → t1 → t1`, then unify t1 with String
-- - if we reused the same α, the two uses would conflict.

-- examples:
-- instantiate (∀α.α → α)      → t0 → t0   (fresh t0)
-- instantiate (∀α.∀β.α → β)   → t0 → t1   (fresh t0, t1)
-- instantiate (α → β)         → α  → β    (no quantifiers, return as is)
instantiate :: Poly -> InferM Mono
instantiate (Mono m) = return m
instantiate (Forall vars m) = do
  freshVars <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars (TyVar <$> freshVars)
  return $ apply s m

-- || type inference algorithm
--
-- with the type signature:
-- `inferW :: Context -> Expr -> InferM (Subst, Mono)`
--
-- given a context Γ and an expression e, the inference algo returns (both):
-- - substitution s: constraints discovered during inference
-- - monotype t: the inferred type
--
-- essentially, the algorithm implements these four typing rules:
--
-- ||| variable rule
--
-- ``````````````````````````````````````````````````````````````````````````````````
--   x : σ ∈ Γ   τ = instantiate(σ)
--   --------------------------------   [VAR]
--   Γ ⊢ x : τ, ∅
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- the algo look up typevar x in context Γ, instantiate its polytype with fresh variables.
-- then return empty substitution (∅) since no constraints are generated.
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example: Γ = {id : ∀α.α → α}, infer (id)
--   x = "id"
--   σ = lookup "id" in Γ  → ∀α.α → α
--   τ = instantiate(σ)    → t0 → t0    (fresh t0)
--   return (∅, t0 → t0)
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- ||| application rule
--
-- ``````````````````````````````````````````````````````````````````````````````````
--   Γ ⊢ e1 : τ1, S1   S1Γ ⊢ e2 : τ2, S2
--   τ' = fresh        S3 = mgu(S2τ1, τ2 → τ')
--   --------------------------------------------   [APP]
--   Γ ⊢ e1e2 : S3τ', S3S2S1
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- the algo:
-- = infer e1 under Γ, get S1 and τ1
-- = apply S1 to context before inferring e2
-- = infer e2 under S1Γ, get S2 and τ2
-- = create fresh result type τ'
-- = apply S2 to τ1 before unifying (propagate e2's constraints to e1's type)
-- = unify S2τ1 with τ2 → τ', get S3
-- = return S3S2S1 (right-to-left composition) and S3τ'
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example: infer ((λx.x) 5)
--   step 1: infer (λx.x) → S1 = ∅, τ1 = t0 → t0
--   step 2: infer 5 in S1Γ = Γ → S2 = ∅, τ2 = Int
--   step 3: fresh → τ' = t1
--   step 4: S2τ1 = t0 → t0
--   step 5: mgu(t0 → t0, Int → t1) → S3 = [t0 ↦ Int, t1 ↦ Int]
--   step 6: S3τ' = S3(t1) = Int
--   step 7: return (S3S2S1 = S3, Int)
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- ||| abstraction rule
--
-- ``````````````````````````````````````````````````````````````````````````````````
--   τ = fresh   Γ, x : τ ⊢ e : τ', S
--   -----------------------------------   [ABS]
--   Γ ⊢ λx.e : Sτ → τ', S
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- the algo:
-- = create fresh type variable τ for parameter x
-- = extend context: Γ' = Γ, x : τ
-- = infer e in Γ', get S and τ'
-- = apply S to parameter type: Sτ
-- = return (S, Sτ → τ')
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example: infer (λx.x)
--   step 1: fresh → τ = t0
--   step 2: Γ' = {x : t0}
--   step 3: infer x in Γ' → S = ∅, τ' = t0
--   step 4: Sτ = t0
--   step 5: return (∅, t0 → t0)
--
-- example: infer (λx.λy.x) assuming some context leads to constraints
--   step 1: fresh → τ = t0
--   step 2: Γ' = {x : t0}
--   step 3: infer (λy.x) in Γ' → S = ∅, τ' = t1 → t0
--   step 4: Sτ = t0 (no substitution affects t0 here)
--   step 5: return (∅, t0 → (t1 → t0))
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- ||| let-polymorphism rule
--
-- ``````````````````````````````````````````````````````````````````````````````````
--   Γ ⊢ e1 : τ, S1   S1Γ, x : generalize(S1Γ, τ) ⊢ e2 : τ', S2
--   -----------------------------------------------------------   [LET]
--   Γ ⊢ let x = e1 in e2 : τ', S2S1
-- ``````````````````````````````````````````````````````````````````````````````````
--
-- the algo:
-- = infer e1 under Γ, get S1 and τ
-- = apply S1 to context: S1Γ
-- = generalize τ under S1Γ to get σ = generalize(S1Γ, τ)
-- = extend context: Γ' = S1Γ, x : σ
-- = infer e2 under Γ', get S2 and τ'
-- = return (S2S1, τ')
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- example: infer (let id = λx.x in (id 5, id "hi"))
--   step 1: infer (λx.x) → S1 = ∅, τ = t0 → t0
--   step 2: S1Γ = Γ (identity, since S1 is empty)
--   step 3: generalize(Γ, t0 → t0) → σ = ∀t0.t0 → t0
--   step 4: Γ' = {id : ∀t0.t0 → t0}
--   step 5: infer (id 5, id "hi") under Γ'
--           first use of id:  instantiate → t1 → t1, unify with Int → ...
--           second use of id: instantiate → t2 → t2, unify with String → ...
--           → S2 = [...], τ' = (Int, String)
--   step 6: return (S2S1 = S2, (Int, String))
-- ``````````````````````````````````````````````````````````````````````````````````

inferW :: Context -> Expr -> InferM (Subst, Mono)
inferW ctx expr = case expr of
  Var x -> case Map.lookup x ctx of
    Nothing   -> throwError $ "unbound variable: " ++ x
    Just sigma -> do
      m <- instantiate sigma
      return (mempty, m)

  App e1 e2 -> do
    (s1, t1) <- inferW ctx e1
    (s2, t2) <- inferW (apply s1 ctx) e2
    tv <- fresh
    let t = TyVar tv
    s3 <- liftUnify (apply s2 t1) (TyFun t2 t)
    return (compose s3 (compose s2 s1), apply s3 t)

  Abs x e -> do
    tv <- fresh
    let ctx' = Map.insert x (Mono $ TyVar tv) ctx
    (s1, t1) <- inferW ctx' e
    return (s1, TyFun (apply s1 (TyVar tv)) t1)

  Let x e1 e2 -> do
    (s1, t1) <- inferW ctx e1
    let sigma = generalize (apply s1 ctx) t1
        ctx' = Map.insert x sigma (apply s1 ctx)
    (s2, t2) <- inferW ctx' e2
    return (compose s2 s1, t2)

-- | tests & examples
--
-- claude-4.5-sonnet-thinking provides several test cases to demonstrate algorithm w in action.

-- helper to run inference from empty context
runInfer :: Expr -> Either String Mono
runInfer e = case runState (runExceptT (inferW Map.empty e)) 0 of
   (Left err, _)     -> Left err
   (Right (_, t), _) -> Right t

-- helper to run inference with a context
runInferWith :: Context -> Expr -> Either String Mono
runInferWith ctx e = case runState (runExceptT (inferW ctx e)) 0 of
   (Left err, _)     -> Left err
   (Right (_, t), _) -> Right t

-- test cases
main :: IO ()
main = do
   putStrLn "=== hindley-milner type inference tests ===\n"

   -- test 1: identity function
   putStrLn "test 1: identity function (λx.x)"
   let idFunc = Abs "x" (Var "x")
   print $ runInfer idFunc
   putStrLn "expected: t0 -> t0\n"

   -- test 2: const function (λx.λy.x)
   putStrLn "test 2: const function (λx.λy.x)"
   let constFunc = Abs "x" (Abs "y" (Var "x"))
   print $ runInfer constFunc
   putStrLn "expected: t0 -> t1 -> t0\n"

   -- test 3: self-application (λx.x x) - should fail
   putStrLn "test 3: self-application (λx.x x) - should fail with occurs check"
   let selfApp = Abs "x" (App (Var "x") (Var "x"))
   print $ runInfer selfApp
   putStrLn "expected: Left \"Occurs check fails: ...\"\n"

   -- test 4: simple application
   putStrLn "test 4: application ((λx.x) (λy.y))"
   let simpleApp = App idFunc idFunc
   print $ runInfer simpleApp
   putStrLn "expected: t1 -> t1\n"

   -- test 5: let-polymorphism - the key test
   putStrLn "test 5: let-polymorphism (let id = λx.x in id)"
   let letPoly = Let "id" idFunc (Var "id")
   print $ runInfer letPoly
   putStrLn "expected: t1 -> t1\n"

   -- test 6: polymorphic let with multiple uses at different types
   -- demonstrates each use of polymorphic id gets fresh type variables
   putStrLn "test 6: let with application (let id = λx.x in id id)"
   let letApp = Let "id" idFunc (App (Var "id") (Var "id"))
   print $ runInfer letApp
   putStrLn "expected: t2 -> t2\n"

   -- test 7: nested let
   putStrLn "test 7: nested let (let x = λa.a in let y = x in y)"
   let nestedLet = Let "x" idFunc (Let "y" (Var "x") (Var "y"))
   print $ runInfer nestedLet
   putStrLn "expected: t2 -> t2\n"

   -- test 8: function composition structure (λf.λg.λx.f (g x))
   putStrLn "test 8: composition structure (λf.λg.λx.f (g x))"
   let compose = Abs "f" (Abs "g" (Abs "x"
                  (App (Var "f") (App (Var "g") (Var "x")))))
   print $ runInfer compose
   putStrLn "expected: (t3 -> t4) -> (t2 -> t3) -> t2 -> t4\n"

   -- test 9: let with shadowing
   putStrLn "test 9: let with shadowing (let x = λa.a in let x = λb.b in x)"
   let shadowLet = Let "x" idFunc (Let "x" (Abs "b" (Var "b")) (Var "x"))
   print $ runInfer shadowLet
   putStrLn "expected: t2 -> t2\n"

   -- test 10: demonstrating generalization matters
   putStrLn "test 10: let id = λx.x in (let y = id in y)"
   let genTest = Let "id" idFunc (Let "y" (Var "id") (Var "y"))
   print $ runInfer genTest
   putStrLn "expected: t2 -> t2\n"

   putStrLn "\n=== all tests completed ==="

-- | appendices
--
-- || notation & symbols
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- ||| type-level symbols
--
--   τ, τ', τ1, τ2          monotypes (concrete type expressions)
--   σ, σ'                  polytypes (type schemes with ∀ quantifiers)
--   α, β, γ                type variables (placeholders for unknown types)
--   t0, t1, t2, ...        fresh type variables generated during inference
--
-- ||| context & environment
--
--   Γ                      typing context (maps variables to polytypes)
--   Γ, x : σ               context extended with binding x : σ
--   S1Γ                    context with substitution S1 applied to all types
--
-- ||| substitutions
--
--   S, S1, S2, S3          substitutions (maps type variables to monotypes)
--   ∅                      empty substitution (identity)
--   [α ↦ τ]                substitution mapping α to τ
--   S2S1                   composition: apply S1 first, then S2
--   Sτ                     apply substitution S to type τ
--
-- ||| typing judgments
--
--   Γ ⊢ e : τ, S           under context Γ, expression e has type τ with substitution S
--   x : σ ∈ Γ              variable x has type σ in context Γ
--   τ ~ τ'                 types τ and τ' can be unified
--
-- ||| operations
--
--   generalize(Γ, τ)       turn monotype τ into polytype by quantifying free vars not in Γ
--   instantiate(σ)         turn polytype σ into monotype by replacing ∀-bound vars with fresh vars
--   mgu(τ1, τ2)            most general unifier: substitution making τ1 and τ2 equal
--   fresh                  generate a new unique type variable
--
-- ||| logical symbols
--
--   →                     function type constructor (τ1 → τ2)
--   ∀                     universal quantifier (∀α.τ)
--   ∈                     element of / membership
--   ⊢                     entailment / typing judgment
--   ---                   inference rule separator (premises above, conclusion below)
-- ``````````````````````````````````````````````````````````````````````````````````

-- ^1. full source code available at [github](https://github.com/luthenwald/luthenwald.github.io/blob/prima/blogs/type-theory/hindley-milner.hs).
-- ^2. there should only exist the `Forall` constructor. we explicitly add a `Mono` branch here to simplify pattern matching.
-- ^3. mgu is analogous to the initial object in the context of category theory.
