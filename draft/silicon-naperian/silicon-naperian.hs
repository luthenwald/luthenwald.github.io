-- title       = Silicon Programming with Naperian Functors
-- pubDate     = 2026-11-1
-- tags        = gpu, haskell
-- description = An attempt to adapt the Naperian Functors from Gibbons ("APLicative Programming with Naperian Functors", 2017) to apple silicon (metal) ecosystem.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE TypeFamilies        #-}

module SiliconNaperian where

import           Control.Monad.State  ( State, evalState, get, put )
import           Control.Monad.Writer ( WriterT, runWriterT, tell )

import           Data.Kind            ( Type )
import           Data.List            ( intercalate, isInfixOf )
import           Data.Proxy           ( Proxy (..) )
import           Data.Word            ( Word32 )

import           GHC.TypeNats         ( KnownNat, Nat, natVal )

import           Prelude              hiding ( Applicative (..), Foldable (..), Functor (..), foldr )
import qualified Prelude              as P

-- | The Silicon-Naperian Typeclass Hierarchy
--
-- || A Leap against array
--
-- [Futhark](https://futhark-lang.org/) is a purely functional, statically typed, data-parallel array language designed for GPUs.
-- It takes *array* as its primitive. Substantially, array is a triad notion: a (contiguous) flat-memory layout, an integer-offset
-- indexing scheme, and a fixed vocabulary of operations (fmap, fold, scatter, gather, etc.). A GPU, though, does not traffic in
-- arrays. It provides a grid of threads, each addressed by a position, and asks the programmer for a per-thread function (kernel).
-- Nothing in this contract requires the word array. It requires a container with positions, a mechanism to apply a function at each
-- one, etc. not just confined to an array.
--
-- Thus our instinct is to factor/purify these properties apart, state them as typeclass constraints, then let any datatype
-- satisfying the constraints compile to the *same dispatch pattern*.
--
-- || From Futhark's array prelude to typeclasses
--
-- To make this concrete, we take the Futhark [prelude/array](https://futhark-lang.org/docs/prelude/doc/prelude/array.html) &
-- [prelude/soacs](https://futhark-lang.org/docs/prelude/doc/prelude/soacs.html) as our specimen and consider: which Haskell typeclass
-- subsumes each function? The answer will build up a typeclass hierarchy until we arrive at exactly the interface Gibbons calls
-- *Dimension*.
--
-- The following categories of functions will be omitted across this prose:
--
-- - functions that are only meaningful in the context of (flat) arrays. e.g. length, head, take
-- - functions that are rarely what you actually want. e.g. foldl, scan
-- - functions whose signature can't be expressed in the context of dependently types. e.g. partition (dynamic size at runtime)
-- - functions that are trivial to implement. e.g. mapk (i.e. liftAk)
--
-- Then we can filter out the following functions:
--
-- ``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
-- # prelude/array
-- open import "/prelude/zip"
-- val replicate                     't : (n: i64)         -> (x: t)        -> *[n]t
-- val transpose             [n] [m] 't : (a: [n][m]t)     -> [m][n]t
-- val foldr                  [n] 'a 'b : (f: b -> a -> a) -> (acc: a)      -> (bs: [n]b) -> a
-- val tabulate                      'a : (n: i64)         -> (f: i64 -> a) -> *[n]a
--
-- # prelude/soacs
-- val map                    'a [n] 'x : (f: a -> x)      -> (as: [n]a)    -> *[n]x
-- ``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
--
-- There are 6 of them (including `zip`), we shall substitute `[]` with `f` to leap from array to a more generic substance.
--
-- Some functions can be abstracted with typeclasses we are familiar with:
--
-- - `map` applies a function uniformly to every element, whose abstraction is `Functor`.

class Functor f where
   fmap :: (a -> b) -> f a -> f b

-- - `foldr` uses an associative binary operator to aggregate values to a summary value, whose abstraction is `Foldable`.

class Foldable t where
   foldr :: (a -> b -> b) -> b -> t a -> b

-- - `replicate` fills every position with the same value, we rename it as `pure`. `zip` is `liftA2 (,)`,
--   which can be represented with `fmap` & `(<*>)`, so we can abstract them with `Applicative`.

class Functor f => Applicative f where
   pure   :: a             -> f a
   (<*>)  :: f (a -> b)    -> f a -> f b
   liftA2 :: (a -> b -> c) -> f a -> f b -> f c

   liftA2 f fa fb = fmap f fa <*> fb

-- However, the remaining `transpose` and `tabulate` have no (obvious) counterparts in the standard typeclass hierarchy. The
-- fundamental cause is structural: `Functor`, `Applicative` & `Foldable` all speak of *elements*, never of *positions*. `tabulate`
-- constructs a container by evaluating a function at each position; `transpose` reorders two layers of nesting by exchanging
-- coordinates. Neither is expressible without first formalizing what a *position* is.

-- ||| Naperian Functors is all you need
--
-- The Naperian functor justifies that missing concept. A functor `f` is Naperian when it admits the isomorphism
--
-- > f a  ≅  Log f → a
--
-- where `Log f` (the *logarithm* of `f`) is the type of positions. The two directions of the isomorphism are `tabulate` and `index`:
-- `tabulate` builds a container by evaluating a function at every position; `index` is its inverse, treating a container as a
-- function on positions.
--
-- `transpose` can then be expressed with these two primitives. Given two Naperian functors `f` and `g`, we have `f (g a) ≅ Log f →
-- Log g → a` and `g (f a) ≅ Log g → Log f → a`, where the two are isomorphic by argument-swapping, giving the derivation:

transpose :: (Naperian f, Naperian g) => f (g a) -> g (f a)
transpose xss = tabulate (\j -> tabulate (\i -> xss `index` i `index` j))

-- With the positional gap now filled, we can state the class as:

class Functor f => Naperian f where
   type Log f
   tabulate :: (Log f -> a) -> f a
   index    :: f a          -> Log f -> a

-- || Dimension and Hyper
--
-- We now bundle `Naperian :*: Applicative :*: Foldable` into the `Dimension` typeclass, or the exact constraint a single
-- data-parallel axis requires.

class (Naperian f, Applicative f, Foldable f) => Dimension f

-- The `Hyper` type promotes a list of `Dimension` functors into a single rank-polymorphic tensor/hypercuboid:

type Hyper :: [Type -> Type] -> Type -> Type
data Hyper fs a where
   HScalar ::                a              -> Hyper '[]       a
   HPrism  :: Dimension f => Hyper fs (f a) -> Hyper (f ': fs) a

-- where `Scalar` closes the recursion at rank zero; `Prism` peels one dimension.
--
-- > Gibbons' paper also defines a `Shapely` class that provides a uniform `foldr` over the entire nested
--   structure. In our setting, aggregation is the responsibility of Metal reduction kernels,
--   not the Haskell runtime. The `Foldable` constraint already bundled into `Dimension` is sufficient for
--   type-checking purposes.

-- | The type-theoretic core of Metal
--
-- The former section completes the algebraic frontend. The typeclass hierarchy `Functor`, `Foldable`, `Applicative`, `Naperian` =>
-- `Dimension` characterises a data-parallel axis with full mathematical precision, & `Hyper` assembles those axes into a
-- rank-polymorphic tensor.
--
-- What remains entirely abstract is Metal itself. We have no notion of a thread, a buffer, or a dispatch call. In this section
-- we shall bridge that gap.

-- || Type-level encoding of Metal's thread-address space
--
-- There are quite a lot attributes for kernel function input arguments (see table 5.8. of Metal Shading Language Specification v4).
-- For simplicity, we will only consider `thread_position_in_grid` & won't distinguish between `ushort`/`uint`.
--
-- `MTLTidKind` is a closed promoted datakind enumerating the three Metal dispatch geometries, i.e. the set of types of
-- `thread_position_in_grid`: `uint`, `uint2` & `uint3`.

type MTLTidKind :: Type
data MTLTidKind
   = Tid1 Nat           -- uint  tid   ── 1-D dispatch
   | Tid2 Nat Nat       -- uint2 tid   ── 2-D dispatch (x = inner col, y = outer row)
   | Tid3 Nat Nat Nat   -- uint3 tid   ── 3-D dispatch (x, y, z)

-- `SMTLTid` is a *singleton* witness for `MTLTidKind`. In Haskell, type-level `Nat` values are erased before runtime, so there is no
-- way to inspect them in ordinary term-level code. The standard workaround is a [singleton GADT](https://www.seas.upenn.edu/~sweirich/papers/haskell12.pdf),
-- where each constructor mirrors one variant of `MTLTidKind` and, crucially, brings `KnownNat` constraints into scope. When code
-- pattern-matches on e.g. `STid2`, GHC unpacks both `KnownNat m` and `KnownNat n`, making `natVal` available for extracting the
-- concrete `Integer` values that the MSL emitter needs (grid sizes, flat-index formulas, etc.).
--
-- Effectively, `SMTLTid k` is a runtime proof that `k` is a specific `MTLTidKind` whose `Nat` parameters are all known.

type SMTLTid :: MTLTidKind -> Type
data SMTLTid k where
   STid1 :: KnownNat n
         => SMTLTid ('Tid1 n)
   STid2 :: (KnownNat m, KnownNat n)
         => SMTLTid ('Tid2 m n)
   STid3 :: (KnownNat d, KnownNat m, KnownNat n)
         => SMTLTid ('Tid3 d m n)

-- ||| Query functions on the singleton
--
-- Each query function pattern-matches on the singleton, thereby gaining access to the `KnownNat` dictionaries, and projects a
-- concrete Metal artifact from the type-level dimension encoding.
--
-- `tidFlatSize` is the total element count (= total number of GPU threads to dispatch). It is the product of all dimension sizes.
-- The emitter uses this to size Metal buffers and to populate the `gsize` uniform in reduction kernels.

tidFlatSize :: SMTLTid k -> Int
tidFlatSize (STid1 @n)       = fromIntegral (natVal (Proxy @n))
tidFlatSize (STid2 @m @n)    = fromIntegral (natVal (Proxy @m) * natVal (Proxy @n))
tidFlatSize (STid3 @d @m @n) = fromIntegral (natVal (Proxy @d) * natVal (Proxy @m) * natVal (Proxy @n))

-- `tidMSLType` is the MSL type keyword that will appear after `\[\[thread_position_in_grid\]\]` in the generated kernel signature.

tidMSLType :: SMTLTid k -> String
tidMSLType STid1{} = "uint"
tidMSLType STid2{} = "uint2"
tidMSLType STid3{} = "uint3"

-- `tidMTLSize` produces the `(width, height, depth)` triple for `MTLSize` in the Swift dispatch call. Metal convention places the innermost
-- (fastest-varying) axis in `width` (= `x`), so for `Tid2 m n` we emit `width = n`, `height = m`. Unused axes are set to 1.

tidMTLSize :: SMTLTid k -> (Int, Int, Int)
tidMTLSize (STid1 @n)       =
   ( fromIntegral (natVal (Proxy @n)), 1, 1 )
tidMTLSize (STid2 @m @n)    =
   ( fromIntegral (natVal (Proxy @n))
   , fromIntegral (natVal (Proxy @m))
   , 1 )
tidMTLSize (STid3 @d @m @n) =
   ( fromIntegral (natVal (Proxy @n))
   , fromIntegral (natVal (Proxy @m))
   , fromIntegral (natVal (Proxy @d)) )

-- `tidFlatExpr` emits the MSL expression that linearises a multi-dimensional thread id into a flat buffer index. For 1-D the thread id is the
-- index. For higher ranks it computes the standard row-major formula:
--
-- > 2-D:  tid.y * n       + tid.x
-- > 3-D:  tid.z * (m * n) + tid.y * n + tid.x
--
-- The string argument `v` is the MSL variable name (usually `tid`).

tidFlatExpr :: SMTLTid k -> String -> String
tidFlatExpr STid1{}          v = v
tidFlatExpr (STid2 @m @n)    v =
   v ++ ".y * " ++ show (natVal (Proxy @n)) ++ "u + " ++ v ++ ".x"
tidFlatExpr (STid3 @d @m @n) v =
   v ++ ".z * " ++ show (natVal (Proxy @m) * natVal (Proxy @n)) ++ "u"
   ++ " + " ++ v ++ ".y * " ++ show (natVal (Proxy @n)) ++ "u"
   ++ " + " ++ v ++ ".x"

-- || Naperian positions as Metal thread IDs
--
-- Metal GPU threads are addressed by a position type: `uint`, `uint2`, or `uint3` (the ``\[\[thread_position_in_grid\]\]``
-- attribute; we omit the `ushort` variants throughout this post). The key insight driving this module is to identify `Log f` —
-- the Naperian position type — with Metal's thread-position type. Under this identification:
--
-- - `tabulate` becomes "one thread per element, writing the output buffer"
-- - `index`    becomes "one thread reads its element from the input buffer"
-- - A kernel combinator `tabulate . f . index` fuses into a single GPU dispatch where every thread reads, transforms, and writes one element.

-- || The DimMTL typeclass
--
-- `DimMTL` extends `Dimension` with a Metal dispatch witness. The `Dimension` superclass (defined earlier) provides the full Gibbons
-- interface — Naperian (positions via `tabulate` / `index`), zippy `Applicative`, and `Foldable`.  `DimMTL` adds:
--
-- - The associated type `TidOf f`, selecting which `MTLTidKind` variant the functor corresponds to.
-- - The singleton value `stid`, through which the emitter can query grid sizes, MSL type keywords, and flat-index formulas at
--   runtime.
--
-- By construction, `Log f = TidIndex (TidOf f)` for every `DimMTL` instance — the Naperian position type and the Metal thread-index
-- type coincide.

class Dimension f => DimMTL f where

   -- The Metal grid kind for this dimension.
   type TidOf f :: MTLTidKind

   -- Singleton value; lets the emitter inspect sizes at runtime.
   stid :: SMTLTid (TidOf f)

-- || HyperMTL GADT

-- `HyperMTL` is a rank-polymorphic tensor indexed by a type-level list of dimension functors, directly mirroring Gibbons' `Hyper`
-- type. The list is ordered /innermost-first/:
--
-- >  HyperMTL '[Vec 4, Mat 2 3] Float
-- >  =  a Mat 2 3 of (Vec 4 of Float)
-- >  =  a 2×3 matrix whose elements are 4-vectors of Float
--
-- Two constructors build the structure:
--
-- - `Scalar a` — a rank-0 tensor (a single value, no dimensions).
-- - `Prism (HyperMTL fs (f a))`` — peels one dimension off. The `DimMTL f` constraint is stored inside the constructor as
--   existential evidence, so pattern-matching on `Prism` brings the full DimMTL dictionary (including `stid`, `tabulate`, `index`)
--   into scope. This is how kernel combinators recover the dispatch geometry from the tensor's type without the caller threading
--   explicit dictionaries.

type HyperMTL :: [Type -> Type] -> Type -> Type
data HyperMTL fs a where
   Scalar :: a
          -> HyperMTL '[]       a
   Prism  :: DimMTL f
          => HyperMTL fs (f a)
          -> HyperMTL (f ': fs) a

-- | A typed AST for MSL expressions
--
-- Kernel combinators (in the next section) do not build MSL source strings directly. Instead they compose `MSLExpr` trees whose type
-- parameter tracks the MSL type of the expression. In this waym every well-typed `MSLExpr a` is guaranteed to emit well-typed MSL,

-- || Typed MSL expression tree
--
-- `MSLExpr a` is a GADT indexed by the Haskell type that mirrors the MSL type of the expression:
--
-- - `MSLExpr Float`  ≈  `float` in MSL
-- - `MSLExpr Word32` ≈  `uint`  in MSL
-- - `MSLExpr Bool`   ≈  `bool`  in MSL
-- - `MSLExpr (a,b)`  ≈  a struct / float2 pair
--
-- The constructors fall into six families:
--
-- - *Literals and variables* inject Haskell values or named MSL variables into the tree. `Var` is polymorphic in `a` to allow the
--   emitter to reference pre-declared locals of any type.
--
-- - *Thread position* `TidFlat`, `TidX`, `TidY`, `TidZ` are nullary constructors representing the `\[\[thread_position_in_grid\]\]`
--   components.
--
-- - *Float arithmetic* is the standard math operations (`+, -, *, /`, transcendentals, min/max, abs).
--
-- - *Uint arithmetic*:`AddU`, `MulU` for index computation (flat-index formulas).
--
-- - *Boolean*: comparisons (`<, >, <=, >=, ==`), connectives (`&&, ||, !`), and `SelF` which compiles to Metal's `select()`
--   intrinsic (a branchless ternary).
--
-- - *Products and buffer access*: `paire`/`fste`/`snde` model multi-value returns (emitted as struct literals / `.x`, `.y`
--   accessors), and `bufidx` models a device-buffer subscript `buf[i]`.

-- TODO: for simplicity, we can only consider float type in this prose, which i suspect is actually what's going on in later sections.
data MSLExpr :: Type -> Type where
   LitF    :: Float   -> MSLExpr Float
   LitU    :: Int     -> MSLExpr Word32
   LitB    :: Bool    -> MSLExpr Bool
   Var     :: String  -> MSLExpr a

   TidFlat :: MSLExpr Word32
   TidX    :: MSLExpr Word32
   TidY    :: MSLExpr Word32
   TidZ    :: MSLExpr Word32

   AddF, SubF, MulF, DivF :: MSLExpr Float -> MSLExpr Float -> MSLExpr Float
   NegF    :: MSLExpr Float -> MSLExpr Float
   SqrtF, SinF, CosF, ExpF, LogF :: MSLExpr Float -> MSLExpr Float
   MinF, MaxF :: MSLExpr Float -> MSLExpr Float -> MSLExpr Float
   AbsF    :: MSLExpr Float -> MSLExpr Float

   AddU, MulU :: MSLExpr Word32 -> MSLExpr Word32 -> MSLExpr Word32

   LtF, GtF, LeF, GeF, EqF :: MSLExpr Float -> MSLExpr Float -> MSLExpr Bool
   AndB, OrB :: MSLExpr Bool -> MSLExpr Bool -> MSLExpr Bool
   NotB    :: MSLExpr Bool -> MSLExpr Bool
   SelF    :: MSLExpr Bool -> MSLExpr Float -> MSLExpr Float -> MSLExpr Float

   PairE   :: MSLExpr a -> MSLExpr b -> MSLExpr (a, b)
   FstE    :: MSLExpr (a, b) -> MSLExpr a
   SndE    :: MSLExpr (a, b) -> MSLExpr b

   BufIdx  :: String -> MSLExpr Word32 -> MSLExpr Float

-- || The expression emitter
--
-- The emitter is essentially a syntax-directed translation from the typed expression tree to an MSL source string. Each GADT
-- constructor maps to exactly one MSL syntax form, the translation is a simple structural recursion with no non-trivial optimisation
-- passes.
--
-- Notable details:
-- - Float literals are suffixed with `f` (e.g. `3.14f`) per MSL rules.
-- - Uint  literals are suffixed with `u`.
-- - `SelF` emits `select(f, t, c)` Metal's branchless ternary; note the argument order is (false-value, true-value, condition).
-- - `PairE` emits a brace-initialiser `{a, b}` (struct literal).
-- - `FstE`/`SndE` emit `.x`/`.y` component access.

emitExpr :: MSLExpr a -> String
emitExpr (LitF f)       = show f ++ "f"
emitExpr (LitU n)       = show n ++ "u"
emitExpr (LitB True)    = "true"
emitExpr (LitB False)   = "false"
emitExpr (Var v)        = v
emitExpr TidFlat        = "tid"
emitExpr TidX           = "tid.x"
emitExpr TidY           = "tid.y"
emitExpr TidZ           = "tid.z"
emitExpr (AddF a b)     = "(" ++ emitExpr a ++ " + " ++ emitExpr b ++ ")"
emitExpr (SubF a b)     = "(" ++ emitExpr a ++ " - " ++ emitExpr b ++ ")"
emitExpr (MulF a b)     = "(" ++ emitExpr a ++ " * " ++ emitExpr b ++ ")"
emitExpr (DivF a b)     = "(" ++ emitExpr a ++ " / " ++ emitExpr b ++ ")"
emitExpr (NegF a)       = "(-" ++ emitExpr a ++ ")"
emitExpr (SqrtF a)      = "sqrt(" ++ emitExpr a ++ ")"
emitExpr (SinF a)       = "sin(" ++ emitExpr a ++ ")"
emitExpr (CosF a)       = "cos(" ++ emitExpr a ++ ")"
emitExpr (AbsF a)       = "abs(" ++ emitExpr a ++ ")"
emitExpr (MinF a b)     = "min(" ++ emitExpr a ++ ", " ++ emitExpr b ++ ")"
emitExpr (MaxF a b)     = "max(" ++ emitExpr a ++ ", " ++ emitExpr b ++ ")"
emitExpr (LtF a b)      = "(" ++ emitExpr a ++ " < "  ++ emitExpr b ++ ")"
emitExpr (GtF a b)      = "(" ++ emitExpr a ++ " > "  ++ emitExpr b ++ ")"
emitExpr (LeF a b)      = "(" ++ emitExpr a ++ " <= " ++ emitExpr b ++ ")"
emitExpr (GeF a b)      = "(" ++ emitExpr a ++ " >= " ++ emitExpr b ++ ")"
emitExpr (EqF a b)      = "(" ++ emitExpr a ++ " == " ++ emitExpr b ++ ")"
emitExpr (AndB a b)     = "(" ++ emitExpr a ++ " && " ++ emitExpr b ++ ")"
emitExpr (OrB a b)      = "(" ++ emitExpr a ++ " || " ++ emitExpr b ++ ")"
emitExpr (NotB a)       = "!(" ++ emitExpr a ++ ")"
emitExpr (SelF c t f)   = "select(" ++ emitExpr f ++ ", " ++ emitExpr t ++ ", " ++ emitExpr c ++ ")"
emitExpr (FstE e)       = emitExpr e ++ ".x"
emitExpr (SndE e)       = emitExpr e ++ ".y"
emitExpr (ExpF a)       = "exp(" ++ emitExpr a ++ ")"
emitExpr (LogF a)       = "log(" ++ emitExpr a ++ ")"
emitExpr (AddU a b)     = "(" ++ emitExpr a ++ " + " ++ emitExpr b ++ ")"
emitExpr (MulU a b)     = "(" ++ emitExpr a ++ " * " ++ emitExpr b ++ ")"
emitExpr (BufIdx buf i) = buf ++ "[" ++ emitExpr i ++ "]"
emitExpr (PairE a b)    = "{" ++ emitExpr a ++ ", " ++ emitExpr b ++ "}"

-- || The necessity of a monoid descriptor
--
-- An `MSLMonoid` names an *associative binary operation with identity*, the algebraic structure required for a correct parallel
-- reduction. Associativity is what allows the GPU to partition the input arbitrarily across SIMD lanes and threadgroups, combine
-- partial results in any order, and still obtain the same answer.
--
-- Each variant determines three code-generation artefacts:
--
-- - `monoidUnit` is the identity element, used to pad out-of-bounds lanes so they do not affect the result.
-- - `monoidOp`   is the scalar binary combiner, used in the inter-group reduction pass.
-- - `monoidSimd` is the Apple SIMD-group intrinsic (`simd_sum`, etc.) that reduces a warp's worth of values in hardware.
--
-- Together, these three projections let the `foldK` combinator emit a fully specialised reduction kernel for any of the four common
-- monoids without runtime branching.

data MSLMonoid
   = MonoidSum     -- identity 0.0,  op +,   intrinsic simd_sum
   | MonoidProduct -- identity 1.0,  op *,   intrinsic simd_product
   | MonoidMax     -- identity -INF, op max, intrinsic simd_max
   | MonoidMin     -- identity +INF, op min, intrinsic simd_min

-- `monoidUnit` is the identity element as an MSL literal string.

monoidUnit :: MSLMonoid -> String
monoidUnit MonoidSum     = "0.0f"
monoidUnit MonoidProduct = "1.0f"
monoidUnit MonoidMax     = "-INFINITY"
monoidUnit MonoidMin     = "INFINITY"

-- `monoidOp` is the scalar binary combiner, emitted as an infix expression or function call.

monoidOp :: MSLMonoid -> String -> String -> String
monoidOp MonoidSum     a b = "(" ++ a ++ " + " ++ b ++ ")"
monoidOp MonoidProduct a b = "(" ++ a ++ " * " ++ b ++ ")"
monoidOp MonoidMax     a b = "max(" ++ a ++ ", " ++ b ++ ")"
monoidOp MonoidMin     a b = "min(" ++ a ++ ", " ++ b ++ ")"

-- `monoidSimd` is The SIMD-group intrinsic that reduces all lanes in a single warp.  These intrinsics execute in hardware with no
-- shared-memory traffic.

monoidSimd :: MSLMonoid -> String -> String
monoidSimd MonoidSum     v = "simd_sum(" ++ v ++ ")"
monoidSimd MonoidProduct v = "simd_product(" ++ v ++ ")"
monoidSimd MonoidMax     v = "simd_max(" ++ v ++ ")"
monoidSimd MonoidMin     v = "simd_min(" ++ v ++ ")"

-- | Kernel specification, combinators, and two-backend emitter.

-- TODO: we should write a more continuous intro for this.
--
-- TODO: we should provide a real metal kernel function here and explain how we will map to it
--
-- The code-generation pipeline is twofold:
--
-- TODO: add more combinators, ideally, all from futhark plus traverse
--
-- *Phase 1 (combinators)*: `mapK` & `foldK`, each construct a declarative `KernelSpec` value: a plain record
-- with no closures describing a single Metal compute kernel. The `MetalM` monad collects these specs via `WriterT`.
--
-- *Phase 2 (emitters)*: `emitMSLFile` renders the specs into a `.metal` source file; `emitSwiftFile` renders a companion Swift
-- harness that compiles the MSL at runtime, allocates buffers, dispatches kernels, and reads back results. Because both emitters
-- consume the same `KernelSpec`, the MSL and Swift outputs are guaranteed to agree on buffer indices, grid sizes, and uniform
-- bindings.
--
-- -- TODO: add more elaboration on how combinators and emitters co-work

-- || Buffer/Kernel specifications
--
-- TODO: need more illustrations here
--
-- - `bsIndex` / the `\[\[buffer(i)\]\]` binding index.
-- - `bsAccess`/ the MSL access qualifier, e.g. `device const float*` for a read-only input or `device float*` for output.
-- - `bsName`  / the variable name used inside the kernel body.
-- - `bsSize`  / the element count; used by the Swift emitter to allocate the corresponding `MTLBuffer`.

data BufferSpec = BufferSpec
   { bsIndex  :: Int
   , bsAccess :: String
   , bsName   :: String
   , bsSize   :: Int
   }

-- A fully inspectable, closure-free description of a single Metal compute kernel.  This is the /sole intermediate representation/
-- between the combinators and both emitters — a deliberate design choice that keeps the two backends decoupled and each individually
-- testable.
--
-- * `ksName`        the kernel function name (`kernel void <name>`).
-- * `ksTidType`     `uint`, `uint2`, or `uint3`; determines the
--                    type of the `tid` parameter.
-- * `ksMTLSize`     `(width, height, depth)` for `MTLSize` in the
--                    Swift `dispatchThreads` call.
-- * `ksBuffers`     ordered list of buffer bindings.
-- * `ksUniforms`    `(msl-type, name, swift-value)` triples; emitted
--                    as `constant uint& name \[\[buffer(...)\]\]` in MSL
--                    and as a small `MTLBuffer` in Swift.
-- * `ksTGMem`       optional threadgroup shared memory allocation
--                    `(element-type, count)`.
-- * `ksExtraAttrs`  additional `\[\[...\]\]` kernel parameters such as
--                    `lid`, `gid`, `tpg` for reduction/stencil kernels.
-- * `ksBodyLines`   the actual MSL statements forming the kernel body.

data KernelSpec = KernelSpec
   { ksName       :: String
   , ksTidType    :: String
   , ksMTLSize    :: (Int, Int, Int)
   , ksBuffers    :: [BufferSpec]
   , ksUniforms   :: [(String, String, String)]
   , ksTGMem      :: Maybe (String, Int)
   , ksExtraAttrs :: [(String, String)]
   , ksBodyLines  :: [String]
   }

-- || MetalM is WriterT [KernelSpec] (State Int)
--
-- The `WriterT` layer accumulates kernel specs as each combinator fires; the `State Int` layer provides a monotonic counter for
-- generating fresh variable names (used when a combinator needs auxiliary locals).

type MetalM = WriterT [KernelSpec] (State Int)

-- Generate a fresh variable name (`v0`, `v1`, ...).

fresh :: MetalM String
fresh = do n <- get; put (n+1); return ("v" ++ show n)

--  Append a kernel spec to the writer output.

emit :: KernelSpec -> MetalM ()
emit k = tell [k]

-- || Kernel combinators

-- `mapK`/The fundamental parallel map the GPU realisation of Gibbons' `tabulate . f . lookup` fusion.
--
-- Semantics: given an input tensor of shape @f@ and a per-element function `body`, emit a kernel where each thread:
--
-- - computes its flat buffer index from `tid` (via `tidFlatExpr`),
-- - reads `input[idx]`,
-- - applies `body` (an `MSLExpr Float -> MSLExpr Float` — a HOAS function that builds the expression tree for the transformation),
-- - writes the result to `output[idx]`.
--
-- The `HyperMTL '[f] Float` argument is used /only/ for its type: the
-- emitter calls `stid \`f` to recover grid sizes and the MSL tid type.
-- Its runtime value is discarded (hence `_`).  This is a deliberate
-- design: shape information flows entirely through the type parameter `f`,
-- not through runtime data.

mapK :: forall f. DimMTL f
     => String                             -- kernel name
     -> HyperMTL '[f] Float                -- input (for shape info only)
     -> (MSLExpr Float -> MSLExpr Float)   -- HOAS body: in -> out
     -> MetalM ()
mapK name _ body = emit KernelSpec
   { ksName       = name
   , ksTidType    = tidMSLType (stid @f)
   , ksMTLSize    = tidMTLSize (stid @f)
   , ksBuffers    =
       [ BufferSpec 0 "device const float*" "input"  (tidFlatSize (stid @f))
       , BufferSpec 1 "device       float*" "output" (tidFlatSize (stid @f)) ]
   , ksUniforms   = []
   , ksTGMem      = Nothing
   , ksExtraAttrs = []
   , ksBodyLines  =
       let flatIdx = tidFlatExpr (stid @f) "tid"
           inElem  = BufIdx "input" (Var flatIdx)
           outExpr = emitExpr (body inElem)
       in  [ "uint idx = " ++ flatIdx ++ ";"
           , "output[idx] = " ++ outExpr ++ ";" ]
   }

-- `foldk`/Parallel reduction using Apple Silicon's SIMD-group intrinsics and threadgroup shared memory.
--
-- The generated kernel implements a two-level reduction:
--
-- - *Intra-SIMD-group*:  Each thread loads one element (or the monoid identity if out of bounds).  The `simd_sum` / `simd_max` /
--      etc. intrinsic reduces all 32 lanes in hardware — no shared-memory traffic, no explicit loop.
--
-- - *Inter-SIMD-group*:  The first lane of each SIMD group writes its partial result into threadgroup shared memory.  After a
--      barrier, the first SIMD group reduces these partials with a second call to the same SIMD intrinsic.  Thread 0 then writes the
--      threadgroup's result to the `partials` output buffer.
--
-- Regardless of the input tensor's dimensionality, reductions are always dispatched as a flat 1-D grid (`ksTidType = "uint"``). The
-- `MSLMonoid` parameter determines which identity, combiner, and SIMD intrinsic appear in the generated code.

foldK :: forall f. DimMTL f
      => String
      -> HyperMTL '[f] Float
      -> MSLMonoid
      -> MetalM ()
foldK name _ m = emit KernelSpec
   { ksName       = name
   , ksTidType    = "uint"
   , ksMTLSize    = (tidFlatSize (stid @f), 1, 1)
   , ksBuffers    =
       [ BufferSpec 0 "device const float*" "input"    (tidFlatSize (stid @f))
       , BufferSpec 1 "device       float*" "partials" 256 ]
   , ksUniforms   = [("uint", "gsize", show (tidFlatSize (stid @f)))]
   , ksTGMem      = Just ("float", 32)
   , ksExtraAttrs =
       [ ("uint", "lid [[thread_position_in_threadgroup]]")
       , ("uint", "gid [[threadgroup_position_in_grid]]")
       , ("uint", "tpg [[threads_per_threadgroup]]") ]
   , ksBodyLines  =
       [ "float val = (tid < gsize) ? input[tid] : " ++ monoidUnit m ++ ";"
       , "val = " ++ monoidSimd m "val" ++ ";"
       , "if (simd_is_first()) shared[lid / 32] = val;"
       , "threadgroup_barrier(mem_flags::mem_threadgroup);"
       , "if (lid < tpg / 32)"
       , "    val = " ++ monoidSimd m "shared[lid]" ++ ";"
       , "if (lid == 0) partials[gid] = val;" ]
   }

-- || Emitters
--
-- The two emitters below translate the list of `KernelSpec`s into complete, self-contained source files:
--
-- - `emitMSLFile`   → `kernels.metal`  (Metal Shading Language)
-- - `emitSwiftFile` → `harness.swift` (Swift host program)
--
-- Each `KernelSpec` field maps to a specific region of the output:
--
--   `ksBuffers`    →  `[[buffer(i)]]` params (MSL) / `makeBuffer` calls (Swift)
--   `ksTGMem`      →  `[[threadgroup(0)]]` param / `setThreadgroupMemoryLength`
--   `ksUniforms`   →  `constant uint& name [[buffer(...)]]` / small UInt32 buffers
--   `ksTidType`    →  `uint / uint2 / uint3 tid [[thread_position_in_grid]]`
--   `ksExtraAttrs` →  additional Metal attribute params (lid, gid, tpg)
--   `ksMTLSize`    →  `MTLSize(width:height:depth:)` in the dispatch call
--   `ksBodyLines`  →  the kernel function body, indented 4 spaces

-- `emitMSLFile` renders the full `.metal` source file: standard includes, namespace declaration, then each kernel in sequence.

emitMSLFile :: [KernelSpec] -> String
emitMSLFile ks = unlines $
   [ "#include <metal_stdlib>"
   , "#include <simd/simd.h>"
   , "using namespace metal;"
   , "" ] ++
   concatMap emitOneKernel ks

-- `emitOneKernel` renders one `kernel void` function. The parameter list is assembled from the spec's buffers, threadgroup memory,
-- uniforms, tid, and extra attributes, then joined with commas.

emitOneKernel :: KernelSpec -> [String]
emitOneKernel KernelSpec{..} =
   let params =
         [ bsAccess b ++ " " ++ bsName b
           ++ " [[buffer(" ++ show (bsIndex b) ++ ")]]"
         | b <- ksBuffers ] ++
         [ "threadgroup " ++ et ++ "* shared [[threadgroup(0)]]"
         | Just (et, _) <- [ksTGMem] ] ++
         [ "constant uint& " ++ uname
          ++ " [[buffer(" ++ show (P.length ksBuffers + i) ++ ")]]"
         | (i, (_, uname, _)) <- zip [0..] ksUniforms ] ++
         [ ksTidType ++ " tid [[thread_position_in_grid]]" ] ++
         [ ty ++ " " ++ attr | (ty, attr) <- ksExtraAttrs ]
       sep = intercalate ",\n    " params
   in  [ "kernel void " ++ ksName ++ "("
       , "    " ++ sep
       , ") {" ] ++
       [ "    " ++ line | line <- ksBodyLines ] ++
       [ "}", "" ]

-- `emitSwiftFile` renders the complete Swift harness: preamble (device/queue setup, helper functions), one runner function per
-- kernel, and a `validate()` entry point that exercises them all.

emitSwiftFile :: [KernelSpec] -> String
emitSwiftFile ks = unlines $
   swiftPreamble ++
   concatMap emitSwiftRunner ks ++
   swiftMain ks

-- `swiftPreamble` is a shared Swift boilerplate: acquire the default Metal device, create a command queue, compile the MSL source at
-- runtime, and provide helper functions for buffer allocation and readback.

swiftPreamble :: [String]
swiftPreamble =
   [ "import Metal"
   , "import Foundation"
   , ""
   , "let device = MTLCreateSystemDefaultDevice()!"
   , "let queue  = device.makeCommandQueue()!"
   , ""
   , "func compileKernels(mslPath: String) -> MTLLibrary {"
   , "    let src = try! String(contentsOfFile: mslPath, encoding: .utf)"
   , "    return try! device.makeLibrary(source: src, options: nil)"
   , "}"
   , ""
   , "func makeBuffer<T>(_ data: [T]) -> MTLBuffer {"
   , "    return data.withUnsafeBytes {"
   , "        device.makeBuffer(bytes: $0.baseAddress!, length: $0.count,"
   , "                          options: .storageModeShared)!"
   , "    }"
   , "}"
   , ""
   , "func makeEmptyBuffer(floatCount: Int) -> MTLBuffer {"
   , "    return device.makeBuffer(length: floatCount * 4, options: .storageModeShared)!"
   , "}"
   , ""
   , "func readBuffer(_ buf: MTLBuffer, count: Int) -> [Float] {"
   , "    let ptr = buf.contents().bindMemory(to: Float.self, capacity: count)"
   , "    return Array(UnsafeBufferPointer(start: ptr, count: count))"
   , "}"
   , ""
   , "let library = compileKernels(mslPath: \"kernels.metal\")"
   , "" ]

-- `emitSwiftRunner` generates a Swift function `run_<name>(inputs...) -> [Float]` that:
-- - Looks up the kernel function in the compiled library.
-- - Creates a compute pipeline state.
-- - Allocates input buffers (from the caller's `[Float]` arrays) and output buffers (empty, sized from `bsSize`).
-- - Binds all buffers, uniforms, and threadgroup memory.
-- - Dispatches with the grid/threadgroup sizes from `ksMTLSize`.
-- - Waits for completion and reads back the output buffer(s).

emitSwiftRunner :: KernelSpec -> [String]
emitSwiftRunner KernelSpec{..} =
   let inBufs  = filter (\b -> "const" `isInfixOf` bsAccess b) ksBuffers
       outBufs = filter (\b -> not ("const" `isInfixOf` bsAccess b)) ksBuffers
       (w,h,d) = ksMTLSize
   in
   [ "func run_" ++ ksName ++ "(" ++
     intercalate ", " ["_ " ++ bsName b ++ ": [Float]" | b <- inBufs] ++
    ") -> " ++ (if P.length outBufs == 1 then "[Float]" else "([Float], [Float])") ++ " {"
   , "    let fn  = library.makeFunction(name: \"" ++ ksName ++ "\")!"
   , "    let pso = try! device.makeComputePipelineState(function: fn)"
   , "    let cb  = queue.makeCommandBuffer()!"
   , "    let enc = cb.makeComputeCommandEncoder()!"
   , "    enc.setComputePipelineState(pso)" ] ++
   [ "    let buf" ++ show (bsIndex b) ++ " = makeBuffer(" ++ bsName b ++ ")"
   | b <- inBufs ] ++
   [ "    let buf" ++ show (bsIndex b) ++ " = makeEmptyBuffer(floatCount: " ++ show (bsSize b) ++ ")"
   | b <- outBufs ] ++
   [ "    enc.setBuffer(buf" ++ show (bsIndex b) ++ ", offset: 0, index: " ++ show (bsIndex b) ++ ")"
   | b <- ksBuffers ] ++
   concatMap (\(i, (_, uname, uval)) ->
    let idx = P.length ksBuffers + i
     in  [ "    var " ++ uname ++ "_val: UInt32 = " ++ uval
         , "    let ubuf" ++ show idx ++ " = device.makeBuffer(bytes: &" ++ uname ++ "_val, length: 4, options: .storageModeShared)!"
         , "    enc.setBuffer(ubuf" ++ show idx ++ ", offset: 0, index: " ++ show idx ++ ")" ])
     (zip [0..] ksUniforms) ++
   (case ksTGMem of
     Just (_, n) -> ["    enc.setThreadgroupMemoryLength(" ++ show (n*4) ++ ", index: 0)"]
     Nothing     -> []) ++
   [ "    let grid = MTLSize(width: " ++ show w ++ ", height: " ++ show h ++ ", depth: " ++ show d ++ ")"
   , "    let tg   = MTLSize(width: min(256, " ++ show w ++ "), height: " ++ (if h > 1 then "min(16, " ++ show h ++ ")" else "1") ++ ", depth: 1)"
   , "    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)"
   , "    enc.endEncoding()"
   , "    cb.commit(); cb.waitUntilCompleted()"
   , "    return " ++ case outBufs of
       [b] -> "readBuffer(buf" ++ show (bsIndex b) ++ ", count: " ++ show (bsSize b) ++ ")"
       _   -> "(" ++ intercalate ", " ["readBuffer(buf" ++ show (bsIndex b) ++ ", count: " ++ show (bsSize b) ++ ")" | b <- outBufs] ++ ")"
   , "}"
   , "" ]

-- `swiftMain` emits the `validate()` entry point that creates synthetic input data for every kernel, runs it, and prints the first 8
-- output elements.

swiftMain :: [KernelSpec] -> [String]
swiftMain ks =
   [ "// ── Validation entry point ───────────────────────────────────────────"
   , "func validate() {" ] ++
   concatMap validationSnippet ks ++
   [ "    print(\"All validations passed.\")"
   , "}"
   , "validate()" ]

validationSnippet :: KernelSpec -> [String]
validationSnippet KernelSpec{..} =
   let inBufs  = filter (\b -> "const" `isInfixOf` bsAccess b) ksBuffers
       pre     = ksName ++ "_"
       mkData b = pre ++ bsName b
   in  [ "    // ── " ++ ksName ] ++
       [ "    let " ++ mkData b ++ ": [Float] = (0..<" ++ show (bsSize b)
         ++ ").map { Float($0) + 1.0 }"
       | b <- inBufs ] ++
       [ "    let " ++ pre ++ "out = run_" ++ ksName ++ "("
         ++ intercalate ", " [ mkData b | b <- inBufs ] ++ ")" ] ++
       [ "    print(\"  " ++ ksName ++ ": \\(" ++ pre ++ "out.prefix(8))\")"
       , "" ]

-- || Top-level runner

-- Run the `MetalM` monadic program, extract the accumulated `[KernelSpec]`, render both output files, write them to disk, and print
-- the generated source together with a build command.
--
-- The evaluation sequence is:
-- - `runWriterT prog` produces `((), [KernelSpec])` inside `State Int`.
-- - `evalState ... 0` runs the fresh-name counter starting at 0.
-- - `emitMSLFile` and `emitSwiftFile` render the specs to strings.
-- - `writeFile` persists them as `kernels.metal` and `harness.swift`.

runMetal :: MetalM () -> IO ()
runMetal prog = do
   let (_, kernels) = evalState (runWriterT prog) 0
   let msl   = emitMSLFile kernels
   let swift = emitSwiftFile kernels
   writeFile "kernels.metal" msl
   writeFile "harness.swift" swift
   putStrLn $ "=== kernels.metal ===\n" ++ msl
   putStrLn $ "=== harness.swift ===\n" ++ swift
   putStrLn "Build with:\n  xcrun -sdk macosx swiftc harness.swift -o run_kernels && ./run_kernels"

-- | At last, examples & some contemplations
--
-- Each newtype wraps a flat Haskell list but carries its shape at the type level via @Nat@ parameters.  For each shape we provide:
--
-- - A `Naperian` instance — the position type `Log f` and the iso (`tabulate` / `index`).
-- - A zippy `Applicative` instance — `(\<*\>)` pairs elements position-wise.
-- - A `Dimension` instance (implied by the above plus derived `Functor`, `Foldable`).
-- - A `DimMTL` instance — `TidOf` and `stid` for Metal dispatch.

-- || Vec n
--
-- A 1-D array of exactly `n` elements — the simplest Naperian functor.
--
-- Its logarithm is `Fin n` (a single bounded integer), and the
-- corresponding Metal dispatch is a flat `uint tid` grid of `n` threads.
-- `tabulate f = Vec [f (Fin 0), f (Fin 1), ..., f (Fin (n-1))]` — one
-- element per position, mirroring one GPU thread per output slot.
--
-- Mapping to Metal:
--
-- >  TidOf (Vec n) = Tid1 n
-- >  → kernel void k(..., uint tid [[thread_position_in_grid]])
-- >  → dispatchThreads(MTLSize(width: n, height: 1, depth: 1), ...)

type Vec :: Nat -> Type -> Type
newtype Vec n a = Vec { unVec :: [a] }
   deriving (Show)

instance Functor (Vec n) where
   fmap f (Vec xs) = Vec (P.map f xs)

instance Foldable (Vec n) where
   foldr f z (Vec xs) = P.foldr f z xs

instance KnownNat n => Applicative (Vec n) where
   pure x         = Vec (replicate n' x)
      where n'    = fromIntegral (natVal (Proxy @n))
   Vec fs <*> Vec xs = Vec (zipWith ($) fs xs)

instance KnownNat n => Naperian (Vec n) where
   type Log (Vec n) = Fin n
   tabulate f       = Vec [ f (Fin i) | i <- [0 .. n' - 1] ]
      where n'      = fromIntegral (natVal (Proxy @n))
   index (Vec xs) (Fin i) = xs !! i

instance KnownNat n => Dimension (Vec n)

instance KnownNat n => DimMTL (Vec n) where
   type TidOf (Vec n) = 'Tid1 n
   stid               = STid1

-- || Mat m n
--
-- A 2-D row-major matrix with `m` rows and `n` columns.
--
-- The logarithm is `(Fin m, Fin n)`, a (row, column) pair and the Metal dispatch is `uint2 tid` where, by Metal convention,
-- `tid.x` is the column (inner/fastest axis) and `tid.y` is the row. The flat buffer index is the standard row-major formula:
--
-- > flat = row * n + col
--
-- Mapping to Metal:
--
-- > TidOf (Mat m n) = Tid2 m n
-- > → kernel void k(..., uint2 tid [[thread_position_in_grid]])
-- > → dispatchThreads(MTLSize(width: n, height: m, depth: 1), ...)
-- > → flat index: tid.y * n + tid.x

type Mat :: Nat -> Nat -> Type -> Type
newtype Mat m n a = Mat { unMat :: [a] }
   deriving (Show)

instance Functor (Mat m n) where
   fmap f (Mat xs) = Mat (P.map f xs)

instance Foldable (Mat m n) where
   foldr f z (Mat xs) = P.foldr f z xs

instance (KnownNat m, KnownNat n) => Applicative (Mat m n) where
   pure x         = Mat (replicate (m' * n') x)
      where
         m'       = fromIntegral (natVal (Proxy @m))
         n'       = fromIntegral (natVal (Proxy @n))
   Mat fs <*> Mat xs = Mat (zipWith ($) fs xs)

instance (KnownNat m, KnownNat n) => Naperian (Mat m n) where
   type Log (Mat m n)   = (Fin m, Fin n)
   tabulate f           = Mat [ f (Fin r, Fin c)
                              | r <- [0 .. m' - 1]
                              , c <- [0 .. n' - 1] ]
      where
         m'             = fromIntegral (natVal (Proxy @m))
         n'             = fromIntegral (natVal (Proxy @n))
   index (Mat xs) (Fin r, Fin c) = xs !! (r * n' + c)
      where n'          = fromIntegral (natVal (Proxy @n))

instance (KnownNat m, KnownNat n) => Dimension (Mat m n)

instance (KnownNat m, KnownNat n) => DimMTL (Mat m n) where
   type TidOf (Mat m n) = 'Tid2 m n
   stid                 = STid2

-- || Cube d m n

-- A 3-D tensor with `d` slices, `m` rows, and `n` columns.
--
-- The logarithm is `(Fin d, Fin m, Fin n)`, a (slice, row, column) triple — and the Metal dispatch is `uint3 tid`. The flat buffer
-- index extends the row-major convention to three axes:
--
-- > flat = slice * (m * n) + row * n + col
--
-- Mapping to Metal:
--
-- > TidOf (Cube d m n) = Tid3 d m n
-- > → kernel void k(..., uint3 tid [[thread_position_in_grid]])
-- > → dispatchThreads(MTLSize(width: n, height: m, depth: d), ...)
-- > → flat index: tid.z * (m*n) + tid.y * n + tid.x

type Cube :: Nat -> Nat -> Nat -> Type -> Type
newtype Cube d m n a = Cube { unCube :: [a] }
   deriving (Show)

instance Functor (Cube d m n) where
   fmap f (Cube xs) = Cube (P.map f xs)

instance Foldable (Cube d m n) where
   foldr f z (Cube xs) = P.foldr f z xs

instance (KnownNat d, KnownNat m, KnownNat n) => Applicative (Cube d m n) where
   pure x            = Cube (replicate (d' * m' * n') x)
      where
         d'          = fromIntegral (natVal (Proxy @d))
         m'          = fromIntegral (natVal (Proxy @m))
         n'          = fromIntegral (natVal (Proxy @n))
   Cube fs <*> Cube xs = Cube (zipWith ($) fs xs)

instance (KnownNat d, KnownNat m, KnownNat n) => Naperian (Cube d m n) where
   type Log (Cube d m n)   = (Fin d, Fin m, Fin n)
   tabulate f              = Cube [ f (Fin s, Fin r, Fin c)
                                  | s <- [0 .. d' - 1]
                                  , r <- [0 .. m' - 1]
                                  , c <- [0 .. n' - 1] ]
      where
         d'                = fromIntegral (natVal (Proxy @d))
         m'                = fromIntegral (natVal (Proxy @m))
         n'                = fromIntegral (natVal (Proxy @n))
   index (Cube xs) (Fin s, Fin r, Fin c) = xs !! (s * m' * n' + r * n' + c)
      where
         m'                = fromIntegral (natVal (Proxy @m))
         n'                = fromIntegral (natVal (Proxy @n))

instance (KnownNat d, KnownNat m, KnownNat n) => Dimension (Cube d m n)

instance (KnownNat d, KnownNat m, KnownNat n) => DimMTL (Cube d m n) where
   type TidOf (Cube d m n) = 'Tid3 d m n
   stid                    = STid3

-- || Example kernel definitions
--
-- Each example follows the same pattern:
--
-- - Build a Haskell-side tensor via `tabulate \`Shape` — this fixes the functor type (and therefore the Metal dispatch geometry) at
--   the type level.
-- - Wrap it in `Prism (Scalar inp)` to form a rank-1 `HyperMTL`.
-- - Pass it to a kernel combinator (`mapK`, `foldK`, ...) together with the body expression. The combinator emits a `KernelSpec`.
--
-- The `TypeApplications` syntax `\``(Vec 8)` is how Haskell's visible type application selects which `DimMTL` instance — and hence
-- which Metal dispatch geometry, the kernel will use. Changing `\``(Vec 8)` to `\``(Mat 4 2)` would change the generated kernel from
-- a 1-D `uint` dispatch to a 2-D `uint2` dispatch with no other code changes.

-- ||| Vec 8:  x -> x * 2 + 1
--
-- A 1-D element-wise map over 8 elements.
--
-- `tabulate \@(Vec 8)` creates an 8-element vector [0..7] on the Haskell side (used only for shape witness — the runtime values are
-- irrelevant to code generation).  `mapK \@(Vec 8)` emits a kernel dispatched as `MTLSize(width: 8, height: 1, depth: 1)` with @uint
-- tid@.  The HOAS body `\\x -> AddF (MulF x (LitF 2.0)) (LitF 1.0)` compiles to the MSL expression `(input[tid] * 2.0f) + 1.0f`.

vecMap :: MetalM ()
vecMap = do
   let inp = tabulate @(Vec 8) (\(Fin i) -> fromIntegral i)
   mapK @(Vec 8) "vec_map"
      (Prism (Scalar inp))
      (\x -> AddF (MulF x (LitF 2.0)) (LitF 1.0))

-- ||| Mat 4 4:  x -> sqrt(x)
--
-- A 2-D element-wise map over a 4x4 matrix.
--
-- `Mat 4 4` selects `Tid2 4 4`, so the generated kernel receives `uint2 tid` and dispatches as `MTLSize(width: 4, height: 4, depth:
-- 1)`. The flat index formula is `tid.y * 4u + tid.x`. The body is simply `SqrtF`, a single MSL `sqrt()` call per element.

matMap :: MetalM ()
matMap = do
   let inp = tabulate @(Mat 4 4) (\(Fin r, Fin c) -> fromIntegral (r * 4 + c + 1))
   mapK @(Mat 4 4) "mat_sqrt"
      (Prism (Scalar inp))
      SqrtF

-- ||| Cube 2 3 4:  x -> sin(x)
--
-- A 3-D element-wise map over a 2x3x4 tensor (24 elements).
--
-- `Cube 2 3 4` selects `Tid3 2 3 4`, yielding `uint3 tid` and `MTLSize(width: 4, height: 3, depth: 2)`. The flat index formula is
-- `tid.z * 12u + tid.y * 4u + tid.x`. The body applies `sin()`.

cubeMap :: MetalM ()
cubeMap = do
   let inp = tabulate @(Cube 2 3 4) (\(Fin s, Fin r, Fin c) ->
               fromIntegral (s * 12 + r * 4 + c))
   mapK @(Cube 2 3 4) "cube_sin"
      (Prism (Scalar inp))
      SinF

-- ||| Vec 256: sum reduction

-- A parallel sum reduction over 256 elements.
--
-- `foldK` always dispatches as a flat 1-D grid regardless of the input shape. `MonoidSum` selects identity `0.0f`, combiner `(+)`,
-- and SIMD intrinsic `simd_sum`. The output is a partial-sums buffer (one entry per threadgroup); a second pass would reduce these
-- partials to a scalar.

vecFold :: MetalM ()
vecFold = do
   let inp = tabulate @(Vec 256) (\(Fin i) -> 1.0)
   foldK @(Vec 256) "vec_sum"
      (Prism (Scalar inp))
      MonoidSum


-- | At last
--
-- `runMetal` executes the `MetalM` do-block: all four kernel specs are collected by the Writer, then rendered into `kernels.metal`
-- and `harness.swift`.  Running the program produces both files and prints a one-liner build command.

main :: IO ()
main = runMetal $ do
   vecMap
   matMap
   cubeMap
   vecFold
