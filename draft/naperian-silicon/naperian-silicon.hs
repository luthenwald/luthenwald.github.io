-- title       = About
-- pubDate     = 2036-11-18
-- tags        = about, personal
-- description = The typical about me page.

-- | The type-theoretic core of hyper-silicon.
--
-- Gibbons ("APLicative Programming with Naperian Functors", 2017) observes that a Naperian functor `f` satisfies the isomorphism
--
-- > f a ≅ Log f → a
--
-- where `Log f` is a type of positions (the "logarithm" of `f`). Every inhabitant of `f a` is equivalently a function from
-- positions to values, and the two directions of this iso are called `tabulate` and `lookup`.
--
-- Metal GPU threads are likewise addressed by a position type: `uint`, `uint2`, or `uint3` (the ``\[\[thread_position_in_grid\]\]``
-- attribute, we will be omitting the `ushort` in this blog). The key insight driving this module is to identify `Log f` with
-- Metal's thread-position type. Under this identification:
--
-- * `tabulate` becomes "one thread per element, writing the output array"
-- * `lookup`   becomes "one thread reads its own element from the input"
-- * A kernel combinator like `tabulate . f . lookup` fuses into a single GPU dispatch where each thread reads, transforms, and writes one element.
--
-- This section defines the type-level vocabulary for that correspondence (`MTLTidKind`, its singleton, and the `DimMTL` typeclass)
-- and the rank-polymorphic `HyperMTL` GADT that mirrors Gibbons' `Hyper`.

module HyperMTL where

import           Data.Kind    ( Type )
import           Data.Proxy   ( Proxy (..) )

import           GHC.TypeNats ( KnownNat, Nat, natVal )

-- A closed promoted data kind enumerating the three Metal dispatch geometries.  Because it is a plain Haskell `data` declaration
-- promoted via `DataKinds`, pattern-matching on its inhabitants is exhaustive — no open-world problem, no orphan instances.  Each
-- constructor carries type-level `Nat` parameters encoding the grid dimensions:
--
--   * `Tid1 n`       — a flat 1-D dispatch of `n` threads (`uint tid`).
--   * `Tid2 m n`     — a 2-D dispatch of `m × n` threads (`uint2 tid`);
--                      by Metal convention, `x` is the inner/column axis
--                      and `y` is the outer/row axis.
--   * `Tid3 d m n`   — a 3-D dispatch of `d × m × n` threads (`uint3 tid`).
--
-- The `Nat` parameters are erased at runtime, so we need a singleton
-- (see `SMTLTid` below) to recover them for the code emitter.
type MTLTidKind :: Type
data MTLTidKind
   = Tid1 Nat           -- uint  tid   ── 1-D dispatch
   | Tid2 Nat Nat       -- uint2 tid   ── 2-D dispatch (x = inner col, y = outer row)
   | Tid3 Nat Nat Nat   -- uint3 tid   ── 3-D dispatch (x, y, z)
