{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind (Type)
import GHC.TypeNats ( KnownNat, Nat )

-- This is a problem i met when trying to design a gpu programming language like futhark but exclusively for apple silicon.
-- I need to know the position of a thread in the grid, or `thread_position_in_grid`.
-- In Metal Shading Language (MSL), `thread_position_in_grid` is of type ushort1, ushort2, ushort3, uint1, uint2 or uint3.
-- For simplicity, we will only omit ushort types in this blog. Thus speaking, a thread's position
-- (we will use something like `tid` in this blog to refer to it) is of type uint1, uint2 or uint3.
--
-- A type-level natural numbers.


-- type data Nat = Z | S Nat


type data TidKind
   = Tid1 Nat
   | Tid2 Nat Nat
   | Tid3 Nat Nat Nat

-- Now consider how we will transform a data structure in Haskell to the corresponding one in MSL.
-- One solution is to define an isomorphism between the position of an item in Haskell and its position in
-- MSL.


class IsoPos f where
   type TidOf f :: TidKind
   type Pos f

   -- won't compile
   -- fw :: TidOf f -> Pos f
   -- bw :: Pos f -> TidOf f
   fw :: (STid (TidOf f)) -> Pos f
   bw :: Pos f -> (STid (TidOf f))

-- GHC provides the following error message:
--
-- ``````````````````````````````````````````````````````````````````````````````````
-- • Expected a type, but ‘TidOf f’ has kind ‘TidKind’
-- • In the type signature: fw :: TidOf f -> Pos f
--   In the class declaration for ‘IsoPos’
-- ``````````````````````````````````````````````````````````````````````````````````

-- | Why TidKind -> TidKind compiles but Tid1 -> Tid2 doesn't


-- | Make IsoPos compile

type STid :: TidKind -> Type
data STid k where
   STid1 :: KnownNat n
         => STid (Tid1 n)
   STid2 :: (KnownNat m, KnownNat n)
         => STid (Tid2 m n)
   STid3 :: (KnownNat d, KnownNat m, KnownNat n)
         => STid (Tid3 d m n)


-- Let's define something interesting, let's say, an image

type Image :: Nat -> Nat -> Type -> Type

-- But actually, we don't need the isomorphism
