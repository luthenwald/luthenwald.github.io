{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeData            #-}
{-# LANGUAGE TypeFamilies        #-}

import           Data.Kind    ( Type )

import           GHC.TypeNats ( KnownNat, Nat )

type data TidKind
   = Tid1 Nat
   | Tid2 Nat Nat
   | Tid3 Nat Nat Nat

type STid :: TidKind -> Type
data STid k where
   STid1 :: KnownNat n
         => STid (Tid1 n)
   STid2 :: (KnownNat m, KnownNat n)
         => STid (Tid2 m n)
   STid3 :: (KnownNat d, KnownNat m, KnownNat n)
         => STid (Tid3 d m n)

class Functor f => Naperian f where
   type Log f

   tabulate :: (Log f -> a) -> f a
   lookup   :: f a -> (Log f -> a)

-- TODO: need a better, more accurate, more elegant name for this typeclass
class Naperian f => NapMTL f where
   type TidOf f :: TidKind

   fw :: (STid (TidOf f)) -> Log f
   bw :: Log f -> (STid (TidOf f))
