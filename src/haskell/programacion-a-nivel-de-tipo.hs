{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeFamilies, StandaloneDeriving #-}

data Nat = Zero | Succ Nat

data Vector (n :: Nat) (a :: *) where
    VNil :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a
deriving instance Show a => Show (Vector n a)

type family Add (n :: Nat) (m :: Nat) where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)

tailv :: Vector ('Succ n) a -> Vector n a
tailv (VCons _ xs) = xs

appendv :: Vector n a -> Vector m a -> Vector (Add n m) a
appendv VNil ys = ys
appendv (VCons x xs) ys = VCons x (appendv xs ys)
