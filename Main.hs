{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Data.Kind (Type)
import Data.Map (Map)

f :: [Int] -> [Bool] -> Int 
f is bs = length is + length bs

class Mutation m r | m -> r where 
    newRef :: a -> m (r a) 
    readRef :: r a -> m a
    writeRef :: r a -> a -> m ()

class Mutation' m where 
     type Ref m :: Type -> Type
     newRef' :: a -> m (Ref m a)
     readRef' :: Ref m a -> m a
     writeRef' :: Ref m a -> a -> m ()

class Add a b where 
    type SumTy a b :: * 
    add :: a -> b -> SumTy a b

instance Add Integer Double where 
    type SumTy Integer Double = Double 
    add x y = fromIntegral x + y 

instance Add Double Integer where 
    type SumTy Double Integer = Double
    add x y = x + fromIntegral y

instance (Num a) => Add a a where 
    type SumTy a a = a
    add x y = x + y 

class Cons a b where 
    type ResTy a b :: * 
    cons :: a -> [b] -> [ResTy a b]

instance Cons Integer Double where 
    type ResTy Integer Double = Double 
    cons x ys = fromIntegral x : ys

class Graph g where 
    type Vertex g :: *
    data Edge g 
    src, tgt :: Edge g -> Vertex g
    outEdges :: g -> Vertex g -> [Edge g]

newtype G1 = G1 [Edge G1]

instance Graph G1 where 
    type Vertex G1 = Int 
    data Edge G1 = MkEdge (Vertex G1) (Vertex G1) 


newtype G2 = G2 (Map (Vertex G2) [Vertex G2])

instance Graph G2 where 
    type Vertex G2 = String 
    data Edge G2 = MkEdge2 Int (Vertex G2) (Vertex G2)

neigbours :: Graph g => g -> Vertex g -> [Vertex g]
neigbours g v = map tgt (outEdges g v)

newtype Age = MkAge Int 

instance Add Age Int where 
    type SumTy Age Int = Age 
    add (MkAge a) n = MkAge (a+n)


instance (Add Integer a) => Add Integer [a] where 
    type SumTy Integer [a] = [SumTy Integer a]
    add x y = map (add x) y

class MonadTrans t where 
    lift :: Monad m => m a -> t m a 

instance (Monad m, Mutation' m, MonadTrans t) 
    => Mutation' (t m) where 
    type Ref (t m) = Ref m 
    newRef' = lift . newRef'
    readRef' = lift . readRef'
    writeRef' = (lift .) . writeRef'

class Memo a where
    data Table a :: * -> *
    toTable :: (a -> w) -> Table a w
    fromTable :: Table a w -> (a -> w)

instance Memo Bool where 
    data Table Bool w = TBool w w
    toTable f = TBool (f True) (f False)
    fromTable (TBool x y) b = if b then x else y 


factorial 0 = 1 
factorial 1 = 1 
factorial n = n * factorial (n-1)


fibonacci 0 = 1 
fibonacci 1 = 1 
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

f' :: Bool -> Int 
f' True = factorial 100
f' False = fibonacci 100

g = fromTable (toTable f')


instance (Memo a, Memo b) => Memo (Either a b) where 
    data Table (Either a b) w = TSum (Table a w) (Table b w)
    toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
    fromTable (TSum t _) (Left v) = fromTable t v
    fromTable (TSum _ t) (Right v) = fromTable t v

v = Left
main :: IO ()
main = putStrLn "Hello, Haskell!"
