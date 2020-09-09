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



main :: IO ()
main = putStrLn "Hello, Haskell!"
