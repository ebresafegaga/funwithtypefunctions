{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Data.Kind (Type)

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

main :: IO ()
main = putStrLn "Hello, Haskell!"
