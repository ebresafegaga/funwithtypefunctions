{-# LANGUAGE FunctionalDependencies #-}


module Main where

--f :: [Int] -> [Bool] -> Int 
f is bs = length is + length bs

class Mutation m r | m -> r where 
    newRef :: a -> m (r a) 
    readRef :: r a -> m a
    writeRef :: r a -> a -> m ()

class Mutation' m where 
    type Ref m :: * -> *

main :: IO ()
main = putStrLn "Hello, Haskell!"
