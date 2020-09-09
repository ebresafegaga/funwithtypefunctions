{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Data.Kind (Type)
import Prelude hiding (lookup)
import qualified Data.Map as M 
import qualified Data.IntMap as DI

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


newtype G2 = G2 (M.Map (Vertex G2) [Vertex G2])

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

instance (Memo a, Memo b) => Memo (a, b) where 
    newtype Table (a, b) w = TProduct (Table a (Table b w))
    toTable f = TProduct (toTable (\x -> toTable (\y -> f (x, y))))
    fromTable (TProduct t) (x, y) = fromTable (fromTable t x) y

instance (Memo a) => Memo [a] where
    data Table [a] w = TList w (Table a (Table [a] w))
    toTable f = TList (f []) (toTable (\x -> toTable (\xs -> f (x:xs))))
    fromTable (TList t _) [] = t
    fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs

v = Left

-- type Map key value = Table key (Maybe value)

class Key k where 
    data Map k :: * -> *
    empty :: Map k v
    lookup :: k -> Map k v -> Maybe v 

instance Key Bool where 
    data Map Bool elt = MB (Maybe elt) (Maybe elt)
    empty = MB Nothing Nothing 
    lookup False (MB mf _) = mf
    lookup True (MB _ mt) = mt 

a :: Map Bool Int 
a = empty

instance (Key a, Key b) => Key (Either a b) where 
    data Map (Either a b) elt = MS (Map a elt) (Map b elt)
    empty = MS empty empty 
    lookup (Left k) (MS m _) = lookup k m
    lookup (Right k) (MS _ m) = lookup k m

instance (Key a, Key b) => Key (a, b) where 
    data Map (a, b) elt = MP (Map a (Map b elt))
    empty = MP empty
    lookup (a, b) (MP m) = do 
            m' <- lookup a m 
            lookup b m'

instance Key Int where 
    newtype Map Int elt = MI (DI.IntMap elt)
    empty = MI DI.empty
    lookup k (MI m) = DI.lookup k m

data Stop = Done 
newtype In a b = In (a -> IO b) 
data Out a b = Out a (IO b)

addServer :: In Int (In Int (Out Int Stop))
addServer = 
    In $ \x -> return $ In $ \y -> do
                    putStrLn "Thinking" 
                    return $ Out (x + y) (return Done) 

class Session a where 
    type Dual a :: * 
    run :: a -> Dual a -> IO ()

instance (Session b) => Session (In a b) where 
    type Dual (In a b) = Out a (Dual b)
    run (In f) (Out a d) = f a >>= \b -> d >>= \c -> run b c

instance (Session b) => Session (Out a b) where 
    type Dual (Out a b) = In a (Dual b) 
    run (Out a d) (In f) = f a >>= \b -> d >>= \c -> run c b

instance Session Stop where 
    type Dual Stop = Stop 
    run Done Done = return ()  


addClient :: Out Int (Out Int (In Int Stop))
addClient = Out 3 $ return $ Out 4 $ do 
                            putStrLn "Waiting"
                            return $ In $ \z -> print z >> return Done

r :: IO ()
r = run addServer addClient

negServer :: In Int (Out Int Stop)
negServer = In $ \x -> do 
                    putStrLn "Thinking"
                    return $ Out (-x) (return Done)

instance (Session a, Session b) => Session (Either a b) where 
    type Dual (Either a b) = (Dual a, Dual b)
    run (Left y) (x,_) = run y x
    run (Right y) (_,x) = run y x

instance (Session a, Session b) => Session (a, b) where 
    type Dual (a, b) = Either (Dual a) (Dual b)
    run (x,_) (Left y) = run x y
    run (_,x) (Right y) = run x y


server :: (In Int (Out Int Stop), 
            In Int (In Int (Out Int Stop)))
server = (negServer, addServer)


client :: Either (Out Int (In Int Stop))
                 (Out Int (Out Int (In Int Stop)))
client = Right addClient

main :: IO ()
main = r
