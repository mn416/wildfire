> module WhileSem (
>   run     -- :: Prog -> IO ()
> ) where

> import WhileSyntax
> import qualified PageSyntax as P
> import WhileCompile
> import MonadInstances
> import Control.Monad
> import Data.Maybe
> import Data.Map as Map
> import Data.List as List
> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import Data.Bits
> import MIFReader

Bit vectors
===========

> data BitStr = BitStr { val :: Integer, width :: Int }

> instance Show BitStr where
>   show b = show (val b)

> instance Eq BitStr where
>   a == b = val a == val b

> instance Ord BitStr where
>   a <= b = val a <= val b

> mask :: Int -> Integer
> mask n = (1 `shiftL` n) - 1

> bitStr :: Integer -> Int -> BitStr
> bitStr i w = BitStr (i .&. mask w) w

> lift1 :: (Integer -> Integer) -> BitStr -> BitStr
> lift1 f a = BitStr (f (val a) .&. mask w) w
>   where w = width a

> lift2 :: (Integer -> Integer -> Integer) -> BitStr -> BitStr -> BitStr
> lift2 f a b = BitStr (f (val a) (val b) .&. mask w) w
>   where w = width a

Big-Step Operational Semantics
==============================

Mapping from variables to values

> type Env = Map.Map Id Val

A value is either an integer or an array

> data Val =
>     IntVal BitStr
>   | ArrayVal (Map.Map BitStr BitStr)
>   deriving (Eq, Show)

Expressions
-----------

> eval :: Env -> Exp -> BitStr
> eval env (Lit (Just w) i) = bitStr i w
> eval env (Var v) = let IntVal i = env Map.! v in i
> eval env (Apply1 op e) =
>   case op of
>     P.Inv   -> lift1 complement n
>     P.Shl i -> lift1 (`shiftL` i) n
>     P.Shr i -> lift1 (`shiftR` i) n
>     P.MSB   -> bitStr (val n `shiftR` (width n - 1)) 1
>   where
>     n = eval env e
> eval env (Apply2 op e1 e2) =
>   case op of
>     P.And -> lift2 (.&.) a b
>     P.Or  -> lift2 (.|.) a b
>     P.Xor -> lift2 xor a b
>     P.Add -> lift2 (+) a b
>     P.Sub -> lift2 (-) a b
>     P.Mul -> lift2 (*) a b
>     P.Div -> lift2 div a b
>     P.Eq  -> bitStr (if val a == val b then 1 else 0) 1
>     P.Neq -> bitStr (if val a /= val b then 1 else 0) 1
>     P.Lt  -> bitStr (if val a <  val b then 1 else 0) 1
>     P.Lte -> bitStr (if val a <= val b then 1 else 0) 1
>     P.Gt  -> bitStr (if val a >  val b then 1 else 0) 1
>     P.Gte -> bitStr (if val a >= val b then 1 else 0) 1
>   where
>     a = eval env e1
>     b = eval env e2
> eval env (Truncate w e) =
>   bitStr (val $ eval env e) w
> eval env (Concat e1 e2) =
>   bitStr (val a `shiftL` width b .|. val b) (width a + width b)
>   where
>     a = eval env e1
>     b = eval env e2
> eval env (Select a b e) =
>   bitStr (val r `shiftR` b) (1+a-b)
>   where r = eval env e

Statements
----------

> exec :: Env -> Stm -> [Env]
> exec env s =
>   case s of
>     Skip -> [env]
>     v := e -> [Map.insert v (IntVal (eval env e)) env]
>     s1 :> s2 -> concat [exec env' s2 | env' <- exec env s1]
>     s1 :|| s2 -> exec env (s1 :> s2)
>     Ifte e s1 s2 ->
>       exec env (if val (eval env e) /= 0 then s1 else s2)
>     While e s ->
>       exec env (if val (eval env e) /= 0 then s :> While e s else Skip)
>     Choice s1 s2 _ -> 
>       exec env s1 ++ exec env s2
>     Fail -> []
>     ArrayAssign v e1 e2 -> [Map.insert v (ArrayVal a') env]
>       where
>         ArrayVal a = env Map.! v
>         a'         = Map.insert (eval env e1) (eval env e2) a
>     ArrayLookup _ v w e -> [Map.insert v (IntVal i) env]
>       where
>         ArrayVal a = env Map.! w
>         i          = a Map.! eval env e

Programs
--------

> initState :: Env -> [Decl] -> IO Env
> initState env [] = return env
> initState env (d:ds) =
>   case (declType d, declInit d) of
>     (TBit w, P.Uninit) ->
>       initState (Map.insert (declId d) (IntVal (bitStr 0 w)) env) ds
>     (TBit w, P.IntInit n) ->
>       initState (Map.insert (declId d) (IntVal (bitStr n w)) env) ds
>     (TArray _ (TBit aw) (TBit dw), P.Uninit) -> do
>       let z = Map.fromList [(bitStr a aw, bitStr 0 dw) | a <- [0..(2^aw)-1]]
>       initState (Map.insert (declId d) (ArrayVal z) env) ds
>     (TArray _ (TBit aw) (TBit dw), P.StrInit filename) -> do
>       let z = Map.fromList [(bitStr a aw, bitStr 0 dw) | a <- [0..(2^aw)-1]]
>       mif <- readMIF filename
>       let m = Map.fromList [ (bitStr a aw, bitStr d dw)
>                            | (a, d) <- Map.toList (mifContents mif) ]
>                 `Map.union` z
>       initState (Map.insert (declId d) (ArrayVal m) env) ds
>     _ -> error "Error constructing initial state"

> run :: Prog -> IO ()
> run p = do
>   let p' = typeCheck $ arrayAnalysis $ desugarTypes p
>   env <- initState Map.empty (decls p')
>   let results = exec env (code p')
>   case results of
>     [] -> putStrLn "No solutions"
>     r:rs -> do
>       putStrLn "Solution exists"
>       putStr "Num solutions = "
>       print (length results)
