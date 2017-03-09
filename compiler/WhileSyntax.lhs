Hardware Compilation of the While Language
==========================================

> module WhileSyntax where

> import Descend
> import Control.Monad
> import PageSyntax (UnaryOp(..), BinOp(..), Init)

A program consists of identifier declarations and a statement.

> data Prog = 
>   Prog { decls      :: [Decl]
>        , code       :: Stm
>        }
>     deriving Show

A declaration associates an identifier with a type.

> data Decl =
>   Decl {
>     declId   :: Id
>   , declType :: Type
>   , declInit :: Init
>   }
>   deriving Show

> type Id = String

> data Type =
>     TNat Width             {- Register with width -}
>     deriving (Eq, Show)

> type Width = Int

Statements.

> data Stm = 
>     Skip                      {- No-op -}
>   | Stm :> Stm                {- Sequential composition -}
>   | Id := Exp                 {- Register Assignment -}
>   | Stm :|| Stm               {- Fork-join parallelism -}
>   | Ifte Exp Stm Stm          {- If-then-else -}
>   | While Exp Stm             {- While loop -}
>   | Choice Stm Stm [Id]       {- Non-deterministic choice -}
>   | Fail                      {- Failure -}
>   | Halt                      {- Halt execution -}
>     deriving Show

The list of identifiers in the choice operator are the live variables
in the second branch.

> infixr 5 :>

Expressions (arithmetic & conditions).

> data Exp =
>     Lit (Maybe Width) Integer {- Integer literal of given width -}
>   | Var Id                    {- Variable reference -}
>   | Apply1 UnaryOp Exp        {- Unary operator application -}
>   | Apply2 BinOp Exp Exp      {- Binary operator application -}
>     deriving Show

Traversals
==========

> instance Descend Stm where
>   descendM f (s1 :> s2) = return (:>) `ap` f s1 `ap` f s2
>   descendM f (s1 :|| s2) = return (:||) `ap` f s1 `ap` f s2
>   descendM f (Ifte e s1 s2) = return (Ifte e) `ap` f s1 `ap` f s2
>   descendM f (While e s) = return (While e) `ap` f s
>   descendM f (Choice s1 s2 live) =
>     return Choice `ap` f s1 `ap` f s2 `ap` return live
>   descendM f other = return other

> instance Descend Exp where
>   descendM f (Apply1 op e) = return (Apply1 op) `ap` f e
>   descendM f (Apply2 op e1 e2) = return (Apply2 op) `ap` f e1 `ap` f e2
>   descendM f other = return other

> onExp :: (Exp -> Exp) -> Stm -> Stm
> onExp f s = descend (onExp f) $
>   case s of 
>     x := e        -> x := f e
>     Ifte e s1 s2  -> Ifte (f e) s1 s2
>     While e s     -> While (f e) s
>     other         -> other

> exprs :: Stm -> [Exp]
> exprs (x := e) = [e]
> exprs (Ifte e s1 s2) = [e] ++ exprs s1 ++ exprs s2
> exprs (While e s) = [e] ++ exprs s
> exprs other = extract exprs other
