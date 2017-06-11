Syntax of source language
=========================

> module WhileSyntax where

> import Descend
> import Control.Monad
> import PageSyntax (UnaryOp(..), BinOp(..), Init)
> import qualified Data.Map as Map

A program consists of identifier declarations and a statement.

> data Prog = 
>   Prog { opts  :: CompilerOpts
>        , types :: [TypeDecl]
>        , decls :: [Decl]
>        , code  :: Stm
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
>     TBit Width             {- Register with width -}
>   | TArray ArrayMode
>       ArrayLive Type Type  {- Array with address and data types -}
>   | TUser Id               {- User-defined type -}
>     deriving (Eq, Show)

> type Width = Int

> data ArrayMode = RO | RW   {- Read-Only and Read/Write arrays -}
>   deriving (Eq, Show)

> data ArrayLive = Live | Dead  {- Array live at choice? -}
>   deriving (Eq, Show)

> type CompilerOpts = Map.Map String Integer

Statements.

> data Stm = 
>     Skip                             {- No-op -}
>   | Stm :> Stm                       {- Sequential composition -}
>   | Id := Exp                        {- Register Assignment -}
>   | Stm :|| Stm                      {- Fork-join parallelism -}
>   | Ifte Exp Stm Stm                 {- If-then-else -}
>   | While Exp Stm                    {- While loop -}
>   | Choice Stm Stm [Id]              {- Non-deterministic choice -}
>   | Fail                             {- Failure -}
>   | Halt                             {- Halt execution -}
>   | ArrayAssign Id Exp Exp           {- Array assignment -}
>   | ArrayLookup ArrayMode Id Id Exp  {- Array lookup -}
>     deriving Show

The list of identifiers in the choice operator are the live variables
in the second branch.

> infixr 5 :>

Expressions (arithmetic & conditions).

> data Exp =
>     Lit (Maybe Width) Integer {- Integer literal of given width -}
>   | Var Id                    {- Variable reference -}
>   | RecSel Id [Id]            {- Record selector -}
>   | Apply1 UnaryOp Exp        {- Unary operator application -}
>   | Apply2 BinOp Exp Exp      {- Binary operator application -}
>   | Truncate Int Exp          {- Truncate to given width -}
>   | Select Int Int Exp        {- Bit vector slice -}
>   | Concat Exp Exp            {- Bit vector concatenation -}
>   | Cond Exp Exp Exp          {- Conditional expression -}
>     deriving Show

Type declarations

> data TypeDecl =
>    TSynonym Id Type
>  | TEnum Id [Id]
>  | TRec Id [(Id, Type)]
>  deriving (Eq, Show)

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
>   descendM f (Cond e1 e2 e3) = return Cond `ap` f e1 `ap` f e2 `ap` f e3
>   descendM f (Truncate w e) = return (Truncate w) `ap` f e
>   descendM f (Select a b e) = return (Select a b) `ap` f e
>   descendM f (Concat e1 e2) = return Concat `ap` f e1 `ap` f e2
>   descendM f other = return other

> onExp :: (Exp -> Exp) -> Stm -> Stm
> onExp f s = descend (onExp f) $
>   case s of 
>     x := e              -> x := f e
>     Ifte e s1 s2        -> Ifte (f e) s1 s2
>     While e s           -> While (f e) s
>     ArrayAssign a e1 e2 -> ArrayAssign a (f e1) (f e2)
>     ArrayLookup m x a e -> ArrayLookup m x a (f e)
>     other               -> other

> exprs :: Stm -> [Exp]
> exprs (x := e) = [e]
> exprs (Ifte e s1 s2) = [e] ++ exprs s1 ++ exprs s2
> exprs (While e s) = [e] ++ exprs s
> exprs (ArrayAssign a e1 e2) = [e1, e2]
> exprs (ArrayLookup m x a e) = [e]
> exprs other = extract exprs other
