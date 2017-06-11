Syntax of intermediate language
===============================

Named after Ian Page who (along with Wayne Luk) first showed how to
compile Occam to hardware.

> module PageSyntax where

> import Descend
> import Control.Monad
> import qualified Data.Map as Map

A program consists of identifier declarations and a statement.

> data Prog = 
>   Prog { opts    :: CompilerOpts
>        , decls   :: [Decl]
>        , code    :: Stm
>        }
>     deriving Show

A declaration associates an identifier with a type.

> data Decl =
>   Decl { declId   :: Id
>        , declType :: Type
>        , declInit :: Init
>        }
>     deriving Show

> type Id = String

> data Type =
>     TReg Width             {- Register with width -}
>   | TPtr Width [Id]        {- Pointer -}
>   | TLab [Id]              {- Label -}
>   | TLock                  {- Lock -}
>   | TRam Width Width       {- RAM with address & data width -}
>   | TRom Width Width       {- ROM with address & data width -}
>   | TMWRam Width Width     {- Mixed width RAM -}
>            Width Width
>     deriving (Eq, Show)

> type Width = Int

> type CompilerOpts = Map.Map String Integer

> data Init =
>     Uninit                 {- Uninitialised -}
>   | IntInit Integer        {- Integer initialiser -}
>   | StrInit String         {- String initialiser -}
>   deriving Show

Statements.

> data Stm = 
>     Skip                      {- No-op -}
>   | Tick                      {- Delay one tick -}
>   | Stm :> Stm                {- Sequential composition -}
>   | Id := Exp                 {- Register Assignment -}
>   | IndAssign Id Exp          {- Indirect Assignment -}
>   | Par [Stm]                 {- Fork-join parallelism -}
>   | Ifte Exp Stm Stm          {- If-then-else -}
>   | While Exp Stm             {- While loop -}
>   | Label Id                  {- Label -}
>   | Jump Id                   {- Direct jump -}
>   | ForkJump Id               {- Jump to label AND continue -}
>   | IndJump Id                {- Indirect jump -}
>   | Acquire Id [Id]           {- Acquire any lock from lock-set -}
>   | Release Id                {- Release lock -}
>   | Print Id                  {- Send byte to the serial port -}
>   | GPrint Width Id           {- Send bit-string to the serial port -}
>   | Fetch Id RamPort Exp      {- Read from RAM port -}
>   | Store Id RamPort Exp Exp  {- Write to RAM port -}
>   | LoadRom Id Id RomPort Exp {- Load from ROM port into register -}
>   | Push Id [Id]              {- Push onto stack given values -}
>   | Pop Id [Id]               {- Pop top stack elements -}
>   | Halt                      {- Halt execution -}
>     deriving Show

RAM port:

> data RamPort = A | B deriving (Eq, Show, Ord)

ROM port:

> type RomPort = String

The list of identifiers in the choice operator are the live variables
in the second branch.

> infixr 5 :>

Expressions (arithmetic & conditions).

> data Exp =
>     Lit (Maybe Width) Integer {- Integer literal of given width -}
>   | Var Id                    {- Variable reference -}
>   | Lab Id                    {- Label literal -}
>   | Ptr Id                    {- Pointer to register -}
>   | Apply1 UnaryOp Exp        {- Unary operator application -}
>   | Apply2 BinOp Exp Exp      {- Binary operator application -}
>   | RamOutput Id RamPort      {- Refer to the output of a RAM -}
>   | Select Int Int Exp        {- Bit selection -}
>   | Concat Exp Exp            {- Bit-string concatenation -}
>   | Available [Id]            {- Are all locks in set available? -}
>   | Cond Exp Exp Exp          {- Conditional expression -}
>     deriving Show

> data UnaryOp =
>     Inv                {- Negation -}
>   | Shl Int | Shr Int  {- Constant left and right shifts -}
>   | MSB                {- Most significant bit -}
>   | Log                {- Log base 2, round up -}
>     deriving (Eq, Show)

> data BinOp =
>     And | Or  | Xor                    {- Bitwise -}
>   | Add | Sub | Mul | Div              {- Arithmetic -}
>   | Eq  | Neq | Lt  | Lte | Gt | Gte   {- Comparison -}
>     deriving (Eq, Show)

> isCmpOp :: BinOp -> Bool
> isCmpOp op = op `elem` [Eq, Neq, Lt, Lte, Gt, Gte]

> block :: [Stm] -> Stm
> block [] = Skip
> block [x] = x
> block (x:xs) = x :> block xs

Traversals
==========

> instance Descend Stm where
>   descendM f (s1 :> s2) = return (:>) `ap` f s1 `ap` f s2
>   descendM f (Par ss) = return Par `ap` mapM f ss
>   descendM f (Ifte e s1 s2) = return (Ifte e) `ap` f s1 `ap` f s2
>   descendM f (While e s) = return (While e) `ap` f s
>   descendM f other = return other

> instance Descend Exp where
>   descendM f (Apply1 op e) = return (Apply1 op) `ap` f e
>   descendM f (Apply2 op e1 e2) = return (Apply2 op) `ap` f e1 `ap` f e2
>   descendM f (Cond e1 e2 e3) = return Cond `ap` f e1 `ap` f e2 `ap` f e3
>   descendM f (Select from to e) = return (Select from to) `ap` f e
>   descendM f (Concat e1 e2) = return Concat `ap` f e1 `ap` f e2
>   descendM f other = return other

> onExp :: (Exp -> Exp) -> Stm -> Stm
> onExp f s = descend (onExp f) $
>   case s of 
>     x := e            -> x := f e
>     IndAssign x e     -> IndAssign x (f e)
>     Ifte e s1 s2      -> Ifte (f e) s1 s2
>     While e s         -> While (f e) s
>     Fetch m p e       -> Fetch m p (f e)
>     Store m p e1 e2   -> Store m p (f e1) (f e2)
>     LoadRom x m p e   -> LoadRom x m p (f e)
>     other             -> other
