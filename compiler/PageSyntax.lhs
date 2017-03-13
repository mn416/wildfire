An Intermediate Language for Hardware Compilation
=================================================

Named after Ian Page who (along with Wayne Luk) first showed how to
compile Occam to hardware.

> module PageSyntax where

> import Descend
> import Control.Monad

A program consists of identifier declarations and a statement.

> data Prog = 
>   Prog { decls   :: [Decl]
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
>     TNat Width             {- Register with width -}
>   | TPtr Width [Id]        {- Pointer -}
>   | TLab [Id]              {- Label -}
>   | TLock                  {- Lock -}
>   | TRam Width Width       {- Ram with address & data width -}
>     deriving (Eq, Show)

> type Width = Int

> data Init =
>     Uninit                 {- Uninitialised -}
>   | IntInit Integer        {- Initialised to integer -}
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
>   | Load Id Exp               {- Read from Block RAM -}
>   | Store Id Exp Exp          {- Write to Block RAM -}
>   | Push Id [Id]              {- Push onto stack given values -}
>   | Pop Id [Id]               {- Pop top stack elements -}
>   | Halt                      {- Halt execution -}
>     deriving Show

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
>   | RamOutput Id              {- Refer to the output of a RAM -}
>   | Select Int Int Exp        {- Bit selection -}
>   | Concat Exp Exp            {- Bit-string concatenation -}
>   | Available [Id]            {- Are all locks in set available? -}
>     deriving Show

> data UnaryOp =
>     Inv                {- Negation -}
>   | Shl Int | Shr Int  {- Constant left and right shifts -}
>   | First              {- Return first high bit -}
>     deriving Show

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
>   descendM f (Select from to e) = return (Select from to) `ap` f e
>   descendM f (Concat e1 e2) = return Concat `ap` f e1 `ap` f e2
>   descendM f other = return other

> onExp :: (Exp -> Exp) -> Stm -> Stm
> onExp f s = descend (onExp f) $
>   case s of 
>     x := e        -> x := f e
>     IndAssign x e -> IndAssign x (f e)
>     Ifte e s1 s2  -> Ifte (f e) s1 s2
>     While e s     -> While (f e) s
>     Load m e      -> Load m (f e)
>     Store m e1 e2 -> Store m (f e1) (f e2)
>     other         -> other
