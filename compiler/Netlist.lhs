A Target Language for Hardware Compilation
==========================================

> module Netlist (
>   Lava
> , Sig
> , newSig, newInputSig, newOutputSig
> , op
> , low, high, lit, bit
> , inv, (<&>), (<|>), (<#>)
> , (<+>), (<->), (<**>), (</>)
> , (*=*), (*/=*), (*<*), (*<=*), (*>*), (*>=*)
> , shl, shr, first
> , (<==), select, concatSigs
> , delay, delayEn, delayInit, delayInitEn, setReset
> , blockRam, RamInputs(..)
> , andBits, orBits, treeM, treeM1
> , mux, (?), mask, width, isZero
> , writeVerilog
> ) where

> import Control.Monad
> import Data.List
> import Data.Map as Map

Syntax
======

A signal comprises a signal and a bit-width.

> type Sig = (SigId, Width)
>
> type SigId = String
>
> type Width = Int
>
> sigId :: Sig -> SigId
> sigId (v, w) = v
>
> width :: Sig -> Width
> width (v, w) = w

A netlist is a list of variables, a list of component instances, and
the next unused signal id.

> data Netlist =
>   Netlist { 
>     inputSigs  :: [Sig],
>     outputSigs :: [Sig],
>     localSigs  :: [Sig],
>     initials   :: Map SigId Integer,
>     insts      :: [Inst],
>     nextId     :: Int
>   }

An instance is a component with inputs and outputs.

> type Inst = (Component, [Sig], [Sig])

> data Component =
>     Const Integer Width                {- Constant value -}
>   | Inv | And | Or  | Xor              {- Bitwise -}
>   | Add | Sub | Mul | Div              {- Arithmetic -}
>   | Eq  | Neq | Lt  | Gt | Lte | Gte   {- Comparison -}
>   | Shl Int | Shr Int                  {- Constant shift -}
>   | Id  | BlockRam Int Int String      {- Identity & Block RAM-}
>   | Delay | DelayEn | SetReset         {- Registers -}
>   | Select Int Int  | Concat           {- Bit selection & concatenation -}
>   | Mux                                {- Multiplexer -}
>     deriving (Eq, Show)

Interface (Lava Monad)
======================

A Lava-style interface for constructing netlists.

> newtype Lava a = Lava { runLava :: Netlist -> (Netlist, a) }

> instance Monad Lava where
>   return a = Lava $ \s -> (s, a)
>   m >>= f  = Lava $ \s -> case runLava m s of (s', a) -> runLava (f a) s'

> instance Functor Lava where
>   fmap = liftM

> instance Applicative Lava where
>   pure = return
>   (<*>) = ap

> newSigId :: Lava SigId
> newSigId = Lava $ \s -> ( s { nextId = nextId s + 1 }, 'v':show (nextId s) )

> modify :: (Netlist -> Netlist) -> Lava ()
> modify f = Lava $ \s -> (f s, ())

> newSig :: Width -> Lava Sig
> newSig w =
>   do v <- newSigId
>      modify $ \s -> s { localSigs = (v, w) : localSigs s }
>      return (v, w)

> newInputSig :: SigId -> Width -> Lava Sig
> newInputSig v w =
>   modify (\s -> s { inputSigs = (v, w) : inputSigs s }) >> return (v, w)

> newOutputSig :: SigId -> Width -> Lava Sig
> newOutputSig v w =
>   modify (\s -> s { outputSigs = (v, w) : outputSigs s }) >> return (v, w)

> addInit :: Sig -> Integer -> Lava ()
> addInit (v, w) init =
>   modify $ \s -> s { initials = Map.insert v init (initials s) }

> newInst :: Inst -> Lava ()
> newInst i = modify $ \s -> s { insts = i : insts s }

Primitive Components
====================

Make an instance of an operator (a function with zero or more inputs
and one output of specified width).

> op :: Component -> [Sig] -> Width -> Lava Sig
> op f inps w =
>   do out <- newSig w
>      newInst (f, inps, [out])
>      return out

> sameWidth :: Sig -> Sig -> Bool
> sameWidth a b
>   | width a == width b = True
>   | otherwise = error "Incompatible signal widths in netlist"

Literals:

> lit :: Integer -> Width -> Lava Sig
> lit i w = op (Const i w) [] w

> low :: Lava Sig
> low = lit 0 1

> high :: Lava Sig
> high = lit 1 1

Return the nth bit of a given signal.

> bit :: Int -> Sig -> Lava Sig
> bit n s = op (Select n n) [s] 1

Select bits in range of a given signal.

> select :: Int -> Int -> Sig -> Lava Sig
> select from to s
>   | to > from = error "selection: to-index larger than from-index!"
>   | otherwise = op (Select from to) [s] ((from+1) - to)

Concatenate two Signals

> concatSigs :: Sig -> Sig -> Lava Sig
> concatSigs s1 s2 = op Concat [s1, s2] (width s1 + width s2)

Unary operators:

> unaryOp :: Component -> Sig -> Lava Sig
> unaryOp f a = op f [a] (width a)

> inv = unaryOp Inv
> delayInit init x = do { y <- unaryOp Delay x ; addInit y init ; return y } 

> delay = delayInit 0

> delayEnable en x
>   | width en == 1 = op DelayEn [en, x] (width x)
>   | otherwise = error "enable input to delay should be a single bit"

> delayInitEn init en x =
>  do { y <- delayEnable en x ; addInit y init ; return y } 

> delayEn en x = delayInitEn 0 en x

Binary operators:

> binOp :: Component -> Sig -> Sig -> Lava Sig
> binOp f a b | sameWidth a b = op f [a, b] (width a)

> (<&>) = binOp And
> (<|>) = binOp Or
> (<#>) = binOp Xor

> x <+> y
>   | width x == 1 = binOp Xor x y
>   | otherwise    = binOp Add x y

> (<->) = binOp Sub
> (<**>) = binOp Mul
> (</>) = binOp Div

> shl n = unaryOp (Shl n)
> shr n = unaryOp (Shr n)

Comparison operators:

> cmpOp :: Component -> Sig -> Sig -> Lava Sig
> cmpOp f a b | sameWidth a b = op f [a, b] 1

> (*=*)  = cmpOp Eq
> (*/=*) = cmpOp Neq
> (*<*)  = cmpOp Lt
> (*<=*) = cmpOp Lte
> (*>*)  = cmpOp Gt
> (*>=*) = cmpOp Gte

A set-reset flip-flop:

> setReset :: Sig -> Sig -> Lava Sig
> setReset s r
>   | width s == 1 && width r == 1 = -- op SetReset [s, r] 1
>     do x <- op SetReset [s, r] 1
>        addInit x 0
>        return x
>   | otherwise = error "setReset flip-flop expects single-bit inputs"

Block-RAMs:

> data RamInputs = 
>   RamInputs {
>     ramEn   :: Sig
>   , writeEn :: Sig
>   , dataBus :: Sig
>   , addrBus :: Sig
>   }

> blockRam :: RamInputs -> Lava Sig
> blockRam inps =
>   do out <- newSig (width (dataBus inps))
>      ramName <- newSigId
>      newInst (BlockRam wid cap ramName,
>        [ramEn inps, writeEn inps, dataBus inps, addrBus inps], [out])
>      return out
>   where
>     wid = width (dataBus inps)
>     cap = width (addrBus inps)

Use signal y to drive signal x.

> (<==) :: Sig -> Sig -> Lava ()
> x <== y = newInst (Id, [y], [x])

Multiplexer:

> mux :: [(Sig, Sig)] -> Lava Sig
> mux [] = error "mux applied to empty list"
> mux ins
>   | all ((== 1) . width . fst) ins
>  && all ((== w) . width . snd) ins =
>       op Mux (concat [ [a, b] | (a, b) <- ins ]) w
>   | otherwise = error "incompatible widths in call to mux"
>   where
>     w = width (snd (head ins))

If expression:

> cond ? (a, b) =
>   do cond' <- inv cond
>      mux [(cond, a), (cond', b)]

> mask :: Sig -> Sig -> Lava Sig
> mask x bit = 
>   do z <- lit 0 (width x)
>      bit ? (x, z)

> isZero :: Sig -> Lava Sig
> isZero x =
>   do z <- lit 0 (width x)
>      x *=* z

Some Combinators
================

> treeM :: (Sig -> Sig -> Lava Sig) -> Sig -> [Sig] -> Lava Sig
> treeM f z [] = return z
> treeM f z [x] = return x
> treeM f z (x:y:ys) = do { r <- f x y ; treeM f z (ys ++ [r]) }

> treeM1 f xs = treeM f (error "treeM1 called on empty list") xs

> andBits :: [Sig] -> Lava Sig
> andBits xs = do { z <- high ; treeM (<&>) z xs }

> orBits :: [Sig] -> Lava Sig
> orBits xs = do { z <- low ; treeM (<|>) z xs }

> first :: Sig -> Lava Sig
> first x =
>   do x'  <- inv x
>      one <- lit 1 (width x)
>      y   <- x' <+> one
>      x <&> y

Code Generation
===============

> writeVerilog :: String -> Lava () -> IO ()
> writeVerilog name m =
>   writeFile (name ++ ".v") code
>   where
>     initial = Netlist [] [] [] empty [] 0
>     code    = verilog name (fst (runLava m initial))

> verilog :: String -> Netlist -> String
> verilog name n =
>   verilogEntity name n ++
>   verilogArch name n

Entity generation:

> verilogEntity :: String -> Netlist -> String
> verilogEntity name n = 
>      "module " ++ name ++ "(\n"
>   ++ consperse ",\n"
>        (["  input wire clock"] ++
>         ["  input wire reset"] ++
>         ["  input " ++ varDecl n v w | (v, w) <- inputSigs n] ++
>         ["  output " ++ varDecl n v w | (v, w) <- outputSigs n])
>   ++ ");\n"

> varDecl :: Netlist -> SigId -> Width -> String
> varDecl n v w
>   | w == 1    = kind ++ " " ++ v ++ init
>   | otherwise = kind ++ " [" ++ show (w-1) ++ ":0] " ++ v ++ init
>   where
>     init = case Map.lookup v (initials n) of
>              Nothing -> ""
>              Just i  -> " = " ++ bitString i w
>     kind = case Map.lookup v (initials n) of
>              Nothing -> "wire"
>              Just i  -> "reg"

Architecture generation:

> verilogArch :: String -> Netlist -> String
> verilogArch name nl =
>      "\n"
>   ++ verilogDecls nl ++ ";\n"
>   ++ "\n"
>   ++ concatMap pureInst (insts nl)
>   ++ "always @(posedge clock) begin\n"
>   ++ "if (reset) begin\n"
>   ++ concat [reset v w | (v, w) <- localSigs nl]
>   ++ "end else begin\n"
>   ++ concatMap syncInst (insts nl)
>   ++ "end\n"
>   ++ "end\n"
>   ++ "endmodule\n"
>   where
>     reset v w = 
>       case Map.lookup v (initials nl) of
>         Nothing -> ""
>         Just i  -> v ++ " <= " ++ bitString i w ++ ";\n"

> verilogDecls :: Netlist -> String
> verilogDecls n = consperse ";\n" $
>      [varDecl n v w | (v, w) <- localSigs n]

Pure structural component instances:

> pureInst :: Inst -> String
> pureInst (Const k w, [], [o]) =
>   "assign " ++ sigId o ++ " = " ++ bitString k w ++ ";\n"
> pureInst (Inv, [x], [o]) =
>   "assign " ++ sigId o ++ " = ~" ++ sigId x ++ ";\n"
> pureInst (Shl n, [x], [o]) =
>   "assign " ++ sigId o ++ " = " ++ sigId x ++ " << " ++ show n ++ ";\n"
> pureInst (Shr n, [x], [o]) =
>   "assign " ++ sigId o ++ " = " ++ sigId x ++ " >> " ++ show n ++ ";\n"
> pureInst (c, [x,y], [o])
>   | c `elem` Prelude.map fst binOps =
>       "assign " ++ sigId o ++ " = " ++ sigId x ++ " " ++
>                         op ++ " " ++ sigId y ++ ";\n"
>   where
>     op     = head [op | (comp, op) <- binOps, comp == c]
>     binOps = [ (And, "&"), (Or, "|"), (Xor, "^")
>              , (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/") ]
> pureInst (c, [x,y], [o])
>   | c `elem` Prelude.map fst cmpOps =
>       "assign " ++ sigId o ++ " = " ++ sigId x ++ " " ++ op ++ " "
>                 ++ sigId y ++ " ? 1'd1 : 1'd0;\n"
>   where
>     op     = head [op | (comp, op) <- cmpOps, comp == c]
>     cmpOps = [ (Eq, "==")  , (Neq, "!="), (Lt, "<")
>              , (Lte, "<="), (Gt, ">")  , (Gte, ">=") ]
> pureInst (Id, [x], [o]) = "assign " ++ sigId o ++ " = " ++ sigId x ++ ";\n"
> pureInst (Select from to, [x], [o])
>   | from == 0 && to == 0 && width x == 1 =
>       "assign " ++ sigId o ++ " = " ++ sigId x ++ ";\n"
>   | from == to =
>       "assign " ++ sigId o ++ " = " ++ sigId x ++ "[" ++ show from ++ "];\n"
>   | otherwise  =
>       "assign " ++ sigId o ++ " = " ++ sigId x ++ "[" ++ show from
>                 ++ ":" ++ show to ++ "];\n"
> pureInst (Concat, ins, [o]) =
>   "assign " ++ sigId o ++ " = {" ++
>     consperse ", " (Prelude.map sigId ins) ++ "};\n"
> pureInst (Mux, ins, [o]) =
>   "assign " ++ sigId o ++ " = " ++ consperse " | "
>     [ "((" ++ rep (width xs) (sigId x) ++ ") & " ++ sigId xs ++ ")"
>     | (x, xs) <- pairs ins] ++ ";\n"
> pureInst (BlockRam dw aw r, [en, we, d, a], [o]) =
>   "AlteraBlockRamTrueMixed#(\n" ++
>   "  .ADDR_WIDTH_A(" ++ show aw ++ "),\n" ++
>   "  .ADDR_WIDTH_B(" ++ show aw ++ "),\n" ++
>   "  .DATA_WIDTH_A(" ++ show dw ++ "),\n" ++
>   "  .DATA_WIDTH_B(" ++ show dw ++ "),\n" ++
>   "  .NUM_ELEMS_A(" ++ show (2^aw) ++ "),\n" ++
>   "  .NUM_ELEMS_B(" ++ show (2^aw) ++ ")) " ++ r ++ " (\n" ++
>   "  .CLK(clock),\n" ++
>   "  .DI_A(" ++ sigId d ++ "),\n" ++
>   "  .DI_B(0),\n" ++
>   "  .ADDR_A(" ++ sigId a ++ "),\n" ++
>   "  .ADDR_B(0),\n" ++
>   "  .WE_A(" ++ sigId we ++ "),\n" ++
>   "  .WE_B(0),\n" ++
>   "  .EN_A(" ++  sigId en ++ "),\n" ++
>   "  .EN_B(0),\n" ++
>   "  .DO_A(" ++ sigId o ++ "),\n" ++
>   "  .DO_B());\n"
> pureInst other = ""

Synchronous (clocked) component instances

> syncInst :: Inst -> String
> syncInst (Delay, [x], [o]) = sigId o ++ " <= " ++ sigId x ++ ";\n"
> syncInst (DelayEn, [en, x], [o]) = 
>   "if (" ++ sigId en ++ " == 1'b1) " ++ sigId o ++
>   " <= " ++ sigId x ++ ";\n"
> syncInst (SetReset, [s, r], [o]) =
>   "if (" ++ sigId r ++ " == 1'b1) " ++ sigId o ++
>   " <= 1'b0; else " ++ sigId o ++ " <= " ++ sigId s ++ " | " ++ 
>   sigId o ++ ";\n"
> syncInst other = ""

Auxiliary functions:

> pairs :: [a] -> [(a, a)]
> pairs (x:y:ys) = (x,y) : pairs ys
> pairs other = []

> consperse :: [a] -> [[a]] -> [a]
> consperse x xs = concat (intersperse x xs)

> bitString :: Integer -> Width -> String
> bitString n w
>   | n < 0     = error "Negative literal"
>   | n >= 2^w  = error "Literal value too large"
>   | otherwise = show w ++ "'d" ++ show n

> natToBin :: Integral a => a -> String
> natToBin 0 = []
> natToBin n = (if odd n then '1' else '0') : natToBin (n `div` 2)

> rep :: Int -> String -> String
> rep n s = "{" ++ show n ++ "{" ++ s ++ "}}"
