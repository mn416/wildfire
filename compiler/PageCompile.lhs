An Intermediate Language for Hardware Compilation
=================================================

> module PageCompile (
>   compile -- :: Prog -> Lava ()
> ) where

> import Netlist
> import Descend
> import PageSyntax
> import PagePretty
> import MonadInstances
> import Control.Monad
> import Data.Maybe
> import Data.Map as Map hiding ((!), foldr)
> import Data.List as List
> import Data.Bits (complement, (.&.), (.|.), xor, shiftL, shiftR)

Static Timing Analysis
======================

Try to determine at compile-time the duration (number of clock ticks)
needed for a statement to complete.  Return Nothing if duration cannot
be determined or is infinite.

> time :: Stm -> Maybe Int
> time Skip = Just 0
> time Tick = Just 1
> time (s1 :> s2) = return (+) `ap` time s1 `ap` time s2
> time (v := e) = Just 0
> time (IndAssign v e) = Just 0
> time (Par ss) = return maximum `ap` mapM time ss
> time (Ifte e s1 s2)
>   | t1 == t2 = t1
>   | otherwise = Nothing
>   where
>     t1 = time s1
>     t2 = time s2
> time (While e s) = Nothing
> time (Label l) = Just 0
> time (Jump x) = Nothing
> time (ForkJump x) = Just 0
> time (IndJump x) = Nothing
> time (Acquire x locks) = Just 0
> time (Release x) = Just 0
> time (Print x) = Nothing
> time (GPrint w x) = Nothing
> time (Load r e) = Just 0
> time (Store r e1 e2) = Just 0
> time Halt = Nothing

Is a given statement known to complete in finite time?

> finite :: Stm -> Bool
> finite = isJust . time

Return the index of the slowest statement in a list of statements.

> slowest :: [Stm] -> Int
> slowest = snd . maximum . flip zip [0..] . List.map time

Label Analysis
==============

(To support computed/indirect jumps.)

For each label variable, compute a label type.  A label type is a set
of labels denoting the possible values of a label variable.  If a
label variable v is assigned to a label variable w then v and w have
the same label type.

> inferLabelTypes :: Prog -> Prog
> inferLabelTypes p = p { decls = [augment d | d <- decls p] }
>   where
>     augment (v, TLab _) = (v, TLab (mapping!v))
>     augment (v, t)      = (v, t)
>
>     mapping = Map.map sort $ fix (propagate indirect) direct
>
>     labelVars = [v | (v, TLab _) <- decls p]
>
>     direct = fromListWith List.union (dir (code p))
>       where
>         dir (v := Lab x) = [(v, [x])]
>         dir s = extract dir s
>
>     indirect = ind (code p)
>       where
>         ind (v := Var w) | v `elem` labelVars = [(v, w)]
>         ind (Push m v) | v `elem` labelVars = [(m, v)]
>         ind (Pop m v) | v `elem` labelVars = [(v, m)]
>         ind s = extract ind s

> propagate :: [(Id, Id)] -> Map.Map Id [Id] -> Map.Map Id [Id]
> propagate [] m = m
> propagate ((v, w):ps) m =
>   Map.insert v t (Map.insert w t (propagate ps m))
>   where t = findWithDefault [] v m `List.union`
>             findWithDefault [] w m

Repeatedly apply a function until nothing changes.

> fix :: Eq a => (a -> a) -> a -> a
> fix f x
>   | x == y = x
>   | otherwise = fix f y
>   where y = f x

Obtain all labels in a program.

> labels :: Prog -> [Id]
> labels = labs . code
>   where
>     labs (Label x) = [x]
>     labs s = extract labs s

Pointer Analysis
================

(To support indirect assignments.)

For each pointer variable, compute a pointer type.  A pointer type is
a set of variables which the pointer may point to.  If a pointer p is
assigned to a pointer q then p and q have the same pointer type.

> inferPointerTypes :: Prog -> Prog
> inferPointerTypes p = p { decls = [augment d | d <- decls p] }
>   where
>     augment (v, TPtr n _) = (v, TPtr n (mapping!v))
>     augment (v, t)        = (v, t)
>
>     mapping = Map.map sort $ fix (propagate indirect) direct
>
>     ptrVars = [v | (v, TPtr _ _) <- decls p]
>
>     direct = fromListWith List.union (dir (code p))
>       where
>         dir (v := Ptr x) = [(v, [x])]
>         dir s = extract dir s
>
>     indirect = ind (code p)
>       where
>         ind (v := Var w) | v `elem` ptrVars = [(v, w)]
>         ind (Push m v) | v `elem` ptrVars = [(m, v)]
>         ind (Pop m v) | v `elem` ptrVars = [(v, m)]
>         ind s = extract ind s

Auxiliary Circuitry
===================

A set-reset latch in which a set is immediately reflected on the
output.  (Take care: a zero-delay wire joining the output to the set
line represents a combinatorial loop.)

> setReset' :: Sig -> Sig -> Lava Sig
> setReset' s r = do { q <- setReset s r ; s <|> q }

A synchroniser for fork-join parallelism:

> sync :: [Sig] -> Lava Sig
> sync [x] = return x
> sync xs = 
>   do done  <- newSig 1
>      ys    <- sequence [setReset' x done | x <- xs]
>      done' <- andBits ys
>      done  <== done'
>      return done

Return the first high bit in a list of bits.

> firstHigh :: [Sig] -> Lava [Sig]
> firstHigh xs = sequence [x `whenNone` ys | (x, ys) <- zip xs (inits xs)]
>   where
>     x `whenNone` ys = orBits ys >>= inv >>= (x <&>)

Print Statements
================

These can be defined in terms of other constructs:

> printDecls :: [Decl]
> printDecls =
>   [ ("reg_dataToSend", TNat 8)
>   , ("reg_send", TNat 1)
>   ]

Print a byte (a bit-string 8 bits)

> printStm :: Exp -> Stm
> printStm e =
>      "reg_dataToSend" := e
>   :> While (Apply1 Inv (Var "serial_ready")) Tick
>   :> Tick
>   :> "reg_send" := Lit (Just 1) 1
>   :> Tick
>   :> "reg_send" := Lit (Just 1) 0
>   :> Tick

Print a bit-string of any width

> gprintStm :: Int -> Id -> Stm
> gprintStm w x = foldr (:>) Skip [printStm e | e <- es]
>   where es = reverse (chunks x w 8)

> desugarPrint :: Prog -> Prog
> desugarPrint p =
>   p { decls = printDecls ++ decls p
>     , code  = trPrint (code p) }
>   where
>     trPrint (Print v) = printStm (Var v)
>     trPrint (GPrint w v) = gprintStm w v
>     trPrint other = descend trPrint other

> setupIO :: Env -> Lava Env
> setupIO env =
>   do s1 <- newInputSig "serial_ready" 1
>      s2 <- newOutputSig "serial_dataToSend" 8
>      s3 <- newOutputSig "serial_send" 1
>      s2 <== (env!"reg_dataToSend")
>      s3 <== (env!"reg_send")
>      return (Map.insert "serial_ready" s1 env)

Stack Statements
================

Push and Pop statements are implemented in terms of other constructs.
Functions pushOne and popOne allow pushing and popping data whose
width is the same as the data width of the stack.  But the Push and
Pop statements are more general, allowing pushing and popping data of
any width, implemented by "chunking". Currently chunking is achieved
by a simple compile-time loop but a run-time loop could be more
efficient.

> pushOne :: Id -> Exp -> Stm
> pushOne s e = top := e :> ForkJump (s ++ "_push")
>   where top = s ++ "_top"

The first argument to pushOneCode is the address width.

> pushOneCode :: Int -> Id -> Stm
> pushOneCode w s =
>      Label (s ++ "_push")
>   :> Store s (Apply2 Add (Var sp) (Lit (Just w) 1)) (Var top)
>   :> sp := Apply2 Add (Var sp) (Lit (Just w) 1)
>   :> Halt
>   where
>     top = s ++ "_top"
>     sp  = s ++ "_sp"

> popOne :: Id -> Stm
> popOne s = ForkJump (s ++ "_pop")

The first argument to popOneCode is the address width.

> popOneCode :: Int -> Id -> Stm
> popOneCode w s =
>      Label (s ++ "_pop")
>   :> top := RamOutput s
>   :> Load s (Apply2 Sub (Var sp) (Lit (Just w) 1))
>   :> sp := Apply2 Sub (Var sp) (Lit (Just w) 1)
>   :> Halt
>   where
>     top = s ++ "_top"
>     sp  = s ++ "_sp"

The chunks of register r of width w for a stack of data-width dw are:

> chunks :: Id -> Int -> Int -> [Exp]
> chunks r w dw = chunks' w
>   where
>     chunks' left
>       | left == 0  = []
>       | left == dw = [ Select (left-1) 0 (Var r) ]
>       | left < dw  = [ Lit (Just (dw-left)) 0 `Concat`
>                          Select (left-1) 0 (Var r) ]
>       | otherwise  = Select (left-1) (left-dw) (Var r) : chunks' (left-dw)

Shift the contents of x rightwards into y.

> shiftInto :: Exp -> Int -> Id -> Int -> Stm
> shiftInto x wx y wy
>   | wx > wy   = error "shiftInto"
>   | wx == wy  = y := x
>   | otherwise = y := (x `Concat` Select (wy-1) wx (Var y))

Desguar all Push and Pop statements into other constructs.

> desugarStacks :: Prog -> Prog
> desugarStacks p =
>   p { decls = decls p ++ concat [ [ (m ++ "_sp" , TNat aw)
>                                   , (m ++ "_top" , TNat dw) ]
>                                  | (m, TRam aw dw) <- decls p ]
>     , code  = trStm (code p)
>            :> Halt
>            :> foldr (:>) Skip
>                 [ pushOneCode aw s :> popOneCode aw s
>                 | (s, TRam aw dw) <- decls p ]
>     }
>   where
>     trStm (Push s x) = pushChunks s x
>     trStm (Pop s x) = popChunks s x
>     trStm other = descend trStm other
>
>     pushChunks :: Id -> Id -> Stm
>     pushChunks s x = foldr (:>) Skip $ intersperse Tick $
>                           [pushOne s c | c <- cs]
>       where
>         cs = chunks x wx ws
>         ws = getWidth s
>         wx = getWidth x
>
>     popChunks :: Id -> Id -> Stm
>     popChunks s x =
>        foldr (:>) Skip $ intersperse Tick $
>            [popOne s :> shiftInto z wz x wx | wz > 0]
>         ++ replicate (wx `div` ws)
>                      (popOne s :> shiftInto (Var top) ws x wx)
>       where
>         top = s ++ "_top"
>         ws  = getWidth s
>         wx  = getWidth x
>         wz  = wx `mod` ws
>         z   = Select (wz-1) 0 (Var top)
> 
>     -- Determine the data width of a register or RAM in a program.
>     getWidth :: Id -> Int
>     getWidth x = hd $ [w | (v, TRam _ w) <- decls p, v == x]
>                    ++ [w | (v, TNat w) <- decls p, v == x]
>                    ++ [length labs | (v, TLab labs) <- decls p, v == x]
>                    ++ [length regs | (v, TPtr _ regs) <- decls p, v == x]
>       where
>         hd (x:xs) = x
>         hd other  = error ("Can't determine width of variable " ++ x)

Compilation
===========

The environment maps each identifier to a signal, be it the output
signal of a regsiter, or the trigger signal of a label.

> type Env = Map Id Sig

Create a signal (of the correct width) for each identifier:

> initialEnv :: Prog -> Lava Env
> initialEnv p =
>   do sigs <- mapM (newSig . snd) ps
>      let m = zip [v | (v, n) <- ps] sigs
>      setupIO (fromList m)
>   where
>     ps = [(v, n) | (v, TNat n) <- decls p]
>       ++ [(v, length labs) | (v, TLab labs) <- decls p]
>       ++ [(v, length regs) | (v, TPtr _ regs) <- decls p]
>       ++ [(v, 1) | (v, TLock) <- decls p]
>       ++ [(v, 1) | v <- labels p]
>       ++ [(v, dw) | (v, TRam aw dw) <- decls p]

An item of a schedule describes an action to be performed on an
identifier at a specified point in time.  It is is a pair containing a
trigger signal and an action.  

> type Schedule = [(Sig, Action)]

> data Action =
>     AssignReg Id Sig     {- Assigment of signal to register -}
>   | JumpToLabel Id       {- Jump to label id -}
>   | LoadRam Id Sig       {- Read from RAM at address -}
>   | StoreRam Id Sig Sig  {- Write to RAM at address given value -}
>   | GrabLock Id Sig Sig  {- Ask for and grab given lock -}
>   | ReleaseLock Id       {- Release given lock -}

Compile a statement to give a schedule.

> compileStm :: Env -> Prog -> Lava Schedule
> compileStm env p =
>   do z <- low
>      start <- delayInit 1 z
>      (done, sched) <- compStm start (code p)
>      return sched
>   where
>   labelTypes = fromList [(v, labs) | (v, TLab labs) <- decls p]
>   ptrTypes = fromList [(v, regs) | (v, TPtr _ regs) <- decls p]
> 
>   compStm :: Sig -> Stm -> Lava (Sig, Schedule)
>   compStm go Skip = return (go, [])
>   compStm go Tick =
>     do done <- delay go
>        return (done, [])
>   compStm go (s1 :> s2) =
>     do (done1, sched1) <- compStm go s1
>        (done2, sched2) <- compStm done1 s2
>        return (done2, sched1 ++ sched2)
>   compStm go (v := Lab x) =
>     case elemIndex x t of
>       Nothing -> error (v ++ " not a label variable")
>       Just n  -> do s <- lit (2^n) (length t)
>                     return (go, [(go, AssignReg v s)])
>     where t = labelTypes!v
>   compStm go (v := Ptr x) =
>     case elemIndex x t of
>       Nothing -> error (v ++ " not a pointer variable")
>       Just n  -> do s <- lit (2^n) (length t)
>                     return (go, [(go, AssignReg v s)])
>     where t = ptrTypes!v
>   compStm go (v := e) =
>     do x <- compExp e
>        return (go, [(go, AssignReg v x)])
>   compStm go (Par []) = return (go, [])
>   compStm go (Par ss) = 
>     do rs <- sequence [compStm go s | s <- ss]
>        let (dones, scheds) = unzip rs
>        if all finite ss
>          then return (dones !! slowest ss, concat scheds)
>          else do done <- sync dones
>                  return (done, concat scheds)
>   compStm go (Ifte e s1 s2) =
>     do cond   <- compExp e
>        cond'  <- inv cond
>        goThen <- go <&> cond
>        goElse <- go <&> cond'
>        (done1, sched1) <- compStm goThen s1
>        (done2, sched2) <- compStm goElse s2
>        let sched = sched1 ++ sched2
>        if time s1 == Just 0 && time s2 == Just 0
>          then return (go, sched)
>          else do done <- done1 <|> done2
>                  return (done, sched)
>   compStm go (While e s) =
>     do cond <- compExp e
>        ready <- newSig 1
>        trigger <- ready <&> cond
>        (done, sched) <- compStm trigger s
>        ready' <- go <|> done
>        ready <== ready'
>        fin <- inv cond >>= (ready <&>)
>        return (fin, sched)
>   compStm go (Label x) =
>     do done <- go <|> (env!x)
>        return (done, [])
>   compStm go (Jump x) =
>     -- Register go as a trigger bit for label x
>     do z <- low
>        return (z, [(go, JumpToLabel x)])
>   compStm go (ForkJump x) =
>     return (go, [(go, JumpToLabel x)])
>   compStm go (IndJump v) =
>     do -- Register go <&> v_i as a trigger bit for
>        -- each label x_i in the label type of v
>        let n = length (labelTypes!v)
>        trigs <- sequence [bit i (env!v) >>= (go <&>) | i <- [0 .. n-1]]
>        let sched = [(t, JumpToLabel x) | (x, t) <- zip (labelTypes!v) trigs]
>        z <- low
>        return (z, sched)
>   compStm go (IndAssign v e) =
>     do -- Register go <&> v_i as a enable bit for
>        -- each variable x_i in the pointer type of v
>        let n = length (ptrTypes!v)
>        ens <- sequence [bit i (env!v) >>= (go <&>) | i <- [0 .. n-1]]
>        y <- compExp e
>        let sched = [(en, AssignReg x y) | (x, en) <- zip (ptrTypes!v) ens]
>        return (go, sched)
>   compStm go (Load r e) =
>     do i <- compExp e
>        return (go, [(go, LoadRam r i)])
>   compStm go (Store r e1 e2) =
>     do i <- compExp e1
>        x <- compExp e2
>        return (go, [(go, StoreRam r i x)])
>   compStm go Halt =
>     do z <- low
>        return (z, [])
>   compStm go (Acquire x []) = error "Acquire with not locks specified"
>   compStm go (Acquire x locks) =
>     do availables <- mapM newSig [1 | lock <- locks]
>        grabs      <- firstHigh availables
>        g          <- joinSigs grabs
>        let sched = [ (go, GrabLock lock avail gr)
>                    | (lock, avail, gr) <- zip3 locks availables grabs ]
>                 ++ [(go, AssignReg x g)]
>        return (go, sched)
>   compStm go (Release lock) =
>     return (go, [(go, ReleaseLock lock)])
>
>   compExp :: Exp -> Lava Sig
>   compExp (Lit Nothing n) = error "literal width unknown"
>   compExp (Lit (Just w) n) = lit n w
>   compExp (Var v) = return (env!v)
>   compExp (Lab x) = error "labels not allowed in nested expressions"
>   compExp (Apply1 f e1) =
>     do x1 <- compExp e1
>        case f of
>          Inv   -> inv x1
>          Shl n -> shl n x1
>          Shr n -> shr n x1
>          First -> first x1
>   compExp (Apply2 f e1 e2) =
>     do x1 <- compExp e1
>        x2 <- compExp e2
>        case f of
>          And -> x1 <&> x2
>          Or  -> x1 <|> x2
>          Xor -> x1 <#> x2
>          Add -> x1 <+> x2
>          Sub -> x1 <-> x2
>          Mul -> x1 <**> x2
>          Div -> x1 </> x2
>          Eq  -> x1 *=* x2
>          Neq -> x1 */=* x2
>          Lt  -> x1 *<* x2
>          Lte -> x1 *<=* x2
>          Gt  -> x1 *>* x2
>          Gte -> x1 *>=* x2
>   compExp (RamOutput r) = return (env!r)
>   compExp (Select from to e) =
>     do compExp e >>= select from to
>   compExp (Concat e1 e2) =
>     do s1 <- compExp e1
>        s2 <- compExp e2
>        concatSigs s1 s2
>   compExp (Available locks) =
>     orBits [env!lock | lock <- locks] >>= inv

> joinSigs :: [Sig] -> Lava Sig
> joinSigs [x] = return x
> joinSigs (x:xs) =
>   do rest <- joinSigs xs
>      concatSigs rest x

Drive the signal of each identifier using the information available in
the given schedule. (Signals are created and driven separately because
loops may exists from a signal back to itself.)

> drive :: Prog -> Env -> Schedule -> Lava ()
> drive p env s =
>   do -- Drive register outputs
>      sequence_ [reg r | r <- regs]
>      -- Drive label triggers
>      sequence_ [lab l | l <- labs]
>      -- Drive RAM-related signals
>      sequence_ [ram r | r <- rams]
>      -- Drive lock-related signals
>      sequence_ [lock l | l <- locks]
>   where
>     regs = [v | (v, TNat n)   <- decls p]
>         ++ [v | (v, TLab _)   <- decls p]
>         ++ [v | (v, TPtr _ _) <- decls p]
>     reg r 
>       | List.null assigns = return ()
>       | otherwise =
>           do regIn <- mux assigns
>              enable <- orBits [go | (go, x) <- assigns]
>              out <- delayEn enable regIn
>              (env!r) <== out
>       where assigns = [(go, x) | (go, AssignReg v x) <- s, v == r]
>
>     labs = labels p
>     lab l =
>       do trig <- orBits [go | (go, JumpToLabel v) <- s, v == l]
>          (env!l) <== trig
>
>     rams = [v | (v, TRam _ _) <- decls p]
>     ram r =
>       do readEn  <- orBits [go | (go, _) <- reads]
>          writeEn <- orBits [go | (go, _, _) <- writes]
>          en      <- readEn <|> writeEn
>          dataIn  <- mux [(go, x) | (go, i, x) <- writes]
>          addrIn  <- mux $ [(go, i) | (go, i) <- reads]
>                        ++ [(go, i) | (go, i, x) <- writes]
>          out     <- blockRam (RamInputs en writeEn dataIn addrIn)
>          (env!r) <== out
>       where
>         reads  = [(go, i) | (go, LoadRam r1 i) <- s, r == r1]
>         writes = [(go, i, x) | (go, StoreRam r1 i x) <- s, r == r1]
>
>     locks = [v | (v, TLock) <- decls p]
>     lock l =
>         do invTaken <- inv taken
>            asks <- sequence [invTaken <&> ask | (ask, _, _) <- grabs]
>            avails <- firstHigh asks
>            zipWithM_ (<==) [avail | (_, _, avail) <- grabs] avails
>            confirm <- orBits [grab | (_, grab, _) <- grabs]
>            release <- orBits rels
>            taken' <- setReset confirm release
>            taken <== taken'
>       where
>         taken = env!l
>         grabs = [ (ask, grab, avail)
>                 | (ask, GrabLock lock avail grab) <- s, lock == l ]
>         rels  = [ go | (go, ReleaseLock lock) <- s, lock == l ]

Compile a program to a netlist.
 
> compile :: Prog -> Lava ()
> compile p =
>   do env <- initialEnv p'
>      s   <- compileStm env p'
>      drive p' env s
>   where
>     p' = desugarPrint
>        $ desugarStacks
>        $ typeCheck
>        $ inferLabelTypes
>        $ inferPointerTypes p

Type Checking
=============

Type-check the program and decorate literal values with their widths.
Return the resulting program, throwing an error message if not
well-typed.  This function is not efficient.

> typeCheck :: Prog -> Prog
> typeCheck p = p { code = tc (code p) }
>   where
>     env  = Map.fromList (decls p)
>     labs = labels p
>
>     typeError x = error ("Type error (" ++ x ++ ")")
>     typeErrorW x w = error ("Type error (" ++ x
>                        ++ ") expected width " ++ show w)
>
>     widthOf :: Exp -> Maybe Int
>     widthOf (Lit w n) = w
>     widthOf (Var v) =
>       case env!v of
>         TNat w -> Just w
>         other  -> Nothing
>     widthOf (Apply1 op e) = widthOf e
>     widthOf (Apply2 op e1 e2) = widthOf e1 `mplus` widthOf e2
>     widthOf (RamOutput m) = dataWidth (env!m)
>     widthOf (Select from to e) = Just ((from+1) - to)
>     widthOf (Concat e1 e2) = return (+) `ap` widthOf e1 `ap` widthOf e2
>
>     dataWidth (TRam aw dw) = Just dw
>     dataWidth other = Nothing
>
>     tcExp :: Int -> Exp -> Exp
>     tcExp w e = ch e
>       where
>         ch (Lit Nothing n) = Lit (Just w) n
>         ch (Lit (Just w') n) | w == w' = Lit (Just w) n
>         ch (Var v) | env!v == TNat w = e
>         ch (Apply1 op e) = Apply1 op (tcExp w e)
>         ch (Apply2 op e1 e2)
>           | isCmpOp op =
>               case widthOf e1 `mplus` widthOf e2 of
>                 Nothing -> typeErrorW (show e) w
>                 Just w -> Apply2 op (tcExp w e1) (tcExp w e2)
>           | not (isCmpOp op) = Apply2 op (tcExp w e1) (tcExp w e2)
>         ch (RamOutput m)
>           | dataWidth (env!m) == Just w = e
>         ch (Select from to e)
>           | w == ((from+1) - to) =
>               case widthOf e of
>                 Just w' | from < w' -> Select from to (tcExp w' e)
>                 other -> typeErrorW (show e) w
>         ch (Concat e1 e2) =
>           case (widthOf e1, widthOf e2) of
>             (Just w1, _) -> Concat (tcExp w1 e1) (tcExp (w-w1) e2)
>             (_, Just w2) -> Concat (tcExp (w-w2) e1) (tcExp w2 e2)
>             other        -> typeErrorW (show other) w
>         ch (Available xs)
>           | and [(env!lock) == TLock | lock <- xs] = Available xs
>         ch other = typeErrorW (show other) w
>
>     -- Type checker for statements
>     tc Skip = Skip
>     tc Tick = Tick
>     tc (s1 :> s2) = tc s1 :> tc s2
>     tc (v := Lab x)
>       | isLab (env!v) && x `elem` labs = v := Lab x
>     tc (v := Var w)
>       | isLab (env!v) && isLab (env!w) = v := Var w
>     tc (v := Ptr w)
>       | isPtr tv && isNat tw && ptrWidth tv == natWidth tw = v := Ptr w
>       where (tv, tw) = (env!v, env!w)
>     tc (v := Var w)
>       | isPtr tv && isPtr tw && ptrWidth tv == ptrWidth tw = v := Var w
>       where (tv, tw) = (env!v, env!w)
>     tc (IndAssign v e)
>       | isPtr tv = IndAssign v (tcExp (ptrWidth tv) e)
>       where tv = env!v
>     tc (v := e) =
>       case env!v of
>         TNat n -> v := tcExp n e
>         other  -> typeError (show (v := e))
>     tc (Par ss) = Par [tc s | s <- ss]
>     tc (Ifte e s1 s2) = Ifte (tcExp 1 e) (tc s1) (tc s2)
>     tc (While e s) = While (tcExp 1 e) (tc s)
>     tc (Label x) = Label x
>     tc (Jump x)
>       | x `elem` labs = Jump x
>       | otherwise     = typeError (x ++ " is an undefined label")
>     tc (ForkJump x)
>       | x `elem` labs = ForkJump x
>       | otherwise     = typeError (x ++ " is an undefined label")
>     tc (IndJump x)
>       | isLab (env!x) = IndJump x
>     tc (Acquire x locks)
>       | and [(env!lock) == TLock | lock <- locks]
>      && (env!x) == TNat (length locks) = Acquire x locks
>     tc (Release x)
>       | (env!x) == TLock = Release x
>     tc (Print x)
>       | (env!x) == TNat 8 = Print x
>     tc (GPrint w x) | isNat tx && natWidth tx == w = GPrint w x
>       where tx = env!x
>     tc (Load r e) =
>       case env!r of
>         TRam aw dw -> Load r (tcExp aw e)
>         other -> typeError (show (Load r e))
>     tc (Store r i e) =
>       case env!r of
>         TRam aw dw ->
>           Store r (tcExp dw i) (tcExp aw e)
>         other -> typeError (show (Store r i e))
>     {- Stack ops are weakly-typed due to chunking feature -}
>     tc (Push s x) = Push s x
>     tc (Pop s x) = Pop s x
>     tc Halt = Halt
>     tc other = typeError (show other)

> isNat :: Type -> Bool
> isNat (TNat _) = True
> isNat other = False

> isLab :: Type -> Bool
> isLab (TLab _) = True
> isLab other = False

> isPtr :: Type -> Bool
> isPtr (TPtr _ _) = True
> isPtr other = False

> natWidth :: Type -> Int
> natWidth (TNat n) = n

> ptrWidth :: Type -> Int
> ptrWidth (TPtr n _) = n

Simplification
==============

Constant folding ensures that no operations exist whose operands are
all literals, which can help when inferring the widths of literals.
(This code is currently not used.)

> constantFolding :: Exp -> Exp
> constantFolding = bottomup fold
>   where
>
>   fold (Apply1 op (Lit _ x)) = Lit Nothing (apply1 op x)
>   fold (Apply2 op (Lit _ x) (Lit _ y)) = Lit Nothing (apply2 op x y)
>   fold (Select from to (Lit _ x)) = Lit Nothing (sel from to x)
>   fold (Concat (Lit _ x) (Lit _ y)) = Lit Nothing (conc x y)
>   fold other = other
>
>   apply1 Inv x = complement x
>
>   apply2 And x y = x .&. y
>   apply2 Or  x y = x .|. y
>   apply2 Xor x y = x `xor` y
>   apply2 Add x y = x + y
>   apply2 Sub x y = x - y
>   apply2 Mul x y = x * y
>   apply2 Div x y = x `div` y
>   apply2 Eq  x y = integer (x == y)
>   apply2 Neq x y = integer (x /= y)
>   apply2 Lt  x y = integer (x < y)
>   apply2 Lte x y = integer (x <= y)
>   apply2 Gt  x y = integer (x > y)
>   apply2 Gte x y = integer (x >= y)
>
>   integer False = 0
>   integer True  = 1
>
>   conc x y = x `shiftL` log2ceil y
>   log2ceil n = if n == 1 then 1 else 1 + log2ceil (n `div` 2)
>   sel from to x = (x `shiftR` from) .&. (2^((to+1) - from)-1)

Auxiliaries
===========

> m ! k = case Map.lookup k m of
>           Nothing -> error (show k ++ " not in map")
>           Just x -> x
