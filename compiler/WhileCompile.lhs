> module WhileCompile (
>   compile       -- :: Prog -> P.Prog
> , typeCheck     -- :: Prog -> Prog
> , arrayAnalysis -- :: Prog -> Prog
> , desugarTypes  -- :: Prog -> Prog
> ) where

> import Netlist
> import Descend
> import WhileSyntax
> import qualified PageSyntax as P
> import MonadInstances
> import Control.Monad
> import Data.Maybe
> import Data.Map as Map hiding ((!))
> import Data.List as List
> import qualified Data.Map as Map
> import qualified Data.Set as Set

Type Checking
=============

Type-check the program and decorate literal values with their widths.
Return the resulting program, throwing an error message if not
well-typed.  This function is not efficient.

> typeCheck :: Prog -> Prog
> typeCheck p = p { decls = List.map tcDecl (decls p), code = tc (code p) }
>   where
>     env  = Map.fromList [(v, t) | Decl v t _ <- decls p]
>
>     typeError x = error ("Type error (" ++ x ++ ")")
>     typeErrorW x w = error ("Type error (" ++ x
>                        ++ ") expected width " ++ show w)
>
>     widthOf :: Exp -> Maybe Int
>     widthOf (Lit w n) = w
>     widthOf (Var v) =
>       case env!v of
>         TBit w -> Just w
>         other  -> Nothing
>     widthOf (Apply1 op e)
>        | op `elem` [P.MSB] = Just 1
>        | otherwise = widthOf e
>     widthOf (Apply2 op e1 e2)
>        | P.isCmpOp op = Just 1
>        | otherwise    = widthOf e1 `mplus` widthOf e2
>     widthOf (Cond e1 e2 e3) = widthOf e2 `mplus` widthOf e3
>     widthOf (Truncate w e) = Just w
>     widthOf (Select a b e) = Just (1+a-b)
>     widthOf (Concat e1 e2) =
>       return (+) `ap` widthOf e1 `ap` widthOf e2
>
>     -- Type checker for expressions
>     tcExp :: Int -> Exp -> Exp
>     tcExp w e = ch e
>       where
>         ch (Lit Nothing n) = Lit (Just w) n
>         ch (Lit (Just w') n) | w == w' = Lit (Just w) n
>         ch (Var v) | env!v == TBit w = e
>         ch (Apply1 op e)
>           | op `elem` [P.MSB] =
>               case widthOf e of
>                 Nothing -> typeError ("Can't determine width: " ++ (show e))
>                 Just w  -> Apply1 P.MSB (tcExp w e)
>           | otherwise = Apply1 op (tcExp w e)
>         ch (Apply2 op e1 e2)
>           | P.isCmpOp op =
>               case widthOf e1 `mplus` widthOf e2 of
>                 Nothing -> typeErrorW (show e) w
>                 Just w -> Apply2 op (tcExp w e1) (tcExp w e2)
>           | not (P.isCmpOp op) = Apply2 op (tcExp w e1) (tcExp w e2)
>         ch (Cond e1 e2 e3) =
>             case widthOf e2 `mplus` widthOf e3 of
>               Nothing -> typeErrorW (show e) w
>               Just w -> Cond (tcExp 1 e1) (tcExp w e2) (tcExp w e3)
>         ch (Truncate n e)
>           | n == w =
>             case widthOf e of
>               Nothing -> Truncate n (tcExp n e)
>               Just m  -> if n <= m then Truncate n (tcExp m e)
>                                    else typeErrorW (show (Truncate n e)) w
>         ch (Select a b e)
>           | n == w =
>             case widthOf e of
>               Nothing -> Select a b (tcExp n e)
>               Just m  -> if a+1 <= m then Select a b (tcExp m e)
>                                      else typeErrorW (show (Select a b e)) w
>           where n = 1+a-b
>         ch (Concat e1 e2) =
>           case (widthOf e1, widthOf e2) of
>             (Just x, Just y)
>               | w == x+y -> Concat (tcExp x e1) (tcExp y e2)
>             (Just x, Nothing)
>               | x < w -> Concat (tcExp x e1) (tcExp (w-x) e2)
>             (Nothing, Just y)
>               | y < w -> Concat (tcExp (w-y) e1) (tcExp y e2)
>             other -> typeErrorW (show (Concat e1 e2)) w
>         ch other = typeErrorW (show other) w
>
>     -- Type checker for statements
>     tc Skip = Skip
>     tc (s1 :> s2) = tc s1 :> tc s2
>     tc (v := e) =
>       case env!v of
>         TBit n -> v := tcExp n e
>         other  -> typeError (show (v := e))
>     tc (s1 :|| s2) = tc s1 :|| tc s2
>     tc (Ifte e s1 s2) = Ifte (tcExp 1 e) (tc s1) (tc s2)
>     tc (While e s) = While (tcExp 1 e) (tc s)
>     tc (Choice s1 s2 live) = Choice (tc s1) (tc s2) live
>     tc (ArrayAssign a e1 e2) =
>       case env!a of
>         TArray _ _ (TBit aw) (TBit dw) ->
>           ArrayAssign a (tcExp aw (Truncate aw e1))
>                         (tcExp dw e2)
>         other -> typeError ("Not an array: " ++ show a)
>     tc (ArrayLookup m x a e) =
>       case (env!x, env!a) of
>         (TBit w, TArray _ _ (TBit aw) (TBit dw)) ->
>           if w == dw then ArrayLookup m x a (tcExp aw (Truncate aw e)) else
>             typeError ("Width mismatch in lookup of array " ++ a)
>         other -> typeError ("Expected " ++ a ++ " to be an array" ++
>                             " and " ++ x ++ "to be a register")
>     tc Fail = Fail
>     tc Halt = Halt
>     tc other = typeError (show other)
>
>     -- Type checker for declarations
>     tcDecl d =
>       case d of
>         Decl v _ P.Uninit -> d
>         Decl v (TBit n) (P.IntInit i) -> d
>         Decl v (TArray _ _ _ _) (P.StrInit s) -> d
>         Decl v _ _ -> typeError ("Invalid initialiser for variable " ++ v)
>         other -> other

Type desugaring
===============

This pass translates user-defined types in the program to primitive
types.

> desugarTypes :: Prog -> Prog
> desugarTypes p =
>   p { types = []
>     , decls = List.map trDecl (decls p)
>     , code  = onExp trExp (code p)
>     }
>   where
>     desugarType nest env (TUser id) = env!id
>     desugarType True env (TArray m l t1 t2) =
>       error "Nested array types are disallowed"
>     desugarType False env (TArray m l t1 t2) =
>       TArray m l (desugarType True env t1) (desugarType True env t2)
>     desugarType nest env (TBit n) = TBit n
>
>     mkTypeEnv env [] = env
>     mkTypeEnv env (TSynonym x t:ds) =
>       mkTypeEnv (Map.insert x (desugarType False env t) env) ds
>     mkTypeEnv env (TEnum x ctrs:ds) =
>       let width = log2 (length ctrs - 1) + 1 in
>         mkTypeEnv (Map.insert x (TBit width) env) ds
>     mkTypeEnv env (TRec x fs:ds) =
>       let ts = [desugarType False env t | (f, t) <- fs]
>           w  = sum [n | TBit n <- ts]
>       in  mkTypeEnv (Map.insert x (TBit w) env) ds
>     
>     typeEnv = mkTypeEnv Map.empty (types p)
>
>     trDecl d = d { declType = desugarType False typeEnv (declType d) }
>
>     ctrEnv = Map.fromList
>       [ let w = log2 (length ctrs - 1) + 1 in (c, Lit (Just w) n) 
>       | TEnum x ctrs <- types p, (n, c) <- zip [0..] ctrs ]
>
>     env = Map.fromList [ (declId d, declType d) | d <- decls p ]
>
>     mkFieldMap n acc [] = acc
>     mkFieldMap n acc ((f, t):rest) =
>       case desugarType False typeEnv t of
>         TBit w -> let acc' = Map.insert f (t, w+n-1, n) acc in
>                     mkFieldMap (w+n) acc' rest
>         other -> error ("Disallowed type in record: " ++ show t)
>
>     fieldMap = Map.fromList
>        [ (x, mkFieldMap 0 Map.empty fs) | TRec x fs <- types p ]
>
>     select e t [] = e
>     select e t (f:fs) =
>       case t of
>         TUser typeName ->
>           case Map.lookup typeName fieldMap of
>             Nothing -> error ("Unknown record type: " ++ typeName)
>             Just m ->
>               case Map.lookup f m of
>                 Nothing -> error ("Unknown field: " ++ f)
>                 Just (tf, a, b) ->
>                   select (Select a b e) tf fs
>         other -> error ("Not a record type: " ++ show e)
>     
>     trExp (Var v) =
>       case Map.lookup v ctrEnv of
>         Nothing  -> Var v
>         Just lit -> lit
>     trExp (RecSel v fs) = select (Var v) (env!v) fs
>     trExp other = descend trExp other

Array analysis
==============

Annotate each array declaration and lookup with the RO or RW flag
depending on whether the array is Read-Only or Read/Write.

> arrayAnalysis :: Prog -> Prog
> arrayAnalysis p =
>     p { decls = List.map trDecl (decls p)
>       , code  = trStm (code p) }
>   where
>     rwArrays (ArrayAssign a e1 e2) = [a]
>     rwArrays s = extract rwArrays s
>
>     rw = Set.fromList $ rwArrays (code p)
>
>     trDecl (Decl v (TArray _ l aw dw) init)
>       | v `Set.member` rw = Decl v (TArray RW l aw dw) init
>       | otherwise         = Decl v (TArray RO l aw dw) init
>     trDecl d = d
>
>     trStm (ArrayLookup _ x a e)
>       | a `Set.member` rw = ArrayLookup RW x a e
>       | otherwise         = ArrayLookup RO x a e
>     trStm s = descend trStm s

Static restrictions
===================

(1) The '?', 'halt', and 'fail' operators must not occur within a
parallel composition: I'm not aware of a sensible semantics for that.

(2) In a statement p || q, if p modifies a variable v then then q must
not refer to v, and vice-versa.  Otherwise parallel threads could
interact in unforeseen ways.  With this restriction, p || q is
semantically equivalent to both p ; q and q ; p.

(3) In a statement p || q, if p accesses an array then q must not
access that same array, and vice-versa.

The restrictions involving the || operator suggest that use of this
operator should be inferred by the compiler rather than specified by
the programmer.  (Future work.)

> staticRestrictions :: Prog -> Prog
> staticRestrictions p =
>     p { code = check1 $ check2 $ check3 $ code p }
>   where
>     check1 (p :|| q)
>       | noChoices p && noChoices q = check1 p :|| check1 q
>       | otherwise =
>           error "'?' and 'halt' and 'fail' operators forbidden inside '||'"
>     check1 other = descend check1 other
>
>     noChoices s = List.null (choices s)
>     choices (Choice p q _) = [()]
>     choices Fail = [()]
>     choices Halt = [()]
>     choices other = extract choices other
>
>     check2 (p :|| q)
>       | not (any (`elem` useStm q) (def p))
>      && not (any (`elem` useStm p) (def q))
>       = check2 p :|| check2 q
>       | otherwise = error "parallel branches must be independent"
>     check2 other = descend check2 other
>
>     check2 (p :|| q)
>       | not (any (`elem` useStm q) (def p))
>      && not (any (`elem` useStm p) (def q))
>       = check2 p :|| check2 q
>       | otherwise = error "parallel branches must be independent"
>     check2 other = descend check2 other
>
>     useStm s = nub (concatMap use (exprs s))
>
>     check3 (p :|| q)
>       | List.null (arrayVarsIn p `List.intersect` arrayVarsIn q) =
>           check3 p :|| check3 q
>       | otherwise = error "parallel branches must be independent"
>     check3 other = descend check3 other
>
>     arrayVarsIn (ArrayAssign a _ _) = [a]
>     arrayVarsIn (ArrayLookup _ _ a _) = [a]
>     arrayVarsIn p = extract arrayVarsIn p

Backtracking choice
===================

To execute

  s1 ? s2

there are two cases:

 1. Another processor (j) is available.  In this case spark execution
    of s2 on j and execute s1 on the current processor i.  Sparking is
    achieved by copying i's live variables to j and jumping to the
    appropriate location.

 2. No other processors are available.  In this case, save the live
    variables on the stack, execute s1 and backtrack to s2 upon
    completion.

A statement s1 ? s2 is compiled to:

  acquire j NEIGHBOURS(i) ; tick ;
  if j /= 0 then
    v1[j] := v1[i] ;
    ... ;
    vn[j] := vn[i] ;
    forkjump spawnLabel[j]
  else
    push stack[i] v1[i] ; tick ;
    ... ;
    push stack[i] vn[i] ; tick ;
    push stack[i] retLabel[i] ; tick
  end ;

  s1 ; jump endLabel[i] ;

  retLabel[i]:
    pop vn[i] ;
    ... ;
    pop v1[i] ;
  spawnLabel[i]:
    s2 ;

  endLabel[i]:

Where v1 to vn are the variables live-in to the statement s2; i is the
index of this processor; j is the index of another processor.

And halt statements are compiled to:

  count[i] := count[i] + 1 ; tick ;
  jump backtrack[i]

And fail statements are compiled to:

  jump backtrack[i]

where

  backtrack[i]:
    pop stack[i] choicePoint[i] ; tick ;
    jump choicePoint[i]

This assumes that initially the stack contains the label "terminate"
to jump to when the program finishes.

  terminate[i]:
    ret := terminate[i] ; tick
    push stack[i] ret ; tick
    release i ; tick ;
    halt

Notice we release lock i because processor i is now available for new
work. We also put the terminate label back on the stack so that
processor i is ready for new work.

Backtracking choice with array variables
========================================

We now extend the above compilation scheme to support array variables.

A statement s1 ? s2 is compiled to:

  acquire j NEIGHBOURS(i) ; tick ;
  if j /= 0 then
    forkjump array1_copy ;
    ...
    forkjump arrayn_copy ;
    v1[j] := v1[i] ;
    ... ;
    vn[j] := vn[i] ;
    tick ;
    while array1(i)_copy_done == 0 | .. | arrayn(i)_copy_done == 0 do tick end ;
    tick ;
    forkjump spawnLabel[j]
  else
    array1(i)_save_marker := 1 ; forkjump array1(i)_save ;
    ... ;
    arrayn(i)_save_marker := 1 ; forkjump arrayn(i)_save ;
    push stack[i] v1[i] ; tick ;
    ... ;
    push stack[i] vn[i] ; tick ;
    push stack[i] retLabel[i] ; tick
  end ;

  s1 ; jump endLabel[i] ;

  retLabel[i]:
    pop vn[i] ;
    ... ;
    pop v1[i] ;
    forkjump array1(i)_restore ;
    ...
    forkjump arrayn(i)_restore ;
    while array1(i)_restored == 0 | .. | arrayn(i)_restored == 0 do tick end ;
    tick ;
  spawnLabel[i]:
    s2 ;

  endLabel[i]:

Definitions of the save, copy, and restore blocks per array and per
thread i are as follows.

  array(i)_save:
    array(i)_undo:A[array(i)_sp] :=
      { array(i)_save_index, data(array(i):B), array(i)_save_marker } ;
    array(i)_sp := array(i)_sp+1 ;
    halt

  array(i)_copy:
    array(i)_iter := 0 ;
    array(i)_copy_done := 0 ;
    fetch array:B(i)[0] ;
    tick ;
    while array(i)_iter /= array(i)_len do
      array(i)_write_addr := array(i)_iter ;
      array(i)_write_val  := data(array:B(i)) ;
      forkjump array(i)_write ;
      fetch array:B(i)[array(i)_iter+1] ;
      array(i)_iter := array(i)_iter+1 ;
      tick ;
    end ;
    array(i)_copy_done := 1 ;
    halt

  array(i)_write:
    tick ;
    if bit 1 of _token(i) then
      array:B(N(0))[array(i)_write_addr] := array(i)_write_val
    end ;
    ...
    if bit n of _token(i) then
      array:B(N(n))[array(i)_write_addr] := array(i)_write_val
    end ;
    halt

  array(i)_restore:
    array(i)_restored := array(i)_sp == 0 ;
    fetch array(i)_undo:B[array(i)_sp-1] ;
    tick ;
    while ~array(i)_restore do
      fetch array(i)_undo:B[array(i)_sp-2] ;
      array(i)_sp := array(i)_sp-1 ;
      let { index, value, marker } = data(array(i)_undo:B) ;
      if marker then
        array(i)_restore := 1
      else
        array(i):A[index] := value
      end ;
      tick
    end
    halt

  terminate[i]:
    array1(i)_sp := 0 ;
    arrayn(i)_sp := 0 ;
    ret := terminate[i] ; tick
    push stack[i] ret ; tick
    release i ; tick ;
    halt

Live variables
==============

Return the variables live-in to any given statement.  The rule for the
|| operator looks suspicous but remember the parallel branches are
independent, as required by the static restrictions, and are therefore
semantically equivalent to sequential execution in any order.

> live :: Stm -> [Id] -> [Id]
> live (x := e) vs = nub (use e) `List.union` (vs List.\\ [x])
> live (Ifte e s1 s2) vs =
>   nub (use e) `List.union` live s1 vs `List.union` live s2 vs
> live (While e s) vs = nub (use e) `List.union` live s [] `List.union` vs
> live (s1 :> s2) vs = live s1 (live s2 vs)
> live (s1 :|| s2) vs = live s1 (live s2 vs)
> live (Choice s1 s2 _) vs = live s1 vs `List.union` live s2 vs
> live (ArrayLookup m x a e) vs = nub (use e) `List.union` (vs List.\\ [x])
> live (ArrayAssign a e1 e2) vs = nub (use e1 ++ use e2) `List.union` vs
> live other vs = vs

> use :: Exp -> [Id]
> use (Var v) = [v]
> use other = extract use other

> def :: Stm -> [Id]
> def (x := e) = [x]
> def (ArrayLookup m x _ _) = [x]
> def other = extract def other

Here we annotate each choice point with the variables live-in to the
second choice.  (Those are the variables that need to be saved for
backtracking or copied when forking.)  There is no rule for the ||
operator because choice points cannot occur inside parallel branches
according to static restriction (1).

> annotateLive :: Prog -> Prog
> annotateLive p = p { code = ann (code p) [] }
>   where
>     ann (s1 :> s2) ks =
>       ann s1 [s2 :> k | k <- ks] :> ann s2 ks
>     ann (Ifte e s1 s2) ks = Ifte e (ann s1 ks) (ann s2 ks)
>     ann (While e s) ks = While e (ann s (s:ks))
>     ann (Choice s1 s2 _) ks =
>       Choice (ann s1 ks) (ann s2 ks) vs
>       where vs = nub (concat [live (s2 :> k) [] | k <- ks])
>     ann other ks = other

Translation to target code
==========================

To allow for parallelism when executing non-deterministic choice,
many instances of a program are going to be created in hardware.  The
translation function from source to target code takes a processor id a
the number of processors available.

(Take care: the same code should be generated for each processor,
barring different instances of variable names.)

> trProg :: Int -> [Int] -> Prog -> P.Prog
> trProg i neighbours p =
>   P.Prog { P.opts  = Map.fromList newOpts
>          , P.decls = trDecls (decls p)
>          , P.code  = snd $ runFresh (trCode (code p)) "_v" 0 }
>   where
>     newOpts :: [(String, Integer)]
>     newOpts =
>       [ ("RomPortsPerBlockRamPort",
>             Map.findWithDefault 4 "ProcessorsPerROM" (opts p) `div` 2)
>       ]
>
>     trCode :: Stm -> Fresh P.Stm
>     trCode s =
>       do s' <- trStm (s :> Halt)
>          return $ P.block $ 
>               [ s' ]
>
>            ++ [ P.Label ("_backtrack" # i) ]
>            ++ [ P.Pop ("_stack" # i) ["_ret" # i], P.Tick ]
>            ++ [ P.IndJump ("_ret" # i) ]
>
>            ++ [ P.Label ("_terminate" # i) ]
>            ++ [ ("_ret" # i) P.:= P.Lab ("_terminate" # i), P.Tick]
>            ++ [ ("_" ++ (a # i) ++ "_sptr") P.:=
>                    P.Lit Nothing 0 | a <- rwArrays]
>            ++ [ P.Push ("_stack" # i) ["_ret" # i], P.Tick ]
>            ++ [ P.Release ("_lock" # i), P.Tick ]
>            ++ [ P.Halt ]
>
>            ++ [ snd (arrayRoutines v aw dw)
>               | Decl v (TArray RW Live (TBit aw) (TBit dw)) init <- decls p ]
>
>     rwArrays = [v | Decl v (TArray RW Live aw dw) init <- decls p]
>
>     deadArrays = Set.fromList
>       [v | Decl v (TArray RW Dead aw dw) init <- decls p]
>
>     arrayRoutines a aw dw = (decls, code)
>       where
>         code = P.block $ 
>                [ P.Label save ]
>             ++ [ P.Store undo P.A (P.Var sp)
>                          (P.Var saveIndex  `P.Concat`
>                            P.RamOutput (a # i) P.B `P.Concat`
>                              P.Var saveMarker) ]
>             ++ [ sp P.:= P.Apply2 P.Add (P.Var sp) (P.Lit Nothing 1) ]
>             ++ [ P.Halt ]
>
>             ++ [ P.Label copy ]
>             ++ [ iter P.:= P.Lit Nothing 0 ]
>             ++ [ done P.:= P.Lit Nothing 0 ]
>             ++ [ P.Fetch (a # i) P.B (P.Lit Nothing 0) ]
>             ++ [ P.Tick ]
>             ++ [ P.While (P.Apply2 P.Neq (P.Var iter) iterMax) $
>                         (writeAddr P.:= P.Select (aw-1) 0 (P.Var iter))
>                    P.:> (writeVal  P.:= P.RamOutput (a # i) P.B)
>                    P.:> (P.ForkJump write)
>                    P.:> (P.Fetch (a # i) P.B (P.Select (aw-1) 0
>                            (P.Apply2 P.Add (P.Var iter)
>                                            (P.Lit Nothing 1))))
>                    P.:> (iter P.:= P.Apply2 P.Add (P.Var iter)
>                                      (P.Lit Nothing 1))
>                    P.:> P.Tick ]
>             ++ [ done P.:= P.Lit Nothing 1 ]
>             ++ [ P.Halt ]
>
>             ++ [ P.Label write ]
>             ++ [ P.Tick ]
>             ++ [ P.Ifte (P.Select j j (P.Var dest))
>                         (P.Store (a # n) P.B (P.Var writeAddr)
>                                              (P.Var writeVal))
>                         (P.Skip)
>                | (j, n) <- zip [0..] neighbours ]
>             ++ [ P.Halt ]
>
>             ++ [ P.Label restore ]
>             ++ [ restored P.:= P.Apply2 P.Eq (P.Var sp) (P.Lit Nothing 0) ]
>             ++ [ P.Fetch undo P.B (P.Apply2 P.Sub (P.Var sp)
>                                     (P.Lit Nothing 1)) ]
>             ++ [ P.Tick ]
>             ++ [ P.While (P.Apply1 P.Inv (P.Var restored)) $
>                         P.Fetch undo P.B (P.Apply2 P.Sub (P.Var sp)
>                                            (P.Lit Nothing 2))
>                    P.:> (sp P.:= P.Apply2 P.Sub (P.Var sp)
>                                    (P.Lit Nothing 1))
>                    P.:> P.Ifte (P.Select 0 0 (P.RamOutput undo P.B))
>                           (restored P.:= (P.Lit Nothing 1))
>                           (P.Store (a # i) P.A
>                              (P.Select (dw+aw) (dw+1) (P.RamOutput undo P.B))
>                              (P.Select dw 1 (P.RamOutput undo P.B)))
>                    P.:> P.Tick ]
>             ++ [ P.Halt ]
>
>         decls =
>           [ P.Decl undo (P.TRam undoDepth undoWidth) P.Uninit
>           , P.Decl sp (P.TReg undoDepth) (P.IntInit 0)
>           , P.Decl saveIndex (P.TReg aw) P.Uninit
>           , P.Decl saveMarker (P.TReg 1) P.Uninit
>           , P.Decl iter (P.TReg (aw+1)) P.Uninit
>           , P.Decl done (P.TReg 1) P.Uninit
>           , P.Decl writeAddr (P.TReg aw) P.Uninit
>           , P.Decl writeVal (P.TReg dw) P.Uninit
>           , P.Decl restored (P.TReg 1) P.Uninit
>           ]
>
>         save        = "_" ++ (a # i) ++ "_save"
>         undo        = "_" ++ (a # i) ++ "_undo"
>         sp          = "_" ++ (a # i) ++ "_sptr"
>         saveIndex   = "_" ++ (a # i) ++ "_save_index"
>         saveMarker  = "_" ++ (a # i) ++ "_save_marker"
>         copy        = "_" ++ (a # i) ++ "_copy"
>         iter        = "_" ++ (a # i) ++ "_copy_iter"
>         iterMax     = P.Lit Nothing (2 ^ aw)
>         done        = "_" ++ (a # i) ++ "_copy_done"
>         writeAddr   = "_" ++ (a # i) ++ "_write_addr"
>         writeVal    = "_" ++ (a # i) ++ "_write_val"
>         write       = "_" ++ (a # i) ++ "_write"
>         dest        = "_token" # i
>         restore     = "_" ++ (a # i) ++ "_restore"
>         restored    = "_" ++ (a # i) ++ "_restored"
>
>         undoWidth = aw + dw + 1
>         undoDepth = fromInteger $
>           log2 (Map.findWithDefault 512 "UndoDepth" (opts p) - 1) + 1
>
>
>     trExp :: Exp -> P.Exp
>     trExp (Lit w n) = P.Lit w n
>     trExp (Var v) = P.Var (v # i)
>     trExp (Apply1 op e) = P.Apply1 op (trExp e)
>     trExp (Apply2 op e1 e2) = P.Apply2 op (trExp e1) (trExp e2)
>     trExp (Cond e1 e2 e3) = P.Cond (trExp e1) (trExp e2) (trExp e3)
>     trExp (Truncate w e) = P.Select (w-1) 0 (trExp e)
>     trExp (Select a b e) = P.Select a b (trExp e)
>     trExp (Concat e1 e2) = P.Concat (trExp e1) (trExp e2)
>
>     trStm :: Stm -> Fresh P.Stm
>     trStm Skip = return P.Skip
>     trStm (x := e) = return $ (((x # i) P.:= trExp e) P.:> P.Tick)
>     trStm (s1 :> s2) = return (P.:>) `ap` trStm s1 `ap` trStm s2
>     trStm (s1 :|| s2) = return par `ap` trStm s1 `ap` trStm s2
>       where par x y = P.Par [x, y]
>     trStm (Ifte e s1 s2) =
>       return (P.Ifte (trExp e)) `ap` trStm s1 `ap` trStm s2
>     trStm (While e s) = -- TODO: ensure no combinatorial loops
>       return (P.While (trExp e)) `ap` trStm s
>     trStm (Choice s1 s2 live) =
>       do retLabel' <- fresh
>          endLabel' <- fresh
>          spawnLabel' <- fresh
>          let stack    = "_stack" # i
>          let ret      = "_ret" # i
>          let retLabel = retLabel' # i
>          let endLabel = endLabel' # i
>          let token    = "_token" # i
>          let spawnLabel = spawnLabel' # i
>          let saveMarker a = "_" ++ (a # i) ++ "_save_marker"
>          let arraySave a = "_" ++ (a # i) ++ "_save"
>          let arrayCopy a = "_" ++ (a # i) ++ "_copy"
>          let copyNotDone a = P.Apply1 P.Inv
>                (P.Var ("_" ++ (a # i) ++ "_copy_done"))
>          let waitCopyDone = if List.null rwArrays then [P.Tick] else
>                [P.Tick, P.While (List.foldr1 (P.Apply2 P.Or)
>                           (List.map copyNotDone rwArrays)) P.Tick, P.Tick]
>          let arrayRestore a = "_" ++ (a # i) ++ "_restore"
>          let restoreNotDone a = P.Apply1 P.Inv
>                (P.Var ("_" ++ (a # i) ++ "_restored"))
>          let waitRestoreDone = if List.null rwArrays then [P.Tick] else
>                [P.Tick, P.While (List.foldr1 (P.Apply2 P.Or)
>                           (List.map restoreNotDone rwArrays)) P.Tick, P.Tick]
>          c1 <- trStm s1
>          c2 <- trStm s2
>          let copyRegs bit j =
>                P.Ifte (P.Select bit bit (P.Var token))
>                       (P.block [ (v # j) P.:= P.Var (v # i) | v <- live ])
>                       P.Skip
>          let spawn bit j =
>                P.Ifte (P.Select bit bit (P.Var token))
>                       (P.ForkJump (spawnLabel' # j))
>                       P.Skip
>          let saveRestore = 
>                P.block $ [ saveMarker a P.:= P.Lit Nothing 1 
>                          | a <- rwArrays ]
>                       ++ [ P.Tick | not (List.null rwArrays) ]
>                       ++ [ P.ForkJump (arraySave a) | a <- rwArrays ]
>                       ++ [ P.Push stack [v # i | v <- live ] ]
>                       ++ [ ret P.:= P.Lab retLabel, P.Tick ]
>                       ++ [ P.Push stack [ret], P.Tick ]
>          return $ P.block $
>            (if length neighbours > 0 then
>                [ P.Acquire token ["_lock" # j | j <- neighbours]
>                , P.Tick
>                , P.Ifte (P.Apply2 P.Neq (P.Var token)
>                                         (P.Lit (Just $ length neighbours) 0))
>                         (P.block $
>                              [ P.ForkJump (arrayCopy a) | a <- rwArrays ]
>                           ++ [ copyRegs bit j
>                              | (bit, j) <- zip [0..] neighbours ]
>                           ++ waitCopyDone
>                           ++ [ spawn bit j
>                              | (bit, j) <- zip [0..] neighbours])
>                         saveRestore
>                ] else [saveRestore])
>            ++ [ c1, P.Jump endLabel ]
>            ++ [ P.Label retLabel ]
>            ++ [ P.ForkJump (arrayRestore a) | a <- rwArrays ]
>            ++ [ P.Pop stack [v # i | v <- live ] ]
>            ++ waitRestoreDone
>            ++ [ P.Label spawnLabel ]
>            ++ [ c2 ]
>            ++ [ P.Label endLabel ]
>     trStm Fail =
>          return $ P.block $
>               [ P.Jump ("_backtrack" # i) ]
>     trStm Halt =
>          -- Emit a bit indicating that a solution has been found
>          return $ P.block $
>               [ P.Label ("_emit_loop" # i),
>                 P.Ifte (P.Var ("_emit" # i))
>                        (P.Tick P.:> P.Jump ("_emit_loop" # i))
>                        P.Skip,
>                 ("_emit" # i) P.:= P.Lit (Just 1) 1,
>                 P.Tick, P.Jump ("_backtrack"  # i) ]
>     trStm (ArrayLookup RO x a e) =
>       return $ P.LoadRom (x # i) a (show i) (trExp e) P.:> P.Tick
>     trStm (ArrayLookup RW x a e) = return $
>       P.Fetch (a # i) P.A (trExp e) P.:> P.Tick P.:>
>       ((x # i) P.:= P.RamOutput (a # i) P.A) P.:> P.Tick
>     trStm (ArrayAssign a e1 e2)
>       | a `Set.member` deadArrays = return $
>              P.Store (a # i) P.A (trExp e1) (trExp e2)
>         P.:> P.Tick
>       | otherwise = return $
>              P.Store (a # i) P.A (trExp e1) (trExp e2)
>         P.:> P.Fetch (a # i) P.B (trExp e1)
>         P.:> (("_" ++ (a # i) ++ "_save_index") P.:= trExp e1)
>         P.:> (("_" ++ (a # i) ++ "_save_marker") P.:= P.Lit (Just 1) 0)
>         P.:> P.Tick
>         P.:> P.ForkJump ("_" ++ (a # i) ++ "_save")
>
>     trDecls :: [Decl] -> [P.Decl]
>     trDecls ds =
>         [P.Decl (v # i) (P.TReg n) init | Decl v (TBit n) init <- ds]
>      ++ [P.Decl (v # i) (P.TRam aw dw) init
>         | Decl v (TArray RW _ (TBit aw) (TBit dw)) init <- ds]
>      ++ [P.Decl ("_stack" # i) (P.TRam stackDepth stackWidth) P.Uninit]
>      ++ [P.Decl ("_ret" # i) (P.TLab []) P.Uninit]
>      ++ [P.Decl ("_emit" # i) (P.TReg 1) P.Uninit]
>      ++ [P.Decl ("_token" # i) (P.TReg (length neighbours)) P.Uninit
>         | length neighbours > 0]
>      ++ concat [ fst (arrayRoutines v aw dw)
>                | Decl v (TArray RW Live (TBit aw) (TBit dw)) init <- ds ]
>
>     par [] = P.Skip
>     par ss = P.Par ss
>
>     stackWidth = fromInteger $ Map.findWithDefault 40 "StackWidth" (opts p)
>     stackDepth = fromInteger $
>       log2 (Map.findWithDefault 512 "StackDepth" (opts p) - 1) + 1
>

> log2 :: Integral a => a -> a
> log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)

A network is a mapping from node ids to neighbouring node ids.

> type Node    = Int
> type Network = Map Node [Node]

Generate target code for a network whose nodes are instances of
program p.  (Node ids must lie in range [0..n-1] where n is the number
of nodes.)

> translate :: Network -> Prog -> P.Prog
> translate network p = P.Prog Map.empty ds c
>   where
>     -- Number of processors
>     n  = size network
>
>     -- Code for each processor
>     ps = [ trProg i (network!i) p | i <- [0..n-1] ]
>
>     -- Code for entire program
>     c  = P.block $ 
>          -- Interface & output logic runs in background
>             [ P.ForkJump "_interface" ]
>          ++ [ P.ForkJump "_output" ]
>
>          -- Print start byte
>          ++ [ "_startbyte" P.:= P.Lit Nothing 49, P.Tick ]
>          ++ [ P.Print "_startbyte" ]
>
>          -- Initialise stacks
>          ++ [ ("_ret" # i) P.:= P.Lab ("_terminate" # i) | i <- [0..n-1] ]
>          ++ [ P.Tick ]
>          ++ [ P.Push ("_stack" # i) ["_ret" # i] | i <- [0..n-1] ]
>          ++ [ P.Tick ]
>
>          -- Initially, only the first processor is executing code
>          ++ [ P.Acquire ("_initial_lock") ["_lock" # 0] ]
>          ++ [ P.Tick ]
>          ++ [ ("_done" # n) P.:= P.Lit (Just 1) 1 ]
>
>          -- Code for each processor
>          ++ List.map P.code ps
>
>          -- External interface maintains:
>          --   1. a "finished" bit, i.e. all cores are idle
>          --   2. a count of the number of solutions found
>          ++ [ P.Label "_interface" ]
>          ++ [ ("_emit_bus" # i) P.:= P.Var ("_emit_bus" # (i+1))
>             | i <- [0..n-1] ]
>          ++ [ ("_done" # i) P.:=
>                 P.Apply2 P.And (P.Var ("_done" # (i+1)))
>                                       (P.Available ["_lock" # i])
>             | i <- [0..n-1] ]
>          ++ [ P.Ifte (P.Var ("_emit_bus" # 0))
>                      ("_count" P.:= P.Apply2 P.Add
>                         (P.Var "_count") (P.Lit Nothing 1))
>                      P.Skip ]
>          ++ [ P.Ifte (P.Apply2 P.And (P.Var ("_emit" # i))
>                         (P.Apply1 P.Inv (P.Var ("_emit_bus" # (i+1)))))
>                      (("_emit" # i) P.:= P.Lit Nothing 0 P.:>
>                       ("_emit_bus" # i) P.:= P.Lit Nothing 1)
>                      P.Skip
>             | i <- [0..n-1] ]
>          ++ [ P.Ifte (P.Var ("_done" # 0))
>                      ("_done_count" P.:= P.Apply2 P.Add
>                          (P.Var "_done_count") (P.Lit Nothing 1))
>                      ("_done_count" P.:= P.Lit Nothing 0) ]
>          ++ [ P.Tick, P.Jump "_interface" ]
>
>          -- Output solution count when done
>          ++ [ P.Label "_output" ]
>          ++ [ P.Ifte (P.Apply2 P.Eq (P.Var "_done_count")
>                                     (P.Lit Nothing (toInteger n)))
>                      (P.GPrint 32 "_count" P.:> P.Halt)
>                      P.Skip ]
>          ++ [ P.Tick, P.Jump "_output" ]
>          
>     -- Declarations for entire program
>     ds = [ P.Decl ("_count") (P.TReg 32) P.Uninit
>          , P.Decl ("_started") (P.TReg 1) P.Uninit
>          , P.Decl ("_initial_lock") (P.TReg 1) P.Uninit
>          , P.Decl ("_done_count") (P.TReg 20) P.Uninit
>          , P.Decl "_startbyte" (P.TReg 8) P.Uninit ]
>       ++ [ P.Decl ("_lock" # i) (P.TLock) P.Uninit | i <- [0..n-1] ]
>       ++ [ P.Decl ("_emit_bus" # i) (P.TReg 1) P.Uninit | i <- [0..n] ]
>       ++ [ P.Decl ("_done" # i) (P.TReg 1) P.Uninit | i <- [0..n] ]
>       ++ concatMap P.decls ps
>       ++ [ P.Decl v (P.TRom aw dw) init
>          | Decl v (TArray RO _ (TBit aw) (TBit dw)) init <- decls p]

> (#) :: String -> Int -> String
> v # i = v ++ "[" ++ show i ++ "]"

Network topologies
==================

Every node connected to every other.

> fullyConnected :: Int -> Network
> fullyConnected n = fromList [(i, [0..n-1] List.\\ [i]) | i <- [0..n-1]]

2D mesh.

> mesh :: Int -> Int -> Network
> mesh x y =
>     fromList [(node i j, neighbours i j) | j <- [0..y-1], i <- [0..x-1]]
>   where
>     node i j = if odd j then j*x + (x-i-1) else j*x+i
>     neighbours i j =
>       [ node (i-1) j | i > 0   ] ++
>       [ node (i+1) j | i < x-1 ] ++
>       [ node i (j-1) | j > 0   ] ++
>       [ node i (j+1) | j < y-1 ]

2D torus (wrapped 2D mesh).

> torus :: Int -> Int -> Network
> torus x y =
>     fromList [(node i j, neighbours i j) | j <- [0..y-1], i <- [0..x-1]]
>   where
>     node i j = if odd j then j*x + (x-i-1) else j*x+i
>     neighbours i j =
>       [if i > 0   then node (i-1) j else node (x-1) j] ++
>       [if i < x-1 then node (i+1) j else node 0 j    ] ++
>       [if j > 0   then node i (j-1) else node i (y-1)] ++
>       [if j < y-1 then node i (j+1) else node i 0    ]

Wrapped butterfly network (radix 2).

> butterfly :: Int -> Network
> butterfly n =
>     fromList [(node, neighbours node) | node <- [0..(n*width)-1]]
>   where
>     width = 2^n
>     nodes = [[width*i .. width*i+(width-1)] | i <- [0..n-1]]
>     edges = bfly (nodes ++ [head nodes])
>     neighbours node = nub
>                     $ [y | (x, y) <- edges, x == node]
>                    ++ [x | (x, y) <- edges, y == node]

> bfly :: [[Node]] -> [(Node, Node)]
> bfly [x] = []
> bfly (x:y:ys) =
>      zip x y
>   ++ zip lx ry
>   ++ zip rx ly
>   ++ bfly (ly:lys)
>   ++ bfly (ry:rys)
>   where
>     (lx, rx) = half x
>     (ly, ry) = half y
>     (lys, rys) = unzip (List.map half ys)

> half :: [a] -> ([a], [a])
> half xs = splitAt n xs
>   where n = length xs `div` 2

Wrapped repeated butterfly network (radix 2).

> wavefly :: Int -> Int -> Network
> wavefly n d =
>     fromList [(node, neighbours node) | node <- nub (concat nodes)]
>   where
>     width = 2^n
>     nodes = transpose
>           $ take width [[d*i .. d*i+(d-1)] | i <- [0..]]
>     edges = wave (head nodes) nodes
>     neighbours node = nub
>                     $ [y | (x, y) <- edges, x == node]
>                    ++ [x | (x, y) <- edges, y == node]
>     wave firstRow rows
>       | length rows <= n   = bfly (rows ++ [firstRow])
>       | otherwise          = bfly block ++ wave firstRow (last block:rest)
>       where (block, rest)  = splitAt (n+1) rows

Auxiliaries
===========

> m ! k = case Map.lookup k m of
>           Nothing -> error ("Undeclared identifier: " ++ show k)
>           Just x -> x

Top-level compiler
==================

> compile :: Prog -> P.Prog
> compile = -- translate (butterfly 2)
>           -- translate (fullyConnected 24)
>           -- translate (mesh 25 25)
>           -- translate (mesh 17 18)
>           -- translate (torus 10 10)
>           -- translate (torus 25 25)
>           -- translate (butterfly 6)
>           -- translate (wavefly 5 6)
>           -- translate (butterfly 4)
>           -- translate (torus 12 12)
>           -- translate (torus 8 8)
>           -- translate (wavefly 6 8)
>           -- translate (wavefly 5 16)
>           -- translate (torus 23 23)
>           translate (torus 3 3)
>           --translate (wavefly 4 5)
>           --translate (wavefly 4 8)
>         . annotateLive
>         . typeCheck
>         . arrayAnalysis
>         . staticRestrictions
>         . desugarTypes
