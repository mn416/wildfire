> module WhileCompile (
>   compile -- :: Prog -> P.Prog ()
> ) where

> import Netlist
> import Descend
> import WhileSyntax
> import qualified PageSyntax as P
> import MonadInstances
> import Control.Monad
> import Data.Maybe
> import Data.Map as Map
> import Data.List as List

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
>         TNat w -> Just w
>         other  -> Nothing
>     widthOf (Apply1 op e) = widthOf e
>     widthOf (Apply2 op e1 e2) = widthOf e1 `mplus` widthOf e2
>
>     -- Type checker for expressions
>     tcExp :: Int -> Exp -> Exp
>     tcExp w e = ch e
>       where
>         ch (Lit Nothing n) = Lit (Just w) n
>         ch (Lit (Just w') n) | w == w' = Lit (Just w) n
>         ch (Var v) | env!v == TNat w = e
>         ch (Apply1 op e) = Apply1 op (tcExp w e)
>         ch (Apply2 op e1 e2)
>           | P.isCmpOp op =
>               case widthOf e1 `mplus` widthOf e2 of
>                 Nothing -> typeErrorW (show e) w
>                 Just w -> Apply2 op (tcExp w e1) (tcExp w e2)
>           | not (P.isCmpOp op) = Apply2 op (tcExp w e1) (tcExp w e2)
>         ch other = typeErrorW (show other) w
>
>     -- Type checker for statements
>     tc Skip = Skip
>     tc (s1 :> s2) = tc s1 :> tc s2
>     tc (v := e) =
>       case env!v of
>         TNat n -> v := tcExp n e
>         other  -> typeError (show (v := e))
>     tc (s1 :|| s2) = tc s1 :|| tc s2
>     tc (Ifte e s1 s2) = Ifte (tcExp 1 e) (tc s1) (tc s2)
>     tc (While e s) = While (tcExp 1 e) (tc s)
>     tc (Choice s1 s2 live) = Choice (tc s1) (tc s2) live
>     tc Fail = Fail
>     tc Halt = Halt
>     tc other = typeError (show other)
>
>     -- Type checker for declarations
>     tcDecl d =
>       case d of
>         Decl v (TNat n) (P.IntInit i) -> d
>         Decl v _ (P.IntInit i) -> typeError ("bad type for variable " ++ v)
>         other -> other

Static restrictions
===================

(1) The choice operator must not occur within a fork-join parallel
block because bactracking to a label within a parallel block will lead
to indefinite blocking (the other branches are not active so
synchronisation will never occur).

(2) In a statement p || q, if p modifies a variable v then then q must
not refer to v, and vice-versa.  Otherwise parallel threads could
interact in unforeseen ways.  With this restriction, p || q is
semantically equivalent to both p ; q and q ; p.

Both these restrictions involve the || operator and suggest that use
of this operator should be inferred by the compiler rather than
specified by the programmer.  (Future work.)

> staticRestrictions :: Prog -> Prog
> staticRestrictions p = p { code = check1 (check2 (code p)) }
>   where
>     check1 (p :|| q)
>       | noChoices p && noChoices q = check1 p :|| check1 q
>       | otherwise = error "? operator forbidden inside ||"
>     check1 other = descend check1 other
>
>     noChoices s = List.null (choices s)
>     choices (Choice p q _) = [()]
>     choices other = extract choices other
>
>     check2 (p :|| q)
>       | not (any (`elem` useStm q) (def p))
>      && not (any (`elem` useStm p) (def q))
>       = check2 p :|| check2 q
>       | otherwise = error "parallel branches must be independent"
>     check2 other = descend check2 other
>
>     useStm s = nub (concatMap use (exprs s))

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

  acquire j NEIGHBOURS(i) ;
  if j /= 0 then
    v1[j] := v1[i] ;
    ... ;
    vn[j] := vn[i] ;
    forkjump spawnLabel[j]
  else
    push stack[i] v1[i] ;
    ... ;
    push stack[i] vn[i] ;
    push stack[i] retLabel[i] 
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

Live variables
==============

Return the variables live-in to any given statement.  The rule for the
|| operator looks suspicous but remember the parallel branches are
independent, as required by static restriction (2), and are therefore
semantically equivalent to sequential execution in any order.

> live :: Stm -> [Id] -> [Id]
> live (x := e) vs = nub (use e) `List.union` (vs List.\\ [x])
> live (Ifte e s1 s2) vs = live s1 vs `List.union` live s2 vs
> live (While e s) vs = live s [] `List.union` vs
> live (s1 :> s2) vs = live s1 (live s2 vs)
> live (s1 :|| s2) vs = live s1 (live s2 vs)
> live (Choice s1 s2 _) vs = live s1 vs `List.union` live s2 vs
> live other vs = vs

> use :: Exp -> [Id]
> use (Var v) = [v]
> use other = extract use other

> def :: Stm -> [Id]
> def (x := e) = [x]
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
>   P.Prog { P.decls = trDecls (decls p)
>          , P.code = snd $ runFresh (trCode (code p)) "_v" 0 }
>   where
>     trCode :: Stm -> Fresh P.Stm
>     trCode s =
>       do s' <- trStm s
>          return $ P.block $ 
>               [ s' ]
>
>            ++ [ P.Label ("_backtrack" # i) ]
>            ++ [ P.Pop ("_stack" # i) ["_ret" # i], P.Tick ]
>            ++ [ P.IndJump ("_ret" # i) ]
>
>            ++ [ P.Label ("_terminate" # i) ]
>            ++ [ ("_ret" # i) P.:= P.Lab ("_terminate" # i), P.Tick]
>            ++ [ P.Push ("_stack" # i) ["_ret" # i], P.Tick ]
>            ++ [ P.Release ("_lock" # i), P.Tick ]
>            ++ [ P.Halt ]
>
>     trExp :: Exp -> P.Exp
>     trExp (Lit w n) = P.Lit w n
>     trExp (Var v) = P.Var (v # i)
>     trExp (Apply1 op e) = P.Apply1 op (trExp e)
>     trExp (Apply2 op e1 e2) = P.Apply2 op (trExp e1) (trExp e2)
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
>          c1 <- trStm s1
>          c2 <- trStm s2
>          let spawn bit j = P.Ifte (P.Select bit bit (P.Var token))
>                   (P.block $ [ (v # j) P.:= P.Var (v # i) | v <- live ]
>                           ++ [ P.Tick, P.ForkJump (spawnLabel' # j) ])
>                   P.Tick
>          let saveRestore = P.block $ intersperse P.Tick
>                               [ P.Push stack [v # i | v <- live ] ]
>                            ++ [ ret P.:= P.Lab retLabel, P.Tick ]
>                            ++ [ P.Push stack [ret], P.Tick ]
>          return $ P.block $
>            (if length neighbours > 0 then
>                [ P.Acquire token ["_lock" # j | j <- neighbours]
>                , P.Tick
>                , P.Ifte (P.Apply2 P.Neq (P.Var token)
>                                         (P.Lit (Just $ length neighbours) 0))
>                         (par [spawn bit j | (bit, j) <- zip [0..] neighbours])
>                         saveRestore
>                ] else [saveRestore])
>            ++ [ c1, P.Jump endLabel ]
>            ++ [ P.Label retLabel ]
>            ++ [ P.Pop stack [v # i | v <- live ] ]
>            ++ [ P.Tick ]
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
>
>     trDecls :: [Decl] -> [P.Decl]
>     trDecls ds =
>         [P.Decl (v # i) (P.TNat n) init | Decl v (TNat n) init <- ds]
>      ++ [P.Decl ("_stack" # i) (P.TRam stackDepth stackWidth) P.Uninit]
>      ++ [P.Decl ("_ret" # i) (P.TLab []) P.Uninit]
>      ++ [P.Decl ("_emit" # i) (P.TNat 1) P.Uninit]
>      ++ [P.Decl ("_token" # i) (P.TNat (length neighbours)) P.Uninit
>         | length neighbours > 0]
>
>     par [] = P.Skip
>     par ss = P.Par ss
>
>     stackWidth = fromInteger $ Map.findWithDefault 40 "StackWidth" (opts p)
>     stackDepth = fromInteger $
>       log2 (Map.findWithDefault 512 "StackDepth" (opts p) - 1) + 1
>
>     log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)

A network is a mapping from node ids to neighbouring node ids.

> type Node    = Int
> type Network = Map Node [Node]

Generate target code for a network whose nodes are instances of
program p.  (Node ids must lie in range [0..n-1] where n is the number
of nodes.)

> translate :: Network -> Prog -> P.Prog
> translate network p = P.Prog ds c
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
>     ds = [ P.Decl ("_count") (P.TNat 32) P.Uninit
>          , P.Decl ("_started") (P.TNat 1) P.Uninit
>          , P.Decl ("_initial_lock") (P.TNat 1) P.Uninit
>          , P.Decl ("_done_count") (P.TNat 20) P.Uninit
>          , P.Decl "_startbyte" (P.TNat 8) P.Uninit ]
>       ++ [ P.Decl ("_lock" # i) (P.TLock) P.Uninit | i <- [0..n-1] ]
>       ++ [ P.Decl ("_emit_bus" # i) (P.TNat 1) P.Uninit | i <- [0..n] ]
>       ++ [ P.Decl ("_done" # i) (P.TNat 1) P.Uninit | i <- [0..n] ]
>       ++ concatMap P.decls ps

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
>           -- translate (wavefly 6 8)
>           -- translate (butterfly 4)
>           -- translate (torus 12 12)
>           -- translate (torus 8 8)
>           -- translate (wavefly 6 8)
>           -- translate (wavefly 5 16)
>           translate (torus 23 23)
>         . annotateLive
>         . typeCheck
>         . staticRestrictions
