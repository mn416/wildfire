module PagePretty (putPageProg, prettyPageProg) where

import PageSyntax
import Text.PrettyPrint
import qualified Data.Map as Map
  
putPageProg :: Prog -> IO ()
putPageProg = putStrLn . prettyPageProg

prettyPageProg :: Prog -> String
prettyPageProg p = renderStyle style (prettyProg p)
  where
    style = Style { mode = PageMode, lineLength = 70, ribbonsPerLine = 1.5 }

prettyProg :: Prog -> Doc
prettyProg p =
     vcat [ text "opt " <+> text k <+> text "=" <+> text (show v)
          | (k, v) <- Map.toList (opts p) ]
  $$ vcat (map prettyDecl (decls p))
  $$ prettyStm (code p)

prettyType :: Type -> Doc
prettyType (TReg w)     = text "reg" <+> text (show w)
prettyType (TPtr w _)   = text "ptr" <> text (show w)
prettyType (TLab labs)  = text "label" <+> text "=" <+> text (show labs)
prettyType TLock        = text "lock"
prettyType (TRom aw dw) = text "rom" <+> text (show aw) <+> text (show dw)
prettyType (TRam aw dw) = text "ram" <+> text (show aw) <+> text (show dw)
prettyType (TMWRam aw1 dw1 aw2 dw2) =
  text "ram" <+> text "<" <> text (show aw1) <> text "->"
             <>  text (show dw1) <> text ">"
             <+> text "<" <> text (show aw2) <> text "->"
             <>  text (show dw2) <> text ">"

prettyDecl :: Decl -> Doc
prettyDecl (Decl v ty init) =
  text "var" <+> text v <+> char ':' <+> prettyType ty <+> prettyInit init

prettyInit :: Init -> Doc
prettyInit (Uninit) = text ""
prettyInit (IntInit i) = text ":=" <+> text (show i)
prettyInit (StrInit s) = text ":=" <+> text s

prettyStm :: Stm -> Doc
prettyStm Skip = text "skip"
prettyStm Tick = text "tick"
prettyStm (Label l :> s) = empty $$ (text l <> text ":") $$ prettyStm s
prettyStm (s1 :> s2) = (prettyStm s1 <+> semi) $$ prettyStm s2
prettyStm (v := e) = text v <+> text ":=" <+> prettyExp e
prettyStm (IndAssign p e) = text "^" <> text p <+> text ":=" <+> prettyExp e
prettyStm (Par ss) =
  vcat (punctuate (text "||") (map (parens . prettyStm) ss))
prettyStm (Ifte e s1 s2) =
     text "if" <+> prettyExp e <+> text "then"
  $$ nest 2 (prettyStm s1)
  $$ text "else"
  $$ nest 2 (prettyStm s2)
  $$ text "end"
prettyStm (While e s) =
     text "while" <+> prettyExp e <+> text "do"
  $$ nest 2 (prettyStm s)
  $$ text "end"
prettyStm (Label lab) = text lab <> text ":"
prettyStm (Jump lab) = text "jump" <+> text lab
prettyStm (ForkJump lab) = text "fork" <+> text lab
prettyStm (IndJump v) = text "indjump" <+> text v
prettyStm (Acquire v locks) =
  text "acquire" <+> text v <+> hsep (map text locks)
prettyStm (Release v) = text "release" <+> text v
prettyStm (Print v) = text "print" <+> text v
prettyStm (GPrint _ v) = text "gprint" <+> text v
prettyStm (Fetch v p e) =
  text "fetch" <+> text (v ++ ":" ++ show p) <+>
  text "[" <+> prettyExp e <+> text "]"
prettyStm (Store v p e1 e2) =
  text (v ++ ":" ++ show p) <+> text "[" <> prettyExp e1 <> text "]"
    <+> text ":=" <+> prettyExp e2
prettyStm (Push m vs) = text "push" <+> text m <+> hsep (map text vs)
prettyStm (Pop m vs) = text "pop" <+> text m <+> hsep (map text vs)
prettyStm (LoadRom x r p e) =
  text "loadrom" <+> text x <+> text (r ++ ":" ++ p) <+>
    text "[" <+> prettyExp e <+> text "]"
prettyStm Halt = text "halt"

prettyExp :: Exp -> Doc
prettyExp (Lit Nothing i) = text (show i)
prettyExp (Lit (Just w) i) = text (show i) <> text ":" <> text (show w)
prettyExp (Var v) = text v
prettyExp (Lab l) = text l
prettyExp (Ptr p) = text "^" <> text p
prettyExp (Apply1 MSB e1) = text "msb" <> parens (prettyExp e1)
prettyExp (Apply1 (Shl n) e1) =
  parens (prettyExp e1 <+> text "<<" <+> text (show n))
prettyExp (Apply1 (Shr n) e1) =
  parens (prettyExp e1 <+> text ">>" <+> text (show n))
prettyExp (Apply1 f e1) = parens (op1 f <+> prettyExp e1)
prettyExp (Apply2 f e1 e2) = parens (prettyExp e1 <+> op2 f <+> prettyExp e2)
prettyExp (Cond e1 e2 e3) = text "cond" <>
  parens (prettyExp e1 <> text "," <+>
          prettyExp e2 <> text "," <+>
          prettyExp e3)
prettyExp (RamOutput v A) = text ("data(" ++ v ++ ":A)")
prettyExp (RamOutput v B) = text ("data(" ++ v ++ ":B)")
prettyExp (Select i j e) =
      text "bits" <+> text (show i) <+> text "to"
  <+> text (show j) <+> text "of" <+> prettyExp e
prettyExp (Concat e1 e2) = prettyExp e1 <+> text "." <+> prettyExp e2
prettyExp (Available xs) =
  text "available" <+> hsep (map text xs)

op1 Inv = text "~"

op2 And = text "&"
op2 Or = text "|"
op2 Xor = text "xor"
op2 Add = text "+"
op2 Sub = text "-"
op2 Mul = text "*"
op2 Div = text "/"
op2 Eq = text "=="
op2 Neq = text "/="
op2 Lt = text "<"
op2 Lte = text "<="
op2 Gt = text ">"
op2 Gte = text ">="
