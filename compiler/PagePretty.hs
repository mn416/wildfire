module PagePretty (putPageProg, prettyPageProg) where

import PageSyntax
import Text.PrettyPrint
  
putPageProg :: Prog -> IO ()
putPageProg = putStrLn . prettyPageProg

prettyPageProg :: Prog -> String
prettyPageProg p = renderStyle style (prettyProg p)
  where
    style = Style { mode = PageMode, lineLength = 70, ribbonsPerLine = 1.5 }

prettyProg :: Prog -> Doc
prettyProg p =
     text "declare"
  $$ nest 2 (vcat (punctuate comma
                             (map prettyDecl (decls p))))
  $$ text "in"
  $$ nest 2 (prettyStm (code p))

prettyType :: Type -> Doc
prettyType (TNat w)     = text "nat" <+> text (show w)
prettyType (TPtr w _)   = text "^" <> parens (text "nat" <+> text (show w))
prettyType (TLab labs)  = text "label" <+> text "=" <+> text (show labs)
prettyType TLock        = text "lock"
prettyType (TRam aw dw) = text "ram" <+> text (show aw) <+> text (show dw)

prettyDecl :: (Id, Type) -> Doc
prettyDecl (v, ty) = text v <+> char ':' <+> prettyType ty

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
  $$ text "end if"
prettyStm (While e s) =
     text "while" <+> prettyExp e
  $$ nest 2 (parens (prettyStm s))
  $$ text "end while"
prettyStm (Label lab) = text lab <> text ":"
prettyStm (Jump lab) = text "jump" <+> text lab
prettyStm (ForkJump lab) = text "fork" <+> text lab
prettyStm (IndJump v) = text "indjump" <+> text v
prettyStm (Acquire v locks) =
  text "acquire" <+> text v <+> hsep (map text locks)
prettyStm (Release v) = text "release" <+> text v
prettyStm (Print v) = text "print" <+> text v
prettyStm (GPrint _ v) = text "gprint" <+> text v
prettyStm (Load v e) =
  text "fetch" <+> text v <+> text "[" <+> prettyExp e <+> text "]"
prettyStm (Store v e1 e2) = text v <+> text "[" <> prettyExp e1 <> text "]"
                                   <+> text ":=" <+> prettyExp e2
prettyStm (Push m v) = text "push" <+> text m <+> text v
prettyStm (Pop m v) = text "pop" <+> text m <+> text v
prettyStm Halt = text "halt"

prettyExp :: Exp -> Doc
prettyExp (Lit Nothing i) = text (show i)
prettyExp (Lit (Just w) i) = text (show i) <> text ":" <> text (show w)
prettyExp (Var v) = text v
prettyExp (Lab l) = text l
prettyExp (Ptr p) = text "^" <> text p
prettyExp (Apply1 (Shl n) e1) =
  parens (prettyExp e1 <+> text "<<" <+> text (show n))
prettyExp (Apply1 (Shr n) e1) =
  parens (prettyExp e1 <+> text ">>" <+> text (show n))
prettyExp (Apply1 f e1) = parens (op1 f <+> prettyExp e1)
prettyExp (Apply2 f e1 e2) = parens (prettyExp e1 <+> op2 f <+> prettyExp e2)
prettyExp (RamOutput v) = text "data" <+> text v
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
