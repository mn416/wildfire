module WhileParser (parseProgFile) where

import WhileSyntax
import PageSyntax (UnaryOp(..), BinOp(..), Init(..))
import Control.Applicative
import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec hiding (many, option, (<|>))
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Data.Map as Map
import Data.Bits
  
page = T.makeTokenParser $ emptyDef
  { commentLine      = "--"
  , nestedComments   = False
  , identStart       = letter
  , identLetter      = alphaNum
  , opStart          = opLetter haskellStyle
  , opLetter         = oneOf "+-*/=<>;:|@.^~?"
  , reservedNames    = ["skip", "if", "then", "else", "end",
                        "while", "declare", "in", "fail", "cond",
                        "opt", "var", "const", "msb", "bit",
                        "log", "type", "enum", "rec", "struct"]
  , caseSensitive    = True
  }
  
identifier = T.identifier page
reservedOp = T.reservedOp page
reserved = T.reserved page
natural = T.natural page
parens = T.parens page
semi = T.semi page
comma = T.comma page
braces = T.braces page
brackets = T.brackets page
symbol = T.symbol page
operator = T.operator page
charLiteral = T.charLiteral page
stringLiteral = T.stringLiteral page
lexeme = T.lexeme page
whitespace = T.whiteSpace page

-- Dotted identifier
dottedIdentifier = lexeme $ do
  x  <- letter
  xs <- many (letter <|> char '_' <|> digit <|> char '.')
  return (x:xs)
  
-- Split dotted identifier
split :: String -> [String]
split s = sp [] s
  where
    sp acc [] = [reverse acc]
    sp acc (c:cs) =
      if c == '.' then reverse acc : sp "" cs else sp (c:acc) cs

-- Parse a "const" expression
-- (These expressions are evaluated by the parser)

type ConstMap = Map.Map String Integer

constEval :: ConstMap -> Exp -> Integer
constEval env e =
  case e of
    Lit w i     -> i
    Var v       -> env!v
    Apply1 op e -> 
      let i = constEval env e in
        case op of
          Inv   -> complement i
          Shl n -> i `shiftL` n
          Shr n -> i `shiftR` n
          Log   -> if i <= 1 then 1 else log2 (i - 1) + 1
          _     -> error "Invalid 'const' expression"
    Apply2 op e1 e2 ->
      let i = constEval env e1
          j = constEval env e2 in
        case op of
          And -> i .&. j
          Or  -> i .|. j
          Xor -> i `xor` j
          Add -> i + j
          Sub -> i - j
          Mul -> i * j
          Div -> i `div` j
          Eq  -> if i == j then 1 else 0
          Neq -> if i /= j then 1 else 0
          Lt  -> if i <  j then 1 else 0
          Lte -> if i <= j then 1 else 0
          Gt  -> if i >  j then 1 else 0
          Gte -> if i >= j then 1 else 0
          _   -> error "Invalid 'const' expression"
    Cond e1 e2 e3 ->
      let i = constEval env e1
          j = constEval env e2
          k = constEval env e3 in
        if i == 0 then k else j
  where
    m ! k  = Map.findWithDefault (err k) k env
    err k  = error ("In 'const' expression, unbound variable: " ++ show k)
    log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)

constDecl :: ConstMap -> Parser ConstMap
constDecl env = do
  reserved "const"
  id <- identifier
  reserved "="
  e <- expr env
  return (Map.insert id (constEval env e) env)

constDecls :: ConstMap -> Parser ConstMap
constDecls env = do
  m <- optionMaybe (constDecl env)
  case m of
    Nothing   -> return env
    Just env' -> constDecls env'

number :: ConstMap -> Parser Integer
number env = natural <|> do
  x <- identifier
  case Map.lookup x env of
    Just i -> return i
    _      -> error ("Unbound variable in 'const' expression: " ++ x)

num :: ConstMap -> Parser Int
num env = pure fromInteger <*> number env

constExpr :: ConstMap -> Parser Integer
constExpr env = do
  e <- expr env
  return (constEval env e)

constExprInt :: ConstMap -> Parser Int
constExprInt env = pure fromInteger <*> constExpr env

-- Expressions

expBinOp op f assoc = Infix (reservedOp op >> return (Apply2 f)) assoc

expUnOp op f = Prefix (reservedOp op >> return (Apply1 f))
  
expBinOp' op f assoc = Infix (reservedOp op >> return f) assoc

opTable =
  [ [ expUnOp "~" Inv ]
  , [ Infix (reservedOp "++" >> return Concat) AssocRight]
  , [ expBinOp' ">>" shiftr AssocLeft, expBinOp' "<<" shiftl AssocLeft]
  , [ expBinOp "*" Mul AssocLeft, expBinOp "/" Div AssocLeft
    , expBinOp "&" And AssocLeft ]
  , [ expBinOp "+" Add AssocLeft, expBinOp "-" Sub AssocLeft
    , expBinOp "|" Or AssocLeft  ]
  , [ expBinOp "==" Eq AssocNone, expBinOp "/=" Neq AssocNone
    , expBinOp "<" Lt AssocNone , expBinOp "<=" Lte AssocNone
    , expBinOp ">" Gt AssocNone , expBinOp ">=" Gte AssocNone
    ]
  ]

shiftl e (Lit w n) = Apply1 (Shl $ fromInteger n) e
shiftl e other = error "Right-hand-side of << operator must be a literal"

shiftr e (Lit w n) = Apply1 (Shr $ fromInteger n) e
shiftr e other = error "Right-hand-side of >> operator must be a literal"

expr :: ConstMap -> Parser Exp
expr env = buildExpressionParser opTable (expr' env)

expr' :: ConstMap -> Parser Exp
expr' env =
        pure (Lit Nothing) <*> natural
    <|> pure (Apply1 MSB) <*> (reserved "msb" *> parens (expr env))
    <|> pure (Apply1 Log) <*> (reserved "log" *> parens (expr env))
    <|> pure Cond <*> (reserved "cond" *> reservedOp "(" *> expr env)
                  <*> (reservedOp "," *> expr env)
                  <*> (reservedOp "," *> expr env <* reservedOp ")")
    <|> do {
          v <- dottedIdentifier ;
          case Map.lookup v env of
            Just i  -> return (Lit Nothing i)
            Nothing -> case split v of
                         [v]  -> return (Var v)
                         x:xs -> return (RecSel x xs)
        }
    <|> parens (expr env)

-- Statements

stmBinOp op f assoc = Infix (reservedOp op >> return f) assoc

stmtOpTable =
  [ [ stmBinOp "?" (\x y -> Choice x y []) AssocLeft ]
  , [ stmBinOp "||" (:||) AssocLeft ]
  , [ stmBinOp ";" (:>) AssocLeft ]
  ]

stmt :: ConstMap -> Parser Stm
stmt env = buildExpressionParser stmtOpTable (stmt' env)

stmt' :: ConstMap -> Parser Stm
stmt' env =
         pure Skip <* reserved "skip"
     <|> ifStmt env
     <|> pure While <*> (reserved "while" *> expr env <* reserved "do") <*>
           (stmt env <* reserved "end")
     <|> pure Fail <* reserved "fail"
--   <|> pure Halt <* reserved "halt"
     <|> parens (stmt env)
     <|> do x <- identifier
            m <- optionMaybe (brackets (expr env))
            reservedOp ":="
            case m of
              Nothing -> try (pure (ArrayLookup RW x) <*>
                           identifier <*> brackets (expr env))
                     <|> pure (x :=) <*> expr env
              Just e  -> do rhs <- expr env
                            return (ArrayAssign x e rhs)

ifStmt :: ConstMap -> Parser Stm
ifStmt env =
  do e  <- reserved "if" *> expr env
     s1 <- reserved "then" *> stmt env
     s2 <- (reserved "else" *> stmt env <* reserved "end")
       <|> (reserved "end" *> return Skip)
     return (Ifte e s1 s2)

-- Declarations

decl :: ConstMap -> Parser Decl
decl env =
   pure Decl <*> (reserved "var" *> identifier <* reserved ":")
             <*> typ env
             <*> initial env

initial :: ConstMap -> Parser Init
initial env =
  do m <- optionMaybe (reserved "=")
     case m of
       Nothing -> return Uninit
       Just _  ->
            pure IntInit <*> number env
        <|> pure StrInit <*> stringLiteral

bitType :: ConstMap -> Parser Int
bitType env = do
  reserved "bit"
  reservedOp "("
  n <- constExprInt env
  reservedOp ")"
  return n

typ :: ConstMap -> Parser Type
typ env = 
  do t1 <- baseType
     m <- optionMaybe $ do
            live <- (reservedOp "->" *> pure Live) <|>
                    (reservedOp "=>" *> pure Dead)
            t    <- baseType
            return (live, t)
     case m of 
       Nothing -> return t1
       Just (live, t2) -> return (TArray RW live t1 t2)
  where
    baseType =
      do o <- optionMaybe (bitType env)
         case o of
           Nothing -> pure TUser <*> identifier
           Just n -> return (TBit n)

log2 :: Integral a => a -> a
log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)
 
typeDecl :: ConstMap -> Parser TypeDecl
typeDecl env =
      do reserved "type"
         name <- identifier
         reservedOp "="
         t <- typ env
         return (TSynonym name t)
  <|> do reserved "enum"
         name <- identifier
         reservedOp "="
         ids <- sepBy1 identifier (reservedOp "|")
         return (TEnum name ids)
  <|> do (reserved "rec" <|> reserved "struct")
         name <- identifier
         reservedOp "="
         reservedOp "{"
         fields <- sepBy1 field comma
         reservedOp "}"
         return (TRec name fields)
  where
    field = do
      id <- identifier 
      reserved ":"
      t <- typ env
      return (id, t)

-- Parse a compiler option

compilerOpt :: ConstMap -> Parser CompilerOpts
compilerOpt env =
  do reserved "opt"
     key <- identifier
     reservedOp "="
     val <- number env
     return (Map.fromList [(key, val)])

-- Parse program prelude

prelude :: Parser (ConstMap, CompilerOpts, [Decl], [TypeDecl])
prelude =
  do env <- constDecls Map.empty
     items <- many (preludeItem env)
     let (opts, decls, typeDecls) = unzip3 items
     return (env, Map.unions opts, concat decls, concat typeDecls)

preludeItem :: ConstMap -> Parser (CompilerOpts, [Decl], [TypeDecl])
preludeItem env =
      do { o <- compilerOpt env ; return (o, [], []) }
  <|> do { d <- decl env ; return (Map.empty, [d], []) }
  <|> do { d <- typeDecl env ; return (Map.empty, [], [d]) }

-- Programs

prog :: Parser Prog
prog =
  do whitespace
     (env, opts, ds, tds) <- prelude
     s <- stmt env
     return (Prog opts tds ds s)
 
parseProgFile :: SourceName -> IO Prog
parseProgFile f = parseFromFile (prog <* eof) f >>= \result ->
  case result of
    Left e  -> error . show $ e
    Right p -> return p
