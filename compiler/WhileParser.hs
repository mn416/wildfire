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
  
page = T.makeTokenParser $ emptyDef
  { commentLine      = "--"
  , nestedComments   = False
  , identStart       = letter
  , identLetter      = alphaNum
  , opStart          = opLetter haskellStyle
  , opLetter         = oneOf "+-*/=<>;:|@.^~?"
  , reservedNames    = ["skip", "if", "then", "else", "end",
                        "while", "declare", "in", "fail", "halt",
                        "opt", "var"]
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

-- Expressions

expBinOp op f assoc = Infix (reservedOp op >> return (Apply2 f)) assoc

expUnOp op f = Prefix (reservedOp op >> return (Apply1 f))
  
expBinOp' op f assoc = Infix (reservedOp op >> return f) assoc

opTable =
  [ [ expUnOp "~" Inv ]
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

expr :: Parser Exp
expr = buildExpressionParser opTable expr'

expr' :: Parser Exp
expr' = pure (Lit Nothing) <*> natural
    <|> pure Var <*> identifier
    <|> parens expr

-- Statements

stmBinOp op f assoc = Infix (reservedOp op >> return f) assoc

stmtOpTable =
  [ [ stmBinOp "?" (\x y -> Choice x y []) AssocLeft ]
  , [ stmBinOp "||" (:||) AssocLeft ]
  , [ stmBinOp ";" (:>) AssocLeft ]
  ]

stmt :: Parser Stm
stmt = buildExpressionParser stmtOpTable stmt'

stmt' :: Parser Stm
stmt'  = pure Skip <* reserved "skip"
     <|> pure (:=) <*> (identifier <* reservedOp ":=") <*> expr
     <|> ifStmt
     <|> pure While <*> (reserved "while" *> expr <* reserved "do") <*>
           (stmt <* reserved "end")
     <|> pure Fail <* reserved "fail"
     <|> pure Halt <* reserved "halt"
     <|> parens stmt

ifStmt :: Parser Stm
ifStmt =
  do e  <- reserved "if" *> expr
     s1 <- reserved "then" *> stmt
     s2 <- (reserved "else" *> stmt <* reserved "end")
       <|> (reserved "end" *> return Skip)
     return (Ifte e s1 s2)

-- Declarations

decl :: Parser Decl
decl = pure Decl <*> (reserved "var" *> identifier <* reserved ":")
                 <*> typ
                 <*> initial

initial :: Parser Init
initial =
  do m <- optionMaybe (reserved "=" *> natural)
     case m of
       Nothing -> return Uninit
       Just i  -> return (IntInit i)

typ :: Parser Type
typ = pure TNat <*> nat

nat :: Parser Int
nat = pure fromIntegral <*> natural

log2 :: Integral a => a -> a
log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)
 
-- Parse a compiler option

compilerOpt :: Parser CompilerOpts
compilerOpt =
  do reserved "opt"
     key <- identifier
     reserved "="
     val <- natural
     return (Map.fromList [(key, val)])

-- Parse program prelude

prelude :: Parser (CompilerOpts, [Decl])
prelude =
  do items <- many preludeItem
     let (opts, decls) = unzip items
     return (Map.unions opts, concat decls)

preludeItem :: Parser (CompilerOpts, [Decl])
preludeItem =
      do { o <- compilerOpt ; return (o, []) }
  <|> do { d <- decl ; return (Map.empty, [d]) }

-- Programs

prog :: Parser Prog
prog =
  do whitespace
     (opts, ds) <- prelude
     reserved "begin"
     s <- stmt
     reserved "end"
     return (Prog opts ds s)
 
parseProgFile :: SourceName -> IO Prog
parseProgFile f = parseFromFile (prog <* eof) f >>= \result ->
  case result of
    Left e  -> error . show $ e
    Right p -> return p
