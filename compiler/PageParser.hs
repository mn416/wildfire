module PageParser (parseProgFile) where

import PageSyntax
import Control.Applicative
import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec hiding (many, option, (<|>))
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
  
page = T.makeTokenParser $ emptyDef
  { commentLine      = "--"
  , nestedComments   = False
  , identStart       = letter
  , identLetter      = alphaNum
  , opStart          = opLetter haskellStyle
  , opLetter         = oneOf "+-*/=<>;:|@.^~?"
  , reservedNames    = ["skip", "tick", "if", "then", "else", "halt",
                        "goto", "acquire", "release", "print", "while",
                        "inv", "declare", "in", "reg", "label", "fork",
                        "lock", "of", "fetch", "ram", "from", "to",
                        "data", "push", "pop", "top", "bits", "end",
                        "take", "return"
                       ]
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

-- Expressions

expBinOp op f assoc = Infix (reservedOp op >> return (Apply2 f)) assoc

expUnOp op f = Prefix (reservedOp op >> return (Apply1 f))
  
opTable =
  [ [ expUnOp "~" Inv ]
  , [ expBinOp "*" Mul AssocLeft, expBinOp "/" Div AssocLeft
    , expBinOp "&" And AssocLeft ]
  , [ expBinOp "+" Add AssocLeft, expBinOp "-" Sub AssocLeft
    , expBinOp "|" Or AssocLeft  ]
  , [ expBinOp "==" Eq AssocNone, expBinOp "/=" Neq AssocNone
    , expBinOp "<" Lt AssocNone , expBinOp "<=" Lte AssocNone
    , expBinOp ">" Gt AssocNone , expBinOp ">=" Gte AssocNone
    ]
  , [ Infix (reservedOp "." >> return Concat) AssocLeft ]
  ]

expr :: Parser Exp
expr = buildExpressionParser opTable expr'

expr' :: Parser Exp
expr' = pure (Lit Nothing) <*> natural
    <|> pure Var <*> var
    <|> pure Ptr <*> (reservedOp "^" *> var)
    <|> pure Lab <*> stmtLabel
    <|> pure RamOutput <*> (reserved "data" *> var)
    <|> pure Select <*> (reserved "bits" *> nat)
                    <*> (reserved "to" *> nat)
                    <*> (reserved "of" *> expr)
    <|> parens expr

var :: Parser Id
var =
  do v <- identifier
     if isLower (head v)
       then return v
       else unexpected (show v) <?> "variable"
 
stmtLabel :: Parser Id
stmtLabel = lexeme (return (:) `ap` char '#' `ap` identifier)

-- Statements

stmBinOp op f assoc = Infix (reservedOp op >> return f) assoc

stmtOpTable =
  [ [ stmBinOp "||" (\x y -> Par [x, y]) AssocLeft ]
  , [ stmBinOp ";" (:>) AssocLeft ]
  ]

stmt :: Parser Stm
stmt = buildExpressionParser stmtOpTable stmt'

stmt' :: Parser Stm
stmt'  = pure Skip <* reserved "skip"
     <|> pure Tick <* reserved "tick"
     <|> pure IndAssign <*> (reservedOp "^" *> var)
                        <*> (reservedOp ":=" *> expr)
     <|> assign
     <|> pure Ifte <*> (reserved "if" *> expr)
                   <*> (reserved "then" *> stmt')
                   <*> (reserved "else" *> stmt' <*
                           reserved "end" <* reserved "if")
     <|> pure While <*> (reserved "while" *> expr) <*>
           (stmt' <* reserved "end" <* reserved "while")
     <|> pure (\l s -> Label l :> s) <*> (stmtLabel <* reservedOp ":")
                                     <*> stmt
     <|> goto
     <|> pure ForkJump <*> (reserved "fork" *> stmtLabel)
--   <|> pure Acquire <*> (reserved "acquire" *> var) <*> return Nothing
     <|> pure Release <*> (reserved "release" *> var)
     <|> pure Print <*> (reserved "print" *> var)
     <|> pure Load <*> (reserved "fetch" *> var) <*> brackets expr
     <|> pure Push <*> (reserved "push" *> var) <*> var
     <|> pure Pop <*> (reserved "pop" *> var) <*> var
     <|> pure Halt <* reserved "halt"
     <|> parens stmt

goto =
  do reserved "goto"
     arg <- stmtLabel <|> var
     return (if head arg == '#' then Jump arg else IndJump arg)

assign =
  do v <- var
     i <- (pure Just <*> brackets expr) <|> pure Nothing
     reservedOp ":="
     e <- expr
     case i of
       Nothing -> return (v := e)
       Just x  -> return (Store v x e)

-- Declarations

declarations :: Parser [Decl]
declarations = decl `sepEndBy` comma

decl :: Parser Decl
decl = pure (,) <*> (var <* reserved ":") <*> typ

typ :: Parser Type
typ = pure TNat <*> (reserved "nat" *> nat)
  <|> pure (`TPtr` []) <*> (reservedOp "^" *> parens (reserved "nat" *> nat))
  <|> pure (TLab []) <* reserved "label"
  <|> pure TLock <* reserved "lock"
  <|> pure TRam <*> (reserved "ram" *> parens (reserved "nat" *> nat))
                <*> parens (reserved "nat" *> nat)

nat :: Parser Int
nat = pure fromIntegral <*> natural

log2 :: Integral a => a -> a
log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)
    
-- Programs

prog :: Parser Prog
prog = pure Prog <*> (reserved "declare" *> declarations)
                 <*> (reserved "in" *> stmt)
 
parseProgFile :: SourceName -> IO Prog
parseProgFile f = parseFromFile (prog <* eof) f >>= \result ->
  case result of
    Left e  -> error . show $ e
    Right p -> return p
