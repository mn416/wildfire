module MIFReader (
  MIF(..),
  readMIF   -- :: IO MIF
) where

import qualified Data.Map as Map

data MIF =
  MIF {
    mifHeader   :: Map.Map String String
  , mifContents :: Map.Map Integer Integer
  } deriving Show


parseMIF :: String -> MIF
parseMIF s = parse Map.empty Map.empty (tokens s)
  where
    tokens s =
      case lex s of
        [(str, rest)] -> str : tokens rest
        _ -> []

    parse h c ("CONTENT" : "BEGIN" : rest) = parseContents h c rest
    parse h c (lhs : "=" : rhs : ";" : rest) = 
      parse (Map.insert lhs rhs h) c rest
    parse h c _ = error "Parse error reading MIF file"

    parseContents h c (lhs : ":" : rhs : ";" : rest) =
      parseContents h (Map.insert (read lhs) (read rhs) c) rest
    parseContents h c ("END" : rest) = MIF h c
    parseContents h c _ = error "Parse error reading MIF file"
          
readMIF :: String -> IO MIF
readMIF filename = parseMIF `fmap` readFile filename
