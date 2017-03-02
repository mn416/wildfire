import PageSyntax
import PageParser
import PageCompile
import Netlist
import System.IO
import System.Environment

main :: IO ()
main =
  do args <- getArgs
     case args of
       [sourceFile] -> do
         prog <- parseProgFile sourceFile
         writeVerilog "Out" (compile prog)
         putStrLn "Written Out.v"
       other -> putStrLn "Usage: page [PROG].p"
