import WhileSyntax
import WhileParser
import WhileCompile
import PagePretty
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
         let prog' = WhileCompile.compile prog
         writeFile "Solver.page" (prettyPageProg prog')
         putStrLn "Written Solver.page"
         writeVerilog "Solver" (PageCompile.compile prog')
         putStrLn "Written Solver.v"
       other -> putStrLn "Usage: wildfire [PROG].w"
