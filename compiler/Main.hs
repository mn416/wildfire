import WhileSyntax
import WhileParser
import WhileCompile
import WhileSem
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
       ["-i", sourceFile] -> do
         prog <- parseProgFile sourceFile
         run prog
       other -> putStrLn "Usage: wildfire [-i] SOURCEFILE"
