module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenRuby

import System.Environment
import System.Exit

import Paths_idris_ruby

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath }

showUsage = do putStrLn "Usage: idris-ruby <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.php") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

c_main :: Opts -> Idris ()
c_main opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- elabMain
                 ir <- compile (Via "ruby") (output opts) (Just mainProg)
                 runIO $ codegenRuby ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts)) 
             then showUsage
             else runMain (c_main opts)


