
module Main (main) where

import System.Environment (getArgs)
import OnPing.DataServer.Language (prelude, runScript, runEval)
import OnPing.DataServer.Language.Parser (parseScriptFile)

main :: IO ()
main = do
  xs <- getArgs
  mapM_ runFile xs

runFile :: FilePath -> IO ()
runFile fp = do
  putStrLn $ "Running file: " ++ fp
  escr <- parseScriptFile fp
  case escr of
    Left err -> print err
    Right scr -> runEval $ prelude >> runScript scr
