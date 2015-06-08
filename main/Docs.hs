
module Main (main) where

import OnPing.DataServer.Language.Docs

main :: IO ()
main = do
  writeFile "commands.md" commandDoc
