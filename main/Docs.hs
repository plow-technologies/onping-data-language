
module Main (main) where

import OnPing.DataServer.Language.Docs
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  createDirectoryIfMissing True "docs"
  writeFile "docs/commands.md" commandDoc
