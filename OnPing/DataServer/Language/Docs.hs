
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module OnPing.DataServer.Language.Docs (
    SyntaxDoc (..)
  , allCommands
  , commandName
  , commandDoc
  ) where

import OnPing.DataServer.Language
-- TH
import qualified Language.Haskell.TH as TH
import OnPing.DataServer.Language.TH
-- base
import Data.Char (toLower)
import Data.List (intercalate, sortOn)
import Data.Typeable (Typeable, typeRep)
import Data.String (fromString)

class SyntaxDoc a where
  syntaxDoc :: a -> String

instance SyntaxDoc Ident where
  syntaxDoc _ = "<identifier>"

instance Typeable a => SyntaxDoc (Exp a) where
  syntaxDoc e = "[ <" ++ show (typeRep e) ++ " expression> ]"

$(fmap (pure . TH.InstanceD Nothing [] (TH.ConT "SyntaxDoc" `TH.AppT` TH.ConT "Command") . pure . TH.FunD "syntaxDoc")
  $ th_Type "Command" $ \c ts -> do
      vs <- mapM (const $ TH.newName "arg") ts
      let str = "!" ++ fmap toLower (TH.nameBase c)
          str' = str ++ " "
          f = TH.VarE "syntaxDoc"
          (+++) e1 e2 = TH.UInfixE e1 (TH.VarE "++") e2
      let e = case length vs of
                0 -> fromString str
                1 -> fromString str' +++ TH.AppE f (TH.VarE $ head vs)
                _ -> fromString str' +++ (TH.VarE "intercalate" `TH.AppE` " " `TH.AppE` TH.ListE (fmap (TH.AppE f . TH.VarE) vs))
      return $ TH.Clause [TH.ConP c $ fmap TH.VarP vs] (TH.NormalB e) []
 )

allCommands :: [Command]
allCommands = sortOn commandName $(fmap TH.ListE $ th_Type "Command" $ \c ts -> return $ foldl (\r _ -> TH.AppE r $ TH.VarE "undefined") (TH.ConE c) ts)

-- | Explanation of commands using markdown.
commandExplanation :: Command -> String
commandExplanation (Print _) = "Display a value on the screen."
commandExplanation Exit = "Interrupt the execution of the script by sending an `Exit` error."
commandExplanation (Assert _) = unlines
  [ "First, it evaluates the given boolean expression. If the result"
  , "is `true`, no further action is performed. However, if the result"
  , "is `false`, `assert` will throw an `AssertionFailed` error. This"
  , "is useful if you want to stop the execution if certain condition"
  , "is not fulfilled."
    ]
-- Default message
commandExplanation _ = "_This command has not been documented yet._"

commandName :: Command -> String
commandName cm = $(do
  fmap (TH.CaseE $ TH.VarE "cm") $ th_Type "Command" $ \c ts -> do
    let str = fmap toLower $ TH.nameBase c
    return $ TH.Match (TH.ConP c $ fmap (const $ TH.WildP) ts) (TH.NormalB $ fromString str) []
 )

-- | Command documentation.
commandDoc :: String
commandDoc = unlines
  [ "# Commands"
  , ""
  , unlines $ fmap
     (\c -> let n = commandName c
            in  "* [" ++ n ++ "](#" ++ n ++ ")"
       ) allCommands
  , ""
  , unlines $ fmap (\c -> unlines
     [ "## " ++ commandName c
     , ""
     , "```"
     , syntaxDoc c
     , "```"
     , ""
     , commandExplanation c
       ]) allCommands
    ]
