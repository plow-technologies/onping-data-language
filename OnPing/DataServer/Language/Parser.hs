
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module OnPing.DataServer.Language.Parser (
    parseScript
  , parseScriptFile
  , cleanComments
  ) where

import OnPing.DataServer.Language
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Parsec.Expr
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (void)
-- TH
import qualified Language.Haskell.TH as TH
import OnPing.DataServer.Language.TH
import Data.Char (toLower)

p_Ident :: Parser Ident
p_Ident = fmap Ident $ (:) <$> letter <*> many alphaNum

p_Var :: Parser Exp
p_Var = EVar <$> p_Ident

p_Int :: Parser Exp
p_Int = EInt . read <$> many1 digit

p_Double :: Parser Exp
p_Double = do
  l <- option "0" $ many digit
  char '.'
  r <- many1 digit
  return $ EDouble $ read $ l ++ "." ++ r

p_Text :: Parser Exp
p_Text = do
  char '\"'
  str <- manyTill anyChar $ char '\"'
  return $ EText $ pack str

p_Arr :: Parser Exp
p_Arr = do
  i <- p_Ident
  optional forcedSpace
  char '['
  skipMany $ char ' '
  e <- p_Exp
  skipMany $ char ' '
  char ']'
  return $ EArr i e

p_SimpleExp :: Parser Exp
p_SimpleExp = choice
  [ try p_Arr <?> "array element"
  , p_Var <?> "variable"
  , try p_Double <?> "double"
  , p_Int <?> "int"
  , p_Text <?> "text"
  , (do char '('
        e <- p_Exp
        char ')'
        return e) <?> "parenthesized expression"
    ]

spaced :: Parser a -> Parser a
spaced p = do
  try $ many $ char ' '
  x <- p
  many $ char ' '
  return x

forcedSpace :: Parser ()
forcedSpace = void $ many1 $ char ' '

p_Term :: Parser Exp
p_Term = fmap (foldl1 EApp) $ many1 $ spaced p_SimpleExp

p_Op :: String -> Parser (Exp -> Exp -> Exp)
p_Op n = try $ do
  spaced $ string n
  lookAhead $ noneOf "!*./+-=<>&|"
  return $ EApp . EApp (EVar $ Ident n)

p_Exp :: Parser Exp
p_Exp = buildExpressionParser op_table p_Term 
  where
    op_table =
      [ [ Infix (p_Op "!" ) AssocLeft ]
      , [ Infix (p_Op "*" ) AssocLeft , Infix (p_Op "/" ) AssocLeft ]
      , [ Infix (p_Op "+" ) AssocLeft , Infix (p_Op "-" ) AssocLeft ]
      , [ Infix (p_Op "++") AssocRight ]
      , [ Infix (p_Op "=" ) AssocNone , Infix (p_Op "/=") AssocNone
        , Infix (p_Op "<=") AssocNone , Infix (p_Op ">=") AssocNone
        , Infix (p_Op ">" ) AssocNone , Infix (p_Op "<" ) AssocNone
          ]
      , [ Infix (p_Op "&" ) AssocRight ]
      , [ Infix (p_Op "|" ) AssocRight ]
        ]

p_Action :: Parser Action
p_Action = choice
  [ (do v <- p_Ident
        spaced $ char '='
        e <- p_Exp
        return $ Assignment v e) <?> "assignment"
  , try $ do char ':'
             ActControl <$> p_Control
  , (do char '!'
        ActCommand <$> p_Command) <?> "command"
    ]

class Argument a where
  p_Arg :: Parser a

instance Argument Exp where
  p_Arg = char '[' *> spaced p_Exp <* char ']'

instance Argument Ident where
  p_Arg = p_Ident

p_Control :: Parser Control
p_Control = choice $(do
  info <- TH.reify $ TH.mkName "Control"
  case info of
    TH.TyConI typeDec ->
      case typeDec of
        TH.DataD _ _ _ cs _ -> do
          let f (TH.NormalC c ts0) = do
                  let ts = init ts0
                  ps <- mapM (\_ -> [|forcedSpace *> p_Arg|]) ts
                  let n = fmap toLower $ TH.nameBase c
                  [| do try (string n)
                        r <- $(return $ th_App (TH.ConE c) ps)
                        skipMany $ char ' '
                        newline
                        spaces
                        scr <- p_Script
                        string ":end"
                        return (r scr)
                   |]
              f _ = fail "Unexpected constructor."
          TH.ListE <$> mapM f cs
        _ -> fail "Type Control found, but not its declaration."
    _ -> fail "Type Control not found."
  )

p_Command :: Parser Command
p_Command = choice $(do
  info <- TH.reify $ TH.mkName "Command"
  case info of
    TH.TyConI typeDec ->
      case typeDec of
        TH.DataD _ _ _ cs _ -> do
          let f (TH.NormalC c ts) = do
                  ps <- mapM (\_ -> [|forcedSpace *> p_Arg|]) ts
                  let n = fmap toLower $ TH.nameBase c
                  [| do try (string n) ; $(return $ th_App (TH.ConE c) ps) |]
              f _ = fail "Unexpected constructor."
          TH.ListE <$> mapM f cs
        _ -> fail "Type Command found, but not its declaration."
    _ -> fail "Type Command not found."
  )

p_LAction :: Parser LabeledAction
p_LAction = do
  l <- sourceLine . statePos <$> getParserState
  act <- p_Action
  return $ LAction
    { actionLine = l
    , theAction = act
      }

p_Script :: Parser Script
p_Script = many $ do
  lact <- p_LAction
  skipMany $ char ' '
  void (char '\n') <|> eof
  spaces
  return lact

p_FullScript :: Parser Script
p_FullScript = do
  spaces
  scr <- p_Script
  spaces
  eof
  return scr

parseScript :: Text -> Either ParseError Script
parseScript = parse p_FullScript "input" . cleanComments

parseScriptFile :: FilePath -> IO (Either ParseError Script)
parseScriptFile fp = parse p_FullScript fp . cleanComments <$> TIO.readFile fp

commentChar :: Char
commentChar = '#'

cleanComments :: Text -> Text
cleanComments = T.unlines . fmap (T.takeWhile (/=commentChar)) . T.lines
