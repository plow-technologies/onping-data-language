
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

p_Ident :: Parser Ident
p_Ident = (:) <$> letter <*> many alphaNum

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

p_SimpleExp :: Parser Exp
p_SimpleExp = choice
  [ p_Var <?> "variable"
  , try p_Double <?> "double"
  , p_Int <?> "int"
  , p_Text <?> "text"
  , (do char '('
        e <- p_Exp
        char ')'
        return e) <?> "parenthesized expression"
    ]

p_Term :: Parser Exp
p_Term = fmap (foldl1 EApp) $ many1 $ do
  try $ many $ char ' '
  se <- p_SimpleExp
  many $ char ' '
  return se

p_Op :: Ident -> Parser (Exp -> Exp -> Exp)
p_Op n = try $ do
  many $ char ' '
  string n
  lookAhead $ noneOf "!*./+-=<>&|"
  many $ char ' '
  return $ EApp . EApp (EVar n)

p_Exp :: Parser Exp
p_Exp = buildExpressionParser op_table p_Term 
  where
    op_table =
      [ [ Infix (p_Op "!" ) AssocLeft ]
      , [ Infix (p_Op "*" ) AssocLeft , Infix (p_Op "/" ) AssocLeft
        , Infix (p_Op "*.") AssocLeft , Infix (p_Op "/.") AssocLeft
          ]
      , [ Infix (p_Op "+" ) AssocLeft , Infix (p_Op "-" ) AssocLeft
        , Infix (p_Op "+.") AssocLeft , Infix (p_Op "-.") AssocLeft
          ]
      , [ Infix (p_Op "++") AssocRight ]
      , [ Infix (p_Op "=" ) AssocNone , Infix (p_Op "/=" ) AssocNone
        , Infix (p_Op "<=") AssocNone , Infix (p_Op "<=.") AssocNone
        , Infix (p_Op ">=") AssocNone , Infix (p_Op ">=.") AssocNone
        , Infix (p_Op ">" ) AssocNone , Infix (p_Op ">." ) AssocNone
        , Infix (p_Op "<" ) AssocNone , Infix (p_Op "<." ) AssocNone
          ]
      , [ Infix (p_Op "&" ) AssocRight ]
      , [ Infix (p_Op "|" ) AssocRight ]
        ]

p_Action :: Parser Action
p_Action = choice
  [ (do v <- p_Ident
        skipMany $ char ' '
        char '='
        skipMany $ char ' '
        e <- p_Exp
        return $ Assign v e) <?> "assignment"
  , try $ do char ':'
             p_Control
  , (do char '!'
        Exec <$> p_Command) <?> "command"
    ]

p_Control :: Parser Action
p_Control = choice
  [ do string "while"
       many1 $ char ' '
       e <- p_Exp
       skipMany $ char ' '
       newline
       spaces
       scr <- p_Script
       string ":end"
       return $ While e scr
  , do string "foreach"
       many1 $ char ' '
       arr <- p_Ident
       many1 $ char ' '
       v <- p_Ident
       skipMany $ char ' '
       newline
       spaces
       scr <- p_Script
       string ":end"
       return $ ForEach arr v scr
  , do string "isolate"
       manyTill (char ' ') newline
       spaces
       scr <- p_Script
       string ":end"
       return $ Isolate scr
    ]

p_Command :: Parser Command
p_Command = choice
  [ do string "print"
       many1 $ char ' '
       e <- p_Exp
       return $ Print e
  , do string "exit"
       return Exit
  , do string "assert"
       many1 $ char ' '
       e <- p_Exp
       return $ Assert e
  , do string "getallkeys"
       many1 $ char ' '
       v <- p_Ident
       return $ GetAllKeys v
    ]

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
