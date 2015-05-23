
{-# LANGUAGE OverloadedStrings #-}

module OnPing.DataServer.Language (
    -- * Values
    Value (..)
  , ToValue (..), FromValue (..)
  , Error (..)
    -- * Expressions
  , Ident
  , Exp (..)
    -- * Evaluation
  , Context (..)
  , Eval
  , runEval
  , evalExp
    -- * Actions
  , Action (..)
  , doAction
  , LabeledAction (..)
  , doLAction
    -- * Commands
  , Command (..)
    -- * Script
  , Script
  , runScript
    -- * Prelude
  , prelude
  ) where

import Data.Text (Text, unpack, pack)
import OnPing.DataServer.Client
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as M
import Numeric.Natural
import Numeric (showFFloat)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Control.Monad (forM_, unless)
import Control.Exception (try, displayException, SomeException)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Errors

data Error =
    RawError String
  | Undefined Ident
  | TypeError
  | NonFunctionApp
  | ClientError String
  | Void
  | ExitCall
  | AssertionFailed

displayError :: Error -> String
displayError (RawError err) = err
displayError (Undefined v) = "Undefined variable `" ++ v ++ "`"
displayError TypeError = "Type error"
displayError NonFunctionApp = "Non-function application"
displayError (ClientError err) = "Client error: " ++ err
displayError Void = "Void"
displayError ExitCall = "Exit call"
displayError AssertionFailed = "Assertion failed"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Values

data Value =
    VError Error
  | VBool Bool
  | VInt Int
  | VDouble Double
  | VText Text
  | VKey Key
  | VArray [Value]
  | VFun (Value -> Value)

displayValue :: Value -> String
displayValue (VError err) = displayError err
displayValue (VBool b) = show b
displayValue (VInt n) = show n
displayValue (VDouble x) = show $ showFFloat (Just 5) x []
displayValue (VText t) = unpack t
displayValue (VKey k) = show k
displayValue (VArray xs) = concat [ "[" , intercalate "," $ fmap displayValue xs , "]" ]
displayValue (VFun _) = "<function>"

applyValue :: Value -> Value -> Value
applyValue (VFun f) v = f v
applyValue _ _ = VError NonFunctionApp

insertArray :: a -- ^ Default element
            -> Int -- ^ Index to insert to (>= 0)
            -> a -- ^ Element to insert
            -> [a] -> [a]
insertArray e i0 x = go i0
  where
    go i [] = replicate i e ++ [x]
    go i (y:ys) = if i == 0 then x : ys else y : go (i-1) ys

class ToValue t where
  toValue :: t -> Value

class FromValue t where
  fromValue :: Value -> Maybe t

instance ToValue Value where
  toValue = id

instance FromValue Value where
  fromValue = Just

instance ToValue Error where
  toValue = VError

instance FromValue Error where
  fromValue (VError err) = Just err
  fromValue _ = Nothing

instance ToValue Bool where
  toValue = VBool

instance FromValue Bool where
  fromValue (VBool b) = Just b
  fromValue _ = Nothing

instance ToValue Int where
  toValue = VInt

instance FromValue Int where
  fromValue (VInt n) = Just n
  fromValue _ = Nothing

instance ToValue Double where
  toValue = VDouble

instance FromValue Double where
  fromValue (VDouble x) = Just x
  fromValue _ = Nothing

instance ToValue Text where
  toValue = VText

instance FromValue Text where
  fromValue (VText t) = Just t
  fromValue _ = Nothing

instance ToValue Key where
  toValue = VKey

instance FromValue Key where
  fromValue (VKey k) = Just k
  fromValue _ = Nothing

instance ToValue a => ToValue [a] where
  toValue = VArray . fmap toValue

instance FromValue a => FromValue [a] where
  fromValue (VArray vs) = mapM fromValue vs
  fromValue _ = Nothing

instance (FromValue a, ToValue b) => ToValue (a -> b) where
  toValue f = VFun $ \v ->
    case fromValue v of
      Just x -> toValue $ f x
      _ -> VError TypeError

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Expressions

type Ident = String

data Exp =
    EVar Ident
  | EInt Int
  | EDouble Double
  | EText Text
  | EApp Exp Exp
    deriving Show

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Evaluation

data Context = Context
  { namespace :: Map Ident Value
  , actionNumber :: Natural
  , currentLine :: Int
    }

type Eval = ExceptT Error (StateT Context IO)

runEval :: Eval a -> IO ()
runEval e = do
  (r,ct) <- runStateT (runExceptT e) $ Context
    { namespace    = M.empty
    , actionNumber = 0
    , currentLine  = 0
      }
  case r of
    Left err -> putStrLn $ "ERROR: Line " ++ show (currentLine ct) ++ ": " ++ displayError err
    _ -> putStrLn $ "Finished. Actions executed: " ++ show (actionNumber ct)

castValue :: (ToValue a, FromValue b) => a -> Eval b
castValue x =
  let v = toValue x
  in  case fromValue v of
        Just y -> return y
        _ ->
          case v of
            VError err -> throwE err
            _ -> throwE TypeError

valueOf :: FromValue a => Ident -> Eval a
valueOf v = do
  ct <- lift get
  case M.lookup v $ namespace ct of
    Just x -> castValue x
    _ -> throwE $ Undefined v

evalExp :: FromValue a => Exp -> Eval a
evalExp (EVar v) = valueOf v
evalExp (EInt n) = castValue n
evalExp (EDouble x) = castValue x
evalExp (EText t) = castValue t
evalExp (EApp e1 e2) = (applyValue <$> evalExp e1 <*> evalExp e2) >>= castValue

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Actions

data Action =
    Assign Ident Exp
  | While Exp Script
  | ForEach Ident Ident Script
  | Exec Command
  | Isolate Script
    deriving Show

assign :: ToValue a => Ident -> a -> Eval ()
assign v x = lift $ modify $ \ct -> ct { namespace = M.insert v (toValue x) $ namespace ct }

increaseActionNumber :: Eval ()
increaseActionNumber = lift $ modify $ \ct -> ct { actionNumber = actionNumber ct + 1 }

doAction :: Action -> Eval ()
doAction (Assign v e) = (evalExp e :: Eval Value) >>= assign v >> increaseActionNumber
doAction (While e scr) = do
  b <- evalExp e
  if b then runScript scr >> doAction (While e scr)
       else return ()
doAction (ForEach arr v scr) = do
  xs <- valueOf arr :: Eval [Value]
  forM_ xs $ \x -> do
    assign v x
    runScript scr
doAction (Exec comm) = execCommand comm >> increaseActionNumber
doAction (Isolate scr) = do
  ct <- lift get
  liftIO $ do
    let io = runStateT (runExceptT $ runScript scr) ct
    er <- try io
    case er of
      Left e -> putStrLn $ "EXCEPTION (ISOLATED): " ++ displayException (e :: SomeException)
      Right (r,isoct) ->
        case r of
          Left err -> putStrLn $ "ERROR (ISOLATED): Line " ++ show (currentLine isoct) ++ ": " ++ displayError err
          _ -> return ()

data LabeledAction = LAction
  { actionLine :: Int
  , theAction :: Action
    } deriving Show

doLAction :: LabeledAction -> Eval ()
doLAction lact = do
  lift $ modify $ \ct -> ct { currentLine = actionLine lact }
  doAction $ theAction lact

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Commands

data Command =
    Print Exp
  | Exit
  | Assert Exp
  | SetArray Ident Exp Exp
  | GetAllKeys Ident
    deriving Show

execCommand :: Command -> Eval ()
execCommand (Print e) = evalExp e >>= liftIO . putStrLn . displayValue
execCommand Exit = throwE ExitCall
execCommand (Assert e) = do
  b <- evalExp e
  unless b $ throwE AssertionFailed
execCommand (SetArray arr ei ex) = do
  xs <- valueOf arr
  i <- evalExp ei
  x <- evalExp ex
  assign arr $ insertArray (VError Void) i x xs
execCommand (GetAllKeys v) = clientActionE clientAllKeys >>= assign v

clientAction :: Client IO a -> Eval a
clientAction c = do
  server <- valueOf "server"
  port <- valueOf "port"
  liftIO $ runClient (AwayServer OnPingData (unpack server) port) c

clientActionM :: Client IO (Maybe String) -> Eval ()
clientActionM c = do
  merr <- clientAction c
  case merr of
    Just err -> throwE $ ClientError err
    _ -> return ()

clientActionE :: Client IO (Either String a) -> Eval a
clientActionE c = do
  ex <- clientAction c
  case ex of
    Left err -> throwE $ ClientError err
    Right x -> return x

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Script

type Script = [LabeledAction]

runScript :: Script -> Eval ()
runScript = mapM_ doLAction

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Prelude

prelude :: Eval ()
prelude = do
  -- Numerical operations
  ---- Int
  assign "+" ((+) :: Int -> Int -> Int)
  assign "-" ((-) :: Int -> Int -> Int)
  assign "*" ((*) :: Int -> Int -> Int)
  assign "/" (div :: Int -> Int -> Int)
  assign "=" ((==) :: Int -> Int -> Bool)
  assign "/=" ((/=) :: Int -> Int -> Bool)
  assign "<=" ((<=) :: Int -> Int -> Bool)
  assign ">=" ((>=) :: Int -> Int -> Bool)
  assign ">" ((>) :: Int -> Int -> Bool)
  assign "<" ((<) :: Int -> Int -> Bool)
  ---- Double
  assign "+." ((+) :: Double -> Double -> Double)
  assign "-." ((-) :: Double -> Double -> Double)
  assign "*." ((*) :: Double -> Double -> Double)
  assign "/." ((/) :: Double -> Double -> Double)
  assign "<=." ((<=) :: Double -> Double -> Bool)
  assign ">=." ((>=) :: Double -> Double -> Bool)
  assign ">." ((>) :: Double -> Double -> Bool)
  assign "<." ((<) :: Double -> Double -> Bool)
  -- Arrays
  assign "length" (length :: [Value] -> Int)
  assign "!" ((!!) :: [Value] -> Int -> Value)
  -- Booleans
  assign "true" True
  assign "false" False
  assign "not" not
  assign "|" (||)
  assign "&" (&&)
  -- Text
  assign "++" ((<>) :: Text -> Text -> Text)
  assign "display" (pack . displayValue :: Value -> Text)
  -- Keys
  assign "Key" Key
  -- Server default info
  assign "server" ("127.0.0.1" :: Text)
  assign "port" (5000 :: Int)
