
{-# LANGUAGE OverloadedStrings #-}

module OnPing.DataServer.Language (
    -- * Values
    Value (..)
  , ToValue (..), FromValue (..)
  , Error (..)
    -- * Expressions
  , Ident (..)
  , Exp (..)
    -- * Evaluation
  , Context (..)
  , Eval
  , runEval
  , evalExp
    -- * Actions
  , Action (..)
  , LabeledAction (..)
   -- * Control
  , Control (..)
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
import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Exception (try, displayException, SomeException)
import Data.Bifunctor (bimap)
import Data.String (IsString (..))
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector as FV
import System.Microtimer (time, formatSeconds)

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
  | OutOfBounds Int Int

displayError :: Error -> String
displayError (RawError err) = err
displayError (Undefined (Ident v)) = "Undefined variable `" ++ v ++ "`"
displayError TypeError = "Type error"
displayError NonFunctionApp = "Non-function application"
displayError (ClientError err) = "Client error: " ++ err
displayError Void = "Void"
displayError ExitCall = "Exit call"
displayError AssertionFailed = "Assertion failed"
displayError (OutOfBounds i n) = "Out of bounds: " ++ show i ++ " in " ++ show (0 :: Int, n)

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
  | VFun (Value -> Value)

displayValue :: Value -> String
displayValue (VError err) = displayError err
displayValue (VBool b) = show b
displayValue (VInt n) = show n
displayValue (VDouble x) = show $ showFFloat (Just 5) x []
displayValue (VText t) = unpack t
displayValue (VKey k) = show k
displayValue (VFun _) = "<function>"

applyValue :: Value -> Value -> Value
applyValue (VFun f) v = f v
applyValue _ _ = VError NonFunctionApp

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

instance (FromValue a, ToValue b) => ToValue (a -> b) where
  toValue f = VFun $ \v ->
    case fromValue v of
      Just x -> toValue $ f x
      _ -> VError TypeError

instance (ToValue a, ToValue b) => ToValue (Either a b) where
  toValue (Left a) = toValue a
  toValue (Right b) = toValue b

instance (FromValue a, FromValue b) => FromValue (Either a b) where
  fromValue v = (Left <$> fromValue v) <|> (Right <$> fromValue v)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Expressions

newtype Ident = Ident String deriving (Eq, Ord, Show)

instance IsString Ident where
  fromString = Ident

data Exp a =
    EVar Ident
  | EArr Ident (Exp Int)
  | EInt Int
  | EDouble Double
  | EText Text
  | EApp (Exp Value) (Exp Value)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Evaluation

type Array = IOVector Value

data Context = Context
  { namespace :: Map Ident Value
  , arrays :: Map Ident Array
  , actionNumber :: !Natural
  , currentLine :: !Int
    }

type Eval = ExceptT Error (StateT Context IO)

runEval :: Eval a -> IO ()
runEval e = do
  (t,(r,ct)) <- time $ runStateT (runExceptT e) $ Context
    { namespace    = M.empty
    , arrays       = M.empty
    , actionNumber = 0
    , currentLine  = 0
      }
  case r of
    Left err -> putStrLn $ "ERROR: Line " ++ show (currentLine ct) ++ ": " ++ displayError err
    _ -> putStrLn $ "Finished. Actions executed: " ++ show (actionNumber ct)
                 ++ ". Time spent: " ++ formatSeconds t ++ "."

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

evalExp :: FromValue a => Exp a -> Eval a
evalExp (EVar v) = valueOf v
evalExp (EArr arr e) = do
  v <- arrayIn arr
  i <- evalExp e
  let n = V.length v
  unless (0 <= i && i < n) $ throwE $ OutOfBounds i n
  liftIO (V.unsafeRead v i) >>= castValue
evalExp (EInt n) = castValue n
evalExp (EDouble x) = castValue x
evalExp (EText t) = castValue t
evalExp (EApp e1 e2) = (applyValue <$> evalExp e1 <*> evalExp e2) >>= castValue

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Arrays

arrayIn :: Ident -> Eval Array
arrayIn v = do
  vs <- arrays <$> lift get
  case M.lookup v vs of
    Just arr -> return arr
    _ -> throwE $ Undefined v

listArray :: ToValue a => Ident -> [a] -> Eval ()
listArray v xs = do
  arr <- liftIO $ FV.unsafeThaw $ FV.fromList $ fmap toValue xs
  lift $ modify $ \ct -> ct { arrays = M.insert v arr $ arrays ct }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Actions

data Action =
    Assignment Ident (Exp Value)
  | ActCommand Command
  | ActControl Control

assign :: ToValue a => Ident -> a -> Eval ()
assign v x = lift $ modify $ \ct -> ct { namespace = M.insert v (toValue x) $ namespace ct }

increaseActionNumber :: Eval ()
increaseActionNumber = lift $ modify' $ \ct -> ct { actionNumber = actionNumber ct + 1 }

runAction :: Action -> Eval ()
runAction (Assignment v e) = (evalExp e :: Eval Value) >>= assign v >> increaseActionNumber
runAction (ActCommand comm) = runCommand comm >> increaseActionNumber
runAction (ActControl ctrl) = runControl ctrl

data LabeledAction = LAction
  { actionLine :: Int
  , theAction :: Action
    }

runLAction :: LabeledAction -> Eval ()
runLAction lact = do
  lift $ modify' $ \ct -> ct { currentLine = actionLine lact }
  runAction $ theAction lact

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Control

data Control =
    While (Exp Bool) Script
  | ForEach Ident Ident Script
  | Isolate Script

runControl :: Control -> Eval ()
runControl (While e scr) = do
  b <- evalExp e
  if b then runScript scr >> runControl (While e scr)
       else return ()
runControl (ForEach arrv v scr) = do
  arr <- arrayIn arrv
  let n = V.length arr
      go i = when (i < n) $ do
               x <- liftIO $ V.unsafeRead arr i
               assign v x
               runScript scr
               go $ i + 1
  go 0
runControl (Isolate scr) = do
  ct <- lift get
  arraysCopy <- mapM V.clone $ arrays ct
  let io = runStateT (runExceptT $ runScript scr) $ ct { arrays = arraysCopy }
  er <- liftIO $ try io
  case er of
    Left e -> liftIO $ putStrLn $ "EXCEPTION (ISOLATED): " ++ displayException (e :: SomeException)
    Right (r,isoct) -> do
      case r of
        Left err -> liftIO $ putStrLn $ "ERROR (ISOLATED): Line " ++ show (currentLine isoct) ++ ": " ++ displayError err
        _ -> return ()
      lift $ put $ ct { actionNumber = actionNumber isoct }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Commands

data Command =
    Print (Exp Value)
  | Exit
  | Assert (Exp Bool)
  | NewArray Ident (Exp Int)
  | SetArray Ident (Exp Int) (Exp Value)
  | GetAllKeys Ident
  | RemoveKey (Exp Key)
  | GetTimeBounds Ident Ident
  | SetTimeBounds (Exp Int) (Exp Int)
  | Sync
  | MemoryUsage Ident
  | Truncate (Exp Key) (Exp Int)

runCommand :: Command -> Eval ()
runCommand (Print e) = evalExp e >>= liftIO . putStrLn . displayValue
runCommand Exit = throwE ExitCall
runCommand (Assert e) = do
  b <- evalExp e
  unless b $ throwE AssertionFailed
runCommand (NewArray v en) = do
  n <- evalExp en
  arr <- liftIO $ V.replicate n $ VError Void
  lift $ modify $ \ct -> ct { arrays = M.insert v arr $ arrays ct }
runCommand (SetArray v ei ex) = do
  arr <- arrayIn v
  i <- evalExp ei
  let n = V.length arr
  unless (0 <= i && i < n) $ throwE $ OutOfBounds i n
  x <- evalExp ex
  liftIO $ V.unsafeWrite arr i x
runCommand (GetAllKeys arr) = clientActionE clientAllKeys >>= listArray arr
runCommand (RemoveKey k) = evalExp k >>= clientActionM . clientRemove
runCommand (GetTimeBounds lbv ubv) = do
  (lb,ub) <- clientActionE clientGetTimeBounds
  assign lbv lb
  assign ubv ub
runCommand (SetTimeBounds lbe ube) = do
  lb <- evalExp lbe
  ub <- evalExp ube
  clientActionM $ clientSetTimeBounds (lb,ub)
runCommand Sync = clientActionM clientSync
runCommand (MemoryUsage v) = clientActionE clientMemoryUsage >>= assign v
runCommand (Truncate ke te) = 
  k <- evalExp ke
  t <- evalExp te
  clientActionM $ clientTruncate ke te

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
runScript = mapM_ runLAction

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Prelude

prelude :: Eval ()
prelude = do
  -- Numerical operations
  assign "+"  (bimap (+) (+) :: Either Int Double -> Either (Int -> Int) (Double -> Double))
  assign "-"  (bimap (-) (-) :: Either Int Double -> Either (Int -> Int) (Double -> Double))
  assign "*"  (bimap (*) (*) :: Either Int Double -> Either (Int -> Int) (Double -> Double))
  assign "/"  (bimap div (/) :: Either Int Double -> Either (Int -> Int) (Double -> Double))
  assign "="  ((==) :: Int -> Int -> Bool)
  assign "/=" ((/=) :: Int -> Int -> Bool)
  assign "<=" (bimap (<=) (<=) :: Either Int Double -> Either (Int -> Bool) (Double -> Bool))
  assign ">=" (bimap (>=) (>=) :: Either Int Double -> Either (Int -> Bool) (Double -> Bool))
  assign ">"  (bimap (>)  (>)  :: Either Int Double -> Either (Int -> Bool) (Double -> Bool))
  assign "<"  (bimap (<)  (<)  :: Either Int Double -> Either (Int -> Bool) (Double -> Bool))
  -- Arrays
  -- assign "length" (length :: [Value] -> Int)
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
  assign "server" ("sltime.plowtech.net" :: Text)
  assign "port" (5000 :: Int)
