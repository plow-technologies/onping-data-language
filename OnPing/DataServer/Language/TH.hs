
{-# OPTIONS_GHC -fno-warn-orphans #-}

module OnPing.DataServer.Language.TH (
    th_var
  , th_App
  , th_Type
  ) where

import Language.Haskell.TH
import Data.String (IsString (..))

th_var :: String -> Exp
th_var = VarE . mkName

th_App :: Exp
       -> [Exp]
       -> Exp
th_App f [] = AppE (th_var "pure") f
th_App f (e:es) =
  let b = UInfixE f (th_var "<$>") e
      op x y = UInfixE x (th_var "<*>") y
  in  foldl op b es

th_Type :: String -- ^ Type name
        -> (Name -> [Type] -> Q a) -- ^ Constructor function
        -> Q [a]
th_Type tyn f = do
  info <- reify $ mkName tyn
  case info of
    TyConI typeDec ->
      case typeDec of
        DataD _ _ _ _ cs _ ->
          mapM (\c ->
                 case c of
                   NormalC cn ts -> f cn $ fmap snd ts
                   _ -> fail $ "th_Type(" ++ tyn ++ "): Unexpected constructor."
                  ) cs
        _ -> fail $ "Type " ++ tyn ++ " found, but not its declaration."
    _ -> fail $ "Type " ++ tyn ++ " not found."

instance IsString Name where
  fromString = mkName

instance IsString Exp where
  fromString = LitE . StringL
