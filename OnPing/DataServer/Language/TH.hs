
module OnPing.DataServer.Language.TH (
    th_var
  , th_App
  ) where

import Language.Haskell.TH

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
