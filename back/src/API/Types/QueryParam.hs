{-# LANGUAGE PolyKinds #-}

module API.Types.QueryParam where

import Fcf.Data.List (DropWhile)
import GHC.Base (Symbol)
import Servant.TypeLevel (FromList, ToList, Eval, Exp, TyEq)

type DropPrefix (s :: Symbol) = FromList (Eval (DropWhile (TyEq '_') (ToList s)))
data Drop :: a -> Exp a

type instance Eval (Drop sym) = DropPrefix sym
