{-# LANGUAGE PolyKinds #-}

module API.Types.QueryParam where

import GHC.Base (Symbol)
import Servant.Symbols (Exp, DropPrefix, Eval)

type family Modifier (sym :: Symbol) :: Symbol where
  Modifier sym = DropPrefix sym

data Drop :: a -> Exp a

type instance Eval (Drop sym) = Modifier sym

