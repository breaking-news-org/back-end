{-# LANGUAGE PolyKinds #-}

module API.Types.Instances where

import GHC.Base (Symbol)
import Servant.Symbols (DropWhile, DropWhileNot, Exp, Modify)

type family Modifier (sym :: Symbol) :: Symbol where
  Modifier sym = DropWhile "_" (DropWhileNot "_" (DropWhile "_" sym))

data DropPrefix :: Symbol -> Exp Symbol

type instance Modify (DropPrefix sym) = Modifier sym