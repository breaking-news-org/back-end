{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.TypeLevel (Eval, Symbol, Exp, Modify) where

import Fcf (Exp)
import GHC.Base (Symbol)

type family Eval (a :: Exp Symbol) :: Symbol

data Modify :: Symbol -> Exp Symbol
