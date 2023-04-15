module API.Types.TypeLevel where

import Data.Data (Proxy (Proxy))
import Fcf (Exp)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol)
import Servant.TypeLevel (DropWhile, DropWhileNot, Eval)

data Modify :: Symbol -> Exp Symbol

type family Modifier (sym :: Symbol) :: Symbol where
  Modifier sym = DropWhile "_" (DropWhileNot "_" (DropWhile "_" sym))

type instance Eval (Modify sym) = Modifier sym

p :: forall d. KnownSymbol d => Proxy d
p = Proxy

s :: Proxy "kd"
s = p @(Modifier "_hello_kd")