{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Servant.TypeLevel where

import Data.Symbol.Ascii (ToList)
import Fcf (Exp)
import GHC.Base (Symbol)
import GHC.TypeLits (AppendSymbol, ErrorMessage (..), TypeError)

type family FromList (syms :: [Symbol]) :: Symbol where
  FromList '[] = ""
  FromList (x : xs) = AppendSymbol x (FromList xs)

type family DropWhile (dropSym :: Symbol) (sym :: Symbol) where
  DropWhile dropSym sym = DropWhile1 dropSym (ToList sym)

type family DropWhile1 (dropSym :: Symbol) (sym :: [Symbol]) :: Symbol where
  DropWhile1 dropSym '[] = TypeError ('Text "No characters left in symbol when dropping" :<>: ShowType dropSym :$$: 'Text "Please, fix the error")
  DropWhile1 dropSym (dropSym : xs) = DropWhile1 dropSym xs
  DropWhile1 dropSym xs = FromList xs

type family DropWhileNot (dropSym :: Symbol) (sym :: Symbol) :: Symbol where
  DropWhileNot dropSym sym = DropWhileNot1 dropSym (ToList sym)

type family DropWhileNot1 (dropSym :: Symbol) (sym :: [Symbol]) :: Symbol where
  DropWhileNot1 dropSym '[] = TypeError (Text "No characters left in symbol when dropping " :<>: ShowType dropSym :$$: Text "Please, fix the error")
  DropWhileNot1 dropSym (dropSym : xs) = FromList (dropSym ': xs)
  DropWhileNot1 dropSym (x : xs) = DropWhileNot1 dropSym xs

type family Eval (a :: Exp Symbol) :: Symbol