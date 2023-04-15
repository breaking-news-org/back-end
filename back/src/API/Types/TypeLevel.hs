module API.Types.TypeLevel where

import Data.Symbol.Ascii (FromList, ToList)
import GHC.Base (Symbol)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Servant.TypeLevel(Eval, Modify)

type family DropWhile (dropSym :: Symbol) (sym :: Symbol) where
  DropWhile dropSym sym = DropWhile1 dropSym (ToList sym)

type family DropWhile1 (dropSym :: Symbol) (sym :: [Symbol]) :: Symbol where
  DropWhile1 dropSym '[] = TypeError ('Text "No characters left in Symbol when dropping" :<>: ShowType dropSym :$$: 'Text "Please, fix the error")
  DropWhile1 dropSym (dropSym : xs) = DropWhile1 dropSym xs
  DropWhile1 dropSym xs = FromList xs

type family DropWhileNot (dropSym :: Symbol) (sym :: Symbol) :: Symbol where
  DropWhileNot dropSym sym = DropWhileNot1 dropSym (ToList sym)

type family DropWhileNot1 (dropSym :: Symbol) (sym :: [Symbol]) :: Symbol where
  DropWhileNot1 dropSym '[] = TypeError ('Text "No characters left in symbol when dropping " :<>: ShowType dropSym :$$: 'Text "Please, fix the error")
  DropWhileNot1 dropSym (dropSym : xs) = FromList (dropSym ': xs)
  DropWhileNot1 dropSym (x : xs) = DropWhileNot1 dropSym xs

type family Modifier (sym :: Symbol) :: Symbol where
  Modifier sym = DropWhile "_" (DropWhileNot "_" (DropWhile "_" sym))

type instance Eval (Modify sym) = Modifier sym