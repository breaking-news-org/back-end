module API.TH (
  aesonOptions,
  processRecord,
  makeFromToJSON,
  processApiRecord,
  processApiRecord',
  mkType,
  makeToSchema,
) where

import API.Prelude (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Common.TH (aesonOptions, makeFromToJSON, makeFromToJSON', mkType, processRecord)
import Language.Haskell.TH (Dec, Name, Q, Type (ConT))

makeToSchema' :: Type -> Q [Dec]
makeToSchema' t = do
  [d|
    instance ToSchema $(pure t) where
      declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions aesonOptions
    |]

makeToSchema :: Name -> Q [Dec]
makeToSchema name = makeToSchema' $ ConT name

processApiRecord :: Name -> Q [Dec]
processApiRecord name = do
  (<>) <$> processRecord name <*> makeToSchema name

processApiRecord' :: [Name] -> Q [Dec]
processApiRecord' (x : xs) = do
  r1 <- makeFromToJSON' t
  r3 <- makeToSchema' t
  pure $ r1 <> r3
 where
  t = mkType (x : xs)
processApiRecord' [] = error "Not enough names: 0"
