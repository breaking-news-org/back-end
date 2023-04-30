module API.TH (
  aesonOptions,
  processType,
  makeFromToJSON,
  processApiType,
  processApiTypes,
  mkType,
  makeToSchema,
  makeToSchemaTypes,
) where

import API.Prelude (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Common.TH (aesonOptions, makeFromToJSON, mkType, processType)
import Language.Haskell.TH (Dec, Name, Q, Type (ConT))

makeToSchema' :: Type -> Q [Dec]
makeToSchema' t = do
  [d|
    instance ToSchema $(pure t) where
      declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions aesonOptions
    |]

makeToSchema :: Name -> Q [Dec]
makeToSchema name = makeToSchema' $ ConT name

makeToSchemaTypes :: [Name] -> Q [Dec]
makeToSchemaTypes ns = concat <$> traverse makeToSchema ns

processApiType :: Name -> Q [Dec]
processApiType name = do
  (<>) <$> processType name <*> makeToSchema name

processApiTypes :: [Name] -> Q [Dec]
processApiTypes ns = concat <$> traverse processApiType ns
