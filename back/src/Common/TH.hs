module Common.TH (
  aesonOptions,
  makeLenses,
  processRecord,
  makeFromToJSON,
  makeFromToJSON',
  mkType,
) where

import API.Prelude
import Common.Prelude
import Language.Haskell.TH

aesonOptions :: Options
aesonOptions =
  defaultOptions
    { fieldLabelModifier = dropWhile (== '_') . dropWhile (/= '_') . dropWhile (== '_')
    }

makeFromToJSON' :: Type -> Q [Dec]
makeFromToJSON' t =
  [d|
    instance FromJSON $a where
      parseJSON = genericParseJSON aesonOptions

    instance ToJSON $a where
      toJSON = genericToJSON aesonOptions
      toEncoding = genericToEncoding aesonOptions
    |]
 where
  a = pure t

makeFromToJSON :: Name -> Q [Dec]
makeFromToJSON t = makeFromToJSON' $ ConT t

processRecord :: Name -> Q [Dec]
processRecord n = (<>) <$> makeLenses n <*> makeFromToJSON n

mkType :: [Name] -> Type
mkType [x] = ConT x
mkType (x : xs) = AppT (ConT x) (mkType xs)
mkType _ = error "Could not construct a type"