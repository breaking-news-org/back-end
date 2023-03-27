module API.TH (
  aesonOptions,
  makeLenses,
  processRecord,
  processRecord',
  processApiRecord,
  processApiRecord',
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

processRecord' :: Type -> Q [Dec]
processRecord' t =
  [d|
    instance FromJSON $a where
      parseJSON = genericParseJSON aesonOptions

    instance ToJSON $a where
      toJSON = genericToJSON aesonOptions
      toEncoding = genericToEncoding aesonOptions
    |]
 where
  a = pure t

processRecord :: Name -> Q [Dec]
processRecord n = (<>) <$> makeLenses n <*> processRecord' (ConT n)

processApiRecord :: Name -> Q [Dec]
processApiRecord name = do
  r1 <- processRecord name
  r2 <-
    [d|
      instance ToSchema $a where
        declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions aesonOptions
      |]
  pure $ r1 <> r2
 where
  a = conT name

processApiRecord' :: [Name] -> Q [Dec]
processApiRecord' (x : xs) = do
  r1 <- processRecord' t
  r2 <- makeLenses x
  r3 <-
    [d|
      instance ToSchema $a where
        declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions aesonOptions
      |]
  pure $ r1 <> r2 <> r3
 where
  t = mkType (x : xs)
  a = pure t
processApiRecord' [] = error "Not enough names: 0"

mkType :: [Name] -> Type
mkType [x] = ConT x
mkType (x : xs) = AppT (ConT x) (mkType xs)
mkType _ = error "Could not construct a type"