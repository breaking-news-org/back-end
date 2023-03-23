module API.TH (
  aesonOptions,
  processRecord,
  makeLenses,
  processApiRecord,
) where

import Common.Prelude
import Data.OpenApi (ToSchema (declareNamedSchema), fromAesonOptions, genericDeclareNamedSchema)
import Language.Haskell.TH

aesonOptions :: Options
aesonOptions =
  defaultOptions
    { fieldLabelModifier = dropWhile (== '_') . dropWhile (/= '_') . dropWhile (== '_')
    }

processRecord :: Name -> Q [Dec]
processRecord name =
  (<>)
    <$> makeLenses name
    <*> [d|
      instance FromJSON $a where
        parseJSON = genericParseJSON aesonOptions

      instance ToJSON $a where
        toJSON = genericToJSON aesonOptions
        toEncoding = genericToEncoding aesonOptions
      |]
 where
  a = conT name

processApiRecord :: Name -> Q [Dec]
processApiRecord name =
  (<>)
    <$> processRecord name
    <*> [d|
      instance ToSchema $a where
        declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions aesonOptions
      |]
 where
  a = conT name