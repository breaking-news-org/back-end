{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.TH where

import API.Prelude (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Common.Prelude (ToParamSchema (..), genericToParamSchema)
import Common.TH
import Data.OpenApi (defaultSchemaOptions)
import Data.OpenApi.Schema (SchemaOptions (..))
import Language.Haskell.TH (Dec, Name, Q, Type (ConT))

schemaOptionsRecord :: SchemaOptions
schemaOptionsRecord = defaultSchemaOptions{unwrapUnaryRecords = True}

makeRecordToSchema' :: Type -> Q [Dec]
makeRecordToSchema' t = do
  [d|
    instance ToSchema $(pure t) where
      declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions aesonOptionsRecord
    |]

makeRecordToParamSchema' :: Type -> Q [Dec]
makeRecordToParamSchema' t = do
  [d|
    instance ToParamSchema $(pure t) where
      toParamSchema = genericToParamSchema schemaOptionsRecord
    |]

makeRecordTypes :: (Type -> Q [Dec]) -> [Name] -> Q [Dec]
makeRecordTypes f names = concat <$> traverse (f . ConT) names

makeRecordToSchema :: Name -> Q [Dec]
makeRecordToSchema name = makeRecordToSchema' $ ConT name

makeRecordToSchemaTypes :: [Name] -> Q [Dec]
makeRecordToSchemaTypes = makeRecordTypes makeRecordToSchema'

makeRecordToParamSchemaTypes :: [Name] -> Q [Dec]
makeRecordToParamSchemaTypes = makeRecordTypes makeRecordToParamSchema'

processRecordApiType :: Name -> Q [Dec]
processRecordApiType name = do
  (<>) <$> processRecord name <*> makeRecordToSchema name

processRecordApiTypes :: [Name] -> Q [Dec]
processRecordApiTypes ns = concat <$> traverse processRecordApiType ns

schemaOptionsSum :: SchemaOptions
schemaOptionsSum = defaultSchemaOptions{unwrapUnaryRecords = True}

makeSumToSchema' :: Type -> Q [Dec]
makeSumToSchema' t = do
  [d|
    instance ToSchema $(pure t) where
      declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions aesonOptionsSum

    instance ToParamSchema $(pure t) where
      toParamSchema = genericToParamSchema schemaOptionsSum
    |]

makeSumToSchema :: Name -> Q [Dec]
makeSumToSchema name = makeSumToSchema' $ ConT name

makeSumToSchemaTypes :: [Name] -> Q [Dec]
makeSumToSchemaTypes ns = concat <$> traverse makeSumToSchema ns

processSumApiType :: Name -> Q [Dec]
processSumApiType name = do
  (<>) <$> processSum name <*> makeSumToSchema name

processSumApiTypes :: [Name] -> Q [Dec]
processSumApiTypes = processTypes processSumApiType

processTypes :: (Traversable t, Applicative f) => (a1 -> f [a2]) -> t a1 -> f [a2]
processTypes process ns = concat <$> traverse process ns

processRecordApiTypes' :: [[Name]] -> Q [Dec]
processRecordApiTypes' = processTypes' makeRecordToSchema'

deriveNewtypeInstances :: Name -> [Name] -> Q [Dec]
deriveNewtypeInstances c ns = concat <$> traverse (\x -> [d|deriving newtype instance $(pure $ ConT c) $(pure $ ConT x)|]) ns

deriveNewtypeInstances' :: [Name] -> [Name] -> Q [Dec]
deriveNewtypeInstances' cs ns = concat <$> traverse (`deriveNewtypeInstances` ns) cs