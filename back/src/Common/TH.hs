{-# LANGUAGE DeriveLift #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Common.TH (
  aesonOptionsRecord,
  processRecord,
  processRecords,
  makeRecordFromToJSON,
  makeRecordFromToJSON',
  processRecord',
  processRecords',
  aesonOptionsSum,
  processSum,
  processSums,
  makeSumFromToJSON,
  makeSumFromToJSON',
  processSum',
  processSums',
  processTypes',
  mkType,
) where

import API.Prelude ()
import Common.Prelude
import Language.Haskell.TH

aesonOptionsRecord :: Options
aesonOptionsRecord =
  defaultOptions
    { fieldLabelModifier = dropWhile (== '_')
    , rejectUnknownFields = True
    }

makeRecordFromToJSON' :: Type -> Q [Dec]
makeRecordFromToJSON' t =
  [d|
    instance FromJSON $a where
      parseJSON = genericParseJSON aesonOptionsRecord

    instance ToJSON $a where
      toJSON = genericToJSON aesonOptionsRecord
      toEncoding = genericToEncoding aesonOptionsRecord
    |]
 where
  a = pure t

makeRecordFromToJSON :: Name -> Q [Dec]
makeRecordFromToJSON t = makeRecordFromToJSON' $ ConT t

-- TODO rename to makeRecordFromToJSON

processRecord :: Name -> Q [Dec]
processRecord = makeRecordFromToJSON

processRecords :: [Name] -> Q [Dec]
processRecords ns = concat <$> traverse processRecord ns

mkType :: [Name] -> Type
mkType [x] = ConT x
mkType (x : xs) = AppT (ConT x) (mkType xs)
mkType _ = error "Could not construct a type"

processRecord' :: [Name] -> Q [Dec]
processRecord' (x : xs) = makeRecordFromToJSON' (mkType (x : xs))
processRecord' [] = error "Not enough names: 0"

processRecords' :: [[Name]] -> Q [Dec]
processRecords' = processTypes' makeRecordFromToJSON'

aesonOptionsSum :: Options
aesonOptionsSum =
  defaultOptions
    { tagSingleConstructors = True
    }

makeSumFromToJSON' :: Type -> Q [Dec]
makeSumFromToJSON' t =
  [d|
    instance FromJSON $a where
      parseJSON = genericParseJSON aesonOptionsSum

    instance ToJSON $a where
      toJSON = genericToJSON aesonOptionsSum
      toEncoding = genericToEncoding aesonOptionsSum
    |]
 where
  a = pure t

makeSumFromToJSON :: Name -> Q [Dec]
makeSumFromToJSON t = makeSumFromToJSON' $ ConT t

processSum :: Name -> Q [Dec]
processSum = makeSumFromToJSON

processSums :: [Name] -> Q [Dec]
processSums ns = concat <$> traverse processSum ns

processSum' :: [Name] -> Q [Dec]
processSum' (x : xs) = makeSumFromToJSON' (mkType (x : xs))
processSum' [] = error "Not enough names: 0"

processSums' :: [[Name]] -> Q [Dec]
processSums' = processTypes' makeSumFromToJSON'

processTypes' :: Applicative f => (Type -> f [a]) -> [[Name]] -> f [a]
processTypes' process (x : xs) = concat <$> traverse (process . mkType) (x : xs)
processTypes' _ [] = error "Not enough names: 0"
