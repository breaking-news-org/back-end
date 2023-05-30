module Common.Prelude (
  module Data.Aeson,
  module Data.Aeson.Types,
  module Control.Lens,
  module Data.Time,
  module Data.IntMap.Strict,
  module Data.ByteString,
  module Data.Text,
  module GHC.Generics,
  module Effectful,
  HKD,
  module Data.OpenApi,
)
where

import Control.Lens hiding ((.=), (<.>))
import Data.Aeson (
  FromJSON (..),
  Options (..),
  ToJSON (..),
  Value,
  defaultOptions,
  fieldLabelModifier,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
  withText,
 )
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.OpenApi (ToParamSchema (..), ToSchema (..), genericDeclareNamedSchema, genericToParamSchema)
import Data.Text (Text)
import Data.Time
import Effectful
import GHC.Generics (Generic)

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a