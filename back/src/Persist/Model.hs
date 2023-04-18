{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Persist.Model where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Persist.Types.News (IndexedImages)
import Service.Prelude (UTCTime)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Users
    email Text
    hashedPassword ByteString
    nickname Text
    deriving Eq Show
News
    title Text
    creationDate UTCTime
    creator Text
    category Int
    text Text
    images (JSONB IndexedImages)
    isPublished Bool
    deriving Ord Eq Show
|]
