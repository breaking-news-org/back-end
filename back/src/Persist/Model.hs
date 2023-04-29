{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Persist.Model where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Persist.Types.News (Images)
import Service.Prelude (UTCTime)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Users
    password ByteString
    userName Text
    authorName Text
    deriving Eq Show
News
    title Text
    creationDate UTCTime
    authorName Text
    category Int
    text Text
    images (JSONB Images)
    isPublished Bool
    deriving Ord Eq Show
RefreshTokens
    sessionId Int
    lastId Int
    deriving Ord Eq Show
|]
