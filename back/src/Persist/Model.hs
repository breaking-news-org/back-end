{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Persist.Model where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Persist.Types.News (Images)
import Persist.Types.User (CategoryId, TokenId, ExpiresAt, UserName, AuthorName, Role)
import Service.Prelude (UTCTime)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Users
    password ByteString
    userName UserName
    authorName AuthorName
    role Role
    deriving Eq Show
News
    title Text
    creationDate UTCTime
    authorId UsersId
    category CategoryId
    text Text
    images (JSONB Images)
    isPublished Bool
    deriving Ord Eq Show
Sessions
    lastAccessTokenId TokenId
    lastRefreshTokenExpiresAt ExpiresAt
    userId UsersId
    deriving Ord Eq Show
|]

-- TODO add to News
-- uniqueAddress Text
-- to make urls constructible