{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Persist.Model where

import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Persist.Types.News (Images, NewsText, NewsTitle, NewsIdHashed)
import Persist.Types.User (AuthorName, CategoryId, CreatedAt, ExpiresAt, HashedPassword, Role, TokenId, UserName)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Users
    password HashedPassword
    userName UserName
    authorName AuthorName
    role Role
    deriving Eq Show
News
    title NewsTitle
    createdAt CreatedAt
    authorId UsersId
    category CategoryId
    text' NewsText
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