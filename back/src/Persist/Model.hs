{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Persist.Model where

import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Persist.Types.News (Images, NewsText, NewsTitle, CategoryName)
import Persist.Types.User (AuthorName, CreatedAt, ExpiresAt, HashedPassword, Role, TokenId, UserName)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Users
    password HashedPassword
    name UserName
    authorName AuthorName
    role Role
    UniqueUserName name
    deriving Eq Show
Sessions
    tokenId TokenId
    tokenExpiresAt ExpiresAt
    userId UsersId OnDeleteCascade
    UniqueUserId userId
    deriving Ord Eq Show
Categories
    name CategoryName
    parent CategoriesId Maybe
    deriving Ord Eq Show
News
    title NewsTitle
    createdAt CreatedAt
    authorId UsersId OnDeleteCascade
    category CategoriesId
    text' NewsText
    images (JSONB Images)
    isPublished Bool
    deriving Ord Eq Show
|]


-- A session has tokens whose ids are incremented when refreshing
-- Tokens may expire.
-- These tokens should be removed from time to time

-- TODO add to News
-- titles may be not unique
-- urls are constructed from news ids