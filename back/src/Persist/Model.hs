{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Persist.Model where

import Common.Types.News (CategoryName, CreatedAt, Images, NewsText, NewsTitle)
import Common.Types.User
import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

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
    UniqueName name
    deriving Ord Eq Show
News
    title NewsTitle
    createdAt CreatedAt
    authorId UsersId OnDeleteCascade
    text' NewsText
    images (JSONB Images)
    isPublished Bool
    deriving Ord Eq Show
NewsCategories
    newsId NewsId OnDeleteCascade
    categoryId CategoriesId OnDeleteCascade
    UniqueNewsCategory newsId categoryId
    deriving Ord Eq Show
|]