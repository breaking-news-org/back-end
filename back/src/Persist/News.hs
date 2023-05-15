{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse on/on" #-}
module Persist.News (
  runNewsRepo,
)
where

import Common.Prelude (UTCTime (..), filtered, fromGregorian, getCurrentTime, non, traversed, (^..))
import Common.Types.News
import Common.Types.User
import Control.Lens (has, _1)
import Control.Lens qualified as L ((^.))
import Control.Monad (forM)
import Control.Monad.Logger.Aeson (Message ((:#)), logDebug, (.=))
import Data.Coerce (coerce)
import Data.Int
import Data.String.Interpolate (i)
import Database.Esqueleto.Experimental (Entity (..), From, SqlExpr, SqlQuery, Value (..), asc, desc, distinct, in_, innerJoin, just, like, limit, notIn, on, orderBy, selectOne, set, unionAll_, updateCount, valList, withRecursive, (=.), (==.), (||.), type (:&) ((:&)), leftJoin, except_, ToSqlSetOperation (toSqlSetOperation))
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import External.Logger (Logger, withLogger)
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (Categories (..), EntityField (..), News (..), Users (..))
import Persist.Prelude (Eff, IOE, MonadIO (liftIO), SqlBackendPool, from, insert, interpret, mkFromSqlKey, mkSqlKey, mkSqlKeyVal, select, table, val, where_, withConn, (&&.), (<=.), (>=.), type (:>))
import Server.Config (App (..), Loader, Web (..), getConfig)
import Database.Esqueleto.Experimental.From

mkSqlTrue :: (a -> SqlExpr (Value Bool)) -> Maybe a -> SqlExpr (Value Bool)
mkSqlTrue = maybe (val True)

runNewsRepo :: (IOE :> es, Logger :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNewsItem insertNewsItem@InsertNewsItem{..} -> do
    -- TODO insert category
    ((unValue <$>) -> authorName) <-
      withConn $
        selectOne do
          users <- from $ table @Users
          where_ (users.id ==. mkSqlKeyVal _insertNews_authorId)
          pure users.name
    case authorName of
      Nothing -> pure . Left $ AuthorDoesNotExist _insertNews_authorId
      Just (UserName authorName') -> do
        let newsModel = newsToModel insertNewsItem
        key <- withConn $ insert newsModel
        withLogger $ logDebug $ "Inserted " :# ["news" .= insertNewsItem]
        pure . Right $ newsFromModel (Entity{entityKey = key, entityVal = newsModel}, Value (AuthorName authorName'))
  RepoSelectNews userId userRole filters@NewsFilters{..} -> do
    withLogger $ logDebug $ "select news" :# ["filters" .= filters]
    now <- liftIO getCurrentTime
    app <- getConfig id
    news <- withConn $ select do
      categoriesAll <-
        selectCategories
          ( CategoryFilters
              { _categoryFilters_include = _newsFilters_categoriesInclude
              , _categoryFilters_exclude = _newsFilters_categoriesExclude
              }
          )
      (news :& users :& _) <-
        from
          $ table @News `innerJoin` table @Users
          `on` (\(news :& users) -> news.authorId ==. users.id)
            `innerJoin` categoriesAll
          `on` (\(news :& _ :& categories) -> news.category ==. categories.id)
      let
        thisYear = UTCTime{utctDay = fromGregorian 2023 1 1, utctDayTime = 0}
        createdUntil = news.createdAt L.^. coerce <=. val (filters L.^. #_newsFilters_createdUntil . coerce . non now)
        createdSince = news.createdAt L.^. coerce >=. val (filters L.^. #_newsFilters_createdSince . coerce . non thisYear)
        authorName = mkSqlTrue (\x -> users.authorName ==. val x) _newsFilters_authorName
        -- TODO FULLTEXT search
        titleLike = mkSqlTrue (\x -> news.title `like` val [i|%#{x}%|]) _newsFilters_titleLike
        textLike = mkSqlTrue (\x -> news.text' `like` val [i|%#{x}%|]) _newsFilters_textLike
        showUnpublished =
          (news.isPublished ==. val False)
            &&. maybe
              (val False)
              (\x -> val x &&. (news.authorId ==. mkSqlKeyVal userId ||. val (has #_RoleAdmin userRole)))
              _newsFilters_showUnpublished
        showPublished = (news.isPublished ==. val True) &&. mkSqlTrue val _newsFilters_showPublished
      where_
        ( (createdUntil &&. createdSince &&. authorName)
            &&. (textLike &&. titleLike &&. (showPublished ||. showUnpublished))
            -- &&. showUnpublished
        )
      let pageSize = fromIntegral app._app_web._web_pageSize
      -- pagination
      limit pageSize
      orderBy [desc news.createdAt]
      pure (news, users.authorName)
    pure $ newsFromModel <$> news
  RepoUpdateIsPublished userId userRole SetIsPublished{..} -> do
    withConn do
      (affected :: [Int64]) <- forM _setIsPublished_newsIds $ \x ->
        updateCount $ \p -> do
          set p [NewsIsPublished =. val _setIsPublished_isPublished]
          where_
            ( p.id
                ==. mkSqlKeyVal x
                &&. if userRole == RoleAdmin
                  then val True
                  else p.authorId ==. mkSqlKeyVal userId
            )
      let unaffected = zip _setIsPublished_newsIds affected ^.. traversed . filtered ((== 0) . snd) . _1
      pure unaffected
  RepoSelectCategoryIds categoryFilters -> do
    withLogger $ logDebug $ "select categories recursively" :# ["filters" .= categoryFilters]
    categories <- withConn $ select do
      cats <- selectCategories categoryFilters
      t <- from cats
      orderBy [asc t.id]
      pure t
    pure $
      ( \Entity{entityVal = Categories{..}, ..} ->
          SelectedCategoryItem
            { _selectedCategory_id = mkFromSqlKey entityKey
            , _selectedCategory_name = categoriesName
            , _selectedCategory_parent = mkFromSqlKey <$> categoriesParent
            }
      )
        <$> categories

selectCategories :: CategoryFilters -> SqlQuery (From (SqlExpr (Entity Categories)))
selectCategories CategoryFilters{..} = do
  -- TODO remove excluded categories and their children from the final result

  let cats list_ =
        distinct $
          withRecursive
            ( do
                category <- from $ table @Categories
                where_ $
                  case list_ of
                    [] -> val True
                    x -> category.id `in_` valList (mkSqlKey <$> x)
                pure category
            )
            unionAll_
            ( \prev -> do
                (categories :& _) <-
                  from
                    $ table @Categories
                      `innerJoin` prev
                    `on` (\(categories :& categoriesPrev) -> categories.parent ==. just categoriesPrev.id)
                pure categories
            )
  cats _categoryFilters_include
  -- cats1 <- from =<< cats _categoryFilters_include
  -- cats2 <- from =<< cats _categoryFilters_exclude

  -- n2 <- from cats2
  -- let k = 
    -- pure cats1 `except_`  pure cats2
  -- pure _
  -- pure $ toFrom (cats1 `except_` cats2)

-- check _categoryFilters_exclude notIn

newsToModel :: InsertNewsItem -> News
newsToModel InsertNewsItem{..} =
  News
    { newsTitle = _insertNews_title
    , newsCreatedAt = _insertNews_createdAt
    , newsAuthorId = mkSqlKey _insertNews_authorId
    , newsCategory = mkSqlKey _insertNews_category
    , newsImages = JSONB _insertNews_images
    , newsIsPublished = _insertNews_isPublished
    , newsText' = _insertNews_text
    }

newsFromModel :: (Entity News, Value AuthorName) -> NewsItem
newsFromModel (Entity{entityKey, entityVal = News{..}}, authorName) =
  NewsItem
    { _newsItem_title = newsTitle
    , _newsItem_createdAt = newsCreatedAt
    , _newsItem_authorName = authorName.unValue
    , _newsItem_category = mkFromSqlKey newsCategory
    , _newsItem_images = unJSONB newsImages
    , _newsItem_isPublished = newsIsPublished
    , _newsItem_text = newsText'
    , _newsItem_id = mkFromSqlKey entityKey
    }