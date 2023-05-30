{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse on/on" #-}
module Persist.News (
  runNewsRepo,
)
where

import Common.Prelude (UTCTime (..), filtered, fromGregorian, getCurrentTime, non, traversed, (<&>), (^..))
import Common.Types.News
import Common.Types.User
import Control.Lens (has, _1)
import Control.Lens qualified as L ((^.))
import Control.Monad (forM, forM_)
import Control.Monad.Logger.Aeson (Message ((:#)), logDebug, (.=))
import Data.Coerce (coerce)
import Data.Int
import Data.String.Interpolate (i)
import Database.Esqueleto.Experimental (Entity (..), PersistEntity (Key), SqlExpr, SqlQuery, Value (..), asc, desc, distinct, in_, innerJoin, just, like, limit, on, orderBy, selectOne, set, updateCount, upsert, valList, withRecursive, (=.), (==.), (||.), type (:&) ((:&)), PersistUniqueWrite (putMany))
import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.From.SqlSetOperation
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB, unJSONB))
import External.Logger (Logger, withLogger)
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (Categories (..), EntityField (..), News (..), NewsCategories (NewsCategories), Users (..))
import Persist.Prelude (Eff, IOE, MonadIO (liftIO), SqlBackendPool, insert, interpret, mkFromSqlKey, mkSqlKey, mkSqlKeyVal, select, val, where_, withConn, (&&.), (<=.), (>=.), type (:>))
import Server.Config (App (..), Loader, Web (..), getConfig)

mkSqlTrue :: (a -> SqlExpr (Value Bool)) -> Maybe a -> SqlExpr (Value Bool)
mkSqlTrue = maybe (val True)

runNewsRepo :: (IOE :> es, Logger :> es, Loader App :> es, SqlBackendPool :> es) => Eff (NewsRepo : es) a -> Eff es a
runNewsRepo = interpret $ \_ -> \case
  RepoInsertNewsItem insertNewsItem@InsertNewsItem{..} -> do
    ((unValue <$>) -> authorName) <-
      withConn $
        selectOne do
          users <- from $ table @Users
          where_ (users.id ==. mkSqlKeyVal _authorId)
          pure users.name
    case authorName of
      Nothing -> pure . Left $ AuthorDoesNotExist _authorId
      Just (UserName _) -> do
        let newsModel = newsToModel insertNewsItem
        newsKey <- withConn $ insert newsModel
        categories <- (entityKey <$>) <$> withConn (forM _categories $ \categoryName -> upsert (Categories categoryName Nothing) [])
        withConn (forM_ categories $ \categoryKey -> upsert (NewsCategories newsKey categoryKey) [])
        withLogger $ logDebug $ "Inserted " :# ["news" .= insertNewsItem]
        pure . Right $ ()
  RepoSelectNews userId userRole filters@NewsFilters{..} -> do
    withLogger $ logDebug $ "select news" :# ["filters" .= filters]
    now <- liftIO getCurrentTime
    app <- getConfig id
    newsAuthor <- withConn $ select do
      categoriesAll <-
        selectCategories
          ( CategoryFilters
              { _include = _categoriesInclude
              , _exclude = _categoriesExclude
              }
          )
      (_ :& _ :& news :& users) <-
        from
          $ categoriesAll `innerJoin` table @NewsCategories
          `on` (\(catsAll :& newsCategories) -> catsAll.id ==. newsCategories.categoryId)
            `innerJoin` table @News
          `on` (\(_ :& newsCategories :& news) -> newsCategories.newsId ==. news.id)
            `innerJoin` table @Users
          `on` (\(_ :& _ :& news :& users) -> news.authorId ==. users.id)
      let
        thisYear = UTCTime{utctDay = fromGregorian 2023 1 1, utctDayTime = 0}
        createdUntil = news.createdAt L.^. coerce <=. val (filters L.^. #_createdUntil . coerce . non now)
        createdSince = news.createdAt L.^. coerce >=. val (filters L.^. #_createdSince . coerce . non thisYear)
        authorName = mkSqlTrue (\x -> users.authorName ==. val x) _authorName
        -- TODO FULLTEXT search
        titleLike = mkSqlTrue (\x -> news.title `like` val [i|%#{x}%|]) _titleLike
        textLike = mkSqlTrue (\x -> news.text' `like` val [i|%#{x}%|]) _textLike
        showUnpublished =
          (news.isPublished ==. val False)
            &&. maybe
              (val False)
              (\x -> val x &&. (news.authorId ==. mkSqlKeyVal userId ||. val (has #_RoleAdmin userRole)))
              _showUnpublished
        showPublished = (news.isPublished ==. val True) &&. mkSqlTrue val _showPublished
      where_
        ( (createdUntil &&. createdSince &&. authorName)
            &&. (textLike &&. titleLike &&. (showPublished ||. showUnpublished))
        )
      let pageSize = fromIntegral app._web._pageSize
      -- pagination
      limit pageSize
      orderBy [desc news.createdAt]
      pure (news, users.authorName)

    -- TODO optimize

    newsCategories <- forM newsAuthor $ \(news, _) -> withConn $ select (selectNewsCategories (val (news.entityKey)))
    pure $ newsFromModel <$> zipWith (\(n, a) c -> (n, a, c)) newsAuthor newsCategories
  RepoUpdateIsPublished userId userRole SetIsPublished{..} -> do
    withConn do
      (affected :: [Int64]) <- forM _newsIds $ \x ->
        updateCount $ \p -> do
          set p [NewsIsPublished =. val _isPublished]
          where_
            ( p.id
                ==. mkSqlKeyVal x
                &&. if userRole == RoleAdmin
                  then val True
                  else p.authorId ==. mkSqlKeyVal userId
            )
      let unaffected = zip _newsIds affected ^.. traversed . filtered ((== 0) . snd) . _1
      pure unaffected
  RepoSelectCategoryIds categoryFilters -> do
    withLogger $ logDebug $ "select categories recursively" :# ["filters" .= categoryFilters]
    categories <- withConn $ select do
      cats <- selectCategories categoryFilters
      cats' <- from cats
      orderBy [asc cats'.id]
      pure cats'
    pure $
      ( \Entity{entityVal = Categories{..}, ..} ->
          SelectedCategoryItem
            { _id = mkFromSqlKey entityKey
            , _name = categoriesName
            , _parent = mkFromSqlKey <$> categoriesParent
            }
      )
        <$> categories
  RepoUpdateCategories categories -> do
    -- overwrites parents
    let categories' = categories <&> (`Categories` Nothing)
    withConn $ putMany categories'

selectCategories :: CategoryFilters -> SqlQuery (SqlSetOperation (SqlExpr (Entity Categories)))
selectCategories CategoryFilters{..} = do
  -- TODO remove excluded categories and their children from the final result
  let cats list_ include =
        distinct $
          withRecursive
            ( do
                category <- from $ table @Categories
                where_ $
                  case list_ of
                    [] -> val include
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
  cats1 <- cats _include True
  cats2 <- cats _exclude False
  pure $ from cats1 `except_` from cats2

selectNewsCategories :: SqlExpr (Value (Key News)) -> SqlQuery (SqlExpr (Value CategoryName))
selectNewsCategories newsId = do
  (newsCategories :& categories) <-
    from
      $ table @NewsCategories
        `innerJoin` table @Categories
      `on` (\(newsCategories :& categories) -> newsCategories.categoryId ==. categories.id)
  where_ (newsCategories.newsId ==. newsId)
  pure $ categories.name

newsToModel :: InsertNewsItem -> News
newsToModel InsertNewsItem{..} =
  News
    { newsTitle = _title
    , newsCreatedAt = _createdAt
    , newsAuthorId = mkSqlKey _authorId
    , newsImages = JSONB _images
    , newsIsPublished = _isPublished
    , newsText' = _text
    }

newsFromModel :: (Entity News, Value AuthorName, [Value CategoryName]) -> NewsItem
newsFromModel (Entity{entityKey, entityVal = News{..}}, authorName, categories) =
  NewsItem
    { _title = newsTitle
    , _createdAt = newsCreatedAt
    , _authorName = authorName.unValue
    , _categories = unValue <$> categories
    , _images = unJSONB newsImages
    , _isPublished = newsIsPublished
    , _text = newsText'
    , _id = mkFromSqlKey entityKey
    }