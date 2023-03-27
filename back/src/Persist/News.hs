module Persist.News (
  runNewsRepo,
)
where

import API.Types.News (Filters, filters_block, filters_categoryId, filters_content, filters_createdAt, filters_createdSince, filters_createdUntil, filters_creator)
import Common.Prelude (Getting, getCurrentTime, non, (&))
import Common.Prelude qualified as Common ((^.))
import Control.Monad (void)
import Database.Esqueleto.Experimental (SqlExpr, like, limit, (==.))
import Persist.Effects.News (NewsRepo (..))
import Persist.Model (EntityField (..), News (..))
import Persist.Prelude
import Server.Config (App (..))
import Service.Types.News qualified as Service

-- TODO pass a config here
runNewsRepo :: (IOE :> es, SqlBackendPool :> es) => App -> Eff (NewsRepo : es) a -> Eff es a
runNewsRepo app = interpret $ \_ -> \case
  InsertNews news -> void $ withConn $ insert $ newsToModel news
  -- TODO use filters
  SelectNews fs -> do
    now <- liftIO getCurrentTime
    let -- TODO initial date
        -- TODO fix
        -- News id?
        -- this might be more complex in production code
        -- TODO fix

        -- mkSql lens_ f = (fs Common.^. lens_) & maybe (val True) f

        -- mkSql' :: Getting (Maybe a) (Filters Maybe) (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)

        -- mkSql' :: Getting a (Filters Maybe) a -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)

    s <- withConn $ select do
      news <- from $ table @News
      let
        mkSql' l f = f (fs Common.^. l . non now)
        createdUntil = mkSql' filters_createdUntil (\x -> news ^. NewsCreationDate <=. val x)
        createdSince = mkSql' filters_createdSince (\x -> news ^. NewsCreationDate >=. val x)
        mkSql :: Getting (Maybe a) (Filters Maybe) (Maybe a) -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
        mkSql l f = (fs Common.^. l) & maybe (val True) f
        createdAt = mkSql filters_createdAt (\x -> news ^. NewsCreationDate ==. val x)
        creator = mkSql filters_creator (\x -> news ^. NewsCreator ==. val x)
        categoryId = mkSql filters_categoryId (\x -> news ^. NewsCategory ==. val x)
        newsText = mkSql filters_content (\x -> val x `like` news ^. NewsText)
        newsBlock = fs Common.^. filters_block . non 0
      where_
        ( createdUntil &&. createdSince &&. createdAt &&. creator &&. categoryId &&. newsText
        )
      limit $ newsBlock * app._app_web._web_pageSize
      pure news

    let t = modelToNews . entityVal <$> s
    pure t

newsToModel :: Service.News -> News
newsToModel news =
  News
    { newsTitle = news._news_title
    , newsCreationDate = news._news_creationDate
    , newsCreator = news._news_creator
    , newsCategory = news._news_category
    , newsImages = news._news_images
    , newsIsPublished = news._news_isPublished
    , newsText = news._news_text
    }

modelToNews :: News -> Service.News
modelToNews = undefined