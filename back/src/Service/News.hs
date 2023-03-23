module Service.News where

import API.Types.News qualified as API (CreateNews (..), GetNews (..))
import External.Logger
import Model.News (NewsRepo)
import Common.Prelude (getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH

-- TODO
-- if a news is unpublished it can only be shown to its author

data NewsService :: Effect where
  CreateNews :: API.CreateNews -> NewsService m ()
  GetNewsList :: NewsService m [API.GetNews]

makeEffect ''NewsService

runNewsService ::
  (NewsRepo :> es, Logger :> es, IOE :> es) =>
  Eff (NewsService : es) a ->
  Eff es a
runNewsService =
  interpret $ \_ -> \case
    CreateNews cn -> do
      now <- liftIO getCurrentTime
      pure ()
    -- putNews
    --   News
    --     { _news_title = cn._createNews_title
    --     , _news_category = cn._createNews_category
    --     , _news_creationDate = now
    --     , _news_creator = cn._newsCreator
    --     }

    -- TODO fix
    GetNewsList -> do
      pure []

--   Register email password -> do
--     createUser $
--       User
--         { _user_email = email
--         , _user_hashedPassword = Password $ hashPassword password
--         , _user_nickname = "" -- default nick name
--         }
--     withLogger $ logInfo "created a new user successfully"

-- hashPassword :: Text -> ByteString
-- hashPassword = T.encodeUtf8 -- dummy implementation. we should generate a salt, and calculate sha512(salt <> password)
