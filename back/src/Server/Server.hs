module Server.Server (Server, runServerEffect, startServer, getWaiApplication) where

import API.Prelude (genericApi, type (:<|>) ((:<|>)))
import API.Root (API, ProxyRoutes)
import API.Types.News()
import Control.Monad.Except (ExceptT (..), runExceptT)
import Controller.News
import Controller.News qualified as NewsController
import Controller.User (UserController)
import Controller.User qualified as UserController
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Effectful (Eff, Effect, IOE, Limit (..), MonadIO (liftIO), Persistence (Ephemeral), UnliftStrategy (ConcUnlift), withEffToIO, withUnliftStrategy, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import External.Logger (Logger, logInfo, withLogger)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Server qualified as Servant
import Server.Config

-- wire up all controllers here
server :: (UserController :> es, NewsController :> es) => Servant.ServerT API (ExceptT Servant.ServerError (Eff es))
server = UserController.register :<|> NewsController.create :<|> NewsController.get

newApiProxy :: Proxy API
newApiProxy = genericApi (Proxy :: ProxyRoutes)

data Server :: Effect where
  GetWaiApplication :: Server m Application
  StartServer :: Server m ()

makeEffect ''Server

type Constraints es = (IOE :> es, Loader App :> es, Logger :> es, UserController :> es, NewsController :> es)

runServerEffect :: forall es a. Constraints es => Eff (Server : es) a -> Eff es a
runServerEffect = interpret $ \_ -> \case
  StartServer -> do
    port <- getConfig (_web_port . _app_web)
    waiApp <- getWaiApplication'
    withLogger . logInfo . fromString $ "listening at port " <> show port
    liftIO $ Warp.run port waiApp
  GetWaiApplication -> getWaiApplication'

getWaiApplication' :: forall es. Constraints es => Eff es Application
getWaiApplication' = do
  withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
    let serverToHandler :: ExceptT Servant.ServerError (Eff es) x -> Servant.Handler x
        serverToHandler = Servant.Handler . ExceptT . liftIO . unlift . runExceptT
        waiApp = Servant.serve newApiProxy (Servant.hoistServer newApiProxy serverToHandler server)
    pure waiApp
