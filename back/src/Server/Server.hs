module Server.Server (Server, runServerEffect, startServer, getWaiApplication) where

import API.Prelude (GServantProduct, NamedRoutes, ToServant, ToServantApi, toServant, type (:<|>) ((:<|>)))
import API.Root (Routes (..))
import API.Types.Client (ClientToken)
import API.Types.News ()
import Control.Monad.Except (ExceptT (..), runExceptT)
import Controller.Effects.News (NewsController)
import Controller.News qualified as NewsController
import Controller.User (UserController)
import Controller.User qualified as UserController
import Crypto.JOSE (JWK, KeyMaterialGenParam (..), genJWK)
import Data.Aeson qualified
import Data.ByteString.Lazy qualified as BSL
import Data.String (fromString)
import Effectful (Eff, Effect, IOE, Limit (..), MonadIO (liftIO), Persistence (Ephemeral), UnliftStrategy (ConcUnlift), withEffToIO, withUnliftStrategy, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import External.Logger (Logger, logInfo, withLogger)
import GHC.Generics (Generic (Rep))
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context (EmptyContext), HasServer (ServerT))
import Servant.Auth.Server (AuthResult (..), defaultCookieSettings, defaultJWTSettings)
import Servant.Auth.Server.Internal.AddSetCookie (AddSetCookieApi, AddSetCookies (..), Nat (S))
import Servant.Server qualified as Servant
import Servant.Server.Generic (AsServerT, genericServeTWithContext)
import Server.Config (App (_app_web), Loader, Web (_web_port), getConfig)

-- wire up all controllers here
type instance AddSetCookieApi (NamedRoutes api) = AddSetCookieApi (ToServantApi api)
instance
  {-# OVERLAPS #-}
  ( AddSetCookies ('S n) (ServerT (ToServantApi api) m) cookiedApi
  , Generic (api (AsServerT m))
  , GServantProduct (Rep (api (AsServerT m)))
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  ) =>
  AddSetCookies ('S n) (api (AsServerT m)) cookiedApi
  where
  addSetCookies cookies = addSetCookies cookies . toServant

withClient :: AuthResult ClientToken -> (ClientToken -> a) -> a
withClient r f = case r of
  Authenticated c -> f c
  _ -> error "sdsdfsd"

data Server :: Effect where
  GetWaiApplication :: Server m Application
  StartServer :: Server m ()

makeEffect ''Server

type Constraints es =
  ( IOE :> es
  , Loader App :> es
  , Loader JWK :> es
  , Logger :> es
  , NewsController :> es
  , UserController :> es
  )

runServerEffect :: forall es a. Constraints es => Eff (Server : es) a -> Eff es a
runServerEffect = interpret $ \_ -> \case
  StartServer -> do
    port <- getConfig (_web_port . _app_web)
    jwk <- getConfig id
    waiApp <- getWaiApplication' jwk
    withLogger . logInfo . fromString $ "listening at port " <> show port
    liftIO $ Warp.run port waiApp
  GetWaiApplication -> getConfig id >>= getWaiApplication'

getWaiApplication' :: forall es. Constraints es => JWK -> Eff es Application
getWaiApplication' jwk = do
  withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
    let
      serverToHandler :: ExceptT Servant.ServerError (Eff es) x -> Servant.Handler x
      serverToHandler = Servant.Handler . ExceptT . liftIO . unlift . runExceptT
      waiApp =
        genericServeTWithContext
          serverToHandler
          Routes
            { api1 =
                UserController.register
                  :<|> (\token -> withClient token NewsController.create :<|> withClient token NewsController.get)
            }
          (defaultJWTSettings jwk Servant.:. defaultCookieSettings Servant.:. EmptyContext)
    pure waiApp

writeJWK :: IO ()
writeJWK = do
  jwk <- genJWK (RSAGenParam 100)
  BSL.writeFile "jwk.json" (Data.Aeson.encode jwk)