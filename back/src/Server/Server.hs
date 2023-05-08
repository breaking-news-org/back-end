module Server.Server (Server, runServerEffect, startServer, getWaiApplication, writeJWK) where

import API.Endpoints.API1.News as ApiNews (API (API, create, get), CategoriesAPI (..), categories, publish)
import API.Endpoints.API1.Root as API1 (API (API, news, user))
import API.Endpoints.API1.User as ApiUser (API (..))
import API.Prelude (NoContent (NoContent), secondsToNominalDiffTime)
import API.Root (Routes (..))
import API.Types.Instances ()
import API.Types.News ()
import API.Types.User (AccessToken, RefreshToken)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Controller.Effects.News (NewsController)
import Controller.Effects.User (UserController)
import Controller.News qualified as NewsController
import Controller.Types.User (JWKSettings (..))
import Controller.User
import Controller.User qualified as UserController
import Crypto.JOSE (JWK, KeyMaterialGenParam (..), genJWK)
import Data.Aeson qualified
import Data.ByteString.Lazy qualified as BSL
import Data.Fixed (Fixed (MkFixed))
import Data.Functor (($>))
import Data.String (fromString)
import Effectful (Eff, Effect, IOE, Limit (..), MonadIO (liftIO), Persistence (Ephemeral), UnliftStrategy (ConcUnlift), withEffToIO, withUnliftStrategy, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import External.Logger (Logger, logInfo, withLogger)
import GHC.Generics (Generic (..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context (..), HasServer (ServerT))
import Servant.API (GServantProduct, NamedRoutes, ToServant, ToServantApi, toServant)
import Servant.Auth.Server (AuthResult (..), defaultCookieSettings, defaultJWTSettings)
import Servant.Auth.Server.Internal.AddSetCookie (AddSetCookieApi, AddSetCookies (..), Nat (S))
import Servant.Server qualified as Servant
import Servant.Server.Generic (AsServerT, genericServeTWithContext)
import Servant.Server.Named ()
import Servant.Server.Record ()
import Server.Config (App (..), JWTParameters (..), Loader, Web (_web_port), getConfig)

-- wire up all controllers here

data Server :: Effect where
  GetWaiApplication :: Server m Application
  StartServer :: Server m ()

makeEffect ''Server

withAccessToken :: AuthResult AccessToken -> (AccessToken -> a) -> a
withAccessToken r f = case r of
  Authenticated c -> f c
  _ -> error "sdsdfsd"

withRefreshToken :: AuthResult RefreshToken -> (RefreshToken -> a) -> a
withRefreshToken r f = case r of
  Authenticated c -> f c
  _ -> error "sdsdfsd"

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
    app <- getConfig @App id
    let port = app._app_web._web_port
        admins = app._app_admins
    waiApp <- getWaiApplication' =<< getJWKSettings
    withLogger . logInfo . fromString $ "listening at port " <> show port
    -- TODO run when database ready
    adminsUpdated <- runExceptT $ updateAdmins admins
    case adminsUpdated of
      Left err -> error $ show err
      Right _ -> pure ()
    liftIO $ Warp.run port waiApp
  GetWaiApplication -> getWaiApplication' =<< getJWKSettings

getJWKSettings :: forall es. Constraints es => Eff es JWKSettings
getJWKSettings = do
  lifeTime <- getConfig @App (._app_jwtParameters._jwtParameters_expirationTime)
  let lifetime_ = secondsToNominalDiffTime (MkFixed (lifeTime * (10 ^ 12)))
  jwk <- getConfig id
  pure JWKSettings{_jwkSettings_jwk = jwk, _jwkSettings_jwtLifetimeSeconds = lifetime_}

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

getWaiApplication' :: forall es. Constraints es => JWKSettings -> Eff es Application
getWaiApplication' jwkSettings = do
  withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
    let
      serverToHandler :: ExceptT Servant.ServerError (Eff es) x -> Servant.Handler x
      serverToHandler = Servant.Handler . ExceptT . liftIO . unlift . runExceptT
      waiApp =
        genericServeTWithContext
          serverToHandler
          Routes
            { api1 =
                API1.API
                  { user =
                      ApiUser.API
                        { register = UserController.register jwkSettings
                        , unRegister = \token -> withRefreshToken token UserController.unRegister $> NoContent
                        , login = UserController.login jwkSettings
                        , rotateRefreshToken = \token -> withRefreshToken token (UserController.rotateRefreshToken jwkSettings)
                        }
                  , news = \token ->
                      ApiNews.API
                        { create = withAccessToken token NewsController.create
                        , get = withAccessToken token NewsController.getNews
                        , publish = withAccessToken token NewsController.setIsPublished
                        , categories =
                            ApiNews.CategoriesAPI
                              { get = NewsController.getCategories
                              }
                        }
                  }
            }
          (defaultJWTSettings jwkSettings._jwkSettings_jwk :. defaultCookieSettings :. EmptyContext)
    pure waiApp

writeJWK :: IO ()
writeJWK = do
  jwk <- genJWK (RSAGenParam 1000)
  BSL.writeFile "jwk.json" (Data.Aeson.encode jwk)