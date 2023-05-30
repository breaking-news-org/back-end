module Server.Server (Server, runServerEffect, startServer, getWaiApplication, writeJWK) where

import API.Endpoints.API1.News as ApiNews
import API.Endpoints.API1.Root as API1 (API (..))
import API.Endpoints.API1.User as ApiUser (API (..))
import API.Prelude (NoContent (NoContent), secondsToNominalDiffTime)
import API.Root
import API.Types.News ()
import API.Types.QueryParam ()
import Common.Types.User
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Logger.Aeson
import Controller.Effects.News (NewsController)
import Controller.Effects.User (UserController)
import Controller.News (updateCategories)
import Controller.News qualified as NewsController
import Controller.Types.User (JWKSettings (..))
import Controller.User (updateAdmins)
import Controller.User qualified as UserController
import Crypto.JOSE (JWK, KeyMaterialGenParam (..), genJWK)
import Data.Aeson qualified
import Data.ByteString.Lazy qualified as BSL
import Data.Fixed (Fixed (MkFixed))
import Data.Functor (($>))
import Effectful (Eff, Effect, IOE, Limit (..), MonadIO (liftIO), Persistence (Ephemeral), UnliftStrategy (ConcUnlift), withEffToIO, withUnliftStrategy, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import External.Logger (Logger, withLogger)
import GHC.Generics (Generic (..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context (..), HasServer (ServerT), serveDirectoryWebApp)
import Servant.API (GServantProduct, NamedRoutes, ToServant, ToServantApi, toServant)
import Servant.Auth.Server (AuthResult (..), defaultCookieSettings, defaultJWTSettings)
import Servant.Auth.Server.Internal.AddSetCookie (AddSetCookieApi, AddSetCookies (..), Nat (S))
import Servant.QueryParam.Server.Record ()
import Servant.Server qualified as Servant
import Servant.Server.Generic (AsServerT, genericServeTWithContext)
import Server.Config
import System.Directory (getCurrentDirectory)

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
    let port = app._web._port
        staticContent = app._web._staticContent
    jwkSettings <- getJWKSettings
    waiApp <- getWaiApplication' jwkSettings staticContent
    dir <- liftIO getCurrentDirectory
    withLogger $ logDebug $ "Server started" :# ["port" .= port, "directory" .= dir, "serves directory" .= staticContent]
    updatedAdmins <- runExceptT $ updateAdmins app._initDB._admins
    updatedCategories <- runExceptT $ updateCategories app._initDB._categories
    case updatedAdmins >> updatedCategories of
      Left err -> error $ show err
      Right _ -> pure ()
    liftIO $ Warp.run port waiApp
  GetWaiApplication -> do
    app <- getConfig @App id
    let staticContent = app._web._staticContent
    jwkSettings <- getJWKSettings
    getWaiApplication' jwkSettings staticContent

getJWKSettings :: forall es. Constraints es => Eff es JWKSettings
getJWKSettings = do
  lifeTime <- getConfig @App (._jwtParameters._expirationTime)
  let lifetime_ = secondsToNominalDiffTime (MkFixed (lifeTime * (10 ^ 12)))
  jwk <- getConfig id
  pure JWKSettings{_jwk = jwk, _jwtLifetimeSeconds = lifetime_}

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

getWaiApplication' :: forall es. Constraints es => JWKSettings -> FilePath -> Eff es Application
getWaiApplication' jwkSettings staticContent = do
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
                        , unregister = \token -> withRefreshToken token UserController.unregister $> NoContent
                        , login = UserController.login jwkSettings
                        , rotateRefreshToken = \token -> withRefreshToken token (UserController.rotateRefreshToken jwkSettings)
                        }
                  , news = \token ->
                      ApiNews.API
                        { create = withAccessToken token NewsController.create
                        , get = withAccessToken token NewsController.getNews
                        , setIsPublished = withAccessToken token NewsController.setIsPublished
                        , categories =
                            ApiNews.CategoriesAPI
                              { get = NewsController.getCategories
                              }
                        }
                  , docs = serveDirectoryWebApp staticContent
                  }
            }
          (defaultJWTSettings jwkSettings._jwk :. defaultCookieSettings :. EmptyContext)
    pure waiApp

writeJWK :: IO ()
writeJWK = do
  jwk <- genJWK (RSAGenParam 1000)
  BSL.writeFile "jwk.json" (Data.Aeson.encode jwk)

-- TODO
-- A session has tokens whose ids are incremented when refreshing
-- Tokens may expire.
-- These tokens should be removed from time to time

-- TODO a category may have several parent categories