module App (
  main,
  AppM,
  runAppM,
)
where

import Controller.Effects.News (NewsController)
import Controller.Effects.Users (UserController)
import Controller.News (runNewsController)
import Controller.User (runUserController)
import Crypto.JOSE (JWK)
import Data.Function ((&))
import Effectful
import External.Logger (Logger, runLogger)
import Persist.Effects.News (NewsRepo)
import Persist.Effects.User (UserRepo)
import Persist.News (runNewsRepo)
import Persist.Prelude (SqlBackendPool, runSqlBackendPool)
import Persist.User (runUserRepo)
import Server.Config
import Server.Server
import Service.Effects.News (ServiceNews)
import Service.News
import Service.User (UserService, runUserService)

main :: IO ()
main = runAppM startServer

type AppM =
  Eff
    '[ Server
     , UserController
     , UserService
     , UserRepo
     , NewsController
     , ServiceNews
     , NewsRepo
     , SqlBackendPool
     , Logger
     , Loader App
     , Loader JWK
     , IOE
     ]

_CONFIG_FILE :: String
_CONFIG_FILE = "CONFIG_FILE"

_JWK_FILE :: String
_JWK_FILE = "JWK_FILE"

runAppM :: AppM () -> IO ()
runAppM appM =
  runServerEffect appM
    & runUserController
    & runUserService
    & runUserRepo
    & runNewsController
    & runNewsService
    & runNewsRepo
    & runSqlBackendPool
    & runLogger
    & runLoader _CONFIG_FILE
    & runLoader _JWK_FILE
    & runEff
