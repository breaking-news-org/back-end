module App (
  main,
  AppM,
  runAppM,
)
where

import Controller.News (ControllerNews, runNewsController)
import Controller.User (ControllerUser, runUserController)
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
import Service.News
import Service.User (UserService, runUserService)

main :: IO ()
main = runAppM startServer

type AppM =
  Eff
    '[ Server
     , ControllerUser
     , UserService
     , UserRepo
     , ControllerNews
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
