module Main (
  main,
  AppM,
  runAppM,
)
where

import Config (App)
import Config qualified
import Controller.News (NewsController, runNewsController)
import Controller.User (UserController, runUserController)
import Data.Function ((&))
import Effectful
import External.Logger (Logger, runLogger)
import Model.News (NewsRepo)
import Model.User (UserRepo)
import Persist.News (runNewsRepo)
import Persist.Prelude (SqlBackendPool, runSqlBackendPool)
import Persist.User (runUserRepo)
import Server
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
     , NewsService
     , NewsRepo
     , SqlBackendPool
     , Logger
     , Config.Loader App
     , IOE
     ]

_CONFIG_FILE :: String
_CONFIG_FILE = "CONFIG_FILE"

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
    & Config.runLoader _CONFIG_FILE
    & runEff
