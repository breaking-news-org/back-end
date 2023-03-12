module Integration.MainTest where

import Control.Concurrent (forkIO, newEmptyMVar)
import Control.Concurrent.MVar
import Control.Exception (catch, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Demo.Backend.Main (runAppM)
import Demo.Backend.Server (getWaiApplication)
import GHC.IO.Exception (ExitCode (..))
import Integration.User (userTests)
import Network.Wai (Application)
import Test.Tasty

-- https://hackage.haskell.org/package/tasty-1.4.3/docs/Test-Tasty.html#v:defaultMain
unit_main :: IO ()
unit_main = do
  defaultMain
    ( withResource initApp releaseApp $ \(fmap fst -> getApp) ->
        testGroup
          "Integration Tests"
          [ userTests getApp
          ]
    )
    `catch` ( \e -> do
                if e == ExitSuccess
                  then putStrLn "Nice"
                  else putStrLn "Bad" >> throwIO e
            )

initApp :: IO (Application, MVar ())
initApp = do
  releaseSignal <- newEmptyMVar
  waiAppMVar <- newEmptyMVar
  void $ forkIO $ runAppM $ do
    waiApp <- getWaiApplication
    liftIO $ putMVar waiAppMVar waiApp
    liftIO $ takeMVar releaseSignal
  (,releaseSignal) <$> takeMVar waiAppMVar

releaseApp :: (Application, MVar ()) -> IO ()
releaseApp (_, releaseSignal) = do
  putMVar releaseSignal ()
