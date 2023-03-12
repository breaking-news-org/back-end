{-# LANGUAGE OverloadedStrings #-}

module Integration.User where

import Network.HTTP.Types.Method
import Network.Wai
import Network.Wai.Test
import Test.Tasty
import Test.Tasty.HUnit
import Data.String.Interpolate

userTests :: IO Application -> TestTree
userTests getApp =
  testGroup
    "User"
    [ testCase "Registration" $ do
        app <- getApp
        withSession app $ do
          resp <-
            srequest $
              SRequest
                defaultRequest
                  { pathInfo = ["api", "user"]
                  , requestMethod = methodPost
                  , requestHeaders = [("Content-Type", "application/json")]
                  }
                [i|{"email": "contact@zelinf.net", "password": "abcdef"}|]
          assertStatus 200 resp
    ]