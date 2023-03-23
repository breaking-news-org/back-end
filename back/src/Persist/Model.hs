{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Persist.Model where

import API.Types.News (IndexedImages)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist.TH
import Service.Prelude (UTCTime)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Users
    email Text
    hashedPassword ByteString
    nickname Text
    deriving Eq Show
News
    title Text
    creationDate UTCTime
    creator Text
    category Text
    images IndexedImages
    isPublished Bool
    deriving Ord Eq Show
|]
