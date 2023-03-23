{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.News where

import Effectful
import Effectful.TH
import Service.Prelude (UTCTime)
import Service.Types.News

data NewsRepo :: Effect where
  PutNews :: News -> NewsRepo m ()
  GetNews :: UTCTime -> NewsRepo m [News]

makeEffect ''NewsRepo