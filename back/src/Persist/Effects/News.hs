{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Persist.Effects.News where

import Effectful
import Effectful.TH
import Service.Types.News

data NewsRepo :: Effect where
  InsertNews :: News -> NewsRepo m ()
  SelectNews :: Filters Maybe -> NewsRepo m [News]

makeEffect ''NewsRepo