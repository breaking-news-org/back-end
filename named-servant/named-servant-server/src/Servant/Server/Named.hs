{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module just exports orphan instances to make named-servant
-- work with servers
module Servant.Server.Named () where
import Servant.API
import Servant.Server
import Servant.Named
import Data.Proxy
import Servant.API.Modifiers
import GHC.TypeLits
import qualified Data.Text as Text
import Named

instance ( KnownSymbol sym
         , FromHttpApiData a
         , HasServer api context
         , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
         )
         => HasServer (NamedQueryParams sym a :> api) context where

  type ServerT (NamedQueryParams sym a :> api) m =
    sym :? [a] -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryParams sym a :> api)) context $
    fmap (\f x -> f (ArgF $ Just x)) subserver

instance ( KnownSymbol sym
         , FromHttpApiData a
         , HasServer api context
         , SBoolI (FoldRequired mods)
         , SBoolI (FoldLenient mods)
         , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
         )
      => HasServer (NamedQueryParam' mods sym a :> api) context where
  type ServerT (NamedQueryParam' mods sym a :> api) m =
    If (FoldRequired mods)
       (If (FoldLenient mods) (sym :! Either Text.Text a) (sym :! a))
       (If (FoldLenient mods) (sym :? Either Text.Text a) (sym :? a))
    -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryParam' mods sym a :> api)) context $
    fmap (\f x ->
          case sbool :: SBool (FoldRequired mods) of
           STrue -> case sbool :: SBool (FoldLenient mods) of
              STrue -> f (Arg x)
              SFalse -> f (Arg x)
           SFalse -> case sbool :: SBool (FoldLenient mods) of
              STrue -> f (ArgF x)
              SFalse -> f (ArgF x))
    subserver

instance (KnownSymbol sym, HasServer api context)
      => HasServer (NamedQueryFlag sym :> api) context where

  type ServerT (NamedQueryFlag sym :> api) m =
    sym :? Bool -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryFlag sym :> api)) context $
    fmap (\f -> f . ArgF . Just) subserver
