{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | -- | This module just exports orphan instances to make named-servant
-- work with servers
module Servant.Server.Record () where

import Data.Kind (Type)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.Record
import Servant.Server
import Servant.Server.Internal
import Servant.TypeLevel (Eval, Modify)

class GHasServer (a :: Type -> Type) context api where
  gRoute ::
    Proxy api ->
    Context context ->
    Delayed env (a () -> Server api) ->
    Router env
  gHoistServerWithContext ::
    Proxy api ->
    Proxy context ->
    (forall x. m x -> n x) ->
    (a () -> ServerT api m) ->
    (a () -> ServerT api n)

data GParam a

instance
  ( Generic a
  , GHasServer (Rep a) context api
  ) =>
  HasServer (RecordParam a :> api) context
  where
  type ServerT (RecordParam a :> api) m = a -> ServerT api m
  route _ context env =
    gRoute (Proxy :: Proxy api) context $
      (\f (x :: Rep a ()) -> f (to x)) <$> env
  {-# INLINE route #-}
  hoistServerWithContext _ pc nt s x =
    gHoistServerWithContext
      (Proxy :: Proxy api)
      pc
      nt
      (s . to)
      (from x :: Rep a ())
  {-# INLINE hoistServerWithContext #-}

instance
  GHasServer a context api =>
  HasServer (GParam (a ()) :> api) context
  where
  type ServerT (GParam (a ()) :> api) m = a () -> ServerT api m
  route _ = gRoute (Proxy :: Proxy api)
  {-# INLINE route #-}
  hoistServerWithContext _ = gHoistServerWithContext (Proxy :: Proxy api)
  {-# INLINE hoistServerWithContext #-}

instance
  GHasServer c context api =>
  GHasServer (D1 m3 c) context api
  where
  gRoute _ context env =
    gRoute (Proxy :: Proxy api) context $
      (\f x -> f (M1 x)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ pc nt s (M1 x) =
    gHoistServerWithContext (Proxy :: Proxy api) pc nt (s . M1) x
  {-# INLINE gHoistServerWithContext #-}

instance
  GHasServer a context (GParam (b ()) :> api) =>
  GHasServer (a :*: b) context api
  where
  gRoute _ context env =
    gRoute
      (Proxy :: Proxy (GParam (b ()) :> api))
      context
      $ (\f x y -> f (x :*: y)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ pc nt s (x :*: y) =
    gHoistServerWithContext
      (Proxy :: Proxy (GParam (b ()) :> api))
      pc
      nt
      (\x' y' -> s (x' :*: y'))
      x
      y
  {-# INLINE gHoistServerWithContext #-}

instance
  GHasServer a context api =>
  GHasServer (C1 n a) context api
  where
  gRoute _ context env =
    gRoute (Proxy :: Proxy api) context $
      (\f x -> f (M1 x)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ pc nt s (M1 x) =
    gHoistServerWithContext (Proxy :: Proxy api) pc nt (s . M1) x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPING #-}
  ( HasServer api context
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  ) =>
  GHasServer
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool))
    context
    api
  where
  gRoute _ context env =
    route
      (Proxy :: Proxy (QueryFlag (Eval (Modify sym)) :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryFlag (Eval (Modify sym)) :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPING #-}
  ( HasServer api context
  , FromHttpApiData a
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]))
    context
    api
  where
  gRoute _ context env =
    route
      (Proxy :: Proxy (QueryParams (Eval (Modify sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParams (Eval (Modify sym)) a :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPING #-}
  ( HasServer api context
  , FromHttpApiData a
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)))
    context
    api
  where
  gRoute _ context env =
    route
      (Proxy :: Proxy (QueryParam (Eval (Modify sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParam (Eval (Modify sym)) a :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPABLE #-}
  ( HasServer api context
  , FromHttpApiData a
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a))
    context
    api
  where
  gRoute _ context env =
    route
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (Modify sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (Modify sym)) a :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}
