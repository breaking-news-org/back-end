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
-- work with clients.  See that package for documentation.
module Servant.Client.Record () where

import Data.Kind (Type)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.Client.Core.HasClient
import Servant.Client.Core.Request
import Servant.Client.Core.RunClient
import Servant.Record
import Servant.TypeLevel (Eval, Modify)

instance
  ( RunClient m
  , Generic a
  , GHasClient m (Rep a) api
  ) =>
  HasClient m (RecordParam a :> api)
  where
  type Client m (RecordParam a :> api) = a -> Client m api
  clientWithRoute pm Proxy req record =
    gClientWithRoute pm (Proxy :: Proxy api) req (from record :: Rep a ())
  {-# INLINE clientWithRoute #-}
  hoistClientMonad pm Proxy f cl as =
    gHoistClientMonad
      pm
      (Proxy :: Proxy api)
      f
      (cl . to)
      (from as :: Rep a ())
  {-# INLINE hoistClientMonad #-}

data GParam a

class GHasClient m (a :: Type -> Type) api where
  gClientWithRoute ::
    RunClient m =>
    Proxy m ->
    Proxy api ->
    Request ->
    a () ->
    Client m api
  gHoistClientMonad ::
    RunClient m =>
    Proxy m ->
    Proxy api ->
    (forall x. mon x -> mon' x) ->
    (a () -> Client mon api) ->
    (a () -> Client mon' api)

instance
  ( RunClient m
  , GHasClient m a api
  ) =>
  HasClient m (GParam (a ()) :> api)
  where
  type Client m (GParam (a ()) :> api) = a () -> Client m api
  clientWithRoute pm _ = gClientWithRoute pm (Proxy :: Proxy api)
  {-# INLINE clientWithRoute #-}
  hoistClientMonad pm _ = gHoistClientMonad pm (Proxy :: Proxy api)
  {-# INLINE hoistClientMonad #-}

instance
  GHasClient m c api =>
  GHasClient m (D1 m3 c) api
  where
  gClientWithRoute pm _ req (M1 x) =
    gClientWithRoute pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm Proxy f cl x =
    gHoistClientMonad pm (Proxy :: Proxy api) f (cl . M1) (unM1 x)
  {-# INLINE gHoistClientMonad #-}

instance
  GHasClient m a (GParam (b ()) :> api) =>
  GHasClient m (a :*: b) api
  where
  gClientWithRoute pm _ req (x :*: y) =
    gClientWithRoute pm (Proxy :: Proxy (GParam (b ()) :> api)) req x y
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm Proxy f cl (x :*: y) =
    gHoistClientMonad
      pm
      (Proxy :: Proxy (GParam (b ()) :> api))
      f
      (\x' y' -> cl (x' :*: y'))
      x
      y
  {-# INLINE gHoistClientMonad #-}

instance GHasClient m a api => GHasClient m (C1 mon a) api where
  gClientWithRoute pm _ req (M1 x) =
    gClientWithRoute pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 x) =
    gHoistClientMonad pm (Proxy :: Proxy api) f (cl . M1) x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  ) =>
  GHasClient m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) api
  where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryFlag (Eval (Modify sym)) :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryFlag (Eval (Modify sym)) :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  ) =>
  GHasClient m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) api
  where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryParams (Eval (Modify sym)) a :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParams (Eval (Modify sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  ) =>
  GHasClient
    m
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)))
    api
  where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute
      pm
      (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (Modify sym)) a :> api))
      req
      x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (Modify sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPABLE #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (Modify sym))
  ) =>
  GHasClient
    m
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a))
    api
  where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute
      pm
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (Modify sym)) a :> api))
      req
      x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (Modify sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}
