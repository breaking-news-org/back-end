{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | -- | This module just exports orphan instances to make named-servant
-- work with clients.  See that package for documentation.
module Servant.Client.Record () where
import Servant.API
import Data.Proxy
import GHC.TypeLits
import GHC.Generics
import Servant.Client.Core.HasClient
import Servant.Client.Core.Request
import Servant.Client.Core.RunClient
import Servant.Record
import Data.Kind (Type)
import Servant.TypeLevel (Eval)

instance ( RunClient m
         , Generic a
         , GHasClient m (Rep a) api) =>
         HasClient m (RecordParam a :> api)
  where
    type Client m (RecordParam a :> api) = a -> Client m api
    clientWithRoute pm Proxy req record =
      gClientWithRoute pm (Proxy :: Proxy api) req (from record :: Rep a ())
    {-# INLINE clientWithRoute #-}
    hoistClientMonad pm Proxy f cl as =
      gHoistClientMonad pm (Proxy :: Proxy api) f (cl . to)
      (from as :: Rep a ())
    {-# INLINE hoistClientMonad #-}

data GParam a

class GHasClient m (a :: Type -> Type) api where
  gClientWithRoute :: RunClient m
                   => Proxy m -> Proxy api -> Request -> a () -> Client m api
  gHoistClientMonad :: RunClient m
                    => Proxy m
                    -> Proxy api
                    -> (forall x. mon x -> mon' x)
                    -> (a () -> Client mon api)
                    -> (a () -> Client mon' api)

instance ( RunClient m
         , GHasClient m a api
         ) =>
         HasClient m (GParam (a ()) :> api) where
  type Client m (GParam (a ()) :> api) = a () -> Client m api
  clientWithRoute pm _ = gClientWithRoute pm (Proxy :: Proxy api)
  {-# INLINE clientWithRoute #-}
  hoistClientMonad pm _ = gHoistClientMonad pm (Proxy :: Proxy api)
  {-# INLINE hoistClientMonad #-}

instance GHasClient m c api =>
         GHasClient m (D1 m3 c) api where
  gClientWithRoute pm _ req (M1 x) =
    gClientWithRoute pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm Proxy f cl x = 
    gHoistClientMonad pm (Proxy :: Proxy api) f (cl . M1) (unM1 x)
  {-# INLINE gHoistClientMonad #-}

instance GHasClient m a (GParam (b ()) :> api)
         => GHasClient m (a :*: b) api where
  gClientWithRoute pm _ req (x :*: y) =
    gClientWithRoute pm (Proxy :: Proxy (GParam (b ()) :> api)) req x y
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm Proxy f cl (x :*: y) =
    gHoistClientMonad pm (Proxy :: Proxy (GParam (b ()) :> api)) f
    (\x' y'-> cl (x' :*: y')) x y
  {-# INLINE gHoistClientMonad #-}

instance GHasClient m a api => GHasClient m (C1 mon a) api where
  gClientWithRoute pm _ req (M1 x) =
    gClientWithRoute pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 x) =
    gHoistClientMonad pm (Proxy :: Proxy api) f (cl . M1) x
  {-# INLINE gHoistClientMonad #-}

instance {-# OVERLAPPING #-}
  ( HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (f sym))
  ) =>
  GHasClient m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) api where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryFlag (Eval (f sym)) :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad pm (Proxy :: Proxy (QueryFlag (Eval (f sym)) :> api)) f (cl . M1 . K1)
    x
  {-# INLINE gHoistClientMonad #-}

instance {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (f sym))
  ) =>
  GHasClient m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) api where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryParams (Eval (f sym)) a :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad pm (Proxy :: Proxy (QueryParams (Eval (f sym)) a :> api)) f
    (cl . M1 . K1)
    x
  {-# INLINE gHoistClientMonad #-}

instance {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (f sym))
  ) =>
  GHasClient m
             (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)))
             api where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute
    pm
    (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (f sym)) a :> api))
    req
    x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
    pm
    (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (f sym)) a :> api))
    f
    (cl . M1 . K1)
    x
  {-# INLINE gHoistClientMonad #-}

instance {-# OVERLAPPABLE #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (f sym))
  ) =>
  GHasClient m
             (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a))
             api where
  gClientWithRoute pm _ req (M1 (K1 x)) =
    clientWithRoute
    pm
    (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (f sym)) a :> api))
    req
    x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
    pm
    (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (f sym)) a :> api))
    f
    (cl . M1 . K1)
    x
  {-# INLINE gHoistClientMonad #-}
