{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Record (RecordParam, UnRecordParam) where

import Data.Kind (Type)
import Data.Proxy
import Fcf (Exp)
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.TypeLevel (Eval)

{- | RecordParam uses the fields in the record to represent the
 parameters.  The name of the field is used as parameter name, and
 the type is the return type.  For example, this api:

 @
 type API = "users" :> (QueryParam "category" Category :>
                        QueryParam' '[Required, Strict] "sort_by" SortBy :>
                        QueryFlag "with_schema" :>
                        QueryParams "filters" Filter :>
                        Get '[JSON] User
 @

 can be written with records:

 @
 data UserParams = UserParams
   { category :: Maybe Category
   , sort_by :: Sortby
   , with_schema :: Bool
   , filters :: [Filter]
   }

 type API = "users" :> RecordParam UserParams :> Get '[JSON] User
 @
-}
data RecordParam (a :: Type)

type family ServantAppend x y where
  ServantAppend (a :> b) c = a :> ServantAppend b c
  ServantAppend a c = a :> c

{- | Type family to rewrite a RecordParam Api to a regular servant API.
 Useful to define instances for classes that extract information from
 the API type, such as Servant.Swagger, or servant-foreign.

 Typical use:

 > instance SomeClass (UnRecordParam (RecordParam a :> api))) =>
 >          SomeClass (RecordParam a :> api) where
 >    someMethod _ =
 >      someMethod (Proxy :: Proxy (UnRecordParam (RecordParam a :> api))
-}
type family UnRecordParam (x :: Type) (f :: Symbol -> Exp Symbol) :: Type where
  UnRecordParam (a :> b) f = ServantAppend (UnRecordParam a f) b
  UnRecordParam (RecordParam a) f = UnRecordParam (Rep a ()) f
  UnRecordParam (D1 m c d) f = UnRecordParam (c d) f
  UnRecordParam ((a :*: b) d) f =
    ServantAppend
      (UnRecordParam (a d) f)
      (UnRecordParam (b d) f)
  UnRecordParam (C1 m a d) f = UnRecordParam (a d) f
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool) d) f =
    QueryFlag (Eval (f sym))
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]) d) f =
    QueryParams (Eval (f sym)) a
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)) d) f =
    QueryParam' [Optional, Strict] (Eval (f sym)) a
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a) d) f =
    QueryParam' [Required, Strict] (Eval (f sym)) a

instance (Generic a, GHasLink (Rep a) sub) => HasLink (RecordParam a :> sub) where
  type MkLink (RecordParam a :> sub) b = a -> MkLink sub b
  toLink toA _ l record =
    gToLink toA (Proxy :: Proxy sub) l (from record :: Rep a ())

data GParam a

instance GHasLink a sub => HasLink (GParam (a ()) :> sub) where
  type MkLink (GParam (a ()) :> sub) b = a () -> MkLink sub b
  toLink toA _ = gToLink toA (Proxy :: Proxy sub)
  {-# INLINE toLink #-}

class HasLink sub => GHasLink (a :: Type -> Type) sub where
  gToLink :: (Link -> b) -> Proxy sub -> Link -> a () -> MkLink sub b

instance GHasLink c sub => GHasLink (D1 m c) sub where
  gToLink toA _ l (M1 x) = gToLink toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance
  ( HasLink sub
  , GHasLink a (GParam (b ()) :> sub)
  ) =>
  GHasLink (a :*: b) sub
  where
  gToLink toA _ l (a :*: b) =
    gToLink toA (Proxy :: Proxy (GParam (b ()) :> sub)) l a b
  {-# INLINE gToLink #-}

instance
  (GHasLink a sub, HasLink sub) =>
  GHasLink (C1 m a) sub
  where
  gToLink toA _ l (M1 x) = gToLink toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (f sym))
  , HasLink sub
  ) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) sub
  where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryFlag (Eval (f sym)) :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (f sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) sub
  where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParams (Eval (f sym)) a :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (f sym)),
    ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a))) sub
  where
  gToLink toA _ l (M1 (K1 x)) =
    toLink
      toA
      (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (f sym)) a :> sub))
      l
      x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (f sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a)) sub
  where
  
  gToLink toA _ l (M1 (K1 x)) =
    toLink
      toA
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (f sym)) a :> sub))
      l
      x
  {-# INLINE gToLink #-}