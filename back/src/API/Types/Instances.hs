{-# LANGUAGE PolyKinds #-}

module API.Types.Instances where

import GHC.Base (Symbol)
import GHC.Generics (Generic (..))
import Servant (HasServer (ServerT, hoistServerWithContext), MkLink, (:>))
import Servant.API (GServantProduct, HasLink (toLink), NamedRoutes, ToServant, ToServantApi, toServant)
import Servant.Auth.Server.Internal.AddSetCookie (AddSetCookieApi, AddSetCookies (..), Nat (S))
import Servant.Record (GHasLink, RecordParam, genericToLink)
import Servant.Server (HasServer (route))
import Servant.Server.Generic (AsServerT)
import Servant.Server.Record (GHasServer (..), genericHoistServerWithContext, genericRoute)
import Servant.Symbols (DropWhile, DropWhileNot, Exp, Modify)

type family Modifier (sym :: Symbol) :: Symbol where
  Modifier sym = DropWhile "_" (DropWhileNot "_" (DropWhile "_" sym))

data DropPrefix :: Symbol -> Exp Symbol

type instance Modify (f sym) = Modifier sym

instance (Generic a, GHasLink DropPrefix (Rep a) sub) => HasLink (RecordParam a :> sub) where
  type MkLink (RecordParam a :> sub) b = a -> MkLink sub b
  toLink = genericToLink @DropPrefix

instance (Generic a, GHasServer DropPrefix (Rep a) context api) => HasServer (RecordParam a :> api) context where
  type ServerT (RecordParam a :> api) m = a -> ServerT api m
  route = genericRoute @DropPrefix
  hoistServerWithContext = genericHoistServerWithContext @DropPrefix

type instance AddSetCookieApi (NamedRoutes api) = AddSetCookieApi (ToServantApi api)
instance
  {-# OVERLAPS #-}
  ( AddSetCookies ('S n) (ServerT (ToServantApi api) m) cookiedApi
  , Generic (api (AsServerT m))
  , GServantProduct (Rep (api (AsServerT m)))
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  ) =>
  AddSetCookies ('S n) (api (AsServerT m)) cookiedApi
  where
  addSetCookies cookies = addSetCookies cookies . toServant
