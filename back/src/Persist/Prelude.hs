module Persist.Prelude (
  module Effectful,
  module Effectful.Dispatch.Dynamic,
  module External.SqlDatabase,
  module Database.Esqueleto.Experimental,
  mkSqlKey,
  mkSqlKeyVal,
  mkFromSqlKey
)
where

import Database.Esqueleto.Experimental (PersistEntity (Key), SqlBackend, SqlExpr, ToBackendKey, Value (..), entityVal, from, insert, select, table, toSqlKey, val, where_, (&&.), (<=.), (>=.), (^.), fromSqlKey)
import Effectful
import Effectful.Dispatch.Dynamic
import External.SqlDatabase

mkSqlKey :: forall typ a. (ToBackendKey SqlBackend typ, Integral a) => a -> Key typ
mkSqlKey = toSqlKey . fromIntegral

mkSqlKeyVal :: forall typ a. (ToBackendKey SqlBackend typ, Integral a) => a -> SqlExpr (Value (Key typ))
mkSqlKeyVal = val . mkSqlKey

mkFromSqlKey :: forall typ a. (ToBackendKey SqlBackend typ, Integral a) => Key typ -> a
mkFromSqlKey = fromIntegral . fromSqlKey