{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module External.Passwords where

import Common.Prelude
import Data.Password.Argon2 qualified as Argon2(checkPassword, hashPassword)
import Data.Password.Argon2 (PasswordCheck(..), PasswordHash(..), mkPassword)
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, unsafeEff, unsafeEff_)
import Persist.Types.User (HashedPassword (..), Password (..))

-- A wrapper for working with passwords
-- https://github.com/haskell-effectful/effectful/blob/6e8d89048a6704f9c2128ffdca4ea8e01be6e91c/effectful-core/src/Effectful.hs#L160

data Passwords :: Effect
type instance DispatchOf Passwords = Static WithSideEffects
data instance StaticRep Passwords = Passwords

liftEff :: (a -> IO b) -> a -> Eff es b
liftEff f a = unsafeEff $ const $ f a

hashPasswordM :: MonadIO m => Password -> m HashedPassword
hashPasswordM (Password p) = HashedPassword . unPasswordHash <$> Argon2.hashPassword (mkPassword p)

hashPassword :: Passwords :> es => Password -> Eff es HashedPassword
hashPassword a = unsafeEff_ $ hashPasswordM a

-- check a password

runPasswords :: IOE :> es => Eff (Passwords : es) a -> Eff es a
runPasswords = evalStaticRep Passwords