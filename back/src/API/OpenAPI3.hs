{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module API.OpenAPI3 where

import API.Prelude (HasOpenApi (..), OpenApi, Proxy (..), encodeFile, type (:>))
import API.Root (API)
import Common.Prelude ((%~))
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.OpenApi (Components (_componentsSecuritySchemes), HasSecurity (security), HttpSchemeType (HttpSchemeBearer), OpenApi (_openApiComponents), SecurityDefinitions (SecurityDefinitions), SecurityRequirement (SecurityRequirement), SecurityScheme (..), SecuritySchemeType (SecuritySchemeHttp), allOperations)
import Data.Text qualified as T
import Servant.Auth (Auth, JWT)
import Servant.Record (RecordParam, UnRecordParam)
import Servant.TypeLevel (Modify)
import API.Types.TypeLevel ()

spec :: OpenApi
spec = toOpenApi (Proxy :: Proxy API)

addSecurityScheme :: T.Text -> SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
  openApi
    { _openApiComponents =
        (_openApiComponents openApi)
          { _componentsSecuritySchemes =
              _componentsSecuritySchemes (_openApiComponents openApi)
                <> SecurityDefinitions (HM.singleton securityIdentifier securityScheme)
          }
    }

addSecurityRequirement :: T.Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
  allOperations
    . security
    %~ ((SecurityRequirement $ HM.singleton securityRequirement []) :)

instance (HasOpenApi (Auth auths a :> api)) => HasOpenApi (Auth (JWT : auths) a :> api) where
  toOpenApi :: HasOpenApi (Auth auths a :> api) => Proxy (Auth (JWT : auths) a :> api) -> OpenApi
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "JWT"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Bearer Authentication"
        }

instance (HasOpenApi api) => HasOpenApi (Auth '[] a :> api) where
  toOpenApi :: Proxy (Auth '[] a :> api) -> OpenApi
  toOpenApi Proxy = toOpenApi $ Proxy @api

instance HasOpenApi (UnRecordParam (RecordParam a :> api) Modify) => HasOpenApi (RecordParam a :> api) where
  toOpenApi :: Proxy (RecordParam a :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnRecordParam (RecordParam a :> api) Modify))

writeSpec :: FilePath -> IO ()
writeSpec f = encodeFile f spec