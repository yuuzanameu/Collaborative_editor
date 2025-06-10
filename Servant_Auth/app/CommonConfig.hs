{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonConfig (AppM, accessCookieSettings, refreshCookieSettings, appjwtSettings, AppContext (..)) where

import Servant (Handler)
import Servant.Auth.Server as SAS (
    CookieSettings (cookieMaxAge, cookieSameSite, cookieXsrfSetting, sessionCookieName),
    JWTSettings,
    SameSite (SameSiteStrict),
    defaultCookieSettings,
    defaultJWTSettings,
    readKey,
 )

import Data.Time (secondsToDiffTime)

import Control.Monad.Trans.Reader (ReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple as Post (Connection)

import Database.Redis as Redis (Connection)

data AppContext = AppContext
    { postPool :: Pool Post.Connection
    , jwtConfig :: JWTSettings
    , redisPool :: Redis.Connection
    }

type AppM = ReaderT AppContext Handler

appjwtSettings :: IO JWTSettings
appjwtSettings = do
    key <- readKey "/etc/servant_jwt/key.key"
    pure $ defaultJWTSettings key

accessCookieSettings :: CookieSettings
accessCookieSettings =
    defaultCookieSettings
        { sessionCookieName = "access_token"
        , cookieMaxAge = Just $ secondsToDiffTime (60 * 15)
        , cookieXsrfSetting = Nothing
        , cookieSameSite = SameSiteStrict
        }

refreshCookieSettings :: CookieSettings
refreshCookieSettings =
    accessCookieSettings
        { sessionCookieName = "refresh_token"
        , cookieMaxAge = Just $ secondsToDiffTime (60 * 60 * 24 * 7)
        }
