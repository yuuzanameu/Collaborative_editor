{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module CookieHandlers (makeAccessCookie, makeRefreshCookie, (...), handleCookies, CookieAuthErrors (..)) where

-- All Redis logic is laid out here

import CommonConfig (AppM, accessCookieSettings, refreshCookieSettings)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import JsonTypes (Session (..))
import Network.Wai (Request (requestHeaders))
import Servant (
    Handler,
    ServerError (errBody),
    err401,
    err500,
    throwError,
 )
import Servant.Auth.Server (JWTSettings, SetCookie, makeSessionCookie, verifyJWT)
import Web.Cookie (parseCookies)

data CookieType = AccessCookie | RefreshCookie
    deriving (Eq, Show)

data CookieAuthErrors
    = UserLoggedOut
    | AccessTokenExpired
    | RefreshTokenExpired
    | InternalError
    deriving (Generic, Show, FromJSON, ToJSON)

type JTI = Text

makeRefreshCookie :: Text -> JWTSettings -> AppM (SetCookie, JTI)
makeRefreshCookie email' jwtConfig = do
    (now, jti_uuid) <- liftIO $ liftA2 (,) getCurrentTime nextRandom
    let jti = [i|#{jti_uuid}|] :: Text
    let expiry = addUTCTime (secondsToNominalDiffTime $ 60 * 60 * 24 * 7) now
        session = Session{userEmail = email', expiresAt = expiry, jti = Just jti}
    cookie <- liftIO $ makeSessionCookie refreshCookieSettings jwtConfig session
    case cookie of
        Nothing -> err500 ... InternalError
        Just c -> pure (c, jti)

makeAccessCookie :: Text -> JWTSettings -> AppM SetCookie
makeAccessCookie email' jwtConfig = do
    now <- liftIO getCurrentTime
    let expiry = addUTCTime (secondsToNominalDiffTime $ 60 * 15) now
        session = Session{userEmail = email', expiresAt = expiry, jti = Nothing}
    cookie <- liftIO $ makeSessionCookie accessCookieSettings jwtConfig session
    case cookie of
        Nothing -> err500 ... InternalError
        Just c -> pure c

(...) :: (MonadError ServerError m, ToJSON a1) => ServerError -> a1 -> m a2
(...) status_code val = throwError status_code{errBody = encode val}

handleCookies :: ByteString -> JWTSettings -> Request -> Handler Session
handleCookies token jwt_settings req = do
    cookies <- attempt $ lookup "cookie" $ requestHeaders req
    let cookies' = parseCookies cookies
    jwt <- attempt $ lookup token cookies'
    session <- attempt =<< liftIO (verifyJWT jwt_settings jwt)
    expired <- liftIO $ isExpired session
    case (token, expired) of
        ("access_token", True) -> err401 ... AccessTokenExpired
        ("refresh_token", True) -> err401 ... RefreshTokenExpired
        ("refresh_token", False) -> pure session
        _ -> pure session
  where
    attempt :: Maybe a -> Handler a
    attempt = maybe (err401 ... UserLoggedOut :: Handler a) pure

    isExpired :: Session -> IO Bool
    isExpired Session{..} = do
        now <- getCurrentTime
        pure $ now >= expiresAt
