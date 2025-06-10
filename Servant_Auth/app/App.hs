{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module App where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Db
import JsonTypes
import Mailing
import Servant
import Servant.Auth.Server as SAS
import Servant.Server.Experimental.Auth as Exp

import CommonConfig (AppContext (..), AppM)
import CookieHandlers
import Database.Redis (runRedis, sadd)
import Network.Wai (Request)
import VerifyEmail

type MainAPI = RefreshAccessToken :<|> HomePage :<|> Signup :<|> VerifyEmail :<|> Login

server :: ServerT MainAPI AppM
server = returnNewAccessToken :<|> returnUserData :<|> signup :<|> verifyEmail :<|> login

app :: AppContext -> IO Application
app appContext@AppContext{..} = do
    let ctx =
            handleRefreshCookie jwtConfig
                :. handleAccessCookie jwtConfig
                :. jwtConfig
                :. defaultCookieSettings
                :. EmptyContext
    pure $ serveWithContextT (Proxy :: Proxy MainAPI) ctx nt server
  where
    nt :: AppM a -> Handler a
    nt appM = runReaderT appM appContext

type instance AuthServerData (AuthProtect "access_jwt") = Session

handleAccessCookie :: JWTSettings -> AuthHandler Request Session
handleAccessCookie jwt_settings = mkAuthHandler $ handleCookies "access_token" jwt_settings

type instance AuthServerData (AuthProtect "refresh_jwt") = Session

handleRefreshCookie :: JWTSettings -> AuthHandler Request Session
handleRefreshCookie jwt_settings = mkAuthHandler $ handleCookies "refresh_token" jwt_settings

type RefreshAccessToken =
    AuthProtect "refresh_jwt"
        :> "auth"
        :> "refresh"
        :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] Text)

returnNewAccessToken :: Session -> AppM (Headers '[Header "Set-Cookie" SetCookie] Text)
returnNewAccessToken Session{..} = do
    AppContext{jwtConfig} <- ask
    c <- makeAccessCookie userEmail jwtConfig
    pure $ addHeader c "Refresh Success"

type HomePage = AuthProtect "access_jwt" :> "auth" :> "home" :> Get '[JSON] LoginResponse

returnUserData :: Session -> AppM LoginResponse
returnUserData Session{..} = pure $ LoggedIn $ Profile{userName = userEmail, profilePic = "J"}

type Signup = "auth" :> "sign-up" :> ReqBody '[JSON] Credentials :> Post '[JSON] Bool

signup :: Credentials -> AppM Bool
signup (Credentials email' pswd) = do
    AppContext{postPool} <- ask
    status <- liftIO $ checkEmailStatus postPool email'
    case status of
        EmailUnverified -> err403 ... VerificationPending
        EmailVerified _ -> err409 ... AccountAlreadyExists
        EmailUnavailable -> do
            token <- liftIO $ addUnverifiedUser postPool email' pswd
            let verificationLink = "http://127.0.0.1/api/auth/verify-email?token=" <> token
            acceptedBySes <- liftIO $ sendEmail email' verificationLink
            if acceptedBySes
                then pure True
                else err400 ... EmailUnreachable

type Login =
    "auth"
        :> "login"
        :> ReqBody '[JSON] Credentials
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)

login :: Credentials -> AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
login (Credentials email' pswd) = do
    AppContext{postPool} <- ask
    status <- liftIO $ checkEmailStatus postPool email'
    case status of
        EmailUnavailable -> err401 ... NoSuchEmail
        EmailUnverified -> err401 ... NoSuchEmail
        EmailVerified hashedpswd -> do
            match <- liftIO $ doPswdMatch pswd hashedpswd
            if match
                then liftIO (print $ "They matched: " <> show match) >> sendSessionCookie email'
                else err401 ... PasswordMismatch

sendSessionCookie ::
    Text -> AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
sendSessionCookie email' = do
    AppContext{jwtConfig, redisPool} <- ask
    ac <- makeAccessCookie email' jwtConfig
    (rc, jti) <- makeRefreshCookie email' jwtConfig
    let prfile = Profile{userName = "Josh_kaizen", profilePic = "J"}
    _ <- liftIO $ runRedis redisPool $ sadd [i|user:<#{email'}>:active_sessions|] [[i|#{jti}|]]
    pure $ addHeader ac $ addHeader rc LoggedIn{userProfile = prfile}
