{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Db where

import Control.Concurrent.Async (wait, withAsync)
import Data.Password.Bcrypt
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.String.Interpolate (iii)
import Data.Text (Text, pack)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple

data EmailStatus
    = EmailVerified {hashedPassword :: Text}
    | EmailUnverified
    | EmailUnavailable
    deriving (Eq, Show)

data TokenStatus
    = TokenUnavailable
    | TokenActive {emailId :: Text}
    deriving (Eq, Show)

type Email = Text

createDbPool :: IO (Pool Connection)
createDbPool = do
    print ("setting up db pool" :: String)
    newPool $ defaultPoolConfig connectDb close 60 50

connectDb :: IO Connection
connectDb =
    connect
        defaultConnectInfo
            { connectHost = "localhost"
            , connectUser = "postgres"
            , connectPassword = "1772"
            , connectDatabase = "editor_userdata"
            , connectPort = 5432
            }

createVerificationToken :: IO Text
createVerificationToken = pack . show <$> nextRandom

hashPswd :: Text -> IO Text
hashPswd pswd = do
    let password = mkPassword pswd
    hashed <- withAsync (hashPassword password) $ \a -> do wait a
    pure . unPasswordHash $ hashed

-- DATABASE QUERY FUNCTIONS

checkEmailStatus :: Pool Connection -> Email -> IO EmailStatus
checkEmailStatus pool email = do
    withResource pool $ \conn -> do
        rows <-
            query
                conn
                "select verified, password from userdata where email = ?;"
                (Only email) ::
                IO [(Bool, Text)]
        let res = case rows of
                [] -> EmailUnavailable
                [(True, pswd)] -> EmailVerified pswd
                [(False, _)] -> EmailUnverified
        pure res

addUnverifiedUser :: Pool Connection -> Email -> Text -> IO Text
addUnverifiedUser pool email pswd = do
    withResource pool $ \conn -> do
        currentTime <- getCurrentTime
        token <- createVerificationToken
        let expires_at = addUTCTime (secondsToNominalDiffTime (24 * 60 * 60)) currentTime
        hashedPswd <- hashPswd pswd
        rowsAffected <-
            execute
                conn
                [iii|
                   insert into userdata 
                   (email, password, verified, expires_at, verification_token)
                   values (?,?,?,?,?)
                |]
                (email, hashedPswd, False, expires_at, token)
        print rowsAffected
        pure token

checkTokenStatus :: Pool Connection -> String -> IO TokenStatus
checkTokenStatus pool token = do
    withResource pool $ \conn -> do
        res <-
            query
                conn
                "select email from userdata where verification_token = ?;"
                (Only token) ::
                IO [Only Text]
        case res of
            [] -> pure TokenUnavailable
            [Only email] -> pure $ TokenActive email

doPswdMatch :: Text -> Text -> IO Bool
doPswdMatch pswd pswdHash = do
    let pswdHash' = PasswordHash{unPasswordHash = pswdHash}
    case checkPassword (mkPassword pswd) pswdHash' of
        PasswordCheckFail -> pure False
        PasswordCheckSuccess -> pure True

setEmailVerified :: Pool Connection -> Email -> IO ()
setEmailVerified pool email' = do
    withResource pool $ \conn -> do
        _ <-
            execute
                conn
                "update userdata set verified = true, verification_token=null where email = ?; "
                (Only email')
        pure ()
