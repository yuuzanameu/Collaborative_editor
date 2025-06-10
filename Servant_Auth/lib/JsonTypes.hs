{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module JsonTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.Aeson.TypeScript.TH

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.Auth.JWT (FromJWT, ToJWT)

data Session = Session
    { userEmail :: Text
    , expiresAt :: UTCTime
    , jti :: Maybe Text -- for Acess token use Nothing
    }
    deriving (Generic, Show, FromJSON, ToJSON, ToJWT, FromJWT)

data Credentials = Credentials
    { email :: Text
    , password :: Text
    }
    deriving (Show)

data SignUpErrors
    = AccountAlreadyExists
    | VerificationPending
    | EmailUnreachable
    deriving (Show)

data Profile = Profile
    { userName :: Text
    , profilePic :: Text
    }
    deriving (Show)

newtype LoginState = LoginState {loggedin :: Bool}
    deriving (Show)

data LoginErrors
    = NoSuchEmail
    | PasswordMismatch
    deriving (Show)

newtype LoginResponse = LoggedIn {userProfile :: Profile}
    deriving (Show)

$( mconcat
    <$> traverse
        (deriveJSONAndTypeScript defaultOptions)
        [ ''Credentials
        , ''SignUpErrors
        , ''Profile
        , ''LoginState
        , ''LoginErrors
        , ''LoginResponse
        ]
 )
