module TSTypes where

import Data.Aeson.TypeScript.TH
import Data.Proxy
import JsonTypes

types :: String
types =
    formatTSDeclarations
        ( getTypeScriptDeclarations (Proxy :: Proxy Credentials)
            <> getTypeScriptDeclarations (Proxy :: Proxy SignUpErrors)
            <> getTypeScriptDeclarations (Proxy :: Proxy Profile)
            <> getTypeScriptDeclarations (Proxy :: Proxy LoginState)
            <> getTypeScriptDeclarations (Proxy :: Proxy LoginErrors)
            <> getTypeScriptDeclarations (Proxy :: Proxy LoginResponse)
        )

main :: IO ()
main =
    putStrLn types

-- Hello
