{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE RecordWildCards #-}

module Main where

import App (app)
import CommonConfig (AppContext (..), appjwtSettings)
import Database.Redis (connect, defaultConnectInfo)
import Db
import Network.Wai.Handler.Warp (run)

-- import Mailing
-- import Data.Text (Text)

main :: IO ()
main = do
    postPool <- createDbPool
    redisPool <- connect defaultConnectInfo
    jwtSettings <- appjwtSettings
    let appContext = AppContext{redisPool, postPool, jwtConfig = jwtSettings}
    app' <- app appContext
    print "making typescript client"
    print "server running on 3000"
    run 3000 app'
