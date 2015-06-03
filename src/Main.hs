{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Servant
-- import Servant.Utils.StaticFiles
--
import           App.User
import           App.Events

import           Network.Wai.Handler.Warp

type API =  Raw

apiProxy :: Proxy API
apiProxy = Proxy

main = do

    let server :: Server API
        server = serveDirectory "static"

        -- app :: Application
        app = serve apiProxy server

    run 8081 app

