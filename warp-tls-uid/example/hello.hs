{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Network.Wai (Application, responseBuilder)
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS.UserId (runTlsWithGroupUserName)
import Network.HTTP.Types (status200)

server :: Application
server _ = ($ responseBuilder status200 [] "hello")

main :: IO ()
main = runTlsWithGroupUserName
	("first_cert.pem", "first_key_ngrd.pem")
	("tatsuya", "tatsuya")
	(setPort 8080 defaultSettings)
	server
