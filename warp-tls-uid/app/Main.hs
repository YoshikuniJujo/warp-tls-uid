module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder.Char.Utf8

import Network.Wai.Handler.WarpTLS

server :: Application
server _ f = f . responseBuilder status200 [] $ fromString "hello"

main :: IO ()
main = runTLS
	(tlsSettings "first_cert.pem" "first_key_ngrd.pem")
	(setPort 8080 defaultSettings)
	server
