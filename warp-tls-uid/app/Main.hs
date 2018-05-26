{-# LANGUAGE BangPatterns #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder.Char.Utf8

import Network.Wai.Handler.WarpTLS

import qualified Data.ByteString as BS
import Network.Socket
import Control.Exception (bracket)
import Data.Streaming.Network (bindPortTCP)
import System.Posix

server :: Application
server _ f = f . responseBuilder status200 [] $ fromString "hello"

main :: IO ()
main = runTlsWithId
	("first_cert.pem", "first_key_ngrd.pem")
	("tatsuya", "tatsuya")
	(setPort 8080 defaultSettings)
	server

runTlsWithId ::
	(FilePath, FilePath) -> (Group, Usr) -> Settings -> Application -> IO ()
runTlsWithId (crt, key) (g, u) set app = do
	!c <- BS.readFile crt
	!k <- BS.readFile key
	let	tset = tlsSettingsMemory c k
	withSocketsDo $ bracket
		(bindPortTCPWithId (g, u) (getPort set) (getHost set))
		close
		(\sock -> runTLSSocket tset set sock app)

type Group = String
type Usr = String

bindPortTCPWithId :: (Group, Usr) -> Int -> HostPreference -> IO Socket
bindPortTCPWithId (g, u) p h = (bindPortTCP p h <*) $ do
	getGroupEntryForName g >>= setGroupID . groupID
	getUserEntryForName u >>= setUserID . userID
