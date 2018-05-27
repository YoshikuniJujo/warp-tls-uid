{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.WarpTLS.UserId (
	GroupName, UserName, runTlsWithGroupUserName
) where

import Control.Exception (bracket)
import Data.Streaming.Network (bindPortTCP)
import System.Posix (
	groupID, getGroupEntryForName, setGroupID,
	userID, getUserEntryForName, setUserID )
import Network.Socket (Socket, withSocketsDo, close)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
	Settings, HostPreference, getPort, getHost )
import Network.Wai.Handler.WarpTLS (runTLSSocket, tlsSettingsMemory)

import qualified Data.ByteString as BS

type GroupName = String
type UserName = String

runTlsWithGroupUserName :: (FilePath, FilePath) ->
	(GroupName, UserName) -> Settings -> Application -> IO ()
runTlsWithGroupUserName (crt, key) (g, u) set app = do
	!c <- BS.readFile crt
	!k <- BS.readFile key
	let	tset = tlsSettingsMemory c k
	withSocketsDo $ bracket
		(bindPortTCPWithName (g, u) (getPort set) (getHost set))
		close
		(\sock -> runTLSSocket tset set sock app)

bindPortTCPWithName ::
	(GroupName, UserName) -> Int -> HostPreference -> IO Socket
bindPortTCPWithName (g, u) p h = (bindPortTCP p h <*) $ do
	getGroupEntryForName g >>= setGroupID . groupID
	getUserEntryForName u >>= setUserID . userID
