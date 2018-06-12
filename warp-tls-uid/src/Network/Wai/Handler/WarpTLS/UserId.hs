{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Network.Wai.Handler.WarpTLS.UserId (
	CertFile, KeyFile, GroupName, UserName, runTlsWithGroupUserName
) where

import Control.Arrow ((***), (&&&))
import Control.Exception (bracket)
import Data.Semigroup ((<>))
import Data.List (unfoldr)
import Data.Streaming.Network (bindPortTCP)
import System.Posix (
	groupID, getGroupEntryForName, setGroupID,
	userID, getUserEntryForName, setUserID )
import Network.Socket (Socket, withSocketsDo, close)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
	Settings, HostPreference, getPort, getHost )
import Network.Wai.Handler.WarpTLS (runTLSSocket, tlsSettingsChainMemory)

import qualified Data.ByteString as BS

type CertFile = FilePath
type KeyFile = FilePath

type GroupName = String
type UserName = String

runTlsWithGroupUserName :: (CertFile, KeyFile) ->
	(GroupName, UserName) -> Settings -> Application -> IO ()
runTlsWithGroupUserName (crt, key) (g, u) set app = do
	(!c, !cs) <- separateChain <$> BS.readFile crt
	!k <- BS.readFile key
	let	tset = tlsSettingsChainMemory c cs k
	withSocketsDo $ bracket
		(bindPortTCPWithName (g, u) (getPort set) (getHost set))
		close
		(\sock -> runTLSSocket tset set sock app)

bindPortTCPWithName ::
	(GroupName, UserName) -> Int -> HostPreference -> IO Socket
bindPortTCPWithName (g, u) p h = (bindPortTCP p h <*) $ do
	getGroupEntryForName g >>= setGroupID . groupID
	getUserEntryForName u >>= setUserID . userID

separateChain :: BS.ByteString -> (BS.ByteString, [BS.ByteString])
separateChain = (head &&& tail) . separate

endCertificate :: BS.ByteString
endCertificate = "-----END CERTIFICATE-----"

separate :: BS.ByteString -> [BS.ByteString]
separate = unfoldr separateOne

separateOne :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
separateOne "" = Nothing
separateOne "\n" = Nothing
separateOne ccs = Just (c <> ec, cs)
	where
	(c, (ec, cs)) = id *** BS.splitAt (BS.length endCertificate)
		$ BS.breakSubstring endCertificate ccs
