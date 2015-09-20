
module Main where

import Control.Concurrent (forkIO)
import Data.ByteString as B (ByteString, tail)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Application, Request, lazyRequestBody, rawPathInfo)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setOnClose, setPort)
import ReadArgs (readArgs)
import SSE (SSErvice, bsEvent, cors, emptyResponse, onClose, send, sservice, subscribe)


main :: IO ()
main = do
	sse <- sservice
	(subPort, pubPort) <- readArgs
	let subSettings = setPort (fromMaybe 3000 subPort) $
		setOnClose (onClose sse) defaultSettings
	forkIO $ runSettings subSettings (cors $ subscriber sse)
	run (fromMaybe 5000 pubPort) (cors $ publisher sse)

subscriber :: SSErvice ByteString -> Application
subscriber sse request = subscribe sse (key request) request

publisher :: SSErvice ByteString -> Application
publisher sse request respond = lazyRequestBody request >>=
	(send sse (key request) . bsEvent) >> respond (emptyResponse ok200)

key :: Request -> ByteString
key = B.tail . rawPathInfo
