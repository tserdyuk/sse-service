
module Main where

import Data.Text (Text)
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Application, lazyRequestBody, pathInfo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnClose, setPort)
import SSE (SSErvice, bsEvent, emptyResponse, onClose, send, sservice, subscribe)


main :: IO ()
main = do
	sse <- sservice
	let settings = setPort 3000 $ setOnClose (onClose sse) defaultSettings
	runSettings settings (application sse)

application :: SSErvice Text -> Application
application sse request respond = return (pathInfo request) >>= \case
	["publish", key] -> lazyRequestBody request >>=
		(send sse key . bsEvent) >> respond (emptyResponse ok200)
	["subscribe", key] -> subscribe sse key request respond
	_ -> respond (emptyResponse notFound404)
