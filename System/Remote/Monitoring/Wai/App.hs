{-# LANGUAGE OverloadedStrings #-}

module System.Remote.Monitoring.Wai.App
    ( startServer
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Exception (throwIO)
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import Data.String
import qualified Data.Text as T
import Data.Word (Word8)
import Network.HTTP.Types.Status
import Network.Socket (NameInfoFlag(NI_NUMERICHOST), addrAddress, getAddrInfo,
                       getNameInfo)
import Paths_ekg_wai (getDataDir)
import Prelude hiding (read)
import System.FilePath ((</>))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static

import System.Metrics
import System.Remote.Monitoring.Wai.Json

------------------------------------------------------------------------

-- | Convert a host name (e.g. \"localhost\" or \"127.0.0.1\") to a
-- numeric host address (e.g. \"127.0.0.1\").
getNumericHostAddress :: BS.ByteString -> IO BS.ByteString
getNumericHostAddress host = do
    ais <- getAddrInfo Nothing (Just (BS8.unpack host)) Nothing
    case ais of
        [] -> unsupportedAddressError
        (ai:_) -> do
            ni <- getNameInfo [NI_NUMERICHOST] True False (addrAddress ai)
            case ni of
                (Just numericHost, _) -> return $! BS8.pack numericHost
                _ -> unsupportedAddressError
  where
    unsupportedAddressError = throwIO $
        userError $ "unsupported address: " ++ BS8.unpack host

startServer :: Store metrics
            -> BS.ByteString -- ^ Host to listen on (e.g. \"localhost\")
            -> Int -- ^ Port to listen on (e.g. 8000)
            -> IO ()
startServer store host port = do
    numericHost <- getNumericHostAddress host
    let conf =
            setHost (fromString (BS8.unpack numericHost)) $
            setPort port $
            defaultSettings
    runSettings conf (monitor store)

-- | A handler that can be installed into an existing Snap application.
monitor :: Store metrics -> Application
monitor store req respond = do
    dataDir <- liftIO getDataDir
    let acceptHdr = (List.head . parseHttpAccept) <$> acceptHeader req
    case acceptHdr of
        Just hdr | hdr == "application/json" && requestMethod req == "GET" ->
            serve store req respond
        _ -> do
            staticApp (defaultFileServerSettings $ dataDir </> "assets") req respond

-- | The Accept header of the request.
acceptHeader :: Request -> Maybe BS.ByteString
acceptHeader req = lookup "Accept" $ requestHeaders req

-- | Serve all counter, gauges and labels, built-in or not, as a
-- nested JSON object.
serve :: Store metrics -> Application
serve store req respond = do
    response <-
        case pathInfo req of
          [] -> serveAll
          segments -> serveOne segments
    respond response
  where
    respHeaders = [("Content-Type","application/json")]
    serveAll :: IO Response
    serveAll = do
        metrics <- liftIO $ sampleAll store
        return $ responseLBS status200 respHeaders $ encodeAll metrics
    serveOne :: [T.Text] -> IO Response
    serveOne segments = do
        let name = T.intercalate "." segments
        metrics <- liftIO $ sampleAll store
        let metrics' = M.filterWithKey (\k _v -> idName k == name) metrics
        return $
          if null metrics' then
            responseLBS status404 respHeaders "\"Metric not found\""
          else
            responseLBS status200 respHeaders $ encodeAll metrics'

------------------------------------------------------------------------
-- Utilities for working with accept headers

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: BS.ByteString -> [BS.ByteString]
parseHttpAccept = List.map fst
                . List.sortBy (rcompare `on` snd)
                . List.map grabQ
                . BS.split 44 -- comma
  where
    rcompare :: Double -> Double -> Ordering
    rcompare = flip compare
    grabQ s =
        let (s', q) = breakDiscard 59 s -- semicolon
            (_, q') = breakDiscard 61 q -- equals sign
         in (trimWhite s', readQ $ trimWhite q')
    readQ s = case reads $ BS8.unpack s of
                (x, _):_ -> x
                _ -> 1.0
    trimWhite = BS.dropWhile (== 32) -- space

breakDiscard :: Word8 -> BS.ByteString -> (BS.ByteString, BS.ByteString)
breakDiscard w s =
    let (x, y) = BS.break (== w) s
    in (x, BS.drop 1 y)
