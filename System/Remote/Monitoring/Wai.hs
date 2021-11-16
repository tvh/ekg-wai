{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides remote monitoring of a running process over
-- HTTP.  It can be used to run an HTTP server that provides both a
-- web-based user interface and a machine-readable API (e.g. JSON.)
-- The former can be used by a human to get an overview of what the
-- program is doing and the latter can be used by automated monitoring
-- tools.
--
-- Typical usage is to start the monitoring server at program startup
--
-- > main = do
-- >     forkServer "localhost" 8000
-- >     ...
--
-- and then periodically check the stats using a web browser or a
-- command line tool (e.g. curl)
--
-- > $ curl -H "Accept: application/json" http://localhost:8000/
module System.Remote.Monitoring.Wai
    (
      -- * Required configuration
      -- $configuration

      -- * Security considerations
      -- $security

      -- * REST API
      -- $api

      -- * The monitoring server
      Server
    , serverThreadId
    , serverMetricStore
    , forkServer
    , forkServerWith
    ) where

import Control.Concurrent (ThreadId, myThreadId, throwTo)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding (read)
import qualified Data.ByteString as BS

import qualified System.Metrics as Metrics
import System.Remote.Monitoring.Wai.App
import Network.Socket (withSocketsDo)

import Control.Concurrent (forkFinally)

-- $configuration
--
-- To make full use out of this module you must first enable GC
-- statistics collection in the run-time system. To enable GC
-- statistics collection, either run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.

-- $security
-- Be aware that if the server started by 'forkServer' is not bound to
-- \"localhost\" (or equivalent) anyone on the network can access the
-- monitoring server. Either make sure the network is secure or bind
-- the server to \"localhost\".

-- $api
-- To use the machine-readable REST API, send an HTTP GET request to
-- the host and port passed to 'forkServer'.
--
-- The API is versioned to allow for API evolution. This document is
-- for version 1. To ensure you're using this version, append @?v=1@
-- to your resource URLs. Omitting the version number will give you
-- the latest version of the API.
--
-- The following resources (i.e. URLs) are available:
--
-- [\/] JSON object containing all metrics. Metrics are stored as
-- nested objects, with one new object layer per \".\" in the metric
-- name (see example below.) Content types: \"text\/html\" (default),
-- \"application\/json\"
--
-- [\/\<namespace\>/\<metric\>] JSON object for a single metric. The
-- metric name is created by converting all \"\/\" to \".\". Example:
-- \"\/foo\/bar\" corresponds to the metric \"foo.bar\". Content
-- types: \"application\/json\"
--
-- Each metric is returned as an object containing a @type@ field.  Available types
-- are:
--
--  * \"c\" - 'Counter.Counter'
--
--  * \"g\" - 'Gauge.Gauge'
--
--  * \"l\" - 'Label.Label'
--
--  * \"d\" - 'Distribution.Distribution'
--
-- In addition to the @type@ field, there are metric specific fields:
--
--  * Counters, gauges, and labels: the @val@ field contains the
--    actual value (i.e. an integer or a string).
--
--  * Distributions: the @mean@, @variance@, @count@, @sum@, @min@,
--    and @max@ fields contain their statistical equivalents.
--
-- Example of a response containing the metrics \"myapp.visitors\" and
-- \"myapp.args\":
--
-- > {
-- >   "myapp": {
-- >     "visitors": {
-- >       "val": 10,
-- >       "type": "c"
-- >     },
-- >     "args": {
-- >       "val": "--a-flag",
-- >       "type": "l"
-- >     }
-- >   }
-- > }

------------------------------------------------------------------------
-- * The monitoring server

-- | A handle that can be used to control the monitoring server.
-- Created by 'forkServer'.
data Server = Server {
      -- | The thread ID of the server. You can kill the server by
      -- killing this thread (i.e. by throwing it an asynchronous
      -- exception.)
      serverThreadId :: {-# UNPACK #-} !ThreadId

      -- | The metric store associated with the server. If you want to
      -- add metric to the default store created by 'forkServer' you
      -- need to use this function to retrieve it.
    , serverMetricStore :: {-# UNPACK #-} !(Metrics.Store Metrics.AllMetrics)
    }

-- | Like 'forkServerWith', but creates a default metric store with
-- some predefined metrics. The predefined metrics are those given in
-- 'System.Metrics.registerGcMetrics'.
forkServer :: BS.ByteString -- ^ Host to listen on (e.g. \"localhost\")
           -> Int -- ^ Port to listen on (e.g. 8000)
           -> IO Server
forkServer host port = do
    store <- Metrics.newStore
    _ <- Metrics.register
          (Metrics.subset Metrics.ofAll store)
          Metrics.registerGcMetrics
    forkServerWith store host port

-- | Start an HTTP server in a new thread.  The server replies to GET
-- requests to the given host and port.  The host argument can be
-- either a numeric network address (dotted quad for IPv4,
-- colon-separated hex for IPv6) or a hostname (e.g. \"localhost\".)
-- The client can control the Content-Type used in responses by
-- setting the Accept header.  At the moment two content types are
-- available: \"application\/json\" and \"text\/html\".
--
-- Registers the following counter, used by the UI:
--
-- [@ekg.server_time_ms@] The server time when the sample was taken,
-- in milliseconds.
--
-- Note that this function, unlike 'forkServer', doesn't register any
-- other predefined metrics. This allows other libraries to create and
-- provide a metric store for use with this library. If the metric
-- store isn't created by you and the creator doesn't register the
-- metrics registered by 'forkServer', you might want to register them
-- yourself.
forkServerWith :: Metrics.Store Metrics.AllMetrics -- ^ Metric store
               -> BS.ByteString -- ^ Host to listen on (e.g. \"localhost\")
               -> Int -- ^ Port to listen on (e.g. 8000)
               -> IO Server
forkServerWith store host port = do
    _ <- Metrics.register store $
          Metrics.registerCounter
            (Metrics.Metric @"ekg.server_timestamp_ms") () getTimeMs
    me <- myThreadId
    tid <- withSocketsDo $ forkFinally (startServer store host port) $ \ r ->
        case r of
            Left e  -> throwTo me e
            Right _ -> return ()
    return $! Server tid store
  where
    getTimeMs :: IO Int64
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime
