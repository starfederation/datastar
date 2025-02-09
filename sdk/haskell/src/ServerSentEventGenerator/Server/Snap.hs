{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ServerSentEventGenerator.Server.Snap
  (
    runSSE
  , sendInApp
  , send
  , readSignals
  ) where

import Control.Concurrent
    ( threadDelay, forkIO, killThread, myThreadId, ThreadId )
import Control.Exception
    ( SomeException, handle, throwIO, Exception(displayException) )
import Control.Monad ( forever )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Aeson ( decode, decodeStrictText, Value )
import Data.ByteString.Builder ( Builder, hPutBuilder, stringUtf8 )
import Data.ByteString.Builder.Extra ( flush )
import Prelude
import Data.Text ( Text )
import Data.Text.Encoding ( encodeUtf8Builder )
import ServerSentEventGenerator
    ( HttpVersion(..),
      singleThreaded,
      SSEapp(..),
      SSEstream,
      sseHeaders )
import ServerSentEventGenerator.Class ()
import Snap
    ( Snap,
      Request(rqVersion),
      escapeHttp,
      getParam,
      getRequest,
      readRequestBody,
      getHeader )
import System.IO ( stdout )
import qualified System.IO.Streams as Streams ( write )
import qualified Data.Text as T ( pack )
import qualified Data.Text.Encoding as T ( decodeUtf8 )

type Tickle = (Int -> Int) -> IO ()

-- set to True to see the text sent to the client on stdout as well,
-- VERY handy for debugging your Datastar code

debug :: Bool
debug = False

pb :: Builder -> IO ()
pb x = if debug then singleThreaded (hPutBuilder stdout x) else return ()

runSSE :: SSEapp -> Snap ()
runSSE (SSEapp app) = do
  request <- Snap.getRequest
  let lastId = getHeader "Last-Event-ID" request
  headers <- sseHeaders
  Snap.escapeHttp $ \tickle _ writeEnd -> do
      singleThreaded $ pb ("Enter runSSE, Last-Event-ID: " <> (stringUtf8 . show) lastId <> "\n")
      pingThreadId <-forkIO (ping tickle writeEnd)
      handle (handleException pingThreadId "runSSE") $ do
        singleThreaded $ do
          pb headers
          Streams.write (Just headers) writeEnd
          Streams.write (Just flush) writeEnd
          app writeEnd
          killThread pingThreadId
          Streams.write Nothing writeEnd

sendInApp :: Text -> SSEstream -> IO ()
sendInApp x writeEnd = singleThreaded $ do
  let bs = encodeUtf8Builder x
  pb bs
  Streams.write (Just bs) writeEnd
  Streams.write (Just flush) writeEnd

send :: Text -> Snap ()
send txt = do
  runSSE (SSEapp f)
  where
    f = sendInApp txt

ping :: Tickle -> SSEstream -> IO ()
ping tickle writeEnd = forever $ do
  pingThreadId <- myThreadId
  singleThreaded $ pb ("PING: " <> (stringUtf8 . show) pingThreadId)
  handle (handleException pingThreadId "ping") $ do
    singleThreaded $ do
      Streams.write  (Just ":\n\n") writeEnd
      Streams.write  (Just flush) writeEnd
  tickle (max 60)
  threadDelay (11 * 1000 * 1000)

instance HttpVersion Snap  where
  isHttpVersion1_1 = do
    version <- rqVersion <$> getRequest
    return (version == (1,1))

handleException :: ThreadId -> String -> SomeException -> IO ()
handleException t s e = do
  killThread t
  putStrLn (s <> ": " <> displayException e)
  throwIO e

{- | >>> :{
do
  signalsAsJsonIO (Nothing, "{\"key1\":{\"one\":1,\"two\":2},\"key2\":\"string\"}")    >>= print
  signalsAsJsonIO (Just "[{\"key1\":{\"one\":1,\"two\":2},\"key2\":\"string\"}]", "")  >>= print
:}
Object (fromList [("key1",Object (fromList [("one",Number 1.0),("two",Number 2.0)])),("key2",String "string")])
Array [Object (fromList [("key1",Object (fromList [("one",Number 1.0),("two",Number 2.0)])),("key2",String "string")])]
-}

-- the OPTIONS_GHC -Wno-missing-signatures is above for this function
-- signalsAsJsonIO :: (Maybe (Map ByteString [ByteString]), ByteString) -> IO Value
signalsAsJsonIO (mbDS,body) = do
  -- if its url encoded, then we only care about one key, named "datastar", 
  -- and the value is a bytestring encoded list with one element
  case mbDS of
      Nothing -> do
        let result = decode body  :: Maybe Value
        maybe (throwIO (JsonBodyException (T.pack . show $ body) )) return result
      Just ds -> do 
        let  txt = T.decodeUtf8 ds
        case ((decodeStrictText txt) :: Maybe Value) of
          Nothing -> throwIO $ JsonDecodingException txt
          Just v -> return v

signalsAsJson :: Snap Value
signalsAsJson = do
  mbDS   <- getParam "datastar"
  body   <- readRequestBody 1024
  liftIO $ signalsAsJsonIO (mbDS,body)

-- | Get the current request, and return the value (as per Aeson) of the datastar parameter
--   which may be in the query parameters for a GET, PuT, etc request, or in the request body
--   if we have a POST request

readSignals :: Snap (Request, Value)
readSignals = do
  req   <- getRequest
  value <- signalsAsJson
  return (req,value)

data SnapExceptions = 
    JsonBodyException       !Text
  | JsonTextTooShort        !Text
  | JsonDecodingException   !Text

  deriving Show
instance Exception SnapExceptions

