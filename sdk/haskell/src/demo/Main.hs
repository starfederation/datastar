module Main where

import Control.Applicative ( Alternative((<|>)) )
import Control.Concurrent ( threadDelay )
import Control.Monad ( foldM_ )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Default ( Default(def) )
import Data.Maybe ( fromMaybe )
import Prelude
import Data.Text ( Text )
import Data.Time ( getCurrentTime )
import ServerSentEventGenerator
import Snap
import Snap.Util.FileServe ( serveDirectory )
import ServerSentEventGenerator.Server.Snap
import System.IO
    ( stdout, hSetBuffering, stderr, BufferMode(NoBuffering) )
import qualified Data.Text as T
    ( concatMap, pack, singleton, unpack )
import qualified Data.Text.IO as T

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  indexText <- T.readFile "src/demo/www/index.html"
  let
    mbPort    = getPort (defaultConfig :: Config Snap a)
    newConfig = setPort (fromMaybe 8000 mbPort) (defaultConfig :: Config Snap a)
  conf <- commandLineConfig newConfig
  print conf
  simpleHttpServe conf (site indexText)

site :: Text -> Snap ()
site indexText =
    ifTop (writeText indexText) <|>
    route [
        ("favicon.ico" , return ())
      , ("feed"        , handlerFeed)
      , ("keats"       , handlerKeats)
      , ("signals"     , handlerSignals)
      , ("clear"       , handlerClear)
      , ("test"       , handlerSignals)
      ] <|> serveDirectory "demo/www"

handlerSignals  :: Snap ()
handlerSignals = do
  ps "handlerSignals"
  req    <- T.pack . show <$> getRequest
  body   <- T.pack . show <$> readRequestBody 1024
  params <- T.pack . show <$> getParams
  let
    output = mconcat [
      "<pre>"
      , "\n<b>Request</b>\n"
      , req
      , "\n<b>Params</b>\n"
      , params
      , "\n<b>Body</b>\n"
      , body
      , "\n<b>End</b>\n"
      , "</pre>"
      ]
    ds = mergeFragments (output) (SEL "#signals") Inner def def
  send ds

handlerClear :: Snap ()
handlerClear = send $ (mergeFragments "<div/>" (SEL "#signals") Inner def def)
  
handlerFeed :: Snap ()
handlerFeed = do
  runSSE (SSEapp f)
  where
    f :: SSEstream -> IO ()
    f w = do
      let x10times = [1..10] :: [Int]
      putStrLn "Write 10 times"
      mapM_ (writeNow w) x10times
      writeBoth sleeping w
      sleep 70
      putStrLn "Wake up"
      putStrLn "Write 10 times"
      mapM_ (writeNow w) x10times
      writeBoth allDone w
      sendInApp removeDstar w
    writeNow :: SSEstream -> Int -> IO ()
    writeNow w n = do
      now <- getCurrentTime >>=
        return . T.pack . ((Prelude.replicate n '.') <> ) . show
      sendInApp (feedDstar now) w
      threadDelay (1 * 1000 * 1000)
    writeBoth x w = putStrLn (T.unpack x) >> sendInApp (feedDstar x) w
    sleeping = "Sleeping for 70 seconds, but continuing to ping"
    allDone  = "All Done, the settleDuration to remove the div is 5 seconds"
    feedDstar :: Text -> Text
    feedDstar x = mergeFragments ("<div id=\"feed\"><b>" <> x <> "</b></div>") def def def def
    removeDstar :: Text
    removeDstar = removeFragments (SEL "#explain") (FO 5000 def) def

handlerKeats :: Snap ()
handlerKeats = do
  liftIO $ putStrLn "Keats"
  ode <- liftIO  $ T.readFile "src/demo/www/keats.txt"
  runSSE (SSEapp (f ode))
  where
    f ::  Text -> SSEstream -> IO ()
    f ode w =  foldM_ (\x -> foldSlowly w x) mempty (T.unpack ode)
    keatsDstar :: Text -> Text
    keatsDstar x =  mergeFragments ("<div>" <> textToHtml x <> "</div>") (SEL "#keats") Inner def def
    foldSlowly :: SSEstream -> Text ->  Char -> IO Text
    foldSlowly w b c = do
      pause
      let s = b <> (T.singleton c)
      sendInApp (keatsDstar s) w
      return s

pause :: IO ()
pause = threadDelay (10 * 100 * 100 `div` 2)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000 * 1000)

textToHtml :: Text -> Text 
textToHtml = T.concatMap escape
  where
    escape ' '  = T.pack "&nbsp;"
    escape '\n' = T.pack "<br/>\n"
    escape '<'  = T.pack "&lt;"
    escape '>'  = T.pack "&gt;"
    escape '&'  = T.pack "&amp;"
    escape c    = T.singleton c

--    <script type="module" src="datastar.js"></script>

ps :: Text ->  Snap ()
ps =  liftIO . T.putStrLn
