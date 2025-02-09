module Main where

import Control.Concurrent ( threadDelay )
import Control.Monad ( foldM_ )
import Data.Aeson
--     ( decodeStrict, (.:), withObject, FromJSON(parseJSON) )
import Data.Default ( Default(def) )
-- import Data.Scientific -- Needed for readSignals
import Relude
import ServerSentEventGenerator
    ( SSEapp(SSEapp), SSEstream, mergeFragments)
import ServerSentEventGenerator.Server.Snap ( runSSE, sendInApp, {- readSignals -} )
import Snap
    ( getParam,
      ifTop,
      writeText,
      route,
      simpleHttpServe,
      defaultConfig,
      Snap,
      Config )
import qualified Text.Show ( Show(show) )
import qualified Data.Text as T ( singleton, unpack )
import qualified Data.Text.IO as T ( readFile )

indexHtml :: FilePath
indexHtml = "src/examples/hello-world/hello-world.html"

main :: IO ()
main = simpleHttpServe (defaultConfig :: Config Snap a) site
site :: Snap ()
site =
  ifTop ((liftIO $ T.readFile indexHtml) >>= writeText)
  <|>
    route [
        ("favicon.ico" , return ())
      , ("hello-world" , handlerHelloWord)
      ]

newtype Delay = Delay Int
  deriving Show

instance FromJSON Delay where
    parseJSON = withObject "Delay" $ \v -> Delay
        <$> v .: "delay"

data DatastarDecodingException = DatastarDecodingException
instance Exception DatastarDecodingException
instance Show DatastarDecodingException where
  show DatastarDecodingException = "We did not find a delay parameter in the datastar GET"

handlerHelloWord :: Snap ()
handlerHelloWord = do
-- Here is how you do it with readSignals
--   (_,obj) <- readSignals
--   -- obj comes back as: Object (fromList [("delay",Number 400.0)])
--   let
--     Object a = obj
--     Just b = Data.Aeson.KeyMap.lookup "delay" a
--     Number c = b
--     Just d = toBoundedInteger c :: Maybe Int
--     delay = Delay  d

--   Here is how you do it directly
  mbDS <- getParam "datastar"
  let
    readDelay = do
      ds <- mbDS
      decodeStrict ds

  case readDelay of
    Nothing    -> bug DatastarDecodingException
    Just delay -> runSSE (SSEapp (f delay))
  where
    hello = "Hello, world!"
    merge x = mergeFragments ("<div id=\"message\">" <> x <> "</div>") def def def def
    f :: Delay -> SSEstream -> IO ()
    f (Delay n) w = foldM_ (\x -> foldSlowly x) mempty (T.unpack hello)
      where
        pause = threadDelay (n * 1000)
        foldSlowly b c = do
          pause
          let s = b <> (T.singleton c)
          sendInApp (merge s) w
          return s
