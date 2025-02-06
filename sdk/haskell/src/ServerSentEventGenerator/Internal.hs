module ServerSentEventGenerator.Internal where

import Control.Concurrent ( newMVar, putMVar, takeMVar )
import Control.Exception ( bracket )
import Data.String ( IsString )
import Prelude
import Data.Text ( Text, lines )
import ServerSentEventGenerator.Class ( StringLike )
import ServerSentEventGenerator.Constants
    ( cData, cSColon, cSpace )
import qualified Data.Text.IO ( putStr )


-- | Combines a list of Texts into a single Text, using the same mechanism
--   as the more commonly known functions unWords or unLines.  A line feed is
--   inserted between each builder in the list.  Empty builders are removed, so
--   there are no blank lines.

buildLines :: (Eq a, Monoid a, IsString a) => [a] -> [a]
buildLines texts = if (mconcat texts) == mempty then [] else [go mempty texts]
  where
    go acc []     = acc
    go acc [x]    = x <> acc
    go acc [x,y]  = case [x,y] of
      ["",z] -> z <> acc
      [z,""] -> z <> acc
      [u,v] -> u <> "\n" <> v <> acc
    go acc (b:bs) = if b == mempty then go acc bs else b <> "\n" <> go acc bs

{- | >>> :{
do
  let
    wa        = "a"      :: Text
    wb        = "b"      :: Text
    prefix    = "prefix" :: Text
    enclose              :: Text -> Text
    enclose x = "[" <> x <> "]\n"
    them = map enclose [
         withDefault prefix wa wa
       , withDefault prefix wa wa
       , withDefault prefix wb wa
       , withDefault prefix wa wb
       , withDefault prefix "" ""
       , withDefault prefix wa ""
       , withDefault prefix "" wa ]
  test them
 :}
[]
[]
[data: prefix a]
[data: prefix b]
[]
[]
[data: prefix a]
-}

prefixed :: StringLike a => a -> a -> a
prefixed name =  ( (cData <> cSColon <> name <> cSpace) <> )

withDefault :: StringLike a => a -> a -> a -> a
withDefault dStarEvent defaultValue value =
  if value == defaultValue || value == mempty
  then mempty
  else prefixed dStarEvent value

-- | Insert "data: " and the given text in front of each element of the list
-- | >>> withList "fragments" ["l1","l2"]
--   ["data: fragments l1","data: fragments l2"]

withList :: Text -> Text -> [Text]
withList name =  Prelude.map (prefixed name) . Data.Text.lines

-- | Handy little helpers to watch the result of sending stuff through sse

-- | send a list of text to stdout
test :: [Text] -> IO ()
test = mapM_ ps

-- send a line of text to stdout
ps :: Text ->  IO ()
ps =  Data.Text.IO.putStr

-- | Make sure the IO action runs without interference from other threads

singleThreaded :: IO () -> IO ()
singleThreaded action = bracket
    (newMVar ())
    (\mvar -> putMVar mvar ())
    (\mvar -> takeMVar mvar >> action)

