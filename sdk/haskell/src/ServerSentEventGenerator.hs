module ServerSentEventGenerator  (
    AutoRemove(..)
  , EventType(..)
  , FragmentOptions(..)
  , HttpVersion(..)
  , MergeMode(..)
  , Options(..)
  , Prompt(..)
  , SSEapp(..)
  , SSEstream
  , Selector(..)
  , executeScript
  , mergeFragments
  , mergeSignals
  , removeFragments
  , removeSignals
  , sendPure
  , singleThreaded
  , sseHeaders
  , test
  , withDefaults
  -- $setup
  )

where

import Data.ByteString.Builder ( Builder )
import Data.Default ( Default(..) )
import Prelude
import Data.Text ( Text, lines, unlines )
import ServerSentEventGenerator.Class
    ( HttpVersion(..), Prompt(..) )
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Types

-- $setup
-- >>> import Data.Text
-- >>> import Data.Maybe
-- >>> import Control.Exception

sseHeaders      :: HttpVersion m => m Builder
sendPure        :: EventType -> [Text] -> Options -> Text
mergeFragments  :: Text -> Selector -> MergeMode -> FragmentOptions -> Options -> Text
removeFragments :: Selector -> FragmentOptions -> Options -> Text
mergeSignals    :: Text -> Bool -> Options -> Text
removeSignals   :: [Text] -> Options -> Text
executeScript   :: Text -> Text -> AutoRemove -> Options -> Text

-- | The sseHeaders output if the server uses Http Version 1.1
-- >>> sseHeaders
-- "HTTP/1.1 200 OK\nCache-control: no-cache\nContent-type: text/event-stream\nConnection: keep-alive\n\n"

sseHeaders = do
  b <- isHttpVersion1_1
  let headers = if b then sseHeaders1_1 else sseHeaders2
  return (headers <> "\n")
  where
    sseHeaders2 = "HTTP/1.1 200 OK\nCache-control: no-cache\nContent-type: text/event-stream\n"
    sseHeaders1_1 = sseHeaders2 <> "Connection: keep-alive\n"

-- | All server sent events can contain and Event Id and a Retry Duration as an option
--   This works, because if the options are equal to their defaults, they will
--   be removed from the output
{- | >>> :{
do
  let
    sampleDataLines :: [Text]
    sampleDataLines = ["data: line 1","data: line 2"]
    them = [
      sendPure MergeFragments sampleDataLines (O "id1" 100) ]
  test them
:}
event: datastar-merge-fragments
id: id1
retry: 100
data: line 1
data: line 2
<BLANKLINE>
-}

sendPure eventType dataLines options = mconcat (buildLines (a:b:dataLines)) <> "\n\n"
  where
    a = cEvent <> cSColon <> prompt eventType
    b = prompt options

{- | >>> :{
do
  let
    sampleDataLines :: Text
    sampleDataLines = "line 1\nline 2"
    them = [
        mergeFragments sampleDataLines def def def def
      , mergeFragments sampleDataLines (SEL "#id") def def def
      , mergeFragments sampleDataLines (SEL "#id") Inner def def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10) ]
  test them
:}
event: datastar-merge-fragments
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: selector #id
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: selector #id
data: mergeMode inner
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: selector #id
data: mergeMode inner
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
id: abc123
retry: 10
data: selector #id
data: mergeMode inner
data: useViewTransition true
data: fragments line 1
data: fragments line 2
<BLANKLINE>
-}

mergeFragments fragments selector mode fragOptions =  sendPure MergeFragments (buildLines (a:b:c:d))
  where
    a = prompt selector
    b = withDefault cMergeMode (prompt (def :: MergeMode)) (prompt mode)
    c = prompt fragOptions
    d = withList cFragments fragments

{- | >>> :{
do
  let
    rt1 :: IO ()
    rt2,rt3,rt4,rt5 :: Text
    rt1 = test [removeFragments def def def] `catch`
             (\(e :: ServerSentEventGeneratorExceptions) -> print e)
    rt2 = removeFragments (SEL "#id") def def
    rt3 = removeFragments (SEL "#id") (FO 1 False) def
    rt4 = removeFragments (SEL "#id") (FO 1 True) def
    rt5 = removeFragments (SEL "#id") (FO 1 False) (O "abc123" 10)
  rt1 >> test [rt2,rt3,rt4,rt5]
:}
The selector field is required in RemoveFragment
event: datastar-remove-fragments
data: selector #id
<BLANKLINE>
event: datastar-remove-fragments
data: selector #id
<BLANKLINE>
event: datastar-remove-fragments
data: selector #id
data: useViewTransition true
<BLANKLINE>
event: datastar-remove-fragments
id: abc123
retry: 10
data: selector #id
<BLANKLINE>
-}

removeFragments selector fragOptions = sendPure RemoveFragments (buildLines [a,b])
  where
    s = prompt selector
    a = if s == def then bug RemoveFragmentSelectorIsMissing else s
    b = prompt fragOptions

{- | >>> :{
do
  let
    testMergeSignal :: Text
    testMergeSignal = "{\"a\":\"b\",\"c\":true,\"d\":1}"
    mst1 = test [mergeSignals def def def] `catch`
            (\(e :: ServerSentEventGeneratorExceptions) -> print e)
    them = [
        mergeSignals  testMergeSignal False def
     ,  mergeSignals  testMergeSignal True (O "abc123" 10) ]
  mst1 >> test them
:}
The selector field is required in MergeSignals
event: datastar-merge-signals
data: signals {"a":"b","c":true,"d":1}
<BLANKLINE>
event: datastar-merge-signals
id: abc123
retry: 10
data: signals {"a":"b","c":true,"d":1}
data: onlyIfMissing true
<BLANKLINE>
-}

mergeSignals signals onlyIfMissing = sendPure MergeSignals (buildLines [a,b])
  where
    a = if signals == mempty
          then bug SignalsSelectorIsMissing
          else withDefault cSignals "" signals
    b = withDefault cOnlyIfMissing (prompt cDefaultOnlyIfMissing) (prompt onlyIfMissing)

{- | >>> :{
do
  let
    testRemoveSignal = ["velocity.x", "velocity.y", "position"] :: [Text]
    them = [
        removeSignals [] def
      , removeSignals  testRemoveSignal def
      , removeSignals  testRemoveSignal (O "abc123" 10) ]
  test them
:}
event: datastar-remove-signals
<BLANKLINE>
event: datastar-remove-signals
data: paths velocity.x
data: paths velocity.y
data: paths position
<BLANKLINE>
event: datastar-remove-signals
id: abc123
retry: 10
data: paths velocity.x
data: paths velocity.y
data: paths position
<BLANKLINE>
-}

removeSignals paths = sendPure RemoveSignals (buildLines c)
  where
    c = Prelude.map (prefixed cPaths) paths

{- | >>> :{
do
  let
    testScript     = "window.location = \"https://data-star.dev\"" :: Text
    testAttributes = "type text/javascript" :: Text
    them = [
        executeScript "" "" (Auto True) def
      , executeScript  testScript "" (Auto False) def
      , executeScript  testScript testAttributes (Auto False) def
      , executeScript  testScript testAttributes def (O "abc123" 10)  ]
  test them
:}
event: datastar-execute-script
<BLANKLINE>
event: datastar-execute-script
data: autoRemove false
data: script window.location = "https://data-star.dev"
<BLANKLINE>
event: datastar-execute-script
data: attributes type text/javascript
data: autoRemove false
data: script window.location = "https://data-star.dev"
<BLANKLINE>
event: datastar-execute-script
id: abc123
retry: 10
data: attributes type text/javascript
data: script window.location = "https://data-star.dev"
<BLANKLINE>
-}

executeScript script attributes autoRemove = sendPure ExecuteScript (buildLines (a <> (b:c)))
  where
    filtered = filter (/= cDefaultAttributes) (Data.Text.lines attributes)
    a = withList cAttributes (Data.Text.unlines filtered)
    b = withDefault cAutoRemove (prompt (def :: AutoRemove)) (prompt autoRemove)
    c = withList cScript script

withDefaults :: EventType -> Text -> Text
withDefaults MergeFragments  txt = mergeFragments txt def def def def
withDefaults RemoveFragments txt = removeFragments (SEL txt) def def
withDefaults MergeSignals    txt = mergeSignals txt def def
withDefaults RemoveSignals   txt = removeSignals (Data.Text.lines txt) def
withDefaults ExecuteScript   txt = executeScript txt def def def
{- | >>> :{
do
  let
    them = [
        withDefaults MergeFragments  "abc123"
      , withDefaults RemoveFragments "abc123"
      , withDefaults MergeSignals    "abc123"
      , withDefaults RemoveSignals   "abc123"
      , withDefaults ExecuteScript   "abc123" ]
  test them
:}
event: datastar-merge-fragments
data: fragments abc123
<BLANKLINE>
event: datastar-remove-fragments
data: selector abc123
<BLANKLINE>
event: datastar-merge-signals
data: signals abc123
<BLANKLINE>
event: datastar-remove-signals
data: paths abc123
<BLANKLINE>
event: datastar-execute-script
data: script abc123
<BLANKLINE>
-}
