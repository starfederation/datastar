{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Types where

import Control.Exception ( throw, Exception )
import Data.ByteString.Builder ( Builder )
import Data.Default ( Default(..) )
import Data.Text ( Text )
import Prelude
import ServerSentEventGenerator.Class ( Prompt(..) )
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Internal
    ( buildLines, withDefault )
import qualified System.IO.Streams as Streams ( OutputStream )


type SSEstream = Streams.OutputStream Builder
newtype SSEapp = SSEapp (SSEstream -> IO ())

data Options = O {
    eventId       :: Text
  , retryDuration :: Int
  } deriving (Show)

instance Default Options where
  def = O {
    eventId = mempty
  , retryDuration = cDefaultSseRetryDuration
  }

instance Prompt Options where
  prompt options =
    let
      eI = eventId options
      eR = retryDuration options
      a = if eI == cDefaultEventId then mempty else cEventId <> cSColon <> eI
      b = if eR == cDefaultSseRetryDuration then mempty else cRetryDuration  <> cSColon <> (prompt eR)
    in mconcat . buildLines $ [a,b]

newtype Selector = SEL {unSelector :: Text}
  deriving (Show, Semigroup, Monoid, Eq)

instance Default Selector where
  def = SEL mempty

instance Prompt Selector where
  prompt (SEL x) = withDefault cSelector cDefaultSelector x

-- | A sum of the possible Datastar specific events that can be sent

data EventType =
    MergeFragments
  | RemoveFragments
  | MergeSignals
  | RemoveSignals
  | ExecuteScript
  deriving (Eq, Show)

instance Default EventType
  where def = MergeFragments

instance Prompt EventType where
  prompt MergeFragments   = cMergeFragments
  prompt RemoveFragments  = cRemoveFragments
  prompt MergeSignals     = cMergeSignals
  prompt RemoveSignals    = cRemoveSignals
  prompt ExecuteScript    = cExecuteScript


data FunctionExecuteScript =  FunctionExecuteScript {
    eType       :: EventType
  , eScript     :: Text
  , eAttributes :: Text
  , eAutoRemove :: Bool
  , eOptions    :: Options
  } deriving Show

-- | A sum of the possible Datastar specific merge modes that can be sent

data MergeMode =
     Morph
   | Inner
   | Outer
   | Prepend
   | Append
   | Before
   | After
   | UpsertAttributes
   deriving (Eq, Show)

instance Default MergeMode
  where def = Morph

instance Prompt MergeMode where
   prompt Morph            = cMorph
   prompt Inner            = cInner
   prompt Outer            = cOuter
   prompt Prepend          = cPrepend
   prompt Append           = cAppend
   prompt Before           = cBefore
   prompt After            = cAfter
   prompt UpsertAttributes = cUpsertAttributes

data FragmentOptions = FO {
    useViewTransition :: Bool
  } deriving (Show)

-- | the MergeFragments and RemoveFragment data types share these options

instance Default FragmentOptions where
  def = FO {
    useViewTransition  = cDefaultFragmentsUseViewTransitions
  }

instance Prompt FragmentOptions where
  prompt (FO a) = mconcat . buildLines $ [
      withDefault cUseViewTransition (prompt cDefaultFragmentsUseViewTransitions) (prompt a)
    ]

newtype AutoRemove = Auto Bool
  deriving (Eq, Show)

instance Default AutoRemove where
  def = Auto True

instance Prompt AutoRemove where
  prompt (Auto True)  = "true"
  prompt (Auto False) = "false"

instance Show ServerSentEventGeneratorExceptions where
 show BuildLineDataIMissing           = "buildLine was call with nothing to build"
 show RemoveFragmentSelectorIsMissing = "The selector field is required in RemoveFragment"
 show SignalsSelectorIsMissing        = "The selector field is required in MergeSignals"
 show RemoveSignalsPathIsMissing      = "The path field is required in RemoveSignals"
 show RemoveSignalsPathIsEmpty        = "The path field cannot be an empty list"
 show ExecuteScriptIsMissing          = "The script field is required in ExecuteScript"

bug :: Exception e => e -> a
bug = throw

data ServerSentEventGeneratorExceptions =
   BuildLineDataIMissing
 | RemoveFragmentSelectorIsMissing
 | SignalsSelectorIsMissing
 | RemoveSignalsPathIsMissing
 | RemoveSignalsPathIsEmpty
 | ExecuteScriptIsMissing

instance Exception ServerSentEventGeneratorExceptions
