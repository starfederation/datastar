{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Class where

import Data.Default ( Default(..) )
import Prelude
import Data.Text ( Text )
import Data.Text.IO ( putStr )
import Data.String

type StringLike a = (Eq a, IsString a, Monoid a)

class Monad m => HttpVersion m where
  -- | Are we running Http Version 1.1? Needed to send out the correct headers
  --   This needs to be implemented and depends on with web server you are using
  --   The default is to just return True
  isHttpVersion1_1 :: m Bool
  isHttpVersion1_1 = return True

instance HttpVersion IO 

class Monad m => Send m where
  send :: Text -> m ()

instance Send IO where
  send = Data.Text.IO.putStr

instance Default Text where
  def = ""

class Prompt a where
  prompt :: a -> Text

-- | I need a way to go from a Datastar type to a StringLike thing that can be
--   sent to the browser.  The Prompt class lets me do things like
--   prompt MergeFragments = "datastar-merge-fragments" without specifying
--   the exact type of the StringLike "datastar-merge-fragments" I also use it
--   to special case the output for Options

instance Prompt Bool where
  prompt True  = "true"
  prompt False = "false"

instance Prompt Int where
  prompt = fromString . show

