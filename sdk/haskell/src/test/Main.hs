{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Default ( Default(def) )
import NeatInterpolation ( trimming )
import Prelude
import ServerSentEventGenerator
    ( test,
      executeScript,
      mergeFragments,
      mergeSignals,
      removeFragments,
      removeSignals )
import ServerSentEventGenerator.Types
    ( AutoRemove(Auto),
      FragmentOptions(FO),
      MergeMode(Morph, Append),
      Options(O),
      Selector(SEL) )

main :: IO ()
main = do
  let
    yourOptions = O "event1" 2000
    oneTwo      = [trimming|{"one":1,"two":2}|]
    firstSecond = [trimming|{"one":"first\\n signal","two":"second signal"}|]
    paths1      = ["one"]
    paths2      = ["one", "two.alpha"]
    script1     = "console.log('hello')"
    script2     = "if (true) {\n  console.log('hello');\n}"
    attributes1 = "type: text/javascript\nblocking: false"
    attributes2 = "type: module"
  test [
      mergeFragments "<div>Merge</div>" (SEL "div") Append (FO 1000 True) yourOptions
    , mergeFragments "<div>Merge</div>" def Morph (FO 1000 True) def
    , mergeFragments "<div>\n  <span>Merge</span>\n</div>" def def def def
    , mergeFragments "<div>Merge</div>" def def def def
    , removeFragments (SEL  "#target") (FO 2000 True) yourOptions
    , removeFragments (SEL  "#target") (FO 300  False) def
    , removeFragments (SEL  "#target") def def
    , mergeSignals oneTwo True yourOptions
    , mergeSignals firstSecond True def
    , mergeSignals oneTwo def def
    , removeSignals paths2 yourOptions
    , removeSignals paths1 def
    , executeScript script1 attributes1 (Auto False) yourOptions
    , executeScript script1 attributes2 (Auto True)  def
    , executeScript script2 def def def
    ]

