# Haskell SDK for Datastar

After familiarizing yourself with the functionality of Datastar, this
haskell interface basically comes down to a few main functions
specified in the Datastar sdK

    mergeFragments  :: Text -> Selector -> MergeMode -> FragmentOptions -> Options -> Text
    removeFragments :: Selector  -> FragmentOptions -> Options -> Text
    mergeSignals    :: Text -> Bool -> Options -> Text
    removeSignals   :: [Text] -> Options -> Text
    executeScript   :: Text -> Text -> Bool -> Options -> Text
    send            :: Text -> Snap ()       -- !!Only for Snap web server!!
    readSignals     :: Snap (Request, Value) -- !!Only for Snap web server!!

However, most likely you will usually take the defaults, which means
you will actually type:

    withDefaults :: EventType -> Text -> Text
    withDefaults MergeFragments  txt
    withDefaults RemoveFragments txt
    withDefaults MergeSignals    txt
    withDefaults RemoveSignals   txt
    withDefaults ExecuteScript   txt

Additionally you, dear user, will need to implement a web server
dependent function named **send** that sends the text you created to
the client. I have included a sample implementation for the Snap
server, in the ServerSentEventGenerator.Server.Snap module, please
have a look at it.  If you implement a similar module for you server
of choice, please create a pull request so I can include it.

You will also need a small function called isHttpVersion1_1 which
returns True if you server speaks Http Version 1.1, and False
otherwise.  This is needed to produce the correct headers for sse
events.

You will notice a Bool named debug, in the
ServerSentEventGenerator.Server.Snap module which is currently set to
False.  Setting it to True will enable debug messages printed to
stdout so you can see what is being sent to the client (web browser)

The readSignals function probably does not do what you expect.  It
returns a Data.Aeson Object, which is generally useless for Haskell
programmers.  You are better off reading the from your server and
dealing with it directly.

Finally, the demo, which you can try out by typing "./run" in the 
top level directory and pointing your browser to:
  http://localhost:8000/
will give you a simple demo of some Datastar features and show that
streaming SSE events to the browser work. 

The code itself is extensively documented, with many doctest examples
that show up in the Haddock files.

Best wishes,  
Henry Laxen  
nadine.and.henry@pobox.com  
http://nadineloveshenry.com/  

