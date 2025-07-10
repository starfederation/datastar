# Datastar SDK testing suite

This test suite uses curl(1), cat(1), sh(1), and awk(1) to test that a server respects the SDK spec found in `../README.md`.

It expects a server to expose a `/test` endpoint that accepts all HTTP methods. The server should then use ReadSignals
to extract the `events` array. It must then loop through the array of events and use `event.type` to decide which server sent event to use. If the output of the server differs from the expected output, then an error will be printed to the terminal.

## Usage

```
$ ./test-all.sh $server_address
Running tests with argument: $server_address
Processing GET cases...
Processing POST cases...
```

If nothing else is output then all tests passed!

Results of the test can be found in `./get_cases/$case_name/testOutput.txt` (or `post_cases` depending on the test).

## Adding new cases

To add a new test case, simply add a folder named after the test in either `./get-cases` or `./post-cases`.

That folder must contain an `input.json` file and an `output.txt` file.

The `input.json` file must contain valid json of the following shape:

```
{"events":
  [
    { "type": "executeScript",
      "script": "console.log('hello');",
      "eventId": 1,
      "retryDuration": 2000,
      "attributes": {
        "type": "text/javascript",
        "blocking": false
      },
      "autoRemove": false
     }
   ]
}
```

The `output.txt` file must contain valid a `text/eventstream` like such:

```
event: datastar-patch-elements
id: 1
retry: 2000
elements: <script type="text/javascript" blocking="false">console.log('hello')</script>;
```

### Special case for multiline signals

For the event type `patchSignals` the `input.json` contains the `signals` as JSON-object which should be converted to a single signals line in the `output.txt`. 

If you want to output multi-line signals, then the input must contain `signals-raw` as String with `\n` in them instead. This is due to the fact that Json parsers would otherwise interpret the input file without the line breaks.

So the impementation of the server has to interpret `signals-raw` as String first, and if not present `signals` as JSON-object.
