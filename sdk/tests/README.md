# Datastar SDK Test Suite

A comprehensive test suite for validating Datastar SDK implementations across different languages.

## Installation

### As a Go Library

```bash
go get github.com/starfederation/datastar/sdk/tests
```

## Usage

### Running Tests

```bash
# Run all tests against default server (localhost:7331)
go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest

# Run with custom server
go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest -server http://localhost:8080

# Run only GET tests
go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest -type get

# Run only POST tests  
go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest -type post

# Verbose output
go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest -v
```

### Using go test directly

```bash
# Clone the repository and navigate to tests directory
cd sdk/tests

# Run all tests
go test -v

# Run with custom server
TEST_SERVER_URL=http://localhost:8080 go test -v
```

## Test Structure

The test suite includes:

- **GET Tests** (`golden/get/`): Test cases for GET endpoints
- **POST Tests** (`golden/post/`): Test cases for POST endpoints

Each test case contains:
- `input.json`: The request payload
- `output.txt`: The expected SSE response

## Features

- **HTML Normalization**: Automatically handles HTML attribute ordering differences
- **Embedded Test Data**: All test cases are embedded in the binary for portability
- **Flexible Runner**: Can be used as a CLI tool or Go library
- **Detailed Output**: Clear error messages and debug information

## For SDK Implementers

To validate your SDK implementation:

1. Start your test server on port 7331 (or specify a different port)
2. Implement the `/test` endpoint that:
   - For GET: reads the `datastar` query parameter
   - For POST: reads the JSON body
   - Returns appropriate SSE responses
3. Run `go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest` to validate your implementation

## Test Endpoint Requirements

The `/test` endpoint should:

1. Use ReadSignals to extract the `events` array from the request
2. Loop through the array of events 
3. Use `event.type` to decide which server sent event to generate
4. Return the appropriate SSE response

### Input Format

```json
{
  "events": [
    {
      "type": "executeScript",
      "script": "console.log('hello');",
      "eventId": "event1",
      "retryDuration": 2000,
      "attributes": {
        "type": "text/javascript",
        "blocking": "false"
      },
      "autoRemove": false
    }
  ]
}
```

### Output Format

```
event: datastar-patch-elements
id: event1
retry: 2000
data: mode append
data: selector body
data: elements <script type="text/javascript" blocking="false">console.log('hello');</script>

```

## Adding New Test Cases

To add a new test case:

1. Create a folder in `golden/get/` or `golden/post/` named after your test
2. Add an `input.json` file with the request payload
3. Add an `output.txt` file with the expected SSE response

### Special Cases

#### Multiline Signals

For `patchSignals` events, if you need multiline output:
- Use `signals-raw` as a string with `\n` characters instead of `signals` as a JSON object
- The server should check for `signals-raw` first, then fall back to `signals`

## Test Cases

The test suite covers:

- Element patching (single and multiline)
- Signal patching  
- Script execution
- Element/signal removal
- Various SSE formatting scenarios
- Edge cases and error conditions
