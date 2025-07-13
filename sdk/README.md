# Datastar SDK Directory

This directory contains the SDK test suite and related tools for Datastar SDK development.

## Structure

- `tests/` - Comprehensive test suite for validating SDK implementations
- `datastar-sdk-config.json` - SDK configuration with constants and defaults
- `datastar-sdk-config.schema.json-v1.json` - JSON schema for configuration validation
- `ADR.md` - Architecture Decision Record for SDK specifications

## SDK Test Suite

The `tests/` directory contains a Go-based test suite that validates Datastar SDK implementations across different languages.

### Installation

```bash
go get github.com/starfederation/datastar/sdk/tests
```

### Usage

#### Running Tests

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

#### Using Task

```bash
# Run all tests
task test-sdk

# Run with custom server URL
task test-sdk SERVER=http://localhost:9000
```

### Test Structure

- `tests/golden/get/` - Test cases for GET endpoints
- `tests/golden/post/` - Test cases for POST endpoints
- Each test case contains:
  - `input.json` - Request payload
  - `output.txt` - Expected SSE response

## SDK Development

### Configuration

The SDK configuration system provides:

- **Constants**: Event types, element patch modes, and other enums
- **Defaults**: Default values for booleans and durations
- **Literals**: List of known dataline literals
- **JSON Schema**: Validation for the configuration format

### Creating a New SDK

1. Read the [Architecture Decision Record](./ADR.md) for SDK specifications
2. Use [`datastar-sdk-config.json`](./datastar-sdk-config.json) as your source of constants
3. Implement the required `ServerSentEventGenerator` interface
4. Validate your implementation using the test suite

### Testing Your SDK

To validate your SDK implementation:

1. Start your test server on port 7331 (or specify a different port)
2. Implement the `/test` endpoint that:
   - For GET: reads the `datastar` query parameter
   - For POST: reads the JSON body
   - Returns appropriate SSE responses based on the `events` array
3. Run `go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest` to validate your implementation

## Features

- **HTML Normalization**: Test suite automatically handles HTML attribute ordering
- **Embedded Test Data**: All test cases are embedded in the binary for portability
- **Comprehensive Coverage**: Tests cover all SSE event types and edge cases
- **Clear Error Messages**: Detailed output helps identify implementation issues

## Official SDKs

| Language | Repository |
|----------|------------|
| Go | [starfederation/datastar-go](https://github.com/starfederation/datastar-go) |
| PHP | [starfederation/datastar-php](https://github.com/starfederation/datastar-php) |

## Community SDKs

We welcome community contributions! If you've created a Datastar SDK for a language not listed above, please open a PR to add it here.

## Support

For SDK-specific issues, please open an issue in the respective SDK repository. For general Datastar questions or cross-SDK concerns, use the main [Datastar repository](https://github.com/starfederation/datastar).