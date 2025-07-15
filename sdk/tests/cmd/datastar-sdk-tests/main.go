package main

import (
	"flag"
	"fmt"
	"os"
	"testing"

	sdktests "github.com/starfederation/datastar/sdk/tests"
)

func main() {
	var (
		verbose   = flag.Bool("v", false, "Verbose output")
		testType  = flag.String("type", "all", "Test type: get, post, or all")
		help      = flag.Bool("h", false, "Show help")
	)
	flag.Parse()

	if *help {
		fmt.Println("datastar-sdk-tests - Datastar SDK test suite")
		fmt.Println("\nUsage:")
		fmt.Println("  datastar-sdk-tests [options]")
		fmt.Println("\nOptions:")
		flag.PrintDefaults()
		fmt.Println("\nExamples:")
		fmt.Println("  datastar-sdk-tests                    # Run all tests")
		fmt.Println("  datastar-sdk-tests -type get          # Run only GET tests")
		fmt.Println("  datastar-sdk-tests -server http://localhost:8080")
		os.Exit(0)
	}

	// Create a testing.M to run tests
	var tests []testing.InternalTest

	switch *testType {
	case "get":
		tests = append(tests, testing.InternalTest{
			Name: "TestSSEGetEndpoints",
			F:    sdktests.TestSSEGetEndpoints,
		})
	case "post":
		tests = append(tests, testing.InternalTest{
			Name: "TestSSEPostEndpoints",
			F:    sdktests.TestSSEPostEndpoints,
		})
	case "all":
		tests = append(tests, 
			testing.InternalTest{
				Name: "TestSSEGetEndpoints",
				F:    sdktests.TestSSEGetEndpoints,
			},
			testing.InternalTest{
				Name: "TestSSEPostEndpoints",
				F:    sdktests.TestSSEPostEndpoints,
			},
		)
	default:
		fmt.Fprintf(os.Stderr, "Unknown test type: %s\n", *testType)
		os.Exit(1)
	}

	// Set verbose flag if requested
	if *verbose {
		os.Args = append(os.Args, "-test.v")
	}

	// Run tests using testing.Main
	testing.Main(
		func(pat, str string) (bool, error) { return true, nil }, // matchString
		tests,     // tests
		nil,       // benchmarks
		nil,       // examples
	)
}
