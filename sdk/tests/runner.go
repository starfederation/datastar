package sdktests

import (
	"bytes"
	"embed"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"path/filepath"
	"time"
)

// TestRunner provides methods to run SDK tests programmatically
type TestRunner struct {
	ServerURL string
	Client    *http.Client
}

// NewTestRunner creates a new test runner
func NewTestRunner(serverURL string) *TestRunner {
	if serverURL == "" {
		serverURL = "http://localhost:7331"
	}
	return &TestRunner{
		ServerURL: serverURL,
		Client: &http.Client{
			Timeout: 10 * time.Second,
		},
	}
}

// RunGetTest runs a single GET test case
func (tr *TestRunner) RunGetTest(inputData []byte) ([]byte, error) {
	u, err := url.Parse(tr.ServerURL + "/test")
	if err != nil {
		return nil, err
	}

	q := u.Query()
	q.Set("datastar", string(inputData))
	u.RawQuery = q.Encode()

	req, err := http.NewRequest("GET", u.String(), nil)
	if err != nil {
		return nil, err
	}

	req.Header.Set("Accept", "text/event-stream")
	req.Header.Set("datastar-request", "true")

	return tr.makeRequest(req)
}

// RunPostTest runs a single POST test case
func (tr *TestRunner) RunPostTest(inputData []byte) ([]byte, error) {
	req, err := http.NewRequest("POST", tr.ServerURL+"/test", bytes.NewReader(inputData))
	if err != nil {
		return nil, err
	}

	req.Header.Set("Accept", "text/event-stream")
	req.Header.Set("datastar-request", "true")
	req.Header.Set("Content-Type", "application/json")

	return tr.makeRequest(req)
}

func (tr *TestRunner) makeRequest(req *http.Request) ([]byte, error) {
	resp, err := tr.Client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("server returned %d: %s", resp.StatusCode, body)
	}

	return io.ReadAll(resp.Body)
}

// GetTestData returns the embedded test data filesystem
func GetTestData() embed.FS {
	return testData
}

// TestCase represents a single test case
type TestCase struct {
	Name     string
	Input    []byte
	Expected []byte
}

// GetTestCases returns all test cases for a given type
func GetTestCases(testType string) ([]TestCase, error) {
	var dir string
	switch testType {
	case "get":
		dir = "golden/get"
	case "post":
		dir = "golden/post"
	default:
		return nil, fmt.Errorf("unknown test type: %s", testType)
	}

	entries, err := testData.ReadDir(dir)
	if err != nil {
		return nil, err
	}

	var cases []TestCase
	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}

		inputPath := filepath.Join(dir, entry.Name(), "input.json")
		outputPath := filepath.Join(dir, entry.Name(), "output.txt")

		input, err := testData.ReadFile(inputPath)
		if err != nil {
			continue
		}

		expected, err := testData.ReadFile(outputPath)
		if err != nil {
			continue
		}

		cases = append(cases, TestCase{
			Name:     entry.Name(),
			Input:    input,
			Expected: expected,
		})
	}

	return cases, nil
}