package sdktests

import (
	"bytes"
	"embed"
  "flag"
	"fmt"
	"io"
	"io/fs"
	"net/http"
	"net/url"
	"os"
  "path"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"golang.org/x/net/html"
)

//go:embed golden
var testData embed.FS

var serverURL string

func init() {
  flag.StringVar(&serverURL, "server", "http://localhost:7331", "Server URL to test against")
}

// TestSSEGetEndpoints is an exported version of the GET endpoint tests
func TestSSEGetEndpoints(t *testing.T) {
	runTestCases(t, testData, "golden/get", runGetTest)
}

// TestSSEPostEndpoints is an exported version of the POST endpoint tests
func TestSSEPostEndpoints(t *testing.T) {
	runTestCases(t, testData, "golden/post", runPostTest)
}

func runTestCases(t *testing.T, embedFS embed.FS, casesDir string, runTest func(string, []byte) ([]byte, error)) {
	entries, err := fs.ReadDir(embedFS, casesDir)
	require.NoError(t, err, "Failed to read %s directory", casesDir)

	// Get unique test case names
	testCases := make(map[string]bool)
	for _, entry := range entries {
		if entry.IsDir() {
			testCases[entry.Name()] = true
		} else {
			// Extract test case name from file path
			dir := filepath.Dir(entry.Name())
			if dir != "." && dir != casesDir {
				testName := filepath.Base(dir)
				testCases[testName] = true
			}
		}
	}

	// Run each test case
	for testName := range testCases {
		testName := testName // capture for closure
		t.Run(testName, func(t *testing.T) {
      inputPath := path.Join(casesDir, testName, "input.json")
			outputPath := path.Join(casesDir, testName, "output.txt")

			// Read input from embedded FS
			inputData, err := embedFS.ReadFile(inputPath)
			require.NoError(t, err, "Failed to read input")

			// Read expected output from embedded FS
			expectedData, err := embedFS.ReadFile(outputPath)
			require.NoError(t, err, "Failed to read expected output")

			// Run test
			actualData, err := runTest(serverURL, inputData)
			require.NoError(t, err, "Request failed")

			// Compare
			err = compareSSE(t, expectedData, actualData)
			if err != nil {
				// Save actual output for debugging
				debugDir := filepath.Join("testdata", casesDir, testName)
				os.MkdirAll(debugDir, 0755)
				actualPath := filepath.Join(debugDir, "testOutput.txt")
				os.WriteFile(actualPath, actualData, 0644)
				
				t.Logf("Test case: %s", testName)
				t.Logf("Actual output saved to: %s", actualPath)
			}
		})
	}
}

func runGetTest(serverURL string, inputData []byte) ([]byte, error) {
	u, err := url.Parse(serverURL + "/test")
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

	return makeRequest(req)
}

func runPostTest(serverURL string, inputData []byte) ([]byte, error) {
	req, err := http.NewRequest("POST", serverURL+"/test", bytes.NewReader(inputData))
	if err != nil {
		return nil, err
	}

	req.Header.Set("Accept", "text/event-stream")
	req.Header.Set("datastar-request", "true")
	req.Header.Set("Content-Type", "application/json")

	return makeRequest(req)
}

func makeRequest(req *http.Request) ([]byte, error) {
	client := &http.Client{
		Timeout: 10 * time.Second,
	}

	resp, err := client.Do(req)
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

// SSE comparison functions

type SSEEvent struct {
	Fields map[string][]string
}

func compareSSE(t *testing.T, expected, actual []byte) error {
	expectedEvents, err := parseSSE(expected)
	require.NoError(t, err, "Failed to parse expected SSE")

	actualEvents, err := parseSSE(actual)
	require.NoError(t, err, "Failed to parse actual SSE")

	compareEvents(t, expectedEvents, actualEvents)
	return nil
}

func parseSSE(data []byte) ([]SSEEvent, error) {
	var events []SSEEvent
	var currentEvent *SSEEvent

	lines := strings.Split(string(data), "\n")
	for _, line := range lines {
		// Empty line marks end of event
		if line == "" {
			if currentEvent != nil && len(currentEvent.Fields) > 0 {
				events = append(events, *currentEvent)
				currentEvent = nil
			}
			continue
		}

		// Parse field
		colonIndex := strings.Index(line, ":")
		if colonIndex == -1 {
			continue
		}

		if currentEvent == nil {
			currentEvent = &SSEEvent{Fields: make(map[string][]string)}
		}

		fieldName := line[:colonIndex]
		fieldValue := strings.TrimSpace(line[colonIndex+1:])

		currentEvent.Fields[fieldName] = append(currentEvent.Fields[fieldName], fieldValue)
	}

	// Handle last event if file doesn't end with empty line
	if currentEvent != nil && len(currentEvent.Fields) > 0 {
		events = append(events, *currentEvent)
	}

	return events, nil
}

func compareEvents(t *testing.T, expected, actual []SSEEvent) {
	require.Equal(t, len(expected), len(actual), "Event count mismatch")

	for i := range expected {
		compareEvent(t, i+1, &expected[i], &actual[i])
	}
}

func compareEvent(t *testing.T, eventNum int, expected, actual *SSEEvent) {
	// Compare all non-data fields first
	for fieldName, expectedValues := range expected.Fields {
		if fieldName == "data" {
			continue
		}

		actualValues, ok := actual.Fields[fieldName]
		assert.True(t, ok, "Event %d: missing field '%s' in actual", eventNum, fieldName)
		assert.Equal(t, expectedValues, actualValues, "Event %d: mismatch in '%s' field", eventNum, fieldName)
	}

	// Check for extra fields in actual
	for fieldName := range actual.Fields {
		if fieldName == "data" {
			continue
		}
		_, ok := expected.Fields[fieldName]
		assert.True(t, ok, "Event %d: unexpected field '%s' in actual", eventNum, fieldName)
	}

	// Compare data fields with special handling
	expectedData := expected.Fields["data"]
	actualData := actual.Fields["data"]

	if len(expectedData) == 0 && len(actualData) == 0 {
		return
	}

	require.Equal(t, len(expectedData) > 0, len(actualData) > 0, "Event %d: data field presence mismatch", eventNum)

	// Parse and group data fields
	expectedGroups := parseDataFields(expectedData)
	actualGroups := parseDataFields(actualData)

	// Compare groups
	compareDataGroups(t, eventNum, expectedGroups, actualGroups)
}

func parseDataFields(fields []string) map[string][]string {
	groups := make(map[string][]string)

	for _, field := range fields {
		parts := strings.SplitN(field, " ", 2)
		if len(parts) >= 1 {
			subgroup := parts[0]
			content := ""
			if len(parts) > 1 {
				content = parts[1]
			}
			groups[subgroup] = append(groups[subgroup], content)
		}
	}

	return groups
}

func compareDataGroups(t *testing.T, eventNum int, expected, actual map[string][]string) {
	// Check all expected groups exist in actual
	for subgroup, expectedLines := range expected {
		actualLines, ok := actual[subgroup]
		assert.True(t, ok, "Event %d: missing data subgroup '%s' in actual", eventNum, subgroup)

		// Special handling for "elements" subgroup - normalize HTML
		if subgroup == "elements" {
			expectedHTML := strings.Join(expectedLines, "\n")
			actualHTML := strings.Join(actualLines, "\n")

			normalizedExpected := normalizeHTML(expectedHTML)
			normalizedActual := normalizeHTML(actualHTML)

			assert.Equal(t, normalizedExpected, normalizedActual,
				"Event %d: mismatch in data 'elements' content\nExpected:\n%s\nActual:\n%s",
				eventNum, expectedHTML, actualHTML)
		} else {
			// For non-elements, exact match
			assert.Equal(t, expectedLines, actualLines,
				"Event %d: mismatch in data '%s' content", eventNum, subgroup)
		}
	}

	// Check for extra subgroups in actual
	for subgroup := range actual {
		_, ok := expected[subgroup]
		assert.True(t, ok, "Event %d: unexpected data subgroup '%s' in actual", eventNum, subgroup)
	}
}

func normalizeHTML(htmlStr string) string {
	doc, err := html.Parse(strings.NewReader(htmlStr))
	if err != nil {
		// If parsing fails, return original
		return htmlStr
	}

	// Normalize attributes in all element nodes
	var normalize func(*html.Node)
	normalize = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Attr != nil && len(n.Attr) > 1 {
			// Sort attributes by key
			sort.Slice(n.Attr, func(i, j int) bool {
				return n.Attr[i].Key < n.Attr[j].Key
			})
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			normalize(c)
		}
	}
	normalize(doc)

	// Render back to string
	var buf bytes.Buffer
	html.Render(&buf, doc)
	
	// The parser adds <html><head></head><body>...</body></html>, so extract just the body content
	result := buf.String()
	
	// Extract content between <body> and </body>
	bodyStart := strings.Index(result, "<body>")
	bodyEnd := strings.Index(result, "</body>")
	
	if bodyStart != -1 && bodyEnd != -1 {
		result = result[bodyStart+6 : bodyEnd]
	}
	
	return strings.TrimSpace(result)
}
