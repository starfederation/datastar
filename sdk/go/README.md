# Go SDK for Datastar

[![Go Reference](https://pkg.go.dev/badge/github.com/starfederation/datastar.svg)](https://pkg.go.dev/github.com/starfederation/datastar)

Implements the [SDK spec](../README.md) and exposes an abstract
ServerSentEventGenerator struct that can be used to implement runtime specific
classes.

Usage is straightforward:

```go
package main

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"log/slog"
	"net/http"
	"os"
	"time"

	datastar "github.com/starfederation/datastar/sdk/go"
)

const port = 9001

func main() {
	logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))
	mux := http.NewServeMux()

	cdn := "https://cdn.jsdelivr.net/gh/starfederation/datastar@develop/bundles/datastar.js"
	style := "display:flex;flex-direction:column;background-color:oklch(25.3267% 0.015896 252.417568);height:100vh;justify-content:center;align-items:center;font-family:ui-sans-serif, system-ui, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';"

	page := []byte(fmt.Sprintf(`
<!DOCTYPE html>
<html lang="en">
<head>
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0" />
    <script type="module" defer src="%s"></script>
</head>
<body style="%s">
    <span id="feed" data-on-load="%s"></span>
</body>
</html>
	`, cdn, style, datastar.GetSSE("/stream")))

	mux.HandleFunc("GET /", func(w http.ResponseWriter, r *http.Request) {
		w.Write(page)
	})

	mux.HandleFunc("GET /stream", func(w http.ResponseWriter, r *http.Request) {
		ticker := time.NewTicker(100 * time.Millisecond)
		defer ticker.Stop()

		sse := datastar.NewSSE(w, r)

		for {
			select {

			case <-r.Context().Done():
				logger.Debug("Client connection closed")
				return

			case <-ticker.C:
				bytes := make([]byte, 3)

				_, err := rand.Read(bytes)
				if err != nil {
					logger.Error("Error generating random bytes: ", slog.String("error", err.Error()))
					return
				}

				hexString := hex.EncodeToString(bytes)

				frag := fmt.Sprintf(`<span id="feed" style="color:#%s;border:1px solid #%s;border-radius:0.25rem;padding:1rem;">%s</span>`, hexString, hexString, hexString)

				sse.MergeFragments(frag)
			}
		}
	})

	logger.Info(fmt.Sprintf("Server starting at 0.0.0.0:%d", port))
	if err := http.ListenAndServe(fmt.Sprintf("0.0.0.0:%d", port), mux); err != nil {
		logger.Error("Error starting server:", slog.String("error", err.Error()))
	}

}
```

## Examples

The [Datastar website](https://data-star.dev) acts as a [set of examples](https://github.com/starfederation/datastar/tree/develop/site) for how to use the SDK.
