package main

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"log/slog"
	"net/http"
	"time"

	"github.com/starfederation/datastar/sdk/go/datastar"
)

const (
	cdn  = "https://cdn.jsdelivr.net/gh/starfederation/datastar@develop/bundles/datastar.js"
	port = 9001
)

func main() {
	mux := http.NewServeMux()

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
				slog.Debug("Client connection closed")
				return
			case <-ticker.C:
				bytes := make([]byte, 3)

				if _, err := rand.Read(bytes); err != nil {
					slog.Error("Error generating random bytes: ", slog.String("error", err.Error()))
					return
				}
				hexString := hex.EncodeToString(bytes)
				frag := fmt.Sprintf(`<span id="feed" style="color:#%s;border:1px solid #%s;border-radius:0.25rem;padding:1rem;">%s</span>`, hexString, hexString, hexString)

				sse.PatchElements(frag)
			}
		}
	})

	slog.Info(fmt.Sprintf("Server starting at 0.0.0.0:%d", port))
	if err := http.ListenAndServe(fmt.Sprintf("0.0.0.0:%d", port), mux); err != nil {
		slog.Error("Error starting server:", slog.String("error", err.Error()))
	}
}
