package main

import (
	"fmt"
	"log/slog"
	"net/http"
	"sync"
	"time"

	"github.com/starfederation/datastar/sdk/go/datastar"
)

const (
	serverAddress = "localhost:9001"
	cdn           = "https://cdn.jsdelivr.net/gh/starfederation/datastar@develop/bundles/datastar.js"
)

var hotReloadOnlyOnce sync.Once

func HotReloadHandler(w http.ResponseWriter, r *http.Request) {
	sse := datastar.NewSSE(w, r)
	hotReloadOnlyOnce.Do(func() {
		// Refresh the client page as soon as connection
		// is established. This will occur only once
		// after the server starts.
		sse.ExecuteScript(
			"window.location.reload()",
			datastar.WithExecuteScriptRetryDuration(time.Second),
		)
	})

	// Freeze the event stream until the connection
	// is lost for any reason. This will force the client
	// to attempt to reconnect after the server reboots.
	<-r.Context().Done()
}

func PageWithHotReload(w http.ResponseWriter, r *http.Request) {
	_, _ = w.Write([]byte(fmt.Sprintf(`
		<!DOCTYPE html>
		<html lang="en">

		<head>
			<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0" />
			<script type="module" defer src="%s"></script>
		</head>

		<!-- next line mounts the refresh script -->
		<body data-on-load="@get('/hotreload', {retryInterval: 100})">
			<main>
				<p>
				  This page will automatically reload on any filesystem change. Update this paragraph, save changes, and
					switch back to the open browser tab to observe
					the update.
				</p>
			</main>
		</body>

		</html>
	`, cdn)))
}

func main() {
	// Hot reload requires a file system watcher and
	// a refresh script. Reflex is one of the best tools
	// for running a command on each file change.
	//
	// $ go install github.com/cespare/reflex@latest
	// $ reflex --start-service -- sh -c 'go run .'
	//
	// The refresh script is a Datastar handler
	// that emits a page refresh event only once
	// for each server start.
	//
	// When the the file watcher forces the server to restart,
	// Datastar client will lose the network connection to the
	// server and attempt to reconnect. Once the connection is
	// established, the client will receive the refresh event.
	http.HandleFunc("/hotreload", HotReloadHandler)
	http.HandleFunc("/", PageWithHotReload)
	slog.Info(fmt.Sprintf(
		"Open your browser to: http://%s/",
		serverAddress,
	))
	http.ListenAndServe(serverAddress, nil)

	// Tip: read the reflex documentation to see advanced usage
	// options like responding to specific file changes by filter.
	//
	// $ reflex --help
}
