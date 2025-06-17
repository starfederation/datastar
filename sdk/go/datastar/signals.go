package datastar

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
	"time"

	"github.com/valyala/bytebufferpool"
)

// patchSignalsOptions holds configuration options for patching signals.
type patchSignalsOptions struct {
	EventID       string
	RetryDuration time.Duration
	OnlyIfMissing bool
}

// PatchSignalsOption configures one [EventTypePatchSignals] event.
type PatchSignalsOption func(*patchSignalsOptions)

// WithPatchSignalsEventID configures an optional event ID for the signals patch event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithPatchSignalsEventID(id string) PatchSignalsOption {
	return func(o *patchSignalsOptions) {
		o.EventID = id
	}
}

// WithPatchSignalsRetryDuration overrides the [DefaultSseRetryDuration] for signal patching.
func WithPatchSignalsRetryDuration(retryDuration time.Duration) PatchSignalsOption {
	return func(o *patchSignalsOptions) {
		o.RetryDuration = retryDuration
	}
}

// WithOnlyIfMissing instructs the client to only patch signals if they are missing.
func WithOnlyIfMissing(onlyIfMissing bool) PatchSignalsOption {
	return func(o *patchSignalsOptions) {
		o.OnlyIfMissing = onlyIfMissing
	}
}

// PatchSignals sends a [EventTypePatchSignals] to the client.
// Requires a JSON-encoded payload.
func (sse *ServerSentEventGenerator) PatchSignals(signalsContents []byte, opts ...PatchSignalsOption) error {
	options := &patchSignalsOptions{
		EventID:       "",
		RetryDuration: DefaultSseRetryDuration,
		OnlyIfMissing: false,
	}
	for _, opt := range opts {
		opt(options)
	}

	dataRows := make([]string, 0, 32)
	if options.OnlyIfMissing {
		dataRows = append(dataRows, OnlyIfMissingDatalineLiteral+strconv.FormatBool(options.OnlyIfMissing))
	}
	lines := bytes.Split(signalsContents, newLineBuf)
	for _, line := range lines {
		dataRows = append(dataRows, SignalsDatalineLiteral+string(line))
	}

	sendOptions := make([]SSEEventOption, 0, 2)
	if options.EventID != "" {
		sendOptions = append(sendOptions, WithSSEEventId(options.EventID))
	}
	if options.RetryDuration != DefaultSseRetryDuration {
		sendOptions = append(sendOptions, WithSSERetryDuration(options.RetryDuration))
	}

	if err := sse.Send(
		EventTypePatchSignals,
		dataRows,
		sendOptions...,
	); err != nil {
		return fmt.Errorf("failed to send patch signals: %w", err)
	}
	return nil
}

// ReadSignals extracts Datastar signals from
// an HTTP request and unmarshals them into the signals target,
// which should be a pointer to a struct.
//
// Expects signals in [URL.Query] for [http.MethodGet] requests.
// Expects JSON-encoded signals in [Request.Body] for other request methods.
func ReadSignals(r *http.Request, signals any) error {
	var dsInput []byte

	if r.Method == "GET" {
		dsJSON := r.URL.Query().Get(DatastarKey)
		if dsJSON == "" {
			return nil
		} else {
			dsInput = []byte(dsJSON)
		}
	} else {
		buf := bytebufferpool.Get()
		defer bytebufferpool.Put(buf)
		if _, err := buf.ReadFrom(r.Body); err != nil {
			if err == http.ErrBodyReadAfterClose {
				return fmt.Errorf("body already closed, are you sure you created the SSE ***AFTER*** the ReadSignals? %w", err)
			}
			return fmt.Errorf("failed to read body: %w", err)
		}
		dsInput = buf.Bytes()
	}

	if err := json.Unmarshal(dsInput, signals); err != nil {
		return fmt.Errorf("failed to unmarshal: %w", err)
	}
	return nil
}
