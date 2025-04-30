package datastar

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strconv"
	"time"

	"github.com/valyala/bytebufferpool"
)

var (
	// ErrNoPathsProvided is returned when no paths were provided for // for [sse.RemoveSignals] call.
	ErrNoPathsProvided = errors.New("no paths provided")
)

// mergeSignalsOptions holds configuration options for merging signals.
type mergeSignalsOptions struct {
	EventID       string
	RetryDuration time.Duration
	OnlyIfMissing bool
}

// MergeSignalsOption configures one [EventTypeMergeSignals] event.
type MergeSignalsOption func(*mergeSignalsOptions)

// WithMergeSignalsEventID configures an optional event ID for the signals merge event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithMergeSignalsEventID(id string) MergeSignalsOption {
	return func(o *mergeSignalsOptions) {
		o.EventID = id
	}
}

// WithMergeSignalsRetryDuration overrides the [DefaultSseRetryDuration] for signal merging.
func WithMergeSignalsRetryDuration(retryDuration time.Duration) MergeSignalsOption {
	return func(o *mergeSignalsOptions) {
		o.RetryDuration = retryDuration
	}
}

// WithOnlyIfMissing instructs the client to only merge signals if they are missing.
func WithOnlyIfMissing(onlyIfMissing bool) MergeSignalsOption {
	return func(o *mergeSignalsOptions) {
		o.OnlyIfMissing = onlyIfMissing
	}
}

// MergeSignals sends a [EventTypeMergeSignals] to the client.
// Requires a JSON-encoded payload.
func (sse *ServerSentEventGenerator) MergeSignals(signalsContents []byte, opts ...MergeSignalsOption) error {
	options := &mergeSignalsOptions{
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
		EventTypeMergeSignals,
		dataRows,
		sendOptions...,
	); err != nil {
		return fmt.Errorf("failed to send merge signals: %w", err)
	}
	return nil
}

// RemoveSignals sends a [EventTypeRemoveSignals] event to the client.
// Requires a non-empty list of paths.
func (sse *ServerSentEventGenerator) RemoveSignals(paths ...string) error {
	if len(paths) == 0 {
		return ErrNoPathsProvided
	}

	dataRows := make([]string, 0, len(paths))
	for _, path := range paths {
		dataRows = append(dataRows, PathsDatalineLiteral+path)
	}

	if err := sse.Send(
		EventTypeRemoveSignals,
		dataRows,
	); err != nil {
		return fmt.Errorf("failed to send remove signals: %w", err)
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
