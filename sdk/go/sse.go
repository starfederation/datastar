package datastar

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"sync"
	"time"

	"github.com/valyala/bytebufferpool"
)

// ServerSentEventGenerator streams events into
// an [http.ResponseWriter]. Each event is flushed immediately.
type ServerSentEventGenerator struct {
	ctx             context.Context
	mu              *sync.Mutex
	w               io.Writer
	rc              *http.ResponseController
	shouldLogPanics bool
	encoding        string
	acceptEncoding  string
}

// SSEOption configures the initialization of an
// HTTP Server-Sent Event stream.
type SSEOption func(*ServerSentEventGenerator)

// NewSSE upgrades an [http.ResponseWriter] to an HTTP Server-Sent Event stream.
// The connection is kept alive until the context is canceled or the response is closed by returning from the handler.
// Run an event loop for persistent streaming.
func NewSSE(w http.ResponseWriter, r *http.Request, opts ...SSEOption) *ServerSentEventGenerator {
	rc := http.NewResponseController(w)

	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Content-Type", "text/event-stream")
	if r.ProtoMajor == 1 {
		w.Header().Set("Connection", "keep-alive")
	}

	sseHandler := &ServerSentEventGenerator{
		ctx:             r.Context(),
		mu:              &sync.Mutex{},
		w:               w,
		rc:              rc,
		shouldLogPanics: true,
		acceptEncoding:  r.Header.Get("Accept-Encoding"),
	}

	// apply options
	for _, opt := range opts {
		opt(sseHandler)
	}

	// set compression encoding
	if sseHandler.encoding != "" {
		w.Header().Set("Content-Encoding", sseHandler.encoding)
	}

	// flush headers
	if err := rc.Flush(); err != nil {
		// Below panic is a deliberate choice as it should never occur and is an environment issue.
		// https://crawshaw.io/blog/go-and-sqlite
		// In Go, errors that are part of the standard operation of a program are returned as values.
		// Programs are expected to handle errors.
		panic(fmt.Sprintf("response writer failed to flush: %v", err))
	}

	return sseHandler
}

// Context returns the context associated with the upgraded connection.
// It is equivalent to calling [request.Context].
func (sse *ServerSentEventGenerator) Context() context.Context {
	return sse.ctx
}

// serverSentEventData holds event configuration data for
// [SSEEventOption]s.
type serverSentEventData struct {
	Type          EventType
	EventID       string
	Data          []string
	RetryDuration time.Duration
}

// SSEEventOption modifies one server-sent event.
type SSEEventOption func(*serverSentEventData)

// WithSSEEventId configures an optional event ID for one server-sent event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithSSEEventId(id string) SSEEventOption {
	return func(e *serverSentEventData) {
		e.EventID = id
	}
}

// WithSSERetryDuration overrides the [DefaultSseRetryDuration] for
// one server-sent event.
func WithSSERetryDuration(retryDuration time.Duration) SSEEventOption {
	return func(e *serverSentEventData) {
		e.RetryDuration = retryDuration
	}
}

var (
	eventLinePrefix = []byte("event: ")
	idLinePrefix    = []byte("id: ")
	retryLinePrefix = []byte("retry: ")
	dataLinePrefix  = []byte("data: ")
)

func writeJustError(w io.Writer, b []byte) (err error) {
	_, err = w.Write(b)
	return err
}

// Send emits a server-sent event to the client. Method is safe for
// concurrent use.
func (sse *ServerSentEventGenerator) Send(eventType EventType, dataLines []string, opts ...SSEEventOption) error {
	sse.mu.Lock()
	defer sse.mu.Unlock()

	// create the event
	evt := serverSentEventData{
		Type:          eventType,
		Data:          dataLines,
		RetryDuration: DefaultSseRetryDuration,
	}

	// apply options
	for _, opt := range opts {
		opt(&evt)
	}

	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)

	// write event type
	if err := errors.Join(
		writeJustError(buf, eventLinePrefix),
		writeJustError(buf, []byte(evt.Type)),
		writeJustError(buf, newLineBuf),
	); err != nil {
		return fmt.Errorf("failed to write event type: %w", err)
	}

	// write id if needed
	if evt.EventID != "" {
		if err := errors.Join(
			writeJustError(buf, idLinePrefix),
			writeJustError(buf, []byte(evt.EventID)),
			writeJustError(buf, newLineBuf),
		); err != nil {
			return fmt.Errorf("failed to write id: %w", err)
		}
	}

	// write retry if needed
	if evt.RetryDuration.Milliseconds() > 0 && evt.RetryDuration.Milliseconds() != DefaultSseRetryDuration.Milliseconds() {
		retry := int(evt.RetryDuration.Milliseconds())
		retryStr := strconv.Itoa(retry)
		if err := errors.Join(
			writeJustError(buf, retryLinePrefix),
			writeJustError(buf, []byte(retryStr)),
			writeJustError(buf, newLineBuf),
		); err != nil {
			return fmt.Errorf("failed to write retry: %w", err)
		}
	}

	// write data lines
	for _, d := range evt.Data {
		if err := errors.Join(
			writeJustError(buf, dataLinePrefix),
			writeJustError(buf, []byte(d)),
			writeJustError(buf, newLineBuf),
		); err != nil {
			return fmt.Errorf("failed to write data: %w", err)
		}
	}

	// write double newlines to separate events
	if err := writeJustError(buf, doubleNewLineBuf); err != nil {
		return fmt.Errorf("failed to write newline: %w", err)
	}

	// copy the buffer to the response writer
	if _, err := buf.WriteTo(sse.w); err != nil {
		return fmt.Errorf("failed to write to response writer: %w", err)
	}

	// flush the write if its a compressing writer
	if f, ok := sse.w.(flusher); ok {
		if err := f.Flush(); err != nil {
			return fmt.Errorf("failed to flush compressing writer: %w", err)
		}
	}

	if err := sse.rc.Flush(); err != nil {
		return fmt.Errorf("failed to flush data: %w", err)
	}

	// log.Print(NewLine + buf.String())
	return nil
}
