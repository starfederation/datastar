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

	"github.com/CAFxX/httpcompression/contrib/andybalholm/brotli"
	"github.com/CAFxX/httpcompression/contrib/compress/gzip"
	"github.com/CAFxX/httpcompression/contrib/compress/zlib"
	"github.com/CAFxX/httpcompression/contrib/klauspost/zstd"
	"github.com/valyala/bytebufferpool"

	zstd_opts "github.com/klauspost/compress/zstd"
)

type ServerSentEventGenerator struct {
	ctx             context.Context
	mu              *sync.Mutex
	w               io.Writer
	rc              *http.ResponseController
	shouldLogPanics bool
	encoding        string
}

type SSEOption func(*ServerSentEventGenerator)

type GzipOption func(*gzip.Options)

func WithGzipLevel(level int) GzipOption {
	return func(opts *gzip.Options) {
		opts.Level = level
	}
}

func WithGzip(opts ...GzipOption) SSEOption {
	return func(sse *ServerSentEventGenerator) {
		// set default options
		options := gzip.Options{
			Level: gzip.DefaultCompression,
		}
		// Apply all provided options.
		for _, opt := range opts {
			opt(&options)
		}
		comp, _ := gzip.New(options)
		sse.w = comp.Get(sse.w)
		sse.encoding = gzip.Encoding
	}
}

type DeflateOption func(*zlib.Options)

func WithDeflateLevel(level int) DeflateOption {
	return func(opts *zlib.Options) {
		opts.Level = level
	}
}

func WithDeflateDictionary(dict []byte) DeflateOption {
	return func(opts *zlib.Options) {
		opts.Dictionary = dict
	}
}

func WithDeflate(opts ...DeflateOption) SSEOption {
	return func(sse *ServerSentEventGenerator) {
		options := zlib.Options{
			Level: zlib.DefaultCompression,
		}

		for _, opt := range opts {
			opt(&options)
		}

		comp, _ := zlib.New(options)
		sse.w = comp.Get(sse.w)
		sse.encoding = zlib.Encoding
	}
}

type brotliOption func(*brotli.Options)

func WithBrotliLevel(level int) brotliOption {
	return func(opts *brotli.Options) {
		opts.Quality = level
	}
}

func WithBrotliLGWin(lgwin int) brotliOption {
	return func(opts *brotli.Options) {
		opts.LGWin = lgwin
	}
}

func WithBrotli(opts ...brotliOption) SSEOption {
	return func(sse *ServerSentEventGenerator) {
		options := brotli.Options{
			Quality: brotli.DefaultCompression,
		}

		for _, opt := range opts {
			opt(&options)
		}

		comp, _ := brotli.New(options)
		sse.w = comp.Get(sse.w)
		sse.encoding = brotli.Encoding
	}
}

func WithZstd(opts ...zstd_opts.EOption) SSEOption {
	return func(sse *ServerSentEventGenerator) {
		comp, _ := zstd.New(opts...)
		sse.w = comp.Get(sse.w)
		sse.encoding = zstd.Encoding
	}
}

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
	}

	// Apply options
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

func (sse *ServerSentEventGenerator) Context() context.Context {
	return sse.ctx
}

type ServerSentEventData struct {
	Type          EventType
	EventID       string
	Data          []string
	RetryDuration time.Duration
}

type SSEEventOption func(*ServerSentEventData)

func WithSSEEventId(id string) SSEEventOption {
	return func(e *ServerSentEventData) {
		e.EventID = id
	}
}

func WithSSERetryDuration(retryDuration time.Duration) SSEEventOption {
	return func(e *ServerSentEventData) {
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

func (sse *ServerSentEventGenerator) Send(eventType EventType, dataLines []string, opts ...SSEEventOption) error {
	sse.mu.Lock()
	defer sse.mu.Unlock()

	// create the event
	evt := ServerSentEventData{
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
