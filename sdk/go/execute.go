package datastar

import (
	"fmt"
	"strconv"
	"strings"
	"time"
)

// executeScriptOptions hold script options that will be translated to [SSEEventOptions].
type executeScriptOptions struct {
	EventID       string
	RetryDuration time.Duration
	Attributes    []string
	AutoRemove    *bool
}

// ExecuteScriptOption configures script execution event that will be sent to the client.
type ExecuteScriptOption func(*executeScriptOptions)

// WithExecuteScriptEventID configures an optional event ID for the script execution event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithExecuteScriptEventID(id string) ExecuteScriptOption {
	return func(o *executeScriptOptions) {
		o.EventID = id
	}
}

// WithExecuteScriptRetryDuration overrides the [DefaultRetryDuration] for this script
// execution only.
func WithExecuteScriptRetryDuration(retryDuration time.Duration) ExecuteScriptOption {
	return func(o *executeScriptOptions) {
		o.RetryDuration = retryDuration
	}
}

// WithExecuteScriptAttributes overrides the default script attribute
// value `type module`, which renders as `<script type="module">` in client's browser.
// Each attribute should include a pair of plain words representing the attribute name and value
// without any formatting.
func WithExecuteScriptAttributes(attributes ...string) ExecuteScriptOption {
	return func(o *executeScriptOptions) {
		o.Attributes = attributes
	}
}

// WithExecuteScriptAttributeKVs is an alternative option for [WithExecuteScriptAttributes].
// Even parameters are keys, odd parameters are their values.
func WithExecuteScriptAttributeKVs(kvs ...string) ExecuteScriptOption {
	if len(kvs)%2 != 0 {
		panic("WithExecuteScriptAttributeKVs requires an even number of arguments")
	}
	attributes := make([]string, 0, len(kvs)/2)
	for i := 0; i < len(kvs); i += 2 {
		attribute := fmt.Sprintf("%s %s", kvs[i], kvs[i+1])
		attributes = append(attributes, attribute)
	}
	return WithExecuteScriptAttributes(attributes...)
}

// WithExecuteScriptAutoRemove requires the client to eliminate the script element after its execution.
func WithExecuteScriptAutoRemove(autoremove bool) ExecuteScriptOption {
	return func(o *executeScriptOptions) {
		o.AutoRemove = &autoremove
	}
}

// ExecuteScript runs a script in the client browser. Seperate commands with semicolons.
func (sse *ServerSentEventGenerator) ExecuteScript(scriptContents string, opts ...ExecuteScriptOption) error {
	options := &executeScriptOptions{
		RetryDuration: DefaultSseRetryDuration,
		Attributes:    []string{"type module"},
	}
	for _, opt := range opts {
		opt(options)
	}

	sendOpts := make([]SSEEventOption, 0, 2)
	if options.EventID != "" {
		sendOpts = append(sendOpts, WithSSEEventId(options.EventID))
	}

	if options.RetryDuration != DefaultSseRetryDuration {
		sendOpts = append(sendOpts, WithSSERetryDuration(options.RetryDuration))
	}

	dataLines := make([]string, 0, 64)
	if options.AutoRemove != nil && *options.AutoRemove != DefaultExecuteScriptAutoRemove {
		dataLines = append(dataLines, AutoRemoveDatalineLiteral+strconv.FormatBool(*options.AutoRemove))
	}
	dataLinesJoined := strings.Join(dataLines, NewLine)

	if dataLinesJoined != DefaultExecuteScriptAttributes {
		for _, attribute := range options.Attributes {
			dataLines = append(dataLines, AttributesDatalineLiteral+attribute)
		}
	}

	scriptLines := strings.Split(scriptContents, NewLine)
	for _, line := range scriptLines {
		dataLines = append(dataLines, ScriptDatalineLiteral+line)
	}

	return sse.Send(
		EventTypeExecuteScript,
		dataLines,
		sendOpts...,
	)
}
