package datastar

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"time"
)

// ConsoleLog is a convenience method for [see.ExecuteScript].
// It is equivalent to calling [see.ExecuteScript] with [see.WithScript] option set to `console.log(msg)`.
func (sse *ServerSentEventGenerator) ConsoleLog(msg string, opts ...ExecuteScriptOption) error {
	call := fmt.Sprintf("console.log(%q)", msg)
	return sse.ExecuteScript(call, opts...)
}

// ConsoleLogf is a convenience method for [see.ExecuteScript].
// It is equivalent to calling [see.ExecuteScript] with [see.WithScript] option set to `console.log(fmt.Sprintf(format, args...))`.
func (sse *ServerSentEventGenerator) ConsoleLogf(format string, args ...any) error {
	return sse.ConsoleLog(fmt.Sprintf(format, args...))
}

// ConsoleError is a convenience method for [see.ExecuteScript].
// It is equivalent to calling [see.ExecuteScript] with [see.WithScript] option set to `console.error(msg)`.
func (sse *ServerSentEventGenerator) ConsoleError(err error, opts ...ExecuteScriptOption) error {
	call := fmt.Sprintf("console.error(%q)", err.Error())
	return sse.ExecuteScript(call, opts...)
}

// Redirectf is a convenience method for [see.ExecuteScript].
// It sends a redirect event to the client formatted using [fmt.Sprintf].
func (sse *ServerSentEventGenerator) Redirectf(format string, args ...any) error {
	url := fmt.Sprintf(format, args...)
	return sse.Redirect(url)
}

// Redirect is a convenience method for [see.ExecuteScript].
// It sends a redirect event to the client .
func (sse *ServerSentEventGenerator) Redirect(url string, opts ...ExecuteScriptOption) error {
	js := fmt.Sprintf("setTimeout(() => window.location.href = %q)", url)
	return sse.ExecuteScript(js, opts...)
}

// dispatchCustomEventOptions holds the configuration data
// modified by [DispatchCustomEventOption]s
// for dispatching custom events to the client.
type dispatchCustomEventOptions struct {
	EventID       string
	RetryDuration time.Duration
	Selector      string
	Bubbles       bool
	Cancelable    bool
	Composed      bool
}

// DispatchCustomEventOption configures one custom
// server-sent event.
type DispatchCustomEventOption func(*dispatchCustomEventOptions)

// WithDispatchCustomEventEventID configures an optional event ID for the custom event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithDispatchCustomEventEventID(id string) DispatchCustomEventOption {
	return func(o *dispatchCustomEventOptions) {
		o.EventID = id
	}
}

// WithDispatchCustomEventRetryDuration overrides the [DefaultSseRetryDuration] for one custom event.
func WithDispatchCustomEventRetryDuration(retryDuration time.Duration) DispatchCustomEventOption {
	return func(o *dispatchCustomEventOptions) {
		o.RetryDuration = retryDuration
	}
}

// WithDispatchCustomEventSelector replaces the default custom event target `document` with a
// [CSS selector]. If the selector matches multiple HTML elements, the event will be dispatched
// from each one. For example, if the selector is `#my-element`, the event will be dispatched
// from the element with the ID `my-element`. If the selector is `main > section`, the event will be dispatched
// from each `<section>` element which is a direct child of the `<main>` element.
//
// [CSS selector]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_selectors
func WithDispatchCustomEventSelector(selector string) DispatchCustomEventOption {
	return func(o *dispatchCustomEventOptions) {
		o.Selector = selector
	}
}

// WithDispatchCustomEventBubbles overrides the default custom [event bubbling] `true` value.
// Setting bubbling to `false` is equivalent to calling `event.stopPropagation()` Javascript
// command on the client side for the dispatched event. This prevents the event from triggering
// event handlers of its parent elements.
//
// [event bubbling]: https://developer.mozilla.org/en-US/docs/Learn_web_development/Core/Scripting/Event_bubbling
func WithDispatchCustomEventBubbles(bubbles bool) DispatchCustomEventOption {
	return func(o *dispatchCustomEventOptions) {
		o.Bubbles = bubbles
	}
}

// WithDispatchCustomEventCancelable overrides the default custom [event cancelability] `true` value.
// Setting cancelability to `false` is blocks `event.preventDefault()` Javascript
// command on the client side for the dispatched event.
//
// [event cancelability]: https://developer.mozilla.org/en-US/docs/Web/API/Event/cancelable
func WithDispatchCustomEventCancelable(cancelable bool) DispatchCustomEventOption {
	return func(o *dispatchCustomEventOptions) {
		o.Cancelable = cancelable
	}
}

// WithDispatchCustomEventComposed overrides the default custom [event composed] `true` value.
// It indicates whether or not the event will propagate across the shadow HTML DOM boundary into
// the document DOM tree. When `false`, the shadow root will be the last node to be offered the event.
//
// [event composed]: https://developer.mozilla.org/en-US/docs/Web/API/Event/composed
func WithDispatchCustomEventComposed(composed bool) DispatchCustomEventOption {
	return func(o *dispatchCustomEventOptions) {
		o.Composed = composed
	}
}

// DispatchCustomEvent is a convenience method for dispatching a custom event by executing
// a client side script via [sse.ExecuteScript] call. The detail struct is marshaled to JSON and
// passed as a parameter to the event.
func (sse *ServerSentEventGenerator) DispatchCustomEvent(eventName string, detail any, opts ...DispatchCustomEventOption) error {
	if eventName == "" {
		return fmt.Errorf("eventName is required")
	}

	detailsJSON, err := json.Marshal(detail)
	if err != nil {
		return fmt.Errorf("failed to marshal detail: %w", err)
	}

	const defaultSelector = "document"
	options := dispatchCustomEventOptions{
		EventID:       "",
		RetryDuration: DefaultSseRetryDuration,
		Selector:      defaultSelector,
		Bubbles:       true,
		Cancelable:    true,
		Composed:      true,
	}

	for _, opt := range opts {
		opt(&options)
	}

	elementsJS := `[document]`
	if options.Selector != "" && options.Selector != defaultSelector {
		elementsJS = fmt.Sprintf(`document.querySelectorAll(%q)`, options.Selector)
	}

	js := fmt.Sprintf(`
const elements = %s

const event = new CustomEvent(%q, {
	bubbles: %t,
	cancelable: %t,
	composed: %t,
	detail: %s,
});

elements.forEach((element) => {
	element.dispatchEvent(event);
});
	`,
		elementsJS,
		eventName,
		options.Bubbles,
		options.Cancelable,
		options.Composed,
		string(detailsJSON),
	)

	executeOptions := make([]ExecuteScriptOption, 0)
	if options.EventID != "" {
		executeOptions = append(executeOptions, WithExecuteScriptEventID(options.EventID))
	}
	if options.RetryDuration != 0 {
		executeOptions = append(executeOptions, WithExecuteScriptRetryDuration(options.RetryDuration))
	}

	return sse.ExecuteScript(js, executeOptions...)

}

// ReplaceURL replaces the current URL in the browser's history.
func (sse *ServerSentEventGenerator) ReplaceURL(u url.URL, opts ...ExecuteScriptOption) error {
	js := fmt.Sprintf(`window.history.replaceState({}, "", %q)`, u.String())
	return sse.ExecuteScript(js, opts...)
}

// ReplaceURLQuerystring is a convenience wrapper for [sse.ReplaceURL] that replaces the query
// string of the current URL request with new a new query built from the provided values.
func (sse *ServerSentEventGenerator) ReplaceURLQuerystring(r *http.Request, values url.Values, opts ...ExecuteScriptOption) error {
	// TODO: rename this function to ReplaceURLQuery
	u := *r.URL
	u.RawQuery = values.Encode()
	return sse.ReplaceURL(u, opts...)
}

// Prefetch is a convenience wrapper for [sse.ExecuteScript] that prefetches the provided links.
// It follows the Javascript [speculation rules API] prefetch specification.
//
// [speculation rules API]: https://developer.mozilla.org/en-US/docs/Web/API/Speculation_Rules_API
func (sse *ServerSentEventGenerator) Prefetch(urls ...string) error {
	wrappedURLs := make([]string, len(urls))
	for i, url := range urls {
		wrappedURLs[i] = fmt.Sprintf(`"%s"`, url)
	}
	script := fmt.Sprintf(`
{
	"prefetch": [
		{
			"source": "list",
			"urls": [
				%s
			]
		}
	]
}
		`, strings.Join(wrappedURLs, ",\n\t\t\t\t"))
	return sse.ExecuteScript(
		script,
		WithExecuteScriptAutoRemove(false),
		WithExecuteScriptAttributes("type speculationrules"),
	)
}
