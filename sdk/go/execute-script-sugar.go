package datastar

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"time"
)

func (sse *ServerSentEventGenerator) ConsoleLog(msg string, opts ...ExecuteScriptOption) error {
	call := fmt.Sprintf("console.log(%q)", msg)
	return sse.ExecuteScript(call, opts...)
}

func (sse *ServerSentEventGenerator) ConsoleLogf(format string, args ...any) error {
	return sse.ConsoleLog(fmt.Sprintf(format, args...))
}

func (sse *ServerSentEventGenerator) ConsoleError(err error, opts ...ExecuteScriptOption) error {
	call := fmt.Sprintf("console.error(%q)", err.Error())
	return sse.ExecuteScript(call, opts...)
}

func (sse *ServerSentEventGenerator) Redirectf(format string, args ...any) error {
	url := fmt.Sprintf(format, args...)
	return sse.Redirect(url)
}

func (sse *ServerSentEventGenerator) Redirect(url string, opts ...ExecuteScriptOption) error {
	js := fmt.Sprintf("setTimeout(() => window.location.href = %q)", url)
	return sse.ExecuteScript(js, opts...)
}

type DispatchCustomEventOptions struct {
	EventID       string
	RetryDuration time.Duration
	Selector      string
	Bubbles       bool
	Cancelable    bool
	Composed      bool
}

type DispatchCustomEventOption func(*DispatchCustomEventOptions)

func WithDispatchCustomEventEventID(id string) DispatchCustomEventOption {
	return func(o *DispatchCustomEventOptions) {
		o.EventID = id
	}
}

func WithDispatchCustomEventRetryDuration(retryDuration time.Duration) DispatchCustomEventOption {
	return func(o *DispatchCustomEventOptions) {
		o.RetryDuration = retryDuration
	}
}

func WithDispatchCustomEventSelector(selector string) DispatchCustomEventOption {
	return func(o *DispatchCustomEventOptions) {
		o.Selector = selector
	}
}

func WithDispatchCustomEventBubbles(bubbles bool) DispatchCustomEventOption {
	return func(o *DispatchCustomEventOptions) {
		o.Bubbles = bubbles
	}
}

func WithDispatchCustomEventCancelable(cancelable bool) DispatchCustomEventOption {
	return func(o *DispatchCustomEventOptions) {
		o.Cancelable = cancelable
	}
}

func WithDispatchCustomEventComposed(composed bool) DispatchCustomEventOption {
	return func(o *DispatchCustomEventOptions) {
		o.Composed = composed
	}
}

func (sse *ServerSentEventGenerator) DispatchCustomEvent(eventName string, detail any, opts ...DispatchCustomEventOption) error {
	if eventName == "" {
		return fmt.Errorf("eventName is required")
	}

	detailsJSON, err := json.Marshal(detail)
	if err != nil {
		return fmt.Errorf("failed to marshal detail: %w", err)
	}

	const defaultSelector = "document"
	options := DispatchCustomEventOptions{
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

func (sse *ServerSentEventGenerator) ReplaceURL(u url.URL, opts ...ExecuteScriptOption) error {
	js := fmt.Sprintf(`window.history.replaceState({}, "", %q)`, u.String())
	return sse.ExecuteScript(js, opts...)
}

func (sse *ServerSentEventGenerator) ReplaceURLQuerystring(r *http.Request, values url.Values, opts ...ExecuteScriptOption) error {
	u := *r.URL
	u.RawQuery = values.Encode()
	return sse.ReplaceURL(u, opts...)
}

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
