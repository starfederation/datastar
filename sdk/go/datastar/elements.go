package datastar

import (
	"fmt"
	"strings"
	"time"
)

// patchElementOptions holds the configuration data for [PatchElementOption]s used
// for initialization of [sse.PatchElements] event.
type patchElementOptions struct {
	EventID            string
	RetryDuration      time.Duration
	Selector           string
	Mode               ElementPatchMode
	UseViewTransitions bool
}

// PatchElementOption configures the [sse.PatchElements] event initialization.
type PatchElementOption func(*patchElementOptions)

// WithPatchElementsEventID configures an optional event ID for the elements patch event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithPatchElementsEventID(id string) PatchElementOption {
	return func(o *patchElementOptions) {
		o.EventID = id
	}
}

// WithSelectorf is a convenience wrapper for [WithSelector] option that formats the selector string
// using the provided format and arguments similar to [fmt.Sprintf].
func WithSelectorf(selectorFormat string, args ...any) PatchElementOption {
	selector := fmt.Sprintf(selectorFormat, args...)
	return WithSelector(selector)
}

// WithSelector specifies the [CSS selector] for HTML elements that an element will be merged over or
// merged next to, depending on the merge mode.
//
// [CSS selector]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors
func WithSelector(selector string) PatchElementOption {
	return func(o *patchElementOptions) {
		o.Selector = selector
	}
}

// WithMode overrides the [DefaultElementPatchMode] for the element.
// Choose a valid [ElementPatchMode].
func WithMode(merge ElementPatchMode) PatchElementOption {
	return func(o *patchElementOptions) {
		o.Mode = merge
	}
}

// WithUseViewTransitions specifies whether to use [view transitions] when merging elements.
//
// [view transitions]: https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API
func WithUseViewTransitions(useViewTransition bool) PatchElementOption {
	return func(o *patchElementOptions) {
		o.UseViewTransitions = useViewTransition
	}
}

// WithRetryDuration overrides the [DefaultSseRetryDuration] for the element patch event.
func WithRetryDuration(retryDuration time.Duration) PatchElementOption {
	return func(o *patchElementOptions) {
		o.RetryDuration = retryDuration
	}
}

// PatchElements sends HTML elements to the client to update the DOM tree with.
func (sse *ServerSentEventGenerator) PatchElements(elements string, opts ...PatchElementOption) error {
	options := &patchElementOptions{
		EventID:       "",
		RetryDuration: DefaultSseRetryDuration,
		Selector:      "",
		Mode:          ElementPatchModeOuter,
	}
	for _, opt := range opts {
		opt(options)
	}

	sendOptions := make([]SSEEventOption, 0, 2)
	if options.EventID != "" {
		sendOptions = append(sendOptions, WithSSEEventId(options.EventID))
	}
	if options.RetryDuration > 0 {
		sendOptions = append(sendOptions, WithSSERetryDuration(options.RetryDuration))
	}

	dataRows := make([]string, 0, 4)
	if options.Selector != "" {
		dataRows = append(dataRows, SelectorDatalineLiteral+options.Selector)
	}
	if options.Mode != ElementPatchModeOuter {
		dataRows = append(dataRows, ModeDatalineLiteral+string(options.Mode))
	}
	if options.UseViewTransitions {
		dataRows = append(dataRows, UseViewTransitionDatalineLiteral+"true")
	}

	if elements != "" {
		parts := strings.Split(elements, "\n")
		for _, part := range parts {
			dataRows = append(dataRows, ElementsDatalineLiteral+part)
		}
	}

	if err := sse.Send(
		EventTypePatchElements,
		dataRows,
		sendOptions...,
	); err != nil {
		return fmt.Errorf("failed to send elements: %w", err)
	}

	return nil
}
