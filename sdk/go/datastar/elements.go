package datastar

import (
	"fmt"
	"strings"
	"time"
)

// mergeElementOptions holds the configuration data for [MergeElementOption]s used
// for initialization of [sse.MergeElements] event.
type mergeElementOptions struct {
	EventID            string
	RetryDuration      time.Duration
	Selector           string
	MergeMode          ElementMergeMode
	UseViewTransitions bool
}

// MergeElementOption configures the [sse.MergeElements] event initialization.
type MergeElementOption func(*mergeElementOptions)

// WithMergeElementsEventID configures an optional event ID for the elements merge event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithMergeElementsEventID(id string) MergeElementOption {
	return func(o *mergeElementOptions) {
		o.EventID = id
	}
}

// WithSelectorf is a convenience wrapper for [WithSelector] option that formats the selector string
// using the provided format and arguments similar to [fmt.Sprintf].
func WithSelectorf(selectorFormat string, args ...any) MergeElementOption {
	selector := fmt.Sprintf(selectorFormat, args...)
	return WithSelector(selector)
}

// WithSelector specifies the [CSS selector] for HTML elements that an element will be merged over or
// merged next to, depending on the merge mode.
//
// [CSS selector]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors
func WithSelector(selector string) MergeElementOption {
	return func(o *mergeElementOptions) {
		o.Selector = selector
	}
}

// WithMergeMode overrides the [DefaultElementMergeMode] for the element.
// Choose a valid [ElementMergeMode].
func WithMergeMode(merge ElementMergeMode) MergeElementOption {
	return func(o *mergeElementOptions) {
		o.MergeMode = merge
	}
}

// WithUseViewTransitions specifies whether to use [view transitions] when merging elements.
//
// [view transitions]: https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API
func WithUseViewTransitions(useViewTransition bool) MergeElementOption {
	return func(o *mergeElementOptions) {
		o.UseViewTransitions = useViewTransition
	}
}

// WithRetryDuration overrides the [DefaultSseRetryDuration] for the element merge event.
func WithRetryDuration(retryDuration time.Duration) MergeElementOption {
	return func(o *mergeElementOptions) {
		o.RetryDuration = retryDuration
	}
}

// MergeElements sends HTML elements to the client to update the DOM tree with.
func (sse *ServerSentEventGenerator) MergeElements(elements string, opts ...MergeElementOption) error {
	options := &mergeElementOptions{
		EventID:       "",
		RetryDuration: DefaultSseRetryDuration,
		Selector:      "",
		MergeMode:     ElementMergeModeOuter,
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
	if options.MergeMode != ElementMergeModeOuter {
		dataRows = append(dataRows, MergeModeDatalineLiteral+string(options.MergeMode))
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
		EventTypeMergeElements,
		dataRows,
		sendOptions...,
	); err != nil {
		return fmt.Errorf("failed to send elements: %w", err)
	}

	return nil
}
