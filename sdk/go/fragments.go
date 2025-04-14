package datastar

import (
	"fmt"
	"strconv"
	"strings"
	"time"
)

// mergeFragmentOptions holds the configuration data for [MergeFragmentOption]s used
// for initialization of [sse.MergeFragments] event.
type mergeFragmentOptions struct {
	EventID            string
	RetryDuration      time.Duration
	Selector           string
	MergeMode          FragmentMergeMode
	UseViewTransitions bool
}

// MergeFragmentOption configures the [sse.MergeFragments] event initialization.
type MergeFragmentOption func(*mergeFragmentOptions)

// WithSelectorf is a convenience wrapper for [WithSelector] option that formats the selector string
// using the provided format and arguments similar to [fmt.Sprintf].
func WithSelectorf(selectorFormat string, args ...any) MergeFragmentOption {
	selector := fmt.Sprintf(selectorFormat, args...)
	return WithSelector(selector)
}

// WithSelector specifies the [CSS selector] for HTML elements that a fragment will be merged over or
// merged next to, depending on the merge mode.
//
// [CSS selector]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors
func WithSelector(selector string) MergeFragmentOption {
	return func(o *mergeFragmentOptions) {
		o.Selector = selector
	}
}

// WithMergeMode overrides the [DefaultFragmentMergeMode] for the fragment.
// Choose a valid [FragmentMergeMode].
func WithMergeMode(merge FragmentMergeMode) MergeFragmentOption {
	return func(o *mergeFragmentOptions) {
		o.MergeMode = merge
	}
}

// WithUseViewTransitions specifies whether to use [view transitions] when merging fragments.
//
// [view transitions]: https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API
func WithUseViewTransitions(useViewTransition bool) MergeFragmentOption {
	return func(o *mergeFragmentOptions) {
		o.UseViewTransitions = useViewTransition
	}
}

// MergeFragments sends an HTML fragment to the client to update the DOM tree with.
func (sse *ServerSentEventGenerator) MergeFragments(fragment string, opts ...MergeFragmentOption) error {
	options := &mergeFragmentOptions{
		EventID:       "", // TODO: Implement EventID option? currently field does nothing.
		RetryDuration: DefaultSseRetryDuration,
		Selector:      "",
		MergeMode:     FragmentMergeModeMorph,
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
	if options.MergeMode != FragmentMergeModeMorph {
		dataRows = append(dataRows, MergeModeDatalineLiteral+string(options.MergeMode))
	}
	if options.UseViewTransitions {
		dataRows = append(dataRows, UseViewTransitionDatalineLiteral+"true")
	}

	if fragment != "" {
		parts := strings.Split(fragment, "\n")
		for _, part := range parts {
			dataRows = append(dataRows, FragmentsDatalineLiteral+part)
		}
	}

	if err := sse.Send(
		EventTypeMergeFragments,
		dataRows,
		sendOptions...,
	); err != nil {
		return fmt.Errorf("failed to send fragment: %w", err)
	}

	return nil
}

// mergeFragmentOptions holds the configuration data for [RemoveFragmentsOption]s used
// for initialization of [sse.RemoveFragments] event.
type removeFragmentsOptions struct {
	EventID            string
	RetryDuration      time.Duration
	UseViewTransitions *bool
}

// RemoveFragmentsOption configures the [sse.RemoveFragments] event.
type RemoveFragmentsOption func(*removeFragmentsOptions)

// WithRemoveEventID configures an optional event ID for the fragment removal event.
// The client message field [lastEventId] will be set to this value.
// If the next event does not have an event ID, the last used event ID will remain.
//
// [lastEventId]: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId
func WithRemoveEventID(id string) RemoveFragmentsOption {
	return func(o *removeFragmentsOptions) {
		o.EventID = id
	}
}

// WithExecuteScriptRetryDuration overrides the [DefaultSseRetryDuration] for this script
// execution only.
func WithRemoveRetryDuration(d time.Duration) RemoveFragmentsOption {
	return func(o *removeFragmentsOptions) {
		o.RetryDuration = d
	}
}

// WithRemoveUseViewTransitions specifies whether to use [view transitions] when merging fragments.
//
// [view transitions]: https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API
func WithRemoveUseViewTransitions(useViewTransition bool) RemoveFragmentsOption {
	return func(o *removeFragmentsOptions) {
		o.UseViewTransitions = &useViewTransition
	}
}

// MergeFragments sends a [CSS selector] to the client to update the DOM tree by removing matching elements.
//
// [CSS selector]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors
func (sse *ServerSentEventGenerator) RemoveFragments(selector string, opts ...RemoveFragmentsOption) error {
	if selector == "" {
		panic("missing " + SelectorDatalineLiteral)
	}

	options := &removeFragmentsOptions{
		EventID:            "",
		RetryDuration:      DefaultSseRetryDuration,
		UseViewTransitions: nil,
	}
	for _, opt := range opts {
		opt(options)
	}

	dataRows := []string{SelectorDatalineLiteral + selector}
	if options.UseViewTransitions != nil {
		dataRows = append(dataRows, UseViewTransitionDatalineLiteral+strconv.FormatBool(*options.UseViewTransitions))
	}

	sendOptions := make([]SSEEventOption, 0, 2)
	if options.EventID != "" {
		sendOptions = append(sendOptions, WithSSEEventId(options.EventID))
	}
	if options.RetryDuration > 0 {
		sendOptions = append(sendOptions, WithSSERetryDuration(options.RetryDuration))
	}

	if err := sse.Send(EventTypeRemoveFragments, dataRows, sendOptions...); err != nil {
		return fmt.Errorf("failed to send remove: %w", err)
	}
	return nil
}
