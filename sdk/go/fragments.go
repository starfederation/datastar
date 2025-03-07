package datastar

import (
	"fmt"
	"strconv"
	"strings"
	"time"
)

type MergeFragmentOptions struct {
	EventID            string
	RetryDuration      time.Duration
	Selector           string
	MergeMode          FragmentMergeMode
	SettleDuration     time.Duration
	UseViewTransitions bool
}

type MergeFragmentOption func(*MergeFragmentOptions)

func WithSelectorf(selectorFormat string, args ...any) MergeFragmentOption {
	selector := fmt.Sprintf(selectorFormat, args...)
	return WithSelector(selector)
}

func WithSelector(selector string) MergeFragmentOption {
	return func(o *MergeFragmentOptions) {
		o.Selector = selector
	}
}

func WithMergeMode(merge FragmentMergeMode) MergeFragmentOption {
	return func(o *MergeFragmentOptions) {
		o.MergeMode = merge
	}
}

func WithSettleDuration(d time.Duration) MergeFragmentOption {
	return func(o *MergeFragmentOptions) {
		o.SettleDuration = d
	}
}

func WithUseViewTransitions(useViewTransition bool) MergeFragmentOption {
	return func(o *MergeFragmentOptions) {
		o.UseViewTransitions = useViewTransition
	}
}

func (sse *ServerSentEventGenerator) MergeFragments(fragment string, opts ...MergeFragmentOption) error {
	options := &MergeFragmentOptions{
		EventID:        "",
		RetryDuration:  DefaultSseRetryDuration,
		Selector:       "",
		MergeMode:      FragmentMergeModeMorph,
		SettleDuration: DefaultFragmentsSettleDuration,
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
	if options.SettleDuration >= 0 && options.SettleDuration != DefaultFragmentsSettleDuration {
		settleDuration := strconv.Itoa(int(options.SettleDuration.Milliseconds()))
		dataRows = append(dataRows, SettleDurationDatalineLiteral+settleDuration)
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

type RemoveFragmentsOptions struct {
	EventID            string
	RetryDuration      time.Duration
	SettleDuration     time.Duration
	UseViewTransitions *bool
}

type RemoveFragmentsOption func(*RemoveFragmentsOptions)

func WithRemoveEventID(id string) RemoveFragmentsOption {
	return func(o *RemoveFragmentsOptions) {
		o.EventID = id
	}
}

func WithRemoveRetryDuration(d time.Duration) RemoveFragmentsOption {
	return func(o *RemoveFragmentsOptions) {
		o.RetryDuration = d
	}
}

func WithRemoveSettleDuration(d time.Duration) RemoveFragmentsOption {
	return func(o *RemoveFragmentsOptions) {
		o.SettleDuration = d
	}
}

func WithRemoveUseViewTransitions(useViewTransition bool) RemoveFragmentsOption {
	return func(o *RemoveFragmentsOptions) {
		o.UseViewTransitions = &useViewTransition
	}
}

func (sse *ServerSentEventGenerator) RemoveFragments(selector string, opts ...RemoveFragmentsOption) error {
	if selector == "" {
		panic("missing " + SelectorDatalineLiteral)
	}

	options := &RemoveFragmentsOptions{
		EventID:            "",
		RetryDuration:      DefaultSseRetryDuration,
		SettleDuration:     DefaultFragmentsSettleDuration,
		UseViewTransitions: nil,
	}
	for _, opt := range opts {
		opt(options)
	}

	dataRows := []string{SelectorDatalineLiteral + selector}
	if options.SettleDuration >= 0 && options.SettleDuration != DefaultFragmentsSettleDuration {
		settleDuration := strconv.Itoa(int(options.SettleDuration.Milliseconds()))
		dataRows = append(dataRows, SettleDurationDatalineLiteral+settleDuration)
	}
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
