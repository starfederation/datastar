package datastar

import (
	"encoding/json"
	"fmt"
)

// MarshalAndMergeSignals is a convenience method for [see.MergeSignals].
// It marshals a given signals struct into JSON and
// emits a [EventTypeMergeSignals] event.
func (sse *ServerSentEventGenerator) MarshalAndMergeSignals(signals any, opts ...MergeSignalsOption) error {
	b, err := json.Marshal(signals)
	if err != nil {
		panic(err)
	}
	if err := sse.MergeSignals(b, opts...); err != nil {
		return fmt.Errorf("failed to merge signals: %w", err)
	}

	return nil
}

// MarshalAndMergeSignalsIfMissing is a convenience method for [see.MarshalAndMergeSignals].
// It is equivalent to calling [see.MarshalAndMergeSignals] with [see.WithOnlyIfMissing(true)] option.
func (sse *ServerSentEventGenerator) MarshalAndMergeSignalsIfMissing(signals any, opts ...MergeSignalsOption) error {
	if err := sse.MarshalAndMergeSignals(
		signals,
		append(opts, WithOnlyIfMissing(true))...,
	); err != nil {
		return fmt.Errorf("failed to merge signals if missing: %w", err)
	}
	return nil
}

// MergeSignalsIfMissingRaw is a convenience method for [see.MergeSignals].
// It is equivalent to calling [see.MergeSignals] with [see.WithOnlyIfMissing(true)] option.
func (sse *ServerSentEventGenerator) MergeSignalsIfMissingRaw(signalsJSON string) error {
	if err := sse.MergeSignals([]byte(signalsJSON), WithOnlyIfMissing(true)); err != nil {
		return fmt.Errorf("failed to merge signals if missing: %w", err)
	}
	return nil
}
