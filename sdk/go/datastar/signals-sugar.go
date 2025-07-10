package datastar

import (
	"encoding/json"
	"fmt"
)

// MarshalAndPatchSignals is a convenience method for [see.PatchSignals].
// It marshals a given signals struct into JSON and
// emits a [EventTypePatchSignals] event.
func (sse *ServerSentEventGenerator) MarshalAndPatchSignals(signals any, opts ...PatchSignalsOption) error {
	b, err := json.Marshal(signals)
	if err != nil {
		panic(err)
	}
	if err := sse.PatchSignals(b, opts...); err != nil {
		return fmt.Errorf("failed to patch signals: %w", err)
	}

	return nil
}

// MarshalAndPatchSignalsIfMissing is a convenience method for [see.MarshalAndPatchSignals].
// It is equivalent to calling [see.MarshalAndPatchSignals] with [see.WithOnlyIfMissing(true)] option.
func (sse *ServerSentEventGenerator) MarshalAndPatchSignalsIfMissing(signals any, opts ...PatchSignalsOption) error {
	if err := sse.MarshalAndPatchSignals(
		signals,
		append(opts, WithOnlyIfMissing(true))...,
	); err != nil {
		return fmt.Errorf("failed to patch signals if missing: %w", err)
	}
	return nil
}

// PatchSignalsIfMissingRaw is a convenience method for [see.PatchSignals].
// It is equivalent to calling [see.PatchSignals] with [see.WithOnlyIfMissing(true)] option.
func (sse *ServerSentEventGenerator) PatchSignalsIfMissingRaw(signalsJSON string) error {
	if err := sse.PatchSignals([]byte(signalsJSON), WithOnlyIfMissing(true)); err != nil {
		return fmt.Errorf("failed to patch signals if missing: %w", err)
	}
	return nil
}
