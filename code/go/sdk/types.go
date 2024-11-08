package datastar

import (
	"errors"
)

var (
	ErrEventTypeError = errors.New("event type is required")

	newLineBuf       = []byte("\n")
	doubleNewLineBuf = []byte("\n\n")
)

type EventType []byte

var (
	EventTypeFragment = EventType("datastar-fragment")
	EventTypeSignal   = EventType("datastar-signal")
	EventTypeRemove   = EventType("datastar-remove")
	EventTypeRedirect = EventType("datastar-redirect")
	EventTypeConsole  = EventType("datastar-console")
)
