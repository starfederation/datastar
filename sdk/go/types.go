package datastar

import (
	"errors"
)

const (
	NewLine       = "\n"
	DoubleNewLine = "\n\n"
)

var (
	ErrEventTypeError = errors.New("event type is required")

	newLineBuf       = []byte(NewLine)
	doubleNewLineBuf = []byte(DoubleNewLine)
)

type flusher interface {
	Flush() error
}
