package datastar

const (
	NewLine       = "\n"
	DoubleNewLine = "\n\n"
)

var (
	newLineBuf       = []byte(NewLine)
	doubleNewLineBuf = []byte(DoubleNewLine)
)

type flusher interface {
	Flush() error
}
