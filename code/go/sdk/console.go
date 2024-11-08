package datastar

import "fmt"

type ConsoleMode string

const (
	// writes an error message to the console if the assertion is false. If the assertion is true, nothing happens.
	ConsoleModeAssert ConsoleMode = "assert"

	// clears the console if the console allows it. A graphical console, like those running on browsers, will allow it; a console displaying on the terminal, like the one running on Node, will not support it, and will have no effect (and no error).
	ConsoleModeClear ConsoleMode = "clear"

	// logs the number of times that this particular call to count() has been called.
	ConsoleModeCount ConsoleMode = "count"

	// resets counter used with console.count().
	ConsoleModeCountReset ConsoleMode = "countReset"

	// outputs a message to the console at the "debug" log level. The message is only displayed to the user if the console is configured to display debug output. In most cases, the log level is configured within the console UI. This log level might correspond to the Debug or Verbose log level.
	ConsoleModeDebug ConsoleMode = "debug"

	// displays a list of the properties of the specified JavaScript object. In browser consoles, the output is presented as a hierarchical listing with disclosure triangles that let you see the contents of child objects.
	// Unlike other logging methods, console.dir() does not attempt to pretty-print the object. For example, if you pass a DOM element to console.dir(), it will not be displayed like in the element inspector, but will instead show a list of properties.
	ConsoleModeDir ConsoleMode = "dir"

	// displays an interactive tree of the descendant elements of the specified XML/HTML element. If it is not possible to display as an element the JavaScript Object view is shown instead. The output is presented as a hierarchical listing of expandable nodes that let you see the contents of child nodes.
	ConsoleModeDirxml ConsoleMode = "dirxml"

	// displays an interactive tree of the descendant elements of the specified XML/HTML element. If it is not possible to display as an element the JavaScript Object view is shown instead. The output is presented as a hierarchical listing of expandable nodes that let you see the contents of child nodes.
	ConsoleModeError ConsoleMode = "error"

	// creates a new inline group in the Web console log, causing any subsequent console messages to be indented by an additional level, until console.groupEnd() is called.
	ConsoleModeGroup ConsoleMode = "group"

	// creates a new inline group in the console. Unlike console.group(), however, the new group is created collapsed. The user will need to use the disclosure button next to it to expand it, revealing the entries created in the group.
	// Call console.groupEnd() to back out to the parent group.
	// See https://developer.mozilla.org/en-US/docs/Web/API/console#using_groups_in_the_console
	ConsoleModeGroupCollapsed ConsoleMode = "groupCollapsed"

	// exits the current inline group in the console. See Using groups in the console in the console documentation for details and examples.
	ConsoleModeGroupEnd ConsoleMode = "groupEnd"

	// outputs a message to the console at the "info" log level. The message is only displayed to the user if the console is configured to display info output. In most cases, the log level is configured within the console UI. The message may receive special formatting, such as a small "i" icon next to it.
	ConsoleModeInfo ConsoleMode = "info"

	// outputs a message to the console.
	ConsoleModeLog ConsoleMode = "log"

	// displays tabular data as a table.
	ConsoleModeTable ConsoleMode = "table"

	// starts a timer you can use to track how long an operation takes. You give each timer a unique name, and may have up to 10,000 timers running on a given page. When you call console.timeEnd() with the same name, the browser will output the time, in milliseconds, that elapsed since the timer was started. See https://developer.mozilla.org/en-US/docs/Web/API/console#timers for details and examples.
	ConsoleModeTime ConsoleMode = "time"

	// The console.timeEnd() static method stops a timer that was previously started by calling console.time(). See https://developer.mozilla.org/en-US/docs/Web/API/console#timers for details and examples.
	ConsoleModeTimeEnd ConsoleMode = "timeEnd"

	// logs the current value of a timer that was previously started by calling console.time().
	ConsoleModeTimeLog ConsoleMode = "timeLog"

	// outputs a stack trace to the console.
	// Note: In some browsers, console.trace() may also output the sequence of calls and asynchronous events leading to the current console.trace() which are not on the call stack — to help identify the origin of the current event evaluation loop.
	// See https://developer.mozilla.org/en-US/docs/Web/API/console#stack_traces in the console documentation for details and examples.
	ConsoleModeTrace ConsoleMode = "trace"

	// outputs a warning message to the console at the "warning" log level. The message is only displayed to the user if the console is configured to display warning output. In most cases, the log level is configured within the console UI. The message may receive special formatting, such as yellow colors and a warning icon.
	ConsoleModeWarn ConsoleMode = "warn"
)

func (sse *ServerSentEventGenerator) Consolef(mode ConsoleMode, messageFormat string, args ...any) error {
	message := fmt.Sprintf(messageFormat, args...)
	return sse.Console(mode, message)
}

func (sse *ServerSentEventGenerator) Console(mode ConsoleMode, message string) error {
	return sse.send(
		EventTypeConsole,
		[]string{string(mode) + " " + message},
	)
}
