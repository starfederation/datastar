import { ConsoleMode, ConsoleEvent, console } from "./console.ts";

/* displays an interactive tree of the descendant elements 
  * of the specified XML/HTML element. If it is not possible
  * to display as an element the JavaScript Object view is shown 
  * instead. The output is presented as a hierarchical listing of
  * expandable nodes that let you see the contents of child nodes. */
export const consoleError = console.implement((message: string): ConsoleEvent => { 
    return { mode: "error", message };
});

/* outputs a warning message to the console at the "warning"
  * log level. The message is only displayed to the user if the console
  *  is configured to display warning output. In most cases, the log level
  *  is configured within the console UI. The message may receive special
  * formatting, such as yellow colors and a warning icon. */
export const consoleWarn = console.implement((message: string): ConsoleEvent => {
    return { mode: "warn", message };
});

/* outputs a message to the console at the "info" log level. 
  * The message is only displayed to the user if the console is
  * configured to display info output. In most cases, the log level
  * is configured within the console UI. The message may receive
  * special formatting, such as a small "i" icon next to it. */
export const consoleInfo = console.implement((message: string): ConsoleEvent => {
    return { mode: "info", message };
});

/* outputs a message to the console.*/
export const consoleLog = console.implement((message: string): ConsoleEvent => {
    return { mode: "log", message };
});

/* outputs a message to the console at the "debug" log level. 
  * The message is only displayed to the user if the console is
  * configured to display debug output. In most cases, the log 
  * level is configured within the console UI. This log level might
  * correspond to the Debug or Verbose log level. */
export const consoleDebug = console.implement((message: string): ConsoleEvent => {
    return { mode: "debug", message };
});

/* creates a new inline group in the Web console log, causing any
  *  subsequent console messages to be indented by an additional
  * level, until console.groupEnd() is called. */
export const consoleGroup = console.implement((label: string): ConsoleEvent => {
    return { mode: "group", message: label };
});

/* exits the current inline group in the console. See Using groups in
  * the console in the console documentation for details and examples. */
export const consoleGroupEnd = (): ConsoleEvent => { 
    return { mode: "groupEnd", message: "" };
};
