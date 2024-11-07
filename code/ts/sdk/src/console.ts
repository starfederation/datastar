import { z } from "zod";

const zConsoleMode = z.enum([
    "assert" 
    , "clear" 
    , "count" 
    , "countReset" 
    , "debug" 
    , "dir" 
    , "dirxml" 
    , "error" 
    , "group" 
    , "groupCollapsed" 
    , "groupEnd" 
    , "info" 
    , "log" 
    , "table" 
    , "time" 
    , "timeEnd" 
    , "timeLog" 
    , "trace" 
    , "warn"
]);

export type ConsoleMode = z.infer<zConsoleMode>;

const zConsoleEvent = z.object({
    mode: zConsoleMode,
    message: z.string()
});

export type ConsoleEvent = z.infer<typeof zConsoleEvent>;

export const console = z.function()
    .args(
       z.string().min(1, {message: "The console message must be a non empty string"})
    ).returns(zConsoleEvent);
