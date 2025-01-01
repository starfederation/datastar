## Execute Script

## Demo

<div data-signals="{shouldRemove:true}">
    <div>Open the console to see the output</div>
    <button class="btn btn-success" data-on-click="@get('/examples/execute_script/log')">Console log, auto remove</button>
    <button class="btn btn-error" data-on-click="@get('/examples/execute_script/error')">Console Error, leave scripts in head</button>
</div>

## Explanation

The SDK provides a way to execute JavaScript code on the client. This is useful for when you need to interact with the DOM or other JavaScript libraries. One of the options is if the script should automatically be removed after execution. This is useful for when you need to run a script once and don't want to pollute the head element with scripts that are no longer needed.

***Note:*** In general you want to use signals for reactivity.  This is a stop gap to interact with legacy code or libraries that don't support signals.
