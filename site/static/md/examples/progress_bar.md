## Progress Bar

[Original HTMX Version](https://htmx.org/examples/progress-bar/)

## Demo

<div id="progress_bar" data-init="@get('/examples/progress_bar/data',{openWhenHidden:true})">
</div>

## Explanation

```html
<div id="progress_bar" data-init="@get('/examples/progress_bar/data',{openWhenHidden:true})">
</div>

```

This example shows how to implement an updating progress graphic. Since Datastar is using SSE this is very easy to
implement. The server sends a progress value every 500 milliseconds, and the client updates the progress bar accordingly
sending down a new SVG. After the progress is complete, the server a button to restart the job.

***Note:*** The `openWhenHidden` option is used to keep the connection open even when the progress bar is not visible.
This is useful for when the user navigates away from the page and then returns. This will use more resources, so use it
judiciously.