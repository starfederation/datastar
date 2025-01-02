## Title Update Backend

## Demo

Look at the title change in the browser tab!

<div data-on-load="@get('/examples/title_update_backend/updates')"></div>

## Explanation

A user [in the Discord channel](https://discord.com/channels/725789699527933952/1180902694999838752) was asking about needing a plugin similar to [HTMX's head support](https://v1.htmx.org/extensions/head-support/) to update title or head elements. With Datastar this is unnecessary as you can just update the title directly with an SSE fragment

```
event: datastar-merge-fragments
data: selector title
data: fragments <title>08:30:36 from server</title>
```

In a similar fashion you can append, prepend directly to the HEAD element using [Merge Options](/examples/merge_options)
