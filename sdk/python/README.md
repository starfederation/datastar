# Datastar-py

The `datastar_py` package provides backend helpers for the [Datastar](https://data-star.dev) JS library.

Datastar requires all backend responses to use SSE. This allows the backend to
send any number of responses, from zero to inifinity.

`Datastar-py` helps with the formatting of these responses, while also
providing helper functions for the different supported responses.

To use `datastar-py`, import the SSE generator in your app and then use
it in your route handler:

```python
from datastar_py import ServerSentEventGenerator as SSE

# ... various app setup. The example below is for the Quart framework

@app.route("/updates")
async def updates():
    async def time_updates():
        while True:
            yield SSE.merge_fragments(
                [f"""<span id="currentTime">{datetime.now().isoformat()}"""]
            )
            await asyncio.sleep(1)
            yield SSE.merge_signals({"currentTime": f"{datetime.now().isoformat()}"})
            await asyncio.sleep(1)

    response = await make_response(time_updates(), SSE_HEADERS)
    response.timeout = None
    return response
```

There are also a number of custom responses/helpers for various frameworks. Current ly the following frameworks are supported:

* [Sanic](https://sanic.dev/en/)
* [Django](https://www.djangoproject.com/)
* [Quart](https://quart.palletsprojects.com/en/stable/)
* [FastAPI](https://fastapi.tiangolo.com/)
* [FastHTML](https://fastht.ml/)
