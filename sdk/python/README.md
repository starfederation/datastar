# datastar-py

The `datastar-py` package provides backend helpers for the [Datastar](https://data-star.dev) JS library.

Datastar sends responses back to the browser using SSE. This allows the backend to
send any number of events, from zero to infinity in response to a single request.

`datastar-py` has helpers for creating those responses, formatting the events,
reading signals from the frontend, and generating the data-* HTML attributes.

The event generator can be used with any framework. There are also custom
helpers included for the following frameworks:

* [Django](https://www.djangoproject.com/)
* [FastAPI](https://fastapi.tiangolo.com/)
* [FastHTML](https://fastht.ml/)
* [Litestar](https://litestar.dev/)
* [Quart](https://quart.palletsprojects.com/en/stable/)
* [Sanic](https://sanic.dev/en/)
* [Starlette](https://www.starlette.io/)

## Event Generation Helpers

To use `datastar-py`, import the SSE generator in your app and then use
it in your route handler:

```python
from datastar_py import ServerSentEventGenerator as SSE


# ... various app setup.
# The example below is for the Quart framework, and is only using the event generation helpers.

@app.route("/updates")
async def updates():
    async def time_updates():
        while True:
            yield SSE.patch_elements(
                [f"""<span id="currentTime">{datetime.now().isoformat()}"""]
            )
            await asyncio.sleep(1)
            yield SSE.patch_signals({"currentTime": f"{datetime.now().isoformat()}"})
            await asyncio.sleep(1)

    response = await make_response(time_updates(), SSE_HEADERS)
    response.timeout = None
    return response
```

## Response Helpers

A datastar response consists of 0..N datastar events. There are response
classes included to make this easy in all of the supported frameworks.

The following examples will work across all supported frameworks when the
response class is imported from the appropriate framework package.
e.g. `from datastar_py.quart import DatastarResponse` The containing functions
are not shown here, as they will differ per framework.

```python
from datastar_py import ServerSentEventGenerator as SSE

# 0 events, a 204
return DatastarResponse()
# 1 event
return DatastarResponse(SSE.patch_elements("<div id='mydiv'></div>"))
# 2 events
return DatastarResponse([
    SSE.patch_elements("<div id='mydiv'></div>"),
    SSE.patch_signals({"mysignal": "myval"}),
])

# N events, a long lived stream (for all frameworks but sanic)
async def updates():
    while True:
        yield SSE.patch_elements("<div id='mydiv'></div>")
        await asyncio.sleep(1)
return DatastarResponse(updates())

# A long lived stream for sanic
response = await datastar_respond(request)
# which is just a helper for the following
# response = await request.respond(DatastarResponse())
while True:
    await response.send(SSE.patch_elements("<div id='mydiv'></div>"))
    await asyncio.sleep(1)
```

### Response Decorator
To make returning a `DatastarResponse` simpler, there is a decorator
`datastar_response` available that automatically wraps a function result in
`DatastarResponse`. It works on async and regular functions and generator
functions. The main use case is when using a generator function, as you can
avoid a second generator function inside your response function. The decorator
works the same for any of the supported frameworks, and should be used under
any routing decorator from the framework.

```python
from datastar_py.sanic import datastar_response, ServerSentEventGenerator as SSE

@app.get('/my_route')
@datastar_response
def my_route(request):
    while True:
        yield SSE.patch_elements("<div id='mydiv'></div>")
        await asyncio.sleep(1)
```

## Signal Helpers
The current state of the datastar signals is included by default in every 
datastar request. A helper is included to load those signals for each
framework. `read_signals`

```python
from datastar_py.quart import read_signals

@app.route("/updates")
async def updates():
    signals = await read_signals()
```

## Attribute Generation Helper
Datastar allows HTML generation to be done on the backend. datastar-py includes
a helper to generate data-* attributes in your HTML with IDE completion and
type checking. It can be used with many different HTML generation libraries.

```python
from datastar_py import attribute_generator as data

# htpy
button(data.on("click", "console.log('clicked')").debounce(1000).stop)["My Button"]
# FastHTML
Button("My Button", **data.on("click", "console.log('clicked')").debounce(1000).stop)
# After next release of FastHTML you don't have to unpack the datastar helpers e.g.
Button("My Button", data.on("click", "console.log('clicked')").debounce(1000).stop)
# f-strings
f"<button {data.on("click", "console.log('clicked')").debounce(1000).stop}>My Button</button>"
# Jinja, but no editor completion :(
<button {{data.on("click", "console.log('clicked')").debounce(1000).stop}}>My Button</button>
```

When using datastar with a different alias, you can instantiate the class yourself.

```python
from datastar_py.attributes import AttributeGenerator

data = AttributeGenerator(alias="data-star-")

# htmy (htmy will transform _ into - unless the attribute starts with _, which will be stripped)
data = AttributeGenerator(alias="_data-")
html.button("My Button", **data.on("click", "console.log('clicked')").debounce("1s").stop)
```