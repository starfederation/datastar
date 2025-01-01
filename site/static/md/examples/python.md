## Explanation

An example backend in Python.

```python
import os, json, time, secrets, uvicorn
from starlette.applications import Starlette
from starlette.responses import HTMLResponse, StreamingResponse

app = Starlette()
target = 'target'

def send_index():
    signals = {'input': '', 'output': '', 'show': True}
    index_page = f'''
<!doctype html><html>
<head>
    <title>Python/Starlette + Datastar Example</title>
    <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script></head>
<body>
    <h2>Python/Starlette + Datastar Example</h2>
    <main class="container" id="main" data-signals=\'{json.dumps(signals)}\'>
        <input type="text" placeholder="Send to server..." data-bind="input"/>
        <button data-on-click="@sse('/get')">Send State Roundtrip</button>
        <button data-on-click="@sse('/target')">Target HTML Element</button>
        <button data-on-click="$show=!$show">Toggle Feed</button>
        <div id="output" data-text="$output"></div>
        <div id="{target}"></div>
        <div data-show="$show">
            <span>Feed from server: </span>
            <span id="feed" data-on-load="@sse('/feed')"></span>
        </div></main></body></html>
'''
    return HTMLResponse(index_page)

def send_event(frag, merge=False):
    yield 'event: datastar-merge-fragments\n'
    if merge:
        yield 'data: mergeMode upsertAttributes\n'
    yield f'data: fragments {frag}\n\n'

def send_stream():
    while True:
        rand = secrets.token_hex(8)
        frag = f'<span id="feed">{rand}</span>'
        yield from send_event(frag)
        time.sleep(1)

@app.route('/')
async def homepage(request):
    return send_index()

@app.route('/get')
async def get_data(request):
    signals = json.loads(dict(request.query_params)['datastar'])
    signals['output'] = f"Your input: {signals['input']}, is {len(signals['input'])} long."
    frag = f'<main id="main" data-signals=\'{json.dumps(signals)}\'></main>'
    return StreamingResponse(
        send_event(frag, True),
        headers={"Cache-Control": "no-cache", "Connection": "keep-alive"},
        media_type="text/event-stream"
        )

@app.route('/target')
async def target_element(request):
    today = time.strftime("%Y-%m-%d %H:%M:%S")
    frag = f'<div id="{target}"><b>{today}</b></div>'
    return StreamingResponse(
        send_event(frag),
        headers={"Cache-Control": "no-cache", "Connection": "keep-alive"},
        media_type="text/event-stream"
        )

@app.route('/feed')
async def feed(request):
    return StreamingResponse(
        send_stream(),
        headers={"Cache-Control": "no-cache", "Connection": "keep-alive"},
        media_type="text/event-stream"
        )

if __name__ == "__main__":
    uvicorn.run(app, host="localhost", port=int(os.environ.get('PORT', 3000)))
```

```html
<div class="container">
  <img src="foo.img" />
</div>
```
