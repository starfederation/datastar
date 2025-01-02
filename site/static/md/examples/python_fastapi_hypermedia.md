# Python - FastAPI, Hypermedia

A Python example using Datastar with FastAPI and Hypermedia.
Based on the Python example at https://data-star.dev/examples/python

Datastar functions:
FragmentMergeType, DatastarEventMessage, SingleDatastarEventMessage are from a litestar-gist by @avila-gabriel

## FastAPI

https://fastapi.tiangolo.com/

## Hypermedia

Create `HTML` using python code
https://github.com/thomasborgen/hypermedia
Version >=5.0.0

`requirements.txt`

```
fastapi[standard]
hypermedia>=5.0.0
```

`main.py`

```python
import asyncio
import json
import secrets
from datetime import datetime
from enum import Enum
from typing import AsyncGenerator
from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, StreamingResponse
from hypermedia import (Body, Button, Div, Doctype, Footer, Head, Header, Header1,
                        Html, Input, Link, Main, Meta, Script, Span, Title)
from hypermedia.models import Element, ElementList

app = FastAPI()
target = "target"


# Datastar utilities
class FragmentMergeType(str, Enum):
  MORPH = "morph"
  INNER = "inner"
  OUTER = "outer"
  PREPEND = "prepend"
  APPEND = "append"
  BEFORE = "before"
  AFTER = "after"
  upsertAttributes = "upsertAttributes"


class DatastarEventMessage:
  def __init__(
    self,
    fragment: str,
    merge: FragmentMergeType | None = None,
    query_selector: str | None = None,
    settle_duration: int | None = None,
    use_view_transitions: bool | None = None
  ):
    data_lines: list[str] = []

    if query_selector:
      data_lines.append(f"selector {query_selector}")

    if merge:
      data_lines.append(f"mergeMode {$merge}")

    if settle_duration:
      data_lines.append(f"settleDuration {settle_duration}")

    if use_view_transitions is not None:
      data_lines.append(f"useViewTransitions {str(use_view_transitions).lower()}")

    data_lines.append(f"fragment {fragment}")

    self.data = "\ndata: ".join(data_lines)
    self.event = "datastar-merge-fragments"

  def format_sse(self):
    return f"event: {self.event}\ndata: {self.data}\n\n"


class SingleDatastarEventMessage:
  def __init__(
    self,
    fragment: str,
    merge: FragmentMergeType | None = None,
    query_selector: str | None = None,
    settle_duration: int | None = None,
    use_view_transitions: bool | None = None
  ):
    event_message = DatastarEventMessage(
      fragment=fragment,
      merge=merge,
      query_selector=query_selector,
      settle_duration=settle_duration,
      use_view_transitions=use_view_transitions
    )
    self.event_message = event_message

  async def single_event_generator(self):
    yield self.event_message.format_sse()


# Hypermedia
def real_base() -> Element:
    """Create the base page."""
    return ElementList(
      Doctype(),
      Html(
        Head(
          Title("FastAPI - Hypermedia - DataStar"),
          Meta(charset="UTF-8"),
          Meta(name="viewport", content="width=device-width, initial-scale=1.0"),
          Link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"),
          Script(type="module", src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"),
          slot="head",
        ),
        Body(slot="body"),
        slot="html",
        lan="en",
      ),
    )


def base() -> Element:
  """Create the base page."""
  return real_base().extend(
    "body",
    Header(id="header", slot="header"),
    Main(id="main", slot="main"),
    Footer(id="footer", slot="footer"),
  )


def render_header():
    return Header1("FastAPI - Hypermedia - DataStar")


def render_footer():
    return Div("FastAPI - Hypermedia - DataStar")


def render_main_partial(signals):
  return Div(
    Div(
      Input(type="text", placeholder="Send to server...", **{"data-bind":"input"}),
      Button("Send State Roundtrip", **{"data-on-click":'@get("/get")'}),
      Button("Target HTML Element", **{"data-on-click":'@get("/target")'}),
      Button("Toggle Feed", **{"data-on-click":'$show=!$show'}),
      Div(id="output", **{"data-text":"$output"}),
      Div(id=f"{target}"),
      Div(
        Span("Feed from server: "),
        Span(id="feed", **{"data-on-load":'@get("/feed")'}),
        **{"$data-show__duration.500ms":"$show"}
        ),
    ),
    id="main",
    **{"data-signals":f'{json.dumps(signals)}'}
  )


def render_index(signals):
    return base().extend(
        "header", render_header()
    ).extend(
        "main", render_main_partial(signals)
    ).extend(
        "footer", render_footer()
    )


# Routes
@app.get("/", response_class=HTMLResponse)
async def index():
    signals = {'input': '', 'output': '', 'show': True}
    page: Element = render_index(signals=signals).dump()
    return HTMLResponse(page)


@app.get("/get")
async def get_data(request: Request):
  query_params = request.query_params.get('datastar')
  signals = json.loads(query_params)
  signals['output'] = f"Your input: {signals['input']}, is {len(signals['input'])} long."
  event_data = json.dumps(signals)
  fragment = f"<div id='main' data-signals='{event_data}'></div>"
  sse = SingleDatastarEventMessage(fragment=fragment, merge=FragmentMergeType.UPSERT_ATTRIBUTES)
  return StreamingResponse(sse.single_event_generator(), media_type="text/event-stream")


@app.get("/target")
async def target_element():
  current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
  fragment = f"<div id='{target}'><b>{current_time}</b></div>"
  sse = SingleDatastarEventMessage(fragment=fragment)
  return StreamingResponse(sse.single_event_generator(), media_type="text/event-stream")


@app.get("/feed")
async def feed():
  async def feed_generator() -> AsyncGenerator[str, None]:
    while True:
      rand = secrets.token_hex(8)
      fragment = f"<span id='feed'>{rand}</span>"
      message = DatastarEventMessage(fragment=fragment, use_view_transitions=False)
      yield message.format_sse()
      await asyncio.sleep(2)

  return StreamingResponse(feed_generator(), media_type="text/event-stream")


if __name__ == "__main__":
  import uvicorn
  uvicorn.run(app, host="localhost", port=8000)
```
