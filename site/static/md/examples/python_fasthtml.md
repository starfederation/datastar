# Python - FastHTML

A Python example using Datastar with FastHTML.
Based on the Python example at https://data-star.dev/examples/python

Datastar functions:
FragmentMergeType, DatastarEventMessage, SingleDatastarEventMessage are from a litestar-gist by @avila-gabriel


## FastHTML
https://www.fastht.ml
Version >=0.3.3


`requirements.txt`
```
python-fasthtml>=0.3.3
uvicorn>=0.30
```

`main.py`
```python
import asyncio
import json
import secrets
from datetime import datetime
from enum import Enum
from typing import AsyncGenerator

from fasthtml.common import (H1, Body, Button, Div, Footer, Head, Header, Html,
                             Input, Link, Main, Meta, Script, Span, Title,
                             fast_app, serve)
from starlette.responses import StreamingResponse

app,rt = fast_app()

target = "target"


# FastHTML
def page_header():
  return Header(
    H1("FastHTML & Datastar", id="header")
  )

def page_footer():
  return Footer(
    Div("FastHTML - DataStar", id="footer")
  )

def page_main(signals):
  return Main(
  Div(
    Input(type="text", placeholder="Send to server...", data_model="input"),
    Button("Send State Roundtrip", data_on_click='sse("/get")'),
    Button("Target HTML Element", data_on_click='sse("/target")'),
    Button("Toggle Feed", data_on_click="$show=!$show"),
    Div(id="output", data_text="$output"),
    Div(id=f"{target}"),
    Div(
      Span("Feed from server: "),
      Span(id="feed", **{'data-on-load':"sse('/feed')"}),
      **{"data-show__duration.500ms":"$show"},
    ),
  ),
  id="main",
  data_signals=f'{json.dumps(signals)}'
  )

def page(signals):
  return Html(
    Head(
        Title("FastHTML - DataStar"),
        Meta(charset="UTF-8"),
        Meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        Link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"),
        Script(type="module", src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js")
      ),
    Body(
        page_header(),
        page_main(signals),
        page_footer(),
      )
    )


# Datastar utilities
class FragmentMergeType(str, Enum):
  MORPH = "morph"
  INNER = "inner"
  OUTER = "outer"
  PREPEND = "prepend"
  APPEND = "append"
  BEFORE = "before"
  AFTER = "after"
  UPSERT_ATTRIBUTES = "upsertAttributes"


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


# Routes
@rt('/')
async def get():
  signals = {'input': '', 'output': '', 'show': True}

  return page(signals)

@app.get("/get")
async def get(req):
  query_params = req.query_params.get('datastar')
  signals = json.loads(query_params)
  signals['output'] = f"Your input: {signals['input']}, is {len(signals['input'])} long."
  event_data = json.dumps(signals)
  fragment = f"<div id='main' data-signals='{event_data}'></div>"
  sse = SingleDatastarEventMessage(fragment=fragment, merge=FragmentMergeType.UPSERT_ATTRIBUTES)

  return StreamingResponse(sse.single_event_generator(), media_type="text/event-stream")


@rt("/target")
async def get():
  current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
  fragment = f"<div id='{target}'><b>{current_time}</b></div>"
  sse = SingleDatastarEventMessage(fragment=fragment)

  return StreamingResponse(sse.single_event_generator(), media_type="text/event-stream")


@rt("/feed")
async def get():
  async def feed_generator() -> AsyncGenerator[str, None]:
    while True:
      rand = secrets.token_hex(8)
      fragment = f"<span id='feed'>{rand}</span>"
      message = DatastarEventMessage(fragment=fragment, use_view_transitions=False)
      yield message.format_sse()
      await asyncio.sleep(2)

  return StreamingResponse(feed_generator(), media_type="text/event-stream")


serve()

```
