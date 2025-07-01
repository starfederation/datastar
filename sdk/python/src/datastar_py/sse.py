from __future__ import annotations

import json
from collections.abc import AsyncIterable, Iterable
from itertools import chain
from typing import Protocol, TypeAlias, Union, runtime_checkable

import datastar_py.consts as consts

SSE_HEADERS: dict[str, str] = {
    "Cache-Control": "no-cache",
    "Content-Type": "text/event-stream",
    "X-Accel-Buffering": "no",
}


@runtime_checkable
class _HtmlProvider(Protocol):
    """A type that produces text ready to be placed in an HTML document.

    This is a convention used by html producing/consuming libraries. This lets
    e.g. fasthtml fasttags, or htpy elements, be passed straight in to
    merge_fragments."""

    def __html__(self) -> str: ...


class DatastarEvent(str):
    pass


# 0..N datastar events
DatastarEvents: TypeAlias = Union[
    DatastarEvent, Iterable[DatastarEvent], AsyncIterable[DatastarEvent], None
]


class ServerSentEventGenerator:
    __slots__ = ()

    @classmethod
    def _send(
        cls,
        event_type: consts.EventType,
        data_lines: list[str],
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        prefix = []
        if event_id:
            prefix.append(f"id: {event_id}")

        prefix.append(f"event: {event_type}")

        if retry_duration and retry_duration != consts.DEFAULT_SSE_RETRY_DURATION:
            prefix.append(f"retry: {retry_duration}")

        data_lines = [f"data: {line}" for line in data_lines]

        return DatastarEvent("\n".join(chain(prefix, data_lines)) + "\n\n")

    @classmethod
    def patch_elements(
        cls,
        elements: str | _HtmlProvider,
        selector: str | None = None,
        mode: consts.ElementPatchMode | None = None,
        use_view_transition: bool | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        if isinstance(elements, _HtmlProvider):
            elements = elements.__html__()
        data_lines = []
        if mode:
            data_lines.append(f"{consts.MODE_DATALINE_LITERAL} {mode}")
        if selector:
            data_lines.append(f"{consts.SELECTOR_DATALINE_LITERAL} {selector}")
        if (
            use_view_transition is not None
            and use_view_transition != consts.DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS
        ):
            data_lines.append(
                f"{consts.USE_VIEW_TRANSITION_DATALINE_LITERAL} {_js_bool(use_view_transition)}"
            )

        data_lines.extend(f"{consts.ELEMENTS_DATALINE_LITERAL} {x}" for x in elements.splitlines())

        return ServerSentEventGenerator._send(
            consts.EventType.PATCH_ELEMENTS,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def patch_signals(
        cls,
        signals: dict,
        event_id: str | None = None,
        only_if_missing: bool | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        data_lines = []
        if (
            only_if_missing is not None
            and only_if_missing != consts.DEFAULT_PATCH_SIGNALS_ONLY_IF_MISSING
        ):
            data_lines.append(
                f"{consts.ONLY_IF_MISSING_DATALINE_LITERAL} {_js_bool(only_if_missing)}"
            )

        data_lines.append(f"{consts.SIGNALS_DATALINE_LITERAL} {json.dumps(signals)}")

        return ServerSentEventGenerator._send(
            consts.EventType.PATCH_SIGNALS, data_lines, event_id, retry_duration
        )

    @classmethod
    def execute_script(
        cls,
        script: str,
        auto_remove: bool = False,
        attributes: list[str] | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        attribute_string = ""
        if auto_remove:
            attribute_string += " data-on-load='el.remove()'"
        if attributes:
            attribute_string += " " + " ".join(attributes)
        script_tag = f"<script{attribute_string}>{script}</script>"

        return ServerSentEventGenerator.patch_elements(
            script_tag,
            mode=consts.ElementPatchMode.APPEND,
            selector="body",
            event_id=event_id,
            retry_duration=retry_duration,
        )

    @classmethod
    def redirect(cls, location: str) -> DatastarEvent:
        return cls.execute_script(f"setTimeout(() => window.location = '{location}')")


def _js_bool(b: bool) -> str:
    return "true" if b else "false"
