from __future__ import annotations

import json
from collections.abc import AsyncIterable, Iterable, Mapping
from itertools import chain
from typing import Literal, Protocol, TypeAlias, Union, overload, runtime_checkable

import datastar_py.consts as consts
from datastar_py.attributes import _escape

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
        prefix = [f"event: {event_type}"]

        if event_id:
            prefix.append(f"id: {event_id}")

        if retry_duration and retry_duration != consts.DEFAULT_SSE_RETRY_DURATION:
            prefix.append(f"retry: {retry_duration}")

        data_lines = [f"data: {line}" for line in data_lines]

        return DatastarEvent("\n".join(chain(prefix, data_lines)) + "\n\n")

    @overload
    @classmethod
    def patch_elements(
        cls,
        *,
        selector: str,
        mode: Literal[consts.ElementPatchMode.REMOVE],
        use_view_transitions: bool | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent: ...
    @overload
    @classmethod
    def patch_elements(
        cls,
        elements: str | _HtmlProvider,
        selector: str | None = None,
        mode: consts.ElementPatchMode | None = None,
        use_view_transitions: bool | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent: ...
    @classmethod
    def patch_elements(
        cls,
        elements: str | _HtmlProvider | None = None,
        selector: str | None = None,
        mode: consts.ElementPatchMode | None = None,
        use_view_transition: bool | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        if isinstance(elements, _HtmlProvider):
            elements = elements.__html__()
        data_lines = []
        if mode and mode != "outer":  # TODO: Should there be a constant for this?
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

        if elements:
            data_lines.extend(
                f"{consts.ELEMENTS_DATALINE_LITERAL} {x}" for x in elements.splitlines()
            )

        return ServerSentEventGenerator._send(
            consts.EventType.PATCH_ELEMENTS,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def remove_elements(
        cls, selector: str, event_id: str | None = None, retry_duration: int | None = None
    ) -> DatastarEvent:
        return ServerSentEventGenerator.patch_elements(
            selector=selector,
            mode=consts.ElementPatchMode.REMOVE,
            event_id=event_id,
            retry_duration=retry_duration,
        )

    @classmethod
    def patch_signals(
        cls,
        signals: dict | str,
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

        signals_str = (
            signals if isinstance(signals, str) else json.dumps(signals, separators=(",", ":"))
        )
        data_lines.extend(
            f"{consts.SIGNALS_DATALINE_LITERAL} {line}" for line in signals_str.splitlines()
        )

        return ServerSentEventGenerator._send(
            consts.EventType.PATCH_SIGNALS, data_lines, event_id, retry_duration
        )

    @classmethod
    def execute_script(
        cls,
        script: str,
        auto_remove: bool = True,
        attributes: Mapping[str, str] | list[str] | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        attribute_string = ""
        if auto_remove:
            attribute_string += ' data-effect="el.remove()"'
        if attributes:
            if isinstance(attributes, Mapping):
                attribute_string += " " + " ".join(
                    f'{_escape(k)}="{_escape(v)}"' for k, v in attributes.items()
                )
            else:
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
