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

        if retry_duration:
            prefix.append(f"retry: {retry_duration}")

        data_lines = [f"data: {line}" for line in data_lines]

        return DatastarEvent("\n".join(chain(prefix, data_lines)) + "\n\n")

    @classmethod
    def merge_fragments(
        cls,
        fragments: str | _HtmlProvider,
        selector: str | None = None,
        merge_mode: consts.FragmentMergeMode | None = None,
        use_view_transition: bool | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        if isinstance(fragments, _HtmlProvider):
            fragments = fragments.__html__()
        data_lines = []
        if merge_mode:
            data_lines.append(f"{consts.MERGE_MODE_DATALINE_LITERAL} {merge_mode}")
        if selector:
            data_lines.append(f"{consts.SELECTOR_DATALINE_LITERAL} {selector}")
        if use_view_transition is not None:
            data_lines.append(
                f"{consts.USE_VIEW_TRANSITION_DATALINE_LITERAL} {_js_bool(use_view_transition)}"
            )

        data_lines.extend(
            f"{consts.FRAGMENTS_DATALINE_LITERAL} {x}" for x in fragments.splitlines()
        )

        return ServerSentEventGenerator._send(
            consts.EventType.MERGE_FRAGMENTS,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def remove_fragments(
        cls,
        selector: str | None = None,
        use_view_transition: bool | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        data_lines = []
        if selector:
            data_lines.append(f"{consts.SELECTOR_DATALINE_LITERAL} {selector}")
        if use_view_transition is not None:
            data_lines.append(
                f"{consts.USE_VIEW_TRANSITION_DATALINE_LITERAL} {_js_bool(use_view_transition)}"
            )

        return ServerSentEventGenerator._send(
            consts.EventType.REMOVE_FRAGMENTS,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def merge_signals(
        cls,
        signals: dict,
        event_id: str | None = None,
        only_if_missing: bool | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        data_lines = []
        if only_if_missing is not None:
            data_lines.append(
                f"{consts.ONLY_IF_MISSING_DATALINE_LITERAL} {_js_bool(only_if_missing)}"
            )

        data_lines.append(f"{consts.SIGNALS_DATALINE_LITERAL} {json.dumps(signals)}")

        return ServerSentEventGenerator._send(
            consts.EventType.MERGE_SIGNALS, data_lines, event_id, retry_duration
        )

    @classmethod
    def remove_signals(
        cls,
        paths: list[str],
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        data_lines = [f"{consts.PATHS_DATALINE_LITERAL} {path}" for path in paths]

        return ServerSentEventGenerator._send(
            consts.EventType.REMOVE_SIGNALS,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def execute_script(
        cls,
        script: str,
        auto_remove: bool | None = None,
        attributes: list[str] | None = None,
        event_id: str | None = None,
        retry_duration: int | None = None,
    ) -> DatastarEvent:
        data_lines = []

        if auto_remove is not None:
            data_lines.append(f"{consts.AUTO_REMOVE_DATALINE_LITERAL} {_js_bool(auto_remove)}")

        if attributes:
            data_lines.extend(
                f"{consts.ATTRIBUTES_DATALINE_LITERAL} {attribute}"
                for attribute in attributes
                if attribute.strip() != consts.DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES
            )

        data_lines.extend(
            f"{consts.SCRIPT_DATALINE_LITERAL} {script_line}"
            for script_line in script.splitlines()
        )

        return ServerSentEventGenerator._send(
            consts.EventType.EXECUTE_SCRIPT,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def redirect(cls, location: str) -> DatastarEvent:
        return cls.execute_script(f"setTimeout(() => window.location = '{location}')")


def _js_bool(b: bool) -> str:
    return "true" if b else "false"
