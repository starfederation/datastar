from __future__ import annotations

import json
from itertools import chain
from typing import (
    Optional,
    Protocol,
    Union,
    runtime_checkable,
    Callable,
    AsyncIterable,
    TypeVar,
    Iterable,
)

import datastar_py.consts as consts

SSE_HEADERS = {
    "Cache-Control": "no-cache",
    "Content-Type": "text/event-stream",
}


@runtime_checkable
class _HtmlProvider(Protocol):
    """A type that produces text ready to be placed in an HTML document.

    This is a convention used by html producing/consuming libraries. This lets
    e.g. fasthtml fasttags, or htpy elements, be passed straight in to
    merge_fragments."""

    def __html__(self) -> str: ...


class ServerSentEventGenerator:
    __slots__ = ()

    @classmethod
    def _send(
        cls,
        event_type: consts.EventType,
        data_lines: list[str],
        event_id: Optional[int] = None,
        retry_duration: int = consts.DEFAULT_SSE_RETRY_DURATION,
    ) -> str:
        prefix = []
        if event_id:
            prefix.append(f"id: {event_id}")

        prefix.append(f"event: {event_type}")

        if retry_duration:
            prefix.append(f"retry: {retry_duration}")

        data_lines.append("\n")

        return "\n".join(chain(prefix, data_lines))

    @classmethod
    def merge_fragments(
        cls,
        fragments: Union[str, _HtmlProvider],
        selector: Optional[str] = None,
        merge_mode: Optional[consts.FragmentMergeMode] = None,
        use_view_transition: bool = consts.DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS,
        event_id: Optional[int] = None,
        retry_duration: int = consts.DEFAULT_SSE_RETRY_DURATION,
    ):
        if isinstance(fragments, _HtmlProvider):
            fragments = fragments.__html__()
        data_lines = []
        if merge_mode:
            data_lines.append(f"data: {consts.MERGE_MODE_DATALINE_LITERAL} {merge_mode}")
        if selector:
            data_lines.append(f"data: {consts.SELECTOR_DATALINE_LITERAL} {selector}")
        if use_view_transition:
            data_lines.append(f"data: {consts.USE_VIEW_TRANSITION_DATALINE_LITERAL} true")
        else:
            data_lines.append(f"data: {consts.USE_VIEW_TRANSITION_DATALINE_LITERAL} false")

        data_lines.extend(
            f"data: {consts.FRAGMENTS_DATALINE_LITERAL} {x}"
            for x in fragments.splitlines()
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
        selector: Optional[str] = None,
        use_view_transition: bool = True,
        event_id: Optional[int] = None,
        retry_duration: int = consts.DEFAULT_SSE_RETRY_DURATION,
    ):
        data_lines = []
        if selector:
            data_lines.append(f"data: {consts.SELECTOR_DATALINE_LITERAL} {selector}")
        if use_view_transition:
            data_lines.append(f"data: {consts.USE_VIEW_TRANSITION_DATALINE_LITERAL} true")
        else:
            data_lines.append(f"data: {consts.USE_VIEW_TRANSITION_DATALINE_LITERAL} false")

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
        event_id: Optional[int] = None,
        only_if_missing: bool = False,
        retry_duration: int = consts.DEFAULT_SSE_RETRY_DURATION,
    ):
        data_lines = []
        if only_if_missing:
            data_lines.append(f"data: {consts.ONLY_IF_MISSING_DATALINE_LITERAL} true")

        data_lines.append(
            f"data: {consts.SIGNALS_DATALINE_LITERAL} {json.dumps(signals)}"
        )

        return ServerSentEventGenerator._send(
            consts.EventType.MERGE_SIGNALS, data_lines, event_id, retry_duration
        )

    @classmethod
    def remove_signals(
        cls,
        paths: list[str],
        event_id: Optional[int] = None,
        retry_duration: int = consts.DEFAULT_SSE_RETRY_DURATION,
    ):
        data_lines = []

        data_lines.extend(
            f"data: {consts.PATHS_DATALINE_LITERAL} {path}" for path in paths
        )

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
        auto_remove: bool = True,
        attributes: Optional[list[str]] = None,
        event_id: Optional[int] = None,
        retry_duration: int = consts.DEFAULT_SSE_RETRY_DURATION,
    ):
        data_lines = []
        data_lines.append(f"data: {consts.AUTO_REMOVE_DATALINE_LITERAL} {auto_remove}")

        if attributes:
            data_lines.extend(
                f"data: {consts.ATTRIBUTES_DATALINE_LITERAL} {attribute}"
                for attribute in attributes
                if attribute.strip() != consts.DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES
            )

        data_lines.extend(
            f"data: {consts.SCRIPT_DATALINE_LITERAL} {script_line}"
            for script_line in script.splitlines()
        )

        return ServerSentEventGenerator._send(
            consts.EventType.EXECUTE_SCRIPT,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def redirect(cls, location: str):
        return cls.execute_script(f"setTimeout(() => window.location = '{location}')")


def _wrap_event(event: str | _HtmlProvider | dict) -> str:
    if isinstance(event, _HtmlProvider) or (
        isinstance(event, str) and event.startswith("<")
    ):
        return ServerSentEventGenerator.merge_fragments(event)
    elif isinstance(event, dict):
        return ServerSentEventGenerator.merge_signals(event)
    else:
        return event


async def _async_map(func: Callable, async_iter: AsyncIterable) -> AsyncIterable:
    async for item in async_iter:
        yield func(item)


SyncOrAsyncIterable = TypeVar("SyncOrAsyncIterable", AsyncIterable, Iterable)


def _sse_iterable_wrapper(iterable: SyncOrAsyncIterable) -> SyncOrAsyncIterable:
    """Wraps an iterable to allow implicitly turning fragments and dictionaries
    into merge-fragments and merge-signals events."""
    if isinstance(iterable, AsyncIterable):
        return _async_map(_wrap_event, iterable)
    else:
        return map(_wrap_event, iterable)
