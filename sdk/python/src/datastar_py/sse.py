import json
from itertools import chain
from typing import Optional

import datastar_py.consts as consts

SSE_HEADERS = {
    "Cache-Control": "no-cache",
    "Connection": "keep-alive",
    "Content-Type": "text/event-stream",
}


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
        fragments: list[str],
        selector: Optional[str] = None,
        merge_mode: Optional[consts.FragmentMergeMode] = None,
        use_view_transition: bool = consts.DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS,
        event_id: Optional[int] = None,
        retry_duration: int = consts.DEFAULT_SSE_RETRY_DURATION,
    ):
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
            for fragment in fragments
            for x in fragment.splitlines()
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

        data_lines.extend(
            f"data: {consts.SIGNALS_DATALINE_LITERAL} {signalLine}"
            for signalLine in json.dumps(signals, indent=2).splitlines()
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
