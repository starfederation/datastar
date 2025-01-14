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
        retry_duration: int = consts.DefaultSseRetryDuration,
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
        settle_duration: Optional[int] = None,
        use_view_transition: bool = consts.DefaultFragmentsUseViewTransitions,
        event_id: Optional[int] = None,
        retry_duration: int = consts.DefaultSseRetryDuration,
    ):
        data_lines = []
        if merge_mode:
            data_lines.append(f"data: {consts.MergeModeDatalineLiteral} {merge_mode}")
        if selector:
            data_lines.append(f"data: {consts.SelectorDatalineLiteral} {selector}")
        if use_view_transition:
            data_lines.append(f"data: {consts.UseViewTransitionDatalineLiteral} true")
        else:
            data_lines.append(f"data: {consts.UseViewTransitionDatalineLiteral} false")
        if settle_duration:
            data_lines.append(
                f"data: {consts.SettleDurationDatalineLiteral} {settle_duration}"
            )

        data_lines.extend(
            f"data: {consts.FragmentsDatalineLiteral} {x}"
            for fragment in fragments
            for x in fragment.splitlines()
        )

        return ServerSentEventGenerator._send(
            consts.EventType.EventTypeMergeFragments,
            data_lines,
            event_id,
            retry_duration,
        )

    @classmethod
    def remove_fragments(
        cls,
        selector: Optional[str] = None,
        settle_duration: Optional[int] = None,
        use_view_transition: bool = True,
        event_id: Optional[int] = None,
        retry_duration: int = consts.DefaultSseRetryDuration,
    ):
        data_lines = []
        if selector:
            data_lines.append(f"data: {consts.SelectorDatalineLiteral} {selector}")
        if use_view_transition:
            data_lines.append(f"data: {consts.UseViewTransitionDatalineLiteral} true")
        else:
            data_lines.append(f"data: {consts.UseViewTransitionDatalineLiteral} false")
        if settle_duration:
            data_lines.append(
                f"data: {consts.SettleDurationDatalineLiteral} {settle_duration}"
            )

        return ServerSentEventGenerator._send(
            consts.EventType.EventTypeRemoveFragments,
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
        retry_duration: int = consts.DefaultSseRetryDuration,
    ):
        data_lines = []
        if only_if_missing:
            data_lines.append(f"data: {consts.OnlyIfMissingDatalineLiteral} true")

        data_lines.extend(
            f"data: {consts.SignalsDatalineLiteral} {signalLine}"
            for signalLine in json.dumps(signals, indent=2).splitlines()
        )

        return ServerSentEventGenerator._send(
            consts.EventType.EventTypeMergeSignals, data_lines, event_id, retry_duration
        )

    @classmethod
    def remove_signals(
        cls,
        paths: list[str],
        event_id: Optional[int] = None,
        retry_duration: int = consts.DefaultSseRetryDuration,
    ):
        data_lines = []

        data_lines.extend(
            f"data: {consts.PathsDatalineLiteral} {path}" for path in paths
        )

        return ServerSentEventGenerator._send(
            consts.EventType.EventTypeRemoveSignals,
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
        retry_duration: int = consts.DefaultSseRetryDuration,
    ):
        data_lines = []
        data_lines.append(f"data: {consts.AutoRemoveDatalineLiteral} {auto_remove}")

        if attributes:
            data_lines.extend(
                f"data: {consts.AttributesDatalineLiteral} {attribute}"
                for attribute in attributes
                if attribute.strip() != consts.DefaultExecuteScriptAttributes
            )

        data_lines.extend(
            f"data: {consts.ScriptDatalineLiteral} {script_line}"
            for script_line in script.splitlines()
        )

        return ServerSentEventGenerator._send(
            consts.EventType.EventTypeExecuteScript,
            data_lines,
            event_id,
            retry_duration,
        )
