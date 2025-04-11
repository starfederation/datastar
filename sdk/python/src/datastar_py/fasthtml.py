from typing import override

from fastcore.xml import to_xml

from .sse import SSE_HEADERS, ServerSentEventGenerator
from .starlette import DatastarStreamingResponse as _DatastarStreamingResponse


class DatastarStreamingResponse(_DatastarStreamingResponse):
    @classmethod
    @override
    def merge_fragments(cls, fragments, *args, **kwargs):
        if not isinstance(fragments, str):
            fragments = to_xml(fragments)
        # From here, business as usual
        return super().merge_fragments(fragments, *args, **kwargs)
