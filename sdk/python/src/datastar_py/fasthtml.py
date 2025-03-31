from typing import override

from fastcore.xml import to_xml

from .sse import SSE_HEADERS, ServerSentEventGenerator as _ServerSentEventGenerator
from .starlette import DatastarStreamingResponse


class ServerSentEventGenerator(_ServerSentEventGenerator):
    @classmethod
    @override
    def merge_fragments(cls, fragments, *args, **kwargs):
        xml_fragments = [f if isinstance(f, str) else to_xml(f) for f in fragments]
        # From here, business as usual
        return super().merge_fragments(xml_fragments, *args, **kwargs)
