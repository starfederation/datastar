from __future__ import annotations

import json
import re
from collections.abc import Mapping
from typing import Literal, Iterable, TYPE_CHECKING, overload, TypeVar, Iterator

if TYPE_CHECKING:
    from typing import Self

__all__ = ["attribute_generator"]

JSEvent = Literal[
    "abort",
    "afterprint",
    "animationend",
    "animationiteration",
    "animationstart",
    "beforeprint",
    "beforeunload",
    "blur",
    "canplay",
    "canplaythrough",
    "change",
    "click",
    "contextmenu",
    "copy",
    "cut",
    "dblclick",
    "drag",
    "dragend",
    "dragenter",
    "dragleave",
    "dragover",
    "dragstart",
    "drop",
    "durationchange",
    "ended",
    "error",
    "focus",
    "focusin",
    "focusout",
    "fullscreenchange",
    "fullscreenerror",
    "hashchange",
    "input",
    "invalid",
    "keydown",
    "keypress",
    "keyup",
    "load",  #
    "loadeddata",
    "loadedmetadata",
    "loadstart",
    "message",
    "mousedown",
    "mouseenter",
    "mouseleave",
    "mousemove",
    "mouseover",
    "mouseout",
    "mouseup",
    "mousewheel",
    "offline",
    "online",
    "open",
    "pagehide",
    "pageshow",
    "paste",
    "pause",
    "play",
    "playing",
    "popstate",
    "progress",
    "ratechange",
    "resize",
    "reset",
    "scroll",
    "search",
    "seeked",
    "seeking",
    "select",
    "show",
    "stalled",
    "storage",
    "submit",
    "suspend",
    "timeupdate",
    "toggle",
    "touchcancel",
    "touchend",
    "touchmove",
    "touchstart",
    "transitionend",
    "unload",
    "volumechange",
    "waiting",
    "wheel",
]


class Attributes:
    @overload
    def signals(
        self, signals_dict: Mapping | None = None, /, **signals: str
    ) -> SignalsAttr: ...
    @overload
    def signals(self, signals_object: str, /) -> SignalsAttr: ...
    def signals(
        self, signals_object: Mapping | str | None = None, /, **signals: str
    ) -> SignalsAttr:
        """Merges one or more signals into the existing signals."""
        if signals and signals_object and not isinstance(signals_object, Mapping):
            raise TypeError(
                "Cannot provide both a string object and keyword arguments."
            )
        if isinstance(signals_object, str):
            return SignalsAttr(signals_object)
        signals = {**(signals_object if signals_object else {}), **signals}
        return SignalsAttr(signals)

    @overload
    def computed(self, **computed: str) -> AttrGroup: ...
    @overload
    def computed(self, signal_name: str, expression: str) -> AttrGroup: ...
    def computed(
        self,
        signal_name: str | None = None,
        expression: str | None = None,
        **computed: str,
    ) -> AttrGroup:
        """Creates a signal that is computed based on an expression."""
        if signal_name and expression:
            computed[signal_name] = expression
        return AttrGroup(ComputedAttr(sig, expr) for sig, expr in computed.items())

    @property
    def star_ignore(self) -> StarIgnoreAttr:
        """Tells Datastar to ignore data-* attributes on the element."""
        return StarIgnoreAttr()

    @overload
    def attr(self, attr_dict: dict | None = None, /, **attrs: str) -> AttrAttr: ...
    @overload
    def attr(self, attr_object: str, /) -> AttrAttr: ...
    def attr(self, attr_object: dict | str | None = None, /, **attrs: str) -> AttrAttr:
        """Sets the value of any HTML attribute to an expression, and keeps it in sync."""
        if attrs and attr_object and not isinstance(attr_object, Mapping):
            raise TypeError(
                "Cannot provide both a string object and keyword arguments."
            )
        if isinstance(attr_object, str):
            return AttrAttr(attr_object)
        attrs = {**(attr_object if attr_object else {}), **attrs}
        return AttrAttr(attrs)

    def bind(self, signal_name: str) -> BaseAttr:
        """Sets up two-way data binding between a signal and an element’s value."""
        return BaseAttr("bind", signal_name)

    @overload
    def class_(
        self, class_dict: dict | None = None, /, **classes: str
    ) -> ClassAttr: ...
    @overload
    def class_(self, class_object: str, /) -> ClassAttr: ...
    def class_(
        self, class_object: dict | str | None = None, **classes: str
    ) -> ClassAttr:
        """Adds or removes classes to or from an element based on expressions."""
        if classes and class_object and not isinstance(class_object, Mapping):
            raise TypeError(
                "Cannot provide both a string object and keyword arguments."
            )
        if isinstance(class_object, str):
            return ClassAttr(class_object)
        classes = {**(class_object if class_object else {}), **classes}
        return ClassAttr(classes)

    @overload
    def on(self, event: Literal["interval"], expression: str) -> OnIntervalAttr: ...
    @overload
    def on(self, event: Literal["load"], expression: str) -> OnLoadAttr: ...
    @overload
    def on(self, event: Literal["raf"], expression: str) -> OnRafAttr: ...
    @overload
    def on(
        self, event: Literal["signal-change"], expression: str
    ) -> OnSignalChangeAttr: ...
    @overload
    def on(self, event: JSEvent | str, expression: str) -> OnAttr: ...
    def on(
        self, event: str, expression: str
    ) -> OnAttr | OnIntervalAttr | OnLoadAttr | OnRafAttr | OnSignalChangeAttr:
        """Attaches an event listener to an element, executing an expression whenever the event is triggered."""
        if event == "interval":
            return OnIntervalAttr(expression)
        elif event == "load":
            return OnLoadAttr(expression)
        elif event == "raf":
            return OnRafAttr(expression)
        elif event == "signal-change":
            return OnSignalChangeAttr(expression)
        return OnAttr(event, expression)

    @property
    def persist(self) -> PersistAttr:
        """Persists signals in local storage. This is useful for storing values between page loads."""
        return PersistAttr()

    def ref(self, signal_name: str) -> BaseAttr:
        """Creates a new signal that is a reference to the element on which the data attribute is placed."""
        return BaseAttr("ref", signal_name)

    def replace_url(self, url_expression: str) -> BaseAttr:
        return BaseAttr("replace-url", url_expression)

    def show(self, expression: str) -> BaseAttr:
        """Show or hides an element based on whether an expression evaluates to true or false."""
        return BaseAttr("show", expression)

    def text(self, expression: str) -> BaseAttr:
        """Binds the text content of an element to an expression."""
        return BaseAttr("text", expression)

    def indicator(self, signal_name: str) -> BaseAttr:
        """Creates a signal and sets its value to true while an SSE request request is in flight, otherwise false."""
        return BaseAttr("indicator", signal_name)

    def custom_validity(self, expression: str) -> BaseAttr:
        """Sets the validity message for an element based on an expression."""
        return BaseAttr("custom-validity", expression)

    @property
    def scroll_into_view(self) -> ScrollIntoViewAttr:
        """Scrolls the element into view."""
        return ScrollIntoViewAttr()

    def view_transition(self, expression: str) -> BaseAttr:
        """Sets the view-transition-name style attribute explicitly."""
        return BaseAttr("view-transition", expression)


class BaseAttr(Mapping):
    def __init__(
        self,
        attr: str,
        value: str | Literal[True] = True,
        suffix: str | None = None,
    ):
        self._attr: str = attr
        self._suffix: str | None = suffix
        self._mods: dict[str, list[str]] = {}
        self._value: str | Literal[True] = value

    def __call__(self) -> Self:
        # Because some attributes and modifiers do not need to be called
        # allow calling them anyway so that everything _could_ be used consistently
        return self

    def _key(self) -> str:
        key = f"data-{self._attr}"
        if self._suffix:
            key += f"-{self._suffix}"
        for mod, values in self._mods.items():
            key += f"__{mod}"
            if values:
                key += f".{'.'.join(values)}"
        return key

    def _to_kebab_suffix(self, signal_name: str):
        if "-" in signal_name:
            kebab_name, from_case = signal_name.lower(), "kebab"
        elif "_" in signal_name:
            kebab_name, from_case = signal_name.lower().replace("_", "-"), "snake"
        elif signal_name[0].isupper():
            kebab_name, from_case = (
                re.sub(r"([A-Z])", r"-\1", signal_name).lstrip("-").lower(),
                "pascal",
            )
        elif signal_name.lower() != signal_name:
            kebab_name, from_case = (
                re.sub(r"([A-Z])", r"-\1", signal_name).lower(),
                "camel",
            )
        else:
            kebab_name, from_case = signal_name, None
        self._suffix = kebab_name
        if from_case:
            self._mods["case"] = [from_case]

    def __getitem__(self, key, /) -> str | Literal[True]:
        return self._value

    def __len__(self) -> Literal[1]:
        return 1

    def __iter__(self) -> Iterator[str]:
        return iter([self._key()])

    def __str__(self):
        r = _escape(self._key())
        if isinstance(self._value, str):
            r += f'="{_escape(self._value)}"'
        return r

    __html__ = __str__


class AttrGroup(Mapping):
    def __init__(self, attrs: Iterable[BaseAttr]):
        self._attrs: list[BaseAttr] = list(attrs)
        self._attr_dict: dict[str, str] = {}
        for attr in self._attrs:
            self._attr_dict.update(attr)
        self._attr_string: str = " ".join(str(attr) for attr in self._attrs)

    def __iter__(self):
        return iter(self._attr_dict)

    def __len__(self):
        return len(self._attr_dict)

    def __getitem__(self, key, /):
        return self._attr_dict[key]

    def __str__(self):
        return self._attr_string

    __html__ = __str__


TAttr = TypeVar("TAttr", bound=BaseAttr)


class TimingMod:
    def debounce(
        self: TAttr,
        wait: int | float | str,
        leading: bool = False,
        notrail: bool = False,
    ) -> TAttr:
        """Debounce the event listener.

        :param wait: The minimum interval between events.
        :param leading: If true, the event listener will be called on the leading edge of the wait time.
        :param notrail: If true, the event listener will not be called on the trailing edge of the wait time.
        """
        self._mods["debounce"] = [str(wait)]
        if leading:
            self._mods["debounce"].append("leading")
        if notrail:
            self._mods["debounce"].append("notrail")
        return self

    def throttle(
        self: TAttr,
        wait: int | float | str,
        noleading: bool = False,
        trail: bool = False,
    ) -> TAttr:
        """Throttle the event listener.

        :param wait: The minimum interval between events.
        :param noleading: If true, the event listener will not be called on the leading edge of the wait time.
        :param trail: If true, the event listener will be called on the trailing edge of the wait time.
        """
        self._mods["throttle"] = [str(wait)]
        if noleading:
            self._mods["throttle"].append("noleading")
        if trail:
            self._mods["throttle"].append("trail")
        return self


class ViewtransitionMod:
    @property
    def viewtransition(self: TAttr) -> TAttr:
        """Wraps the expression in document.startViewTransition() when the View Transition API is available."""
        self._mods["view-transition"] = []
        return self


class SignalsAttr(BaseAttr):
    def __init__(self, signals_object: dict | str):
        super().__init__("signals")
        if isinstance(signals_object, dict):
            self._value = json.dumps(signals_object)
        else:
            self._value = signals_object

    @property
    def ifmissing(self) -> Self:
        """Only merges signals if their keys do not already exist."""
        self._mods["ifmissing"] = []
        return self


class ComputedAttr(BaseAttr):
    def __init__(self, signal_name: str, expression: str):
        super().__init__("computed", expression)
        self._to_kebab_suffix(signal_name)


class StarIgnoreAttr(BaseAttr):
    def __init__(self):
        super().__init__("star-ignore", True)

    @property
    def self(self) -> Self:
        """Only ignore the element itself, not its descendants."""
        self._mods["self"] = []
        return self


class AttrAttr(BaseAttr):
    def __init__(self, attr_object: dict | str):
        super().__init__("attr")
        if isinstance(attr_object, dict):
            # Serialize to a Javascript object without the values quoted
            # We want the values to be d* expressions, not strings
            r = "{"
            for k, v in attr_object.items():
                r += json.dumps(str(k))
                r += f": {v},"
            r = r.rstrip(",")
            r += "}"
            self._value = r
        else:
            self._value = attr_object


class ClassAttr(BaseAttr):
    def __init__(self, class_object: dict | str):
        super().__init__("class")
        if isinstance(class_object, dict):
            # Serialize to a Javascript object without the values quoted
            # We want the values to be d* expressions, not strings
            r = "{"
            for k, v in class_object.items():
                r += json.dumps(str(k))
                r += f": {v},"
            r = r.rstrip(",")
            r += "}"
            self._value = r
        else:
            self._value = class_object


class OnAttr(BaseAttr, TimingMod, ViewtransitionMod):
    def __init__(self, event: str, expression: str):
        super().__init__("on", expression)
        self._to_kebab_suffix(event)

    @property
    def once(self) -> Self:
        """Only trigger the event listener once."""
        self._mods["once"] = []
        return self

    @property
    def passive(self) -> Self:
        """Do not call preventDefault on the event listener."""
        self._mods["passive"] = []
        return self

    @property
    def capture(self) -> Self:
        """Use a capture event listener."""
        self._mods["capture"] = []
        return self

    @property
    def window(self) -> Self:
        """Attaches the event listener to the window element."""
        self._mods["window"] = []
        return self

    @property
    def outside(self) -> Self:
        """Triggers when the event is outside the element."""
        self._mods["outside"] = []
        return self

    @property
    def prevent(self) -> Self:
        """Calls preventDefault on the event listener."""
        self._mods["prevent"] = []
        return self

    @property
    def stop(self) -> Self:
        """Calls stopPropagation on the event listener."""
        self._mods["stop"] = []
        return self


class PersistAttr(BaseAttr):
    def __init__(self):
        super().__init__("persist", True)

    def __call__(self, signal_names: str | list[str] | None = None) -> Self:
        if isinstance(signal_names, str):
            self._value = signal_names
        elif isinstance(signal_names, list):
            self._value = " ".join(signal_names)
        return self

    @property
    def session(self) -> Self:
        """Persists signals in session storage."""
        self._mods["session"] = []
        return self


class ScrollIntoViewAttr(BaseAttr):
    def __init__(self):
        super().__init__("scroll-into-view", True)

    @property
    def smooth(self) -> Self:
        """Scrolling is animated smoothly."""
        self._mods["smooth"] = []
        return self

    @property
    def instant(self) -> Self:
        """Scrolling is instant."""
        self._mods["instant"] = []
        return self

    @property
    def auto(self) -> Self:
        """Scrolling is determined by the computed scroll-behavior CSS property."""
        self._mods["auto"] = []
        return self

    @property
    def hstart(self) -> Self:
        """Scrolls to the left of the element."""
        self._mods["hstart"] = []
        return self

    @property
    def hcenter(self) -> Self:
        """Scrolls to the horizontal center of the element."""
        self._mods["hcenter"] = []
        return self

    @property
    def hend(self) -> Self:
        """Scrolls to the right of the element."""
        self._mods["hend"] = []
        return self

    @property
    def hnearest(self) -> Self:
        """Scrolls to the nearest horizontal edge of the element."""
        self._mods["hnearest"] = []
        return self

    @property
    def vstart(self) -> Self:
        """Scrolls to the top of the element."""
        self._mods["vstart"] = []
        return self

    @property
    def vcenter(self) -> Self:
        """Scrolls to the vertical center of the element."""
        self._mods["vcenter"] = []
        return self

    @property
    def vend(self) -> Self:
        """Scrolls to the bottom of the element."""
        self._mods["vend"] = []
        return self

    @property
    def vnearest(self) -> Self:
        """Scrolls to the nearest vertical edge of the element."""
        self._mods["vnearest"] = []
        return self

    @property
    def focus(self) -> Self:
        """Focuses the element after scrolling."""
        self._mods["focus"] = []
        return self


class OnIntervalAttr(BaseAttr, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-interval", expression)

    def duration(self, duration: int | float | str, leading: bool = False) -> Self:
        """Sets the interval duration."""
        self._mods["duration"] = [str(duration)]
        if leading:
            self._mods["duration"].append("leading")
        return self


class OnLoadAttr(BaseAttr, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-load", expression)

    def delay(self, delay: int | float | str) -> Self:
        """Delay the event listener."""
        self._mods["delay"] = [str(delay)]
        return self


class OnRafAttr(BaseAttr, TimingMod, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-raf", expression)


class OnSignalChangeAttr(BaseAttr, TimingMod, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-signal-change", expression)


def _escape(s: str) -> str:
    return (
        s.replace("&", "&amp;")
        .replace("'", "&#39;")
        .replace('"', "&#34;")
        .replace(">", "&gt;")
        .replace("<", "&lt;")
    )


attribute_generator = Attributes()
