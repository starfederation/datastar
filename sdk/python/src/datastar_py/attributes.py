from __future__ import annotations

import json
import re
from collections.abc import Mapping
from typing import Literal, Iterable, TYPE_CHECKING, overload, TypeVar

if TYPE_CHECKING:
    from typing import Self

__all__ = ["data"]

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
    def signals(self, signals_object: dict | str) -> SignalsAttr:
        return SignalsAttr(signals_object)

    def computed(self, signal_name: str, expression: str) -> ComputedAttr:
        return ComputedAttr(signal_name, expression)

    @property
    def star_ignore(self) -> StarIgnoreAttr:
        return StarIgnoreAttr()

    def attr(self, attr_object: dict | str) -> AttrAttr:
        return AttrAttr(attr_object)

    def bind(self, signal_name: str) -> BaseAttr:
        return BaseAttr("bind", signal_name)

    def class_(self, class_object: dict | str) -> ClassAttr:
        return ClassAttr(class_object)

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
        return PersistAttr()

    def ref(self, signal_name: str) -> BaseAttr:
        return BaseAttr("ref", signal_name)

    def replace_url(self, url_expression: str) -> BaseAttr:
        return BaseAttr("replace-url", url_expression)

    def show(self, expression: str) -> BaseAttr:
        return BaseAttr("show", expression)

    def text(self, expression: str) -> BaseAttr:
        return BaseAttr("text", expression)

    def indicator(self, signal_name: str) -> BaseAttr:
        return BaseAttr("indicator", signal_name)

    def custom_validity(self, expression: str) -> BaseAttr:
        return BaseAttr("custom-validity", expression)

    @property
    def scroll_into_view(self) -> ScrollIntoViewAttr:
        return ScrollIntoViewAttr()

    def view_transition(self, expression: str) -> BaseAttr:
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

    def __getitem__(self, key, /) -> str | Literal[True]:
        return self._value

    def __len__(self) -> Literal[1]:
        return 1

    def __iter__(self) -> Iterable[str]:
        return [self._key()]

    def __str__(self):
        r = self._key()
        if self._value is not True:
            r += f"={self._value!r}"
        return r

    def __html__(self):
        return str(self)


TAttr = TypeVar("TAttr", bound=BaseAttr)


class CaseMod:
    def case(self: TAttr, case: Literal["camel", "kebab", "snake", "pascal"]) -> TAttr:
        self._mods["case"] = [case]
        return self

    def _to_kebab_suffix(self: TAttr, signal_name: str):
        if "-" in signal_name:
             kebab_name, from_case = signal_name.lower(), "kebab"
        elif "_" in signal_name:
            kebab_name, from_case = signal_name.lower().replace("_", "-"), "snake"
        elif signal_name[0].isupper():
            kebab_name, from_case = re.sub(r"([A-Z])", r"-\1", signal_name).lstrip("-").lower(), "pascal"
        elif signal_name.lower() != signal_name:
            kebab_name, from_case = re.sub(r"([A-Z])", r"-\1", signal_name).lower(), "camel"
        else:
            kebab_name, from_case = signal_name, None
        self._suffix = kebab_name
        if from_case:
            self._mods["case"] = [from_case]


class TimingMod:
    def debounce(
        self: TAttr,
        wait: int | float | str,
        leading: bool = False,
        notrail: bool = False,
    ) -> TAttr:
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
        self._mods["throttle"] = [str(wait)]
        if noleading:
            self._mods["throttle"].append("noleading")
        if trail:
            self._mods["throttle"].append("trail")
        return self


class ViewtransitionMod:
    @property
    def viewtransition(self: TAttr) -> TAttr:
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
        self._mods["ifmissing"] = []
        return self


class ComputedAttr(BaseAttr, CaseMod):
    def __init__(self, signal_name: str, expression: str):
        super().__init__("computed")
        self._to_kebab_suffix(signal_name)
        self._value = expression


class StarIgnoreAttr(BaseAttr):
    def __init__(self):
        super().__init__("star-ignore", True)

    @property
    def self(self) -> Self:
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


class OnAttr(BaseAttr, TimingMod, ViewtransitionMod, CaseMod):
    def __init__(self, event: str, expression: str):
        super().__init__("on")
        self._to_kebab_suffix(event)
        self._value = expression

    @property
    def once(self) -> Self:
        self._mods["once"] = []
        return self

    @property
    def passive(self) -> Self:
        self._mods["passive"] = []
        return self

    @property
    def capture(self) -> Self:
        self._mods["capture"] = []
        return self

    @property
    def window(self) -> Self:
        self._mods["window"] = []
        return self

    @property
    def outside(self) -> Self:
        self._mods["outside"] = []
        return self

    @property
    def prevent(self) -> Self:
        self._mods["prevent"] = []
        return self

    @property
    def stop(self) -> Self:
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
        self._mods["session"] = []
        return self


class ScrollIntoViewAttr(BaseAttr):
    def __init__(self):
        super().__init__("scroll-into-view", True)

    @property
    def smooth(self) -> Self:
        self._mods["smooth"] = []
        return self

    @property
    def instant(self) -> Self:
        self._mods["instant"] = []
        return self

    @property
    def auto(self) -> Self:
        self._mods["auto"] = []
        return self

    @property
    def hstart(self) -> Self:
        self._mods["hstart"] = []
        return self

    @property
    def hcenter(self) -> Self:
        self._mods["hcenter"] = []
        return self

    @property
    def hend(self) -> Self:
        self._mods["hend"] = []
        return self

    @property
    def hnearest(self) -> Self:
        self._mods["hnearest"] = []
        return self

    @property
    def vstart(self) -> Self:
        self._mods["vstart"] = []
        return self

    @property
    def vcenter(self) -> Self:
        self._mods["vcenter"] = []
        return self

    @property
    def vend(self) -> Self:
        self._mods["vend"] = []
        return self

    @property
    def vnearest(self) -> Self:
        self._mods["vnearest"] = []
        return self

    @property
    def focus(self) -> Self:
        self._mods["focus"] = []
        return self


class OnIntervalAttr(BaseAttr, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-interval")
        self._value = expression

    def duration(self, duration: int | float | str, leading: bool = False) -> Self:
        self._mods["duration"] = [duration]
        if leading:
            self._mods["duration"].append("leading")
        return self


class OnLoadAttr(BaseAttr, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-load")
        self._value = expression

    def delay(self, delay: int | float | str) -> Self:
        self._mods["delay"] = [str(delay)]
        return self


class OnRafAttr(BaseAttr, TimingMod, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-raf")
        self._value = expression


class OnSignalChangeAttr(BaseAttr, TimingMod, ViewtransitionMod):
    def __init__(self, expression: str):
        super().__init__("on-signal-change")
        self._value = expression


data = Attributes()
