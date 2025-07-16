import { Peek } from '../plugins/actions/peek'
import { SetAll } from '../plugins/actions/setAll'
import { ToggleAll } from '../plugins/actions/toggleAll'
import { Attr } from '../plugins/attributes/attr'
import { Bind } from '../plugins/attributes/bind'
import { Class } from '../plugins/attributes/class'
import { Computed } from '../plugins/attributes/computed'
import { Effect } from '../plugins/attributes/effect'
import { Indicator } from '../plugins/attributes/indicator'
import { JsonSignals } from '../plugins/attributes/jsonSignals'
import { On } from '../plugins/attributes/on'
import { OnIntersect } from '../plugins/attributes/onIntersect'
import { OnInterval } from '../plugins/attributes/onInterval'
import { OnLoad } from '../plugins/attributes/onLoad'
import { OnSignalPatch } from "../plugins/attributes/onSignalPatch";
import { Ref } from '../plugins/attributes/ref'
import { Show } from '../plugins/attributes/show'
import { Signals } from '../plugins/attributes/signals'
import { Style } from '../plugins/attributes/style'
import { Text } from '../plugins/attributes/text'
import { DELETE } from '../plugins/backend/actions/delete'
import { GET } from '../plugins/backend/actions/get'
import { PATCH } from '../plugins/backend/actions/patch'
import { POST } from '../plugins/backend/actions/post'
import { PUT } from '../plugins/backend/actions/put'
import { PatchElements } from '../plugins/backend/watchers/patchElements'
import { PatchSignals } from '../plugins/backend/watchers/patchSignals'

export {
  // Backend
  GET,
  POST,
  PUT,
  PATCH,
  DELETE,
  PatchElements,
  PatchSignals,
  // Attributes
  Attr,
  Bind,
  Class,
  Computed,
  Effect,
  Indicator,
  JsonSignals,
  On,
  OnIntersect,
  OnInterval,
  OnLoad,
	OnSignalPatch,
  Ref,
  Show,
  Signals,
  Style,
  Text,
  // Actions
  Peek,
  SetAll,
  ToggleAll,
}