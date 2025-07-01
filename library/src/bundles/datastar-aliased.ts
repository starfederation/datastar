import { apply, load, setAlias } from '../engine'
import { Peek } from '../plugins/framework/actions/peek'
import { SetAll } from '../plugins/framework/actions/setAll'
import { ToggleAll } from '../plugins/framework/actions/toggleAll'
import { Attr } from '../plugins/framework/attributes/attr'
import { Bind } from '../plugins/framework/attributes/bind'
import { Class } from '../plugins/framework/attributes/class'
import { Indicator } from '../plugins/framework/attributes/indicator'
import { JsonSignals } from '../plugins/framework/attributes/jsonSignals'
import { On } from '../plugins/framework/attributes/on'
import { OnIntersect } from '../plugins/framework/attributes/onIntersect'
import { OnInterval } from '../plugins/framework/attributes/onInterval'
import { OnLoad } from '../plugins/framework/attributes/onLoad'
import { Ref } from '../plugins/framework/attributes/ref'
import { Scope } from '../plugins/framework/attributes/scope'
import { Show } from '../plugins/framework/attributes/show'
import { Text } from '../plugins/framework/attributes/text'
import { DELETE } from '../plugins/framework/backend/actions/delete'
import { GET } from '../plugins/framework/backend/actions/get'
import { PATCH } from '../plugins/framework/backend/actions/patch'
import { POST } from '../plugins/framework/backend/actions/post'
import { PUT } from '../plugins/framework/backend/actions/put'
import { PatchElements } from '../plugins/framework/backend/watchers/patchElements'
import { PatchSignals } from '../plugins/framework/backend/watchers/patchSignals'

setAlias('star')

load(
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
  Indicator,
  JsonSignals,
  On,
  OnIntersect,
  OnInterval,
  OnLoad,
  Ref,
  Scope,
  Show,
  Text,
  // Actions
  Peek,
  SetAll,
  ToggleAll,
)

apply()

export { apply, load, setAlias }
