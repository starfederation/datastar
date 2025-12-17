export { action, actions, attribute, watcher } from '@engine'
export {
  beginBatch,
  computed,
  effect,
  endBatch,
  filtered,
  getPath,
  mergePatch,
  mergePaths,
  root,
  signal,
  startPeeking,
  stopPeeking,
} from '@engine/signals'

import '@plugins/actions/peek'
import '@plugins/actions/setAll'
import '@plugins/actions/toggleAll'
import '@plugins/actions/fetch'
import '@plugins/attributes/attr'
import '@plugins/attributes/bind'
import '@plugins/attributes/class'
import '@plugins/attributes/computed'
import '@plugins/attributes/effect'
import '@plugins/attributes/indicator'
import '@plugins/attributes/init'
import '@plugins/attributes/jsonSignals'
import '@plugins/attributes/on'
import '@plugins/attributes/onIntersect'
import '@plugins/attributes/onInterval'
import '@plugins/attributes/onSignalPatch'
import '@plugins/attributes/ref'
import '@plugins/attributes/show'
import '@plugins/attributes/signals'
import '@plugins/attributes/style'
import '@plugins/attributes/text'
import '@plugins/watchers/patchElements'
import '@plugins/watchers/patchSignals'
