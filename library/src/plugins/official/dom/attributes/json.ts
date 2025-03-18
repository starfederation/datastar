// Authors: Ben Croker
// Icon: tabler:typography
// Slug: Set the text content of an element to a reactive JSON stringified version of all signals
// Description: This attribute sets the text content of an element to a reactive JSON stringified version of all signals.

import { runtimeErr } from '../../../../engine/errors'
import {
  type AttributePlugin,
  DATASTAR_SIGNAL_EVENT,
  PluginType,
  Requirement,
} from '../../../../engine/types'

export const Json: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'json',
  keyReq: Requirement.Denied,
  valReq: Requirement.Denied,
  onLoad: (ctx) => {
    const { el, signals } = ctx

    if (!(el instanceof HTMLElement)) {
      runtimeErr('JsonInvalidElement', ctx)
    }

    const callback = () => {
      el.textContent = signals.JSON()
    }

    document.addEventListener(DATASTAR_SIGNAL_EVENT, callback)
    callback()
    
    return () => {
      document.removeEventListener(DATASTAR_SIGNAL_EVENT, callback)
    }
  },
}
