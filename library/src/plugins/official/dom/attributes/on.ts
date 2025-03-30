// Authors: Delaney Gillilan
// Icon: material-symbols:mail
// Slug: Add an event listener to an element
// Description: This plugin adds an event listener to an element. The event listener can be triggered by a variety of events, such as clicks, keypresses, and more. The event listener can also be set to trigger only once, or to be passive or capture. The event listener can also be debounced or throttled. The event listener can also be set to trigger only when the event target is outside the element.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { kebab, modifyCasing } from '../../../../utils/text'
import { modifyTiming } from '../../../../utils/timing'
import { modifyViewTransition } from '../../../../utils/view-transtions'
import { DATASTAR_SSE_EVENT } from '../../backend/shared'

export const On: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'on',
  keyReq: Requirement.Must,
  valReq: Requirement.Must,
  argNames: ['evt'],
  onLoad: ({ el, key, mods, genRX }) => {
    const rx = genRX()
    let target: Element | Window | Document = el
    if (mods.has('window')) target = window

    let callback = (evt?: Event) => {
      if (evt) {
        // Always prevent default on submit events (because forms)
        if (mods.has('prevent') || key === 'submit') evt.preventDefault()
        if (mods.has('stop')) evt.stopPropagation()
      }
      rx(evt)
    }

    callback = modifyTiming(callback, mods)
    callback = modifyViewTransition(callback, mods)

    const evtListOpts: AddEventListenerOptions = {
      capture: false,
      passive: false,
      once: false,
    }
    if (mods.has('capture')) evtListOpts.capture = true
    if (mods.has('passive')) evtListOpts.passive = true
    if (mods.has('once')) evtListOpts.once = true

    const testOutside = mods.has('outside')
    if (testOutside) {
      target = document
      const cb = callback
      const targetOutsideCallback = (e?: Event) => {
        const targetHTML = e?.target as HTMLElement
        if (!el.contains(targetHTML)) {
          cb(e)
        }
      }
      callback = targetOutsideCallback
    }

    // Default to kebab-case and allow modifying
    let eventName = kebab(key)
    eventName = modifyCasing(eventName, mods)

    // Listen for Datastar SSE events on the document
    if (eventName === DATASTAR_SSE_EVENT) {
      target = document
    }

    target.addEventListener(eventName, callback, evtListOpts)
    return () => {
      target.removeEventListener(eventName, callback)
    }
  },
}
