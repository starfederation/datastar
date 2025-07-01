// Icon: material-symbols:mail
// Slug: Add an event listener to an element
// Description: This plugin adds an event listener to an element. The event listener can be triggered by a variety of events, such as clicks, keypresses, and more. The event listener can also be set to trigger only once, or to be passive or capture. The event listener can also be debounced or throttled. The event listener can also be set to trigger only when the event target is outside the element.

import {
  type AttributePlugin,
  DATASTAR_SIGNAL_PATCH_EVENT,
} from '../../../engine/types'
import { kebab, modifyCasing } from '../../../utils/text'
import { modifyTiming } from '../../../utils/timing'
import { modifyViewTransition } from '../../../utils/view-transitions'
import { DATASTAR_SSE_EVENT } from '../backend/shared'

export const On: AttributePlugin = {
  type: 'attribute',
  name: 'on',
  keyReq: 'must',
  valReq: 'must',
  argNames: ['evt'],
  onLoad: (ctx) => {
    const { el, key, mods, rx } = ctx
    let target: Element | Window | Document = el
    if (mods.has('window')) target = window
    let callback = (evt?: Event) => {
      if (evt) {
        if (mods.has('prevent')) {
          evt.preventDefault()
        }
        if (mods.has('stop')) {
          evt.stopPropagation()
        }
        // Return if not a trusted event, not a custom event and no `trust` modifier exists
        if (
          !(evt.isTrusted || evt instanceof CustomEvent || mods.has('trust'))
        ) {
          return
        }
        ctx.evt = evt
      }
      rx(evt)
    }
    callback = modifyTiming(callback, mods)
    callback = modifyViewTransition(callback, mods)
    const evtListOpts: AddEventListenerOptions = {
      capture: mods.has('capture'),
      passive: mods.has('passive'),
      once: mods.has('once'),
    }
    if (mods.has('outside')) {
      target = document
      const cb = callback
      callback = (evt?: Event) => {
        if (!el.contains(evt?.target as HTMLElement)) {
          cb(evt)
        }
      }
    }
    // Default to kebab-case and allow modifying
    let eventName = kebab(key)
    eventName = modifyCasing(eventName, mods)
    // Listen for Datastar events on the document
    if (
      eventName === DATASTAR_SSE_EVENT ||
      eventName === DATASTAR_SIGNAL_PATCH_EVENT
    ) {
      target = document
    }
    // Prevent default on form submit events
    if (el instanceof HTMLFormElement && eventName === 'submit') {
      const cb = callback
      callback = (evt?: Event) => {
        evt?.preventDefault()
        cb(evt)
      }
    }
    target.addEventListener(eventName, callback, evtListOpts)
    return () => {
      target.removeEventListener(eventName, callback)
    }
  },
}
