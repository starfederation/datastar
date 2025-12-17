// Icon: material-symbols:mail
// Slug: Attaches an event listener to an element.
// Description: Attaches an event listener to an element, executing an expression whenever the event is triggered.

import { attribute } from '@engine'
import {
  DATASTAR_FETCH_EVENT,
  DATASTAR_SIGNAL_PATCH_EVENT,
} from '@engine/consts'
import { beginBatch, endBatch } from '@engine/signals'
import { modifyCasing } from '@utils/text'
import { modifyTiming } from '@utils/timing'
import { modifyViewTransition } from '@utils/view-transitions'

attribute({
  name: 'on',
  requirement: 'must',
  argNames: ['evt'],
  apply({ el, key, mods, rx }) {
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
      }
      beginBatch()
      rx(evt)
      endBatch()
    }
    callback = modifyViewTransition(callback, mods)
    callback = modifyTiming(callback, mods)
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
    const eventName = modifyCasing(key, mods, 'kebab')
    // Listen for Datastar events on the document
    if (
      eventName === DATASTAR_FETCH_EVENT ||
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
})
