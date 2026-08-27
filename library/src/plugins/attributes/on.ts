// Icon: material-symbols:mail
// Slug: Attaches an event listener to an element.
// Description: Attaches an event listener to an element, executing an expression whenever the event is triggered.

import { attribute } from '@engine'
import {
  DATASTAR_FETCH_EVENT,
  DATASTAR_SIGNAL_PATCH_EVENT,
  DOCUMENT,
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
    if (mods.has('window')) {
      target = window
    } else if (mods.has('document')) {
      target = DOCUMENT
    }
    let callback = (evt?: Event) => {
      beginBatch()
      try {
        rx(evt)
      } finally {
        endBatch()
      }
    }
    callback = modifyViewTransition(callback, mods)
    callback = modifyTiming(callback, mods)
    const eventName = modifyCasing(key, mods, 'kebab')
    const evtListOpts: AddEventListenerOptions = {
      capture: mods.has('capture'),
      passive: mods.has('passive'),
      once: mods.has('once'),
    }
    if (mods.has('outside')) {
      target = DOCUMENT
      const cb = callback
      callback = (evt?: Event) => {
        if (!el.contains(evt?.target as HTMLElement)) {
          cb(evt)
        }
      }
    }
    // Listen for Datastar events on the document
    if (
      eventName === DATASTAR_FETCH_EVENT ||
      eventName === DATASTAR_SIGNAL_PATCH_EVENT
    ) {
      target = DOCUMENT
    }
    // Apply event-side effects before timing/view-transition wrappers run.
    const listener = (evt?: Event) => {
      if (evt) {
        if (mods.has('prevent')) evt.preventDefault()
        if (mods.has('stop')) evt.stopPropagation()
        // Keep data-on:submit from falling through to native form submission.
        if (el instanceof HTMLFormElement && eventName === 'submit') evt.preventDefault()
      }
      callback(evt)
    }
    target.addEventListener(eventName, listener, evtListOpts)
    return () => {
      target.removeEventListener(eventName, listener, evtListOpts)
    }
  },
})
