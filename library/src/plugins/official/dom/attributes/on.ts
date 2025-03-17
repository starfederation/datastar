// Authors: Delaney Gillilan
// Icon: material-symbols:mail
// Slug: Add an event listener to an element
// Description: This action adds an event listener to an element. The event listener can be triggered by a variety of events, such as clicks, keypresses, and more. The event listener can also be set to trigger only once, or to be passive or capture. The event listener can also be debounced or throttled. The event listener can also be set to trigger only when the event target is outside the element.

import {
  type AttributePlugin,
  DATASTAR_SIGNAL_EVENT,
  type DatastarSignalEvent,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { tagHas, tagToMs } from '../../../../utils/tags'
import { camel, kebab, modifyCasing } from '../../../../utils/text'
import { debounce, delay, throttle } from '../../../../utils/timing'
import { supportsViewTransitions } from '../../../../utils/view-transtions'

const EVT = 'evt'
export const SIGNALS_CHANGE_PREFIX = 'signalsChange'
const signalChangeKeyLength = SIGNALS_CHANGE_PREFIX.length

export const On: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'on',
  keyReq: Requirement.Must,
  valReq: Requirement.Must,
  argNames: [EVT],
  onLoad: ({ el, key, mods, genRX }) => {
    const { rxFn } = genRX()
    let target: Element | Window | Document = el
    if (mods.has('window')) target = window

    let callback = (evt?: Event) => {
      if (evt) {
        // Always prevent default on submit events (because forms)
        if (mods.has('prevent') || key === 'submit') evt.preventDefault()
        if (mods.has('stop')) evt.stopPropagation()
      }
      rxFn(evt)
    }

    const delayArgs = mods.get('delay')
    if (delayArgs) {
      const wait = tagToMs(delayArgs)
      callback = delay(callback, wait)
    }

    const debounceArgs = mods.get('debounce')
    if (debounceArgs) {
      const wait = tagToMs(debounceArgs)
      const leading = tagHas(debounceArgs, 'leading', false)
      const trailing = !tagHas(debounceArgs, 'notrail', false)
      callback = debounce(callback, wait, leading, trailing)
    }

    const throttleArgs = mods.get('throttle')
    if (throttleArgs) {
      const wait = tagToMs(throttleArgs)
      const leading = !tagHas(throttleArgs, 'noleading', false)
      const trailing = tagHas(throttleArgs, 'trail', false)
      callback = throttle(callback, wait, leading, trailing)
    }

    if (mods.has('viewtransition') && supportsViewTransitions) {
      const cb = callback // I hate javascript
      callback = (...args: any[]) =>
        document.startViewTransition(() => cb(...args))
    }

    const evtListOpts: AddEventListenerOptions = {
      capture: true,
      passive: false,
      once: false,
    }
    if (!mods.has('capture')) evtListOpts.capture = false
    if (mods.has('passive')) evtListOpts.passive = true
    if (mods.has('once')) evtListOpts.once = true

    if (key === 'load') {
      // Delay the callback to the next microtask so that indicators can be set
      setTimeout(callback, 0)
      return () => {}
    }

    if (key === 'interval') {
      let duration = 1000
      const durationArgs = mods.get('duration')
      if (durationArgs) {
        duration = tagToMs(durationArgs)
        const leading = tagHas(durationArgs, 'leading', false)
        if (leading) {
          callback()
        }
      }
      const intervalId = setInterval(callback, duration)

      return () => {
        clearInterval(intervalId)
      }
    }

    if (key === 'raf') {
      let rafId: number | undefined
      const raf = () => {
        callback()
        rafId = requestAnimationFrame(raf)
      }
      rafId = requestAnimationFrame(raf)

      return () => {
        if (rafId) {
          cancelAnimationFrame(rafId)
        }
      }
    }

    if (key.startsWith(SIGNALS_CHANGE_PREFIX)) {
      const hasPrefix = key !== SIGNALS_CHANGE_PREFIX
      const signalPath = modifyCasing(
        camel(key.slice(signalChangeKeyLength)),
        mods,
      )
      const signalFn = (event: CustomEvent<DatastarSignalEvent>) => {
        if (hasPrefix) {
          const { added, removed, updated } = event.detail
          if (
            ![...added, ...removed, ...updated].some((d) =>
              d.startsWith(signalPath),
            )
          ) {
            return
          }
        }
        callback(event)
      }
      document.addEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
      return () => {
        document.removeEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
      }
    }

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

    target.addEventListener(eventName, callback, evtListOpts)
    return () => {
      target.removeEventListener(eventName, callback)
    }
  },
}
