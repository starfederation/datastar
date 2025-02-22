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
import { camel, modifyCasing } from '../../../../utils/text'
import { debounce, throttle } from '../../../../utils/timing'
import { supportsViewTransitions } from '../../../../utils/view-transtions'
import type { Signal } from '../../../../vendored/preact-core'

const EVT = 'evt'
const SIGNALS_CHANGE_PREFIX = 'signalsChange'
const signalChangeKeyLength = SIGNALS_CHANGE_PREFIX.length

export const On: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'on',
  keyReq: Requirement.Must,
  valReq: Requirement.Must,
  argNames: [EVT],
  removeOnLoad: (rawKey: string) => rawKey.startsWith('onLoad'),
  onLoad: ({ el, key, mods, rawKey, signals, value, effect, genRX }) => {
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

    const delayArgs = mods.get('delay')
    if (delayArgs) {
      const delay = tagToMs(delayArgs)
      setTimeout(() => {
        callback()
      }, delay)
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
      setTimeout(() => callback(), 0)
      return () => {}
    }

    if (key === 'interval') {
      let duration = 1000
      const durationArgs = mods.get('duration')
      if (durationArgs) {
        duration = tagToMs(durationArgs)
        const leading = tagHas(durationArgs, 'leading', false)
        if (leading) {
          // Remove `.leading` from the dataset so the callback is only ever called on page load
          el.dataset[rawKey.replace('.leading', '')] = value
          delete el.dataset[rawKey]
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
        if (rafId) cancelAnimationFrame(rafId)
      }
    }

    if (key.startsWith(SIGNALS_CHANGE_PREFIX)) {
      if (key === SIGNALS_CHANGE_PREFIX) {
        const skipinit = mods.has('skipinit')
        if (!skipinit) {
          callback()
        }
        const signalFn = (event: CustomEvent<DatastarSignalEvent>) =>
          callback(event)
        document.addEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
        return () => {
          document.removeEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
        }
      }

      const signalPath = modifyCasing(
        camel(key.slice(signalChangeKeyLength)),
        mods,
      )
      const signalValues = new Map<Signal, any>()
      signals.walk((path, signal) => {
        if (path.startsWith(signalPath)) {
          signalValues.set(signal, signal.value)
        }
      })
      return effect(() => {
        for (const [signal, prev] of signalValues) {
          if (prev !== signal.value) {
            callback()
            signalValues.set(signal, signal.value)
          }
        }
      })
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

    const eventName = modifyCasing(key, mods)
    target.addEventListener(eventName, callback, evtListOpts)
    return () => {
      target.removeEventListener(eventName, callback)
    }
  },
}
