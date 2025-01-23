// Authors: Delaney Gillilan
// Icon: material-symbols:mail
// Slug: Add an event listener to an element
// Description: This action adds an event listener to an element. The event listener can be triggered by a variety of events, such as clicks, keypresses, and more. The event listener can also be set to trigger only once, or to be passive or capture. The event listener can also be debounced or throttled. The event listener can also be set to trigger only when the event target is outside the element.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { onElementRemoved } from '../../../../utils/dom'
import { tagHas, tagToMs } from '../../../../utils/tags'
import { kebabize } from '../../../../utils/text'
import { debounce, delay, throttle } from '../../../../utils/timing'

const lastSignalsMarshalled = new Map<string, any>()

const EVT = 'evt'
export const On: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'on',
  keyReq: Requirement.Must,
  valReq: Requirement.Must,
  argNames: [EVT],
  onLoad: ({ el, key, rawKey, genRX, mods, signals, effect }) => {
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

    const evtListOpts: AddEventListenerOptions = {
      capture: true,
      passive: false,
      once: false,
    }
    if (!mods.has('capture')) evtListOpts.capture = false
    if (mods.has('passive')) evtListOpts.passive = true
    if (mods.has('once')) evtListOpts.once = true

    const eventName = kebabize(key).toLowerCase()
    switch (eventName) {
      case 'load': {
        callback()
        delete el.dataset[rawKey]
        return () => {}
      }

      case 'interval': {
        let duration = 1000
        const durationArgs = mods.get('duration')
        if (durationArgs) {
          duration = tagToMs(durationArgs)
        }
        const intervalId = setInterval(callback, duration)

        return () => {
          clearInterval(intervalId)
        }
      }

      case 'raf': {
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

      case 'signals-change': {
        onElementRemoved(el, () => {
          lastSignalsMarshalled.delete(el.id)
        })
        return effect(() => {
          const onlyRemoteSignals = mods.has('remote')
          const current = signals.JSON(false, onlyRemoteSignals)
          const last = lastSignalsMarshalled.get(el.id) || ''
          if (last !== current) {
            lastSignalsMarshalled.set(el.id, current)
            callback()
          }
        })
      }

      default: {
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

        target.addEventListener(eventName, callback, evtListOpts)
        return () => {
          target.removeEventListener(eventName, callback)
        }
      }
    }
  },
}
