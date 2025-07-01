import type { EventCallbackHandler, Modifiers } from '../engine/types'
import { tagHas, tagToMs } from './tags'

export function delay(
  callback: EventCallbackHandler,
  wait: number,
): EventCallbackHandler {
  return (...args: any[]) => {
    setTimeout(() => {
      callback(...args)
    }, wait)
  }
}

export function debounce(
  callback: EventCallbackHandler,
  wait: number,
  leading = false,
  trailing = true,
): EventCallbackHandler {
  let timer = 0
  return (...args: any[]) => {
    timer && clearTimeout(timer)

    if (leading && !timer) {
      callback(...args)
    }

    timer = setTimeout(() => {
      if (trailing) {
        callback(...args)
      }
      timer && clearTimeout(timer)
    }, wait)
  }
}

export function throttle(
  callback: EventCallbackHandler,
  wait: number,
  leading = true,
  trailing = false,
): EventCallbackHandler {
  let waiting = false

  return (...args: any[]) => {
    if (waiting) return

    if (leading) {
      callback(...args)
    }

    waiting = true
    setTimeout(() => {
      waiting = false
      if (trailing) {
        callback(...args)
      }
    }, wait)
  }
}

export function modifyTiming(
  callback: EventCallbackHandler,
  mods: Modifiers,
): EventCallbackHandler {
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

  return callback
}
