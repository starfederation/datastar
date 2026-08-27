import type { EventCallbackHandler, Modifiers } from '@engine/types'
import { tagHas, tagToMs } from '@utils/tags'

export const delay = (
  callback: EventCallbackHandler,
  wait: number,
): EventCallbackHandler => {
  return (...args: any[]) => {
    setTimeout(callback, wait, ...args)
  }
}

export const throttle = (
  callback: EventCallbackHandler,
  wait: number,
  leading = true,
  trailing = false,
  debounce = false,
): EventCallbackHandler => {
  let lastArgs: Parameters<EventCallbackHandler> | null = null
  let timer: ReturnType<typeof setTimeout> | number = 0

  return (...args: any[]) => {
    if (leading && !timer) {
      callback(...args)
      lastArgs = null
    } else {
      lastArgs = args
    }
    if (!timer || debounce) {
      if (timer) {
        clearTimeout(timer)
      }
      timer = setTimeout(() => {
        if (trailing && lastArgs !== null) {
          callback(...lastArgs)
        }
        lastArgs = null
        timer = 0
      }, wait)
    }
  }
}

export const modifyTiming = (
  callback: EventCallbackHandler,
  mods: Modifiers,
): EventCallbackHandler => {
  const delayArgs = mods.get('delay')
  if (delayArgs) {
    const wait = tagToMs(delayArgs)
    callback = delay(callback, wait)
  }

  const debounceArgs = mods.get('debounce')
  if (debounceArgs) {
    const wait = tagToMs(debounceArgs)
    const leading = tagHas(debounceArgs, 'leading')
    const trailing = !tagHas(debounceArgs, 'notrailing')
    callback = throttle(callback, wait, leading, trailing, true)
  }

  const throttleArgs = mods.get('throttle')
  if (throttleArgs) {
    const wait = tagToMs(throttleArgs)
    const leading = !tagHas(throttleArgs, 'noleading')
    const trailing = tagHas(throttleArgs, 'trailing')
    callback = throttle(callback, wait, leading, trailing)
  }

  return callback
}
