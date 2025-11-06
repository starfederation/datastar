import type { EventCallbackHandler, Modifiers } from '@engine/types'
import { tagHas, tagToMs } from '@utils/tags'

export const delay = (
  callback: EventCallbackHandler,
  wait: number,
): EventCallbackHandler => {
  return (...args: any[]) => {
    setTimeout(() => {
      callback(...args)
    }, wait)
  }
}

export const debounce = (
  callback: EventCallbackHandler,
  wait: number,
  leading = false,
  trailing = true,
): EventCallbackHandler => {
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
      timer = 0
    }, wait)
  }
}

export const throttle = (
  callback: EventCallbackHandler,
  wait: number,
  leading = true,
  trailing = false,
): EventCallbackHandler => {
  let waiting = false
  let lastArgs: Parameters<EventCallbackHandler> | null = null

  return (...args: any[]) => {
    if (waiting) {
      lastArgs = args
      return
    }

    if (leading) {
      callback(...args)
    } else {
      lastArgs = args
    }

    waiting = true
    setTimeout(() => {
      if (trailing && lastArgs !== null) {
        callback(...lastArgs)
        lastArgs = null
      }
      waiting = false
    }, wait)
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
    const leading = tagHas(debounceArgs, 'leading', false)
    const trailing = !tagHas(debounceArgs, 'notrailing', false)
    callback = debounce(callback, wait, leading, trailing)
  }

  const throttleArgs = mods.get('throttle')
  if (throttleArgs) {
    const wait = tagToMs(throttleArgs)
    const leading = !tagHas(throttleArgs, 'noleading', false)
    const trailing = tagHas(throttleArgs, 'trailing', false)
    callback = throttle(callback, wait, leading, trailing)
  }

  return callback
}
