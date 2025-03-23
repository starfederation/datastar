import type { Modifiers } from '../engine/types'
import { tagHas, tagToMs } from './tags'

export type TimerHandler = (...args: any[]) => void

export function delay(callback: TimerHandler, wait: number): TimerHandler {
  return (...args: any[]) => {
    setTimeout(() => {
      callback(...args)
    }, wait)
  }
}

export function debounce(
  callback: TimerHandler,
  wait: number,
  leading = false,
  trailing = true,
): TimerHandler {
  let timer = -1

  const resetTimer = () => timer && clearTimeout(timer)

  return (...args: any[]) => {
    resetTimer()

    if (leading && !timer) {
      callback(...args)
    }

    timer = setTimeout(() => {
      if (trailing) {
        callback(...args)
      }
      resetTimer()
    }, wait)
  }
}

export function throttle(
  callback: TimerHandler,
  wait: number,
  leading = true,
  trailing = false,
): TimerHandler {
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
  callback: TimerHandler,
  mods: Modifiers,
): TimerHandler {
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
