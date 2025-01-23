export type TimerHandler = (...args: any[]) => void

export function delay(
  callback: TimerHandler,
  wait: number,
): TimerHandler {
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
