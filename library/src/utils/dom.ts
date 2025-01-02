import { DATASTAR } from '../engine/consts'

export class Hash {
  #value = 0
  #prefix: string

  constructor(prefix = DATASTAR) {
    this.#prefix = prefix
  }

  with(x: number | string): Hash {
    if (typeof x === 'string') {
      for (const c of x.split('')) {
        this.with(c.charCodeAt(0))
      }
    } else {
      this.#value = (this.#value << 5) - this.#value + x
    }
    return this
  }

  reset(): Hash {
    this.#value = 0
    return this
  }

  get value() {
    return this.#prefix + Math.abs(this.#value).toString(36)
  }
}

export function elUniqId(el: Element) {
  if (el.id) return el.id
  const hash = new Hash()

  let currentEl = el
  while (currentEl.parentNode) {
    if (currentEl.id) {
      hash.with(currentEl.id)
      break
    }
    if (currentEl === currentEl.ownerDocument.documentElement) {
      hash.with(currentEl.tagName)
    } else {
      for (
        let i = 1, e = el;
        e.previousElementSibling;
        e = e.previousElementSibling, i++
      ) {
        hash.with(i)
      }
      currentEl = currentEl.parentNode as Element
    }

    currentEl = currentEl.parentNode as Element
  }
  return hash.value
}

export function onElementRemoved(element: Element, callback: () => void) {
  const observer = new MutationObserver((mutations) => {
    for (const mutation of mutations) {
      for (const removedNode of mutation.removedNodes) {
        if (removedNode === element) {
          observer.disconnect()
          callback()
          return
        }
      }
    }
  })
  observer.observe(element.parentNode as Node, { childList: true })
}
