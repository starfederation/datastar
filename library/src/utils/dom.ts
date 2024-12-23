import { DATASTAR } from '~/engine/consts'

export function elUniqId(el: Element) {
  if (el.id) return el.id
  let hash = 0
  const hashUpdate = (n: number) => {
    hash = (hash << 5) - hash + n
    return hash & hash
  }
  const hashUpdateFromStr = (str: string) => {
    for (const c of str.split('')) {
      hashUpdate(c.charCodeAt(0))
    }
  }

  let currentEl = el
  while (currentEl.parentNode) {
    if (currentEl.id) {
      hashUpdateFromStr(`${currentEl.id}`)
      break
    }
    if (currentEl === currentEl.ownerDocument.documentElement) {
      hashUpdateFromStr(currentEl.tagName)
    } else {
      for (
        let i = 1, e = el;
        e.previousElementSibling;
        e = e.previousElementSibling, i++
      ) {
        hashUpdate(i)
      }
      currentEl = currentEl.parentNode as Element
    }

    currentEl = currentEl.parentNode as Element
  }
  return DATASTAR + hash
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
