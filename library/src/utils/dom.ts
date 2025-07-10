import type { HTMLOrSVG } from '../engine/types'

export function isHTMLOrSVG(el: Node): el is HTMLOrSVG {
  return el instanceof HTMLElement || el instanceof SVGElement
}
