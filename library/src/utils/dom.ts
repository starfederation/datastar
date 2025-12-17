import type { HTMLOrSVG } from '@engine/types'

export const isHTMLOrSVG = (el: Node): el is HTMLOrSVG =>
  el instanceof HTMLElement ||
  el instanceof SVGElement ||
  el instanceof MathMLElement
