import type { HTMLOrSVG } from '../engine/types'

export function isHTMLOrSVG(el: Node): el is HTMLOrSVG {
  return el instanceof HTMLElement || el instanceof SVGElement
}

export function findClosestScoped(el: HTMLOrSVG | null): string | null {
  return (
    (el?.closest('[data-scope]') as HTMLOrSVG | null)?.dataset.scope ?? null
  )
}
