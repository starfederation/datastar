// Authors: Delaney Gillilan
// Icon: hugeicons:mouse-scroll-01
// Slug: Scroll an element into view
// Description: This attribute scrolls the element into view.

import { dsErr } from '~/engine/errors'
import { type AttributePlugin, PluginType, Requirement } from '~/engine/types'

const SMOOTH = 'smooth'
const INSTANT = 'instant'
const AUTO = 'auto'
const HSTART = 'hstart'
const HCENTER = 'hcenter'
const HEND = 'hend'
const HNEAREST = 'hnearest'
const VSTART = 'vstart'
const VCENTER = 'vcenter'
const VEND = 'vend'
const VNEAREST = 'vnearest'
const FOCUS = 'focus'

const CENTER = 'center'
const START = 'start'
const END = 'end'
const NEAREST = 'nearest'

// Scrolls the element into view
export const ScrollIntoView: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'scrollIntoView',
  keyReq: Requirement.Denied,
  valReq: Requirement.Denied,
  tags: new Set([
    SMOOTH,
    INSTANT,
    AUTO,
    HSTART,
    HCENTER,
    HEND,
    HNEAREST,
    VSTART,
    VCENTER,
    VEND,
    VNEAREST,
    FOCUS,
  ]),

  onLoad: ({ el, tags, rawKey }) => {
    if (!el.tabIndex) el.setAttribute('tabindex', '0')
    const opts: ScrollIntoViewOptions = {
      behavior: SMOOTH,
      block: CENTER,
      inline: CENTER,
    }
    if (tags.has(SMOOTH)) opts.behavior = SMOOTH
    if (tags.has(INSTANT)) opts.behavior = INSTANT
    if (tags.has(AUTO)) opts.behavior = AUTO
    if (tags.has(HSTART)) opts.inline = START
    if (tags.has(HCENTER)) opts.inline = CENTER
    if (tags.has(HEND)) opts.inline = END
    if (tags.has(HNEAREST)) opts.inline = NEAREST
    if (tags.has(VSTART)) opts.block = START
    if (tags.has(VCENTER)) opts.block = CENTER
    if (tags.has(VEND)) opts.block = END
    if (tags.has(VNEAREST)) opts.block = NEAREST

    if (!(el instanceof HTMLElement || el instanceof SVGElement)) {
      throw dsErr('NotHtmlSvgElement, el')
    }
    if (!el.tabIndex) {
      el.setAttribute('tabindex', '0')
    }

    el.scrollIntoView(opts)
    if (tags.has('focus')) {
      el.focus()
    }

    delete el.dataset[rawKey]
    return () => {}
  },
}
