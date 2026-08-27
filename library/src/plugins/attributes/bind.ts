// Icon: akar-icons:link-chain
// Slug: Creates a signal with two-way data binding.
// Description: Creates a signal (if one doesn’t already exist) and sets up two-way data binding between it and an element’s value.

import { attribute } from '@engine'
import {
  DATASTAR_PROP_CHANGE_EVENT,
  DOCUMENT,
  HTMLInput,
  MutationObserverClass,
} from '@engine/consts'
import { effect, getPath, mergePaths } from '@engine/signals'
import type { Paths } from '@engine/types'
import { hasOwn } from '@utils/polyfills'
import { aliasify, camel, modifyCasing } from '@utils/text'

type SignalFile = {
  name: string
  contents: string
  mime: string
}

type BindAdapter = {
  get_: (el: any, type: string) => any
  set_: (el: any, value: any) => void
  events_: string[]
}

const propAdapter = (prop: string, ...events: string[]): BindAdapter => ({
  get_: (el: any) => el[prop],
  set_: (el: any, value: any) => {
    el[prop] = value
  },
  events_: events,
})

const attrAdapter = (attr: string, ...events: string[]): BindAdapter => ({
  get_: (el: Element) => el.getAttribute(attr),
  set_: (el: Element, value: any) => {
    el.setAttribute(attr, `${value}`)
  },
  events_: events,
})

const valueAdapter = (
  treatUndefinedAsString = false,
  ...events: string[]
): BindAdapter => ({
  get_: (el: HTMLInputElement | HTMLSelectElement, type: string) =>
    type === 'string' || (treatUndefinedAsString && type === 'undefined')
      ? el.value
      : +el.value,
  set_: (el: HTMLInputElement | HTMLSelectElement, value: string | number) => {
    el.value = `${value}`
  },
  events_: events,
})

const selectAdapter = (): BindAdapter => {
  const numbers = new Set<string>()
  return {
    get_: (el: HTMLSelectElement, type: string) =>
      el.multiple
        ? [...el.selectedOptions].map((option) =>
            numbers.has(option.value) ? +option.value : option.value,
          )
        : type === 'string' || type === 'undefined'
          ? el.value
          : +el.value,
    set_: (el: HTMLSelectElement, value: any) => {
      if (!el.multiple) {
        el.value = `${value}`
        return
      }
      for (const option of el.options) {
        if (value.includes(option.value)) {
          numbers.delete(option.value)
          option.selected = true
        } else if (value.includes(+option.value)) {
          numbers.add(option.value)
          option.selected = true
        } else {
          option.selected = false
        }
      }
    },
    events_: ['change'],
  }
}

const dataURIRegex = /^data:(?<mime>[^;]+);base64,(?<contents>.*)$/
const empty = Symbol('empty')

const boundPath = (
  el: Element,
  key: string | null | undefined,
  rawKey: string,
  signalName: string,
  adapter: BindAdapter,
  initialValue: any,
) => {
  const rawAttribute = aliasify(CSS.escape(rawKey))
  const selector = key
    ? `[${rawAttribute}]`
    : `[${rawAttribute}="${CSS.escape(signalName)}"]`
  if (
    initialValue === undefined &&
    el instanceof HTMLInput &&
    el.type === 'radio'
  ) {
    const checked = [...DOCUMENT.querySelectorAll(selector)].find(
      (input): input is HTMLInputElement =>
        input instanceof HTMLInput && input.checked,
    )
    // Missing radio binds adopt the checked option.
    if (checked) {
      mergePaths([[signalName, checked.value]], { ifMissing: true })
    }
  }

  if (
    !Array.isArray(initialValue) ||
    (el instanceof HTMLSelectElement && el.multiple)
  ) {
    mergePaths([[signalName, adapter.get_(el, typeof initialValue)]], {
      ifMissing: true,
    })
    return signalName
  }

  const inputs = DOCUMENT.querySelectorAll(selector) as NodeListOf<Element>

  const paths: Paths = []
  let i = 0
  for (const input of inputs) {
    // Missing proxy slots materialize as '', which breaks `ifMissing`.
    paths.push([
      `${signalName}.${i}`,
      adapter.get_(
        input,
        typeof (hasOwn(initialValue, i) ? initialValue[i] : undefined),
      ),
    ])
    if (el === input) {
      break
    }
    i++
  }
  mergePaths(paths, { ifMissing: true })
  return `${signalName}.${i}`
}

attribute({
  name: 'bind',
  requirement: 'exclusive',
  apply({ el, key, rawKey, mods, value, error }) {
    const signalName = key != null ? modifyCasing(key, mods) : value

    const props = mods.get('prop')
    const events = mods.get('event')
    let adapter: BindAdapter | null = null

    if (el instanceof HTMLInput) {
      switch (el.type) {
        case 'range':
        case 'number':
          adapter = valueAdapter(false, 'input')
          break
        case 'checkbox':
          adapter = {
            get_: (el: HTMLInputElement, type: string) => {
              if (el.value !== 'on') {
                return type === 'boolean'
                  ? el.checked
                  : el.checked
                    ? el.value
                    : ''
              }
              return type === 'string'
                ? el.checked
                  ? el.value
                  : ''
                : el.checked
            },
            set_: (el: HTMLInputElement, value: string | boolean) => {
              el.checked =
                typeof value === 'string' ? value === el.value : value
            },
            events_: ['input'],
          }
          break
        case 'radio':
          if (!el.getAttribute('name')?.length) {
            el.setAttribute('name', signalName)
          }
          adapter = {
            get_: (el: HTMLInputElement, type: string) =>
              el.checked ? (type === 'number' ? +el.value : el.value) : empty,
            set_: (el: HTMLInputElement, value: string | number) => {
              el.checked =
                value === (typeof value === 'number' ? +el.value : el.value)
            },
            events_: ['input'],
          }
          break
        case 'file': {
          const syncSignal = () => {
            const files = [...(el.files || [])]
            const signalFiles: SignalFile[] = []
            Promise.all(
              files.map(
                (f) =>
                  new Promise<void>((resolve) => {
                    const reader = new FileReader()
                    reader.onload = () => {
                      if (typeof reader.result !== 'string') {
                        throw error('InvalidFileResultType', {
                          resultType: typeof reader.result,
                        })
                      }
                      const match = reader.result.match(dataURIRegex)
                      if (!match?.groups) {
                        throw error('InvalidDataUri', {
                          result: reader.result,
                        })
                      }
                      signalFiles.push({
                        name: f.name,
                        contents: match.groups.contents,
                        mime: match.groups.mime,
                      })
                    }
                    reader.onloadend = () => resolve()
                    reader.readAsDataURL(f)
                  }),
              ),
            ).then(() => {
              mergePaths([[signalName, signalFiles]])
            })
          }

          el.addEventListener('change', syncSignal)
          return () => {
            el.removeEventListener('change', syncSignal)
          }
        }
        default:
          adapter = valueAdapter(true, 'input')
      }
    } else if (el instanceof HTMLSelectElement) {
      adapter = selectAdapter()
    } else if (el instanceof HTMLTextAreaElement) {
      adapter = propAdapter('value', 'input')
    } else if (el instanceof HTMLElement && el.tagName.includes('-')) {
      adapter =
        'value' in el
          ? propAdapter('value', 'input', 'change')
          : attrAdapter('value', 'input', 'change')
    } else if (el instanceof HTMLElement && 'value' in el) {
      adapter = propAdapter('value', 'change')
    } else {
      adapter = attrAdapter('value', 'change')
    }
    if (!adapter) {
      throw error('InvalidBindAdapter')
    }

    const firstProp = props && [...props][0]
    if (props && !firstProp) throw error('BindPropNameMissing')
    if (firstProp) {
      const prop = camel(firstProp)
      adapter = propAdapter(prop, ...(events ? [...events] : adapter.events_))
    } else if (events) {
      adapter.events_ = [...events]
    }

    let path = boundPath(
      el,
      key,
      rawKey,
      signalName,
      adapter,
      getPath(signalName),
    )
    const syncSignal = () => {
      const signalValue = getPath(path)
      if (signalValue != null) {
        const value = adapter.get_(el, typeof signalValue)
        if (value !== empty) {
          mergePaths([[path, value]])
        }
      }
    }
    const syncElement = () => {
      adapter.set_(el, getPath(path))
    }

    for (const eventName of adapter.events_) {
      el.addEventListener(eventName, syncSignal)
    }
    el.addEventListener(DATASTAR_PROP_CHANGE_EVENT, syncSignal)
    let cleanup = effect(syncElement)
    const multipleObserver =
      el instanceof HTMLSelectElement
        ? new MutationObserverClass(() => {
            cleanup()
            path = boundPath(
              el,
              key,
              rawKey,
              signalName,
              adapter,
              getPath(signalName),
            )
            cleanup = effect(syncElement)
          })
        : null
    multipleObserver?.observe(el, { attributeFilter: ['multiple'] })

    return () => {
      multipleObserver?.disconnect()
      cleanup()
      for (const eventName of adapter.events_) {
        el.removeEventListener(eventName, syncSignal)
      }
      el.removeEventListener(DATASTAR_PROP_CHANGE_EVENT, syncSignal)
    }
  },
})
