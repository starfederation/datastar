// Icon: akar-icons:link-chain
// Slug: Creates a signal with two-way data binding.
// Description: Creates a signal (if one doesn’t already exist) and sets up two-way data binding between it and an element’s value.

import { aliasify } from '../../engine/engine'
import type { AttributePlugin, Paths } from '../../engine/types'
import { modifyCasing } from '../../utils/text'

const dataURIRegex = /^data:(?<mime>[^;]+);base64,(?<contents>.*)$/
const empty = Symbol('empty')

export const Bind: AttributePlugin = {
  type: 'attribute',
  name: 'bind',
  keyReq: 'exclusive',
  valReq: 'exclusive',
  shouldEvaluate: false,
  onLoad: ({
    el,
    key,
    mods,
    value,
    effect,
    mergePaths,
    runtimeErr,
    getPath,
  }) => {
    const signalName = key ? modifyCasing(key, mods) : value

    let get = (el: any, type: string) =>
      type === 'number' ? +el.value : el.value

    let set = (value: any) => {
      ;(el as HTMLInputElement).value = `${value}`
    }

    if (el instanceof HTMLInputElement) {
      switch (el.type) {
        case 'range':
        case 'number':
          get = (el: any, type: string) =>
            type === 'string' ? el.value : +el.value
          break

        case 'checkbox':
          get = (el: HTMLInputElement, type: string) => {
            if (el.value !== 'on') {
              if (type === 'boolean') {
                return el.checked
              } else {
                return el.checked ? el.value : ''
              }
            } else {
              if (type === 'string') {
                return el.checked ? el.value : ''
              } else {
                return el.checked
              }
            }
          }
          set = (value: string | boolean) => {
            el.checked = typeof value === 'string' ? value === el.value : value
          }
          break

        case 'radio':
          // Set up radio button name attribute if not present
          if (!el.getAttribute('name')?.length) {
            el.setAttribute('name', signalName)
          }

          get = (el: HTMLInputElement, type: string) =>
            el.checked ? (type === 'number' ? +el.value : el.value) : empty
          set = (value: string | number) => {
            el.checked =
              value === (typeof value === 'number' ? +el.value : el.value)
          }
          break
        case 'file': {
          const syncSignal = () => {
            const files = [...(el.files || [])]
            const contents: string[] = []
            const mimes: string[] = []
            const names: string[] = []
            Promise.all(
              files.map(
                (f) =>
                  new Promise<void>((resolve) => {
                    const reader = new FileReader()
                    reader.onload = () => {
                      if (typeof reader.result !== 'string') {
                        throw runtimeErr('InvalidFileResultType', {
                          resultType: typeof reader.result,
                        })
                      }
                      const match = reader.result.match(dataURIRegex)
                      if (!match?.groups) {
                        throw runtimeErr('InvalidDataUri', {
                          result: reader.result,
                        })
                      }
                      contents.push(match.groups.contents)
                      mimes.push(match.groups.mime)
                      names.push(f.name)
                    }
                    reader.onloadend = () => resolve()
                    reader.readAsDataURL(f)
                  }),
              ),
            ).then(() => {
              mergePaths([
                [signalName, contents],
                [`${signalName}Mimes`, mimes],
                [`${signalName}Names`, names],
              ])
            })
          }

          el.addEventListener('change', syncSignal)
          el.addEventListener('input', syncSignal)

          return () => {
            el.removeEventListener('change', syncSignal)
            el.removeEventListener('input', syncSignal)
          }
        }
      }
    } else if (el instanceof HTMLSelectElement) {
      if (el.multiple) {
        const typeMap = new Map<string, string>()
        get = (el: HTMLSelectElement) =>
          [...el.selectedOptions].map((option) => {
            const type = typeMap.get(option.value)
            return type === 'string' || type == null
              ? option.value
              : +option.value
          })

        set = (value: (string | number)[]) => {
          for (const option of el.options) {
            if (value.includes(option.value)) {
              typeMap.set(option.value, 'string')
              option.selected = true
            } else if (value.includes(+option.value)) {
              typeMap.set(option.value, 'number')
              option.selected = true
            } else {
              option.selected = false
            }
          }
        }
      }
    } else if (el instanceof HTMLTextAreaElement) {
      // default case
    } else {
      // web component
      get = (el: Element) =>
        'value' in el ? el.value : el.getAttribute('value')
      set = (value: any) => {
        if ('value' in el) {
          el.value = value
        } else {
          el.setAttribute('value', value)
        }
      }
    }

    const initialValue = getPath(signalName)
    const type = typeof initialValue

    let path = signalName
    if (
      Array.isArray(initialValue) &&
      !(el instanceof HTMLSelectElement && el.multiple)
    ) {
      const inputs = document.querySelectorAll(
        `[${aliasify('bind')}-${key}],[${aliasify('bind')}="${value}"]`,
      ) as NodeListOf<HTMLInputElement>

      const paths: Paths = []
      let i = 0
      for (const input of inputs) {
        paths.push([`${path}.${i}`, get(input, 'none')])

        if (el === input) {
          break
        }
        i++
      }
      mergePaths(paths, { ifMissing: true })
      path = `${path}.${i}`
    } else {
      mergePaths([[path, get(el, type)]], {
        ifMissing: true,
      })
    }

    const syncSignal = () => {
      const signalValue = getPath(path)
      if (signalValue != null) {
        const value = get(el, typeof signalValue)
        if (value !== empty) {
          mergePaths([[path, value]])
        }
      }
    }

    el.addEventListener('input', syncSignal)
    el.addEventListener('change', syncSignal)
    const cleanup = effect(() => {
      set(getPath(path))
    })

    return () => {
      cleanup()
      el.removeEventListener('input', syncSignal)
      el.removeEventListener('change', syncSignal)
    }
  },
}
