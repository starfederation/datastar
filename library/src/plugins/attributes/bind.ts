// Icon: akar-icons:link-chain
// Slug: Creates a signal with two-way data binding.
// Description: Creates a signal (if one doesn’t already exist) and sets up two-way data binding between it and an element’s value.

import { attribute } from '@engine'
import { effect, getPath, mergePaths } from '@engine/signals'
import type { Paths } from '@engine/types'
import { aliasify, modifyCasing } from '@utils/text'

type SignalFile = {
  name: string
  contents: string
  mime: string
}

const dataURIRegex = /^data:(?<mime>[^;]+);base64,(?<contents>.*)$/
const empty = Symbol('empty')

const aliasedBind = aliasify('bind')

attribute({
  name: 'bind',
  requirement: 'exclusive',
  apply({ el, key, mods, value, error }) {
    const signalName = key != null ? modifyCasing(key, mods) : value

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
      const signalNameKebab = key ? key : value!
      const inputs = document.querySelectorAll(
        `[${aliasedBind}\\:${CSS.escape(signalNameKebab)}],[${aliasedBind}="${CSS.escape(signalNameKebab)}"]`,
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
})
