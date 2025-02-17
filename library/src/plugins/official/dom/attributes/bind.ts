// Authors: Delaney Gillilan
// Icon: akar-icons:link-chain
// Slug: Bind attributes to expressions
// Description: Any attribute can be bound to an expression. The attribute will be updated reactively whenever the expression signal changes.

import { runtimeErr } from '../../../../engine/errors'
import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyCasing, trimDollarSignPrefix } from '../../../../utils/text'

const dataURIRegex = /^data:(?<mime>[^;]+);base64,(?<contents>.*)$/
const updateEvents = ['change', 'input', 'keydown']

export const Bind: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'bind',
  keyReq: Requirement.Exclusive,
  valReq: Requirement.Exclusive,
  onLoad: (ctx) => {
    const { el, key, mods, signals, value, effect } = ctx
    const signalName = key ? modifyCasing(key, mods) : trimDollarSignPrefix(value)

    let setFromSignal = () => {}
    let el2sig = () => {}

    const tnl = el.tagName.toLowerCase()
    let signalDefault: string | boolean | number | File = ''
    const isInput = tnl.includes('input')
    const type = el.getAttribute('type')
    const isCheckbox =
      tnl.includes('checkbox') || (isInput && type === 'checkbox')
    if (isCheckbox) {
      signalDefault = false
    }
    const isNumber = isInput && type === 'number'
    if (isNumber) {
      signalDefault = 0
    }
    const isSelect = tnl.includes('select')
    const isRadio = tnl.includes('radio') || (isInput && type === 'radio')
    const isFile = isInput && type === 'file'
    if (isFile) {
      // can't set a default value for a file input, yet
    }
    if (isRadio) {
      const name = el.getAttribute('name')
      if (!name?.length) {
        el.setAttribute('name', signalName)
      }
    }

    signals.upsertIfMissing(signalName, signalDefault)

    setFromSignal = () => {
      const hasValue = 'value' in el
      const v = signals.value(signalName)
      const vStr = `${v}`
      if (isCheckbox || isRadio) {
        const input = el as HTMLInputElement
        if (Array.isArray(v)) {
          input.checked = v.includes(input.value)
        } else if (typeof v === 'string') {
          input.checked = vStr === input.value
        } else {
          input.checked = !!v || v === 'true'
        }
      } else if (isSelect) {
        const select = el as HTMLSelectElement
        if (select.multiple) {
          for (const opt of select.options) {
            if (opt?.disabled) return
            if (Array.isArray(v) || typeof v === 'string') {
              opt.selected = v.includes(opt.value)
            } else if (typeof v === 'number') {
              opt.selected = v === Number(opt.value)
            } else {
              opt.selected = v as boolean
            }
          }
        } else {
          select.value = vStr
        }
      } else if (isFile) {
        // File input reading from a signal is not supported
      } else if (hasValue) {
        el.value = vStr
      } else {
        el.setAttribute('value', vStr)
      }
    }

    el2sig = async () => {
      if (isFile) {
        const files = [...((el as HTMLInputElement)?.files || [])]
        const allContents: string[] = []
        const allMimes: string[] = []
        const allNames: string[] = []

        await Promise.all(
          files.map((f) => {
            return new Promise<void>((resolve) => {
              const reader = new FileReader()
              reader.onload = () => {
                if (typeof reader.result !== 'string') {
                  throw runtimeErr('InvalidFileResultType', ctx, {
                    resultType: typeof reader.result,
                  })
                }
                const match = reader.result.match(dataURIRegex)
                if (!match?.groups) {
                  throw runtimeErr('InvalidDataUri', ctx, {
                    result: reader.result,
                  })
                }
                allContents.push(match.groups.contents)
                allMimes.push(match.groups.mime)
                allNames.push(f.name)
              }
              reader.onloadend = () => resolve(void 0)
              reader.readAsDataURL(f)
            })
          }),
        )

        signals.setValue(signalName, allContents)
        signals.setValue(`${signalName}Mimes`, allMimes)
        signals.setValue(`${signalName}Names`, allNames)

        return
      }

      const current = signals.value(signalName)
      const input = (el as HTMLInputElement) || (el as HTMLElement)

      if (isCheckbox) {
        const checked = input.checked || input.getAttribute('checked') === 'true'
        if (Array.isArray(current)) {
          const values = new Set(current)
          if (checked) {
            values.add(input.value)
          } else {
            values.delete(input.value)
          }
          signals.setValue(signalName, [...values])
        } else if (typeof current === 'string') {
          const value = checked ? input.value : ''
          signals.setValue(signalName, value)
        } else {
          signals.setValue(signalName, checked)
        }

        return
      }

      const value = input.value || input.getAttribute('value') || ''
      if (typeof current === 'number') {
        signals.setValue(signalName, Number(value))
      } else if (typeof current === 'string') {
        signals.setValue(signalName, value || '')
      } else if (typeof current === 'boolean') {
        signals.setValue(signalName, Boolean(value))
      } else if (Array.isArray(current)) {
        if (isSelect) {
          const select = el as HTMLSelectElement
          const selectedOptions = [...select.selectedOptions]
          const selectedValues = selectedOptions
            .filter((opt) => opt.selected)
            .map((opt) => opt.value)
          signals.setValue(signalName, selectedValues)
        } else {
          // assume it's a comma-separated string
          signals.setValue(signalName, JSON.stringify(value.split(',')))
        }
      } else if (typeof current === 'undefined') {
      } else {
        throw runtimeErr('BindUnsupportedSignalType', ctx, {
          signalType: typeof current,
        })
      }
    }

    for (const event of updateEvents) {
      el.addEventListener(event, el2sig)
    }
    const elSigClean = effect(() => setFromSignal())
    /*
     * The signal value needs to be updated after the "pageshow" event.
     * Sometimes, the browser might populate inputs with previous values
     * when navigating between pages using the back/forward navigation.
     *
     * For more information, read about bfcache:
     * https://web.dev/articles/bfcache
     */
    const onPageshow = (ev: PageTransitionEvent) => {
      if (!ev.persisted) return
      el2sig()
    }
    window.addEventListener("pageshow", onPageshow)

    return () => {
      elSigClean()
      for (const event of updateEvents) {
        el.removeEventListener(event, el2sig)
      }
      window.removeEventListener("pageshow", onPageshow)
    }
  },
}
