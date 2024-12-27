// Authors: Delaney Gillilan
// Icon: akar-icons:link-chain
// Slug: Bind attributes to expressions
// Description: Any attribute can be bound to an expression. The attribute will be updated reactively whenever the expression signal changes.

import { dsErr } from '~/engine/errors'
import { type AttributePlugin, PluginType, Requirement } from '~/engine/types'
import { IngnoreAnythingMacro } from '~/plugins/official/core/macros/signals'

const dataURIRegex = /^data:(?<mime>[^;]+);base64,(?<contents>.*)$/
const updateEvents = ['change', 'input', 'keydown']

export const Bind: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'bind',
  keyReq: Requirement.Exclusive,
  valReq: Requirement.Exclusive,
  macros: { pre: [IngnoreAnythingMacro] },
  onLoad: (ctx) => {
    const { el, value, key, signals, effect } = ctx
    const signalName = key ? key : value

    let setFromSignal = () => {}
    let el2sig = () => {}

    // I better be tied to a signal
    if (typeof signalName !== 'string') {
      throw dsErr('InvalidExpression')
    }

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

    signals.upsert(signalName, signalDefault)

    setFromSignal = () => {
      const hasValue = 'value' in el
      const v = signals.value(signalName)
      const vStr = `${v}`
      if (isCheckbox || isRadio) {
        const input = el as HTMLInputElement
        if (isCheckbox) {
          input.checked = !!v || v === 'true'
        } else if (isRadio) {
          // evaluate the value as string to handle any type casting
          // automatically since the attribute has to be a string anyways
          input.checked = vStr === input.value
        }
      } else if (isFile) {
        // File input reading from a signal is not supported yet
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
                  throw dsErr('InvalidFileResultType', {
                    type: typeof reader.result,
                  })
                }
                const match = reader.result.match(dataURIRegex)
                if (!match?.groups) {
                  throw dsErr('InvalidDataUri', {
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
        const mimeName = `${signalName}Mimes`
        const nameName = `${signalName}Names`
        if (mimeName in signals) {
          signals.upsert(mimeName, allMimes)
        }
        if (nameName in signals) {
          signals.upsert(nameName, allNames)
        }
        return
      }

      const current = signals.value(signalName)
      const input = (el as HTMLInputElement) || (el as HTMLElement)

      if (typeof current === 'number') {
        const v = Number(input.value || input.getAttribute('value'))
        signals.setValue(signalName, v)
      } else if (typeof current === 'string') {
        const v = input.value || input.getAttribute('value') || ''
        signals.setValue(signalName, v)
      } else if (typeof current === 'boolean') {
        if (isCheckbox) {
          const v = input.checked || input.getAttribute('checked') === 'true'
          signals.setValue(signalName, v)
        } else {
          const v = Boolean(input.value || input.getAttribute('value'))
          signals.setValue(signalName, v)
        }
      } else if (typeof current === 'undefined') {
      } else if (Array.isArray(current)) {
        // check if the input is a select element
        if (isSelect) {
          const select = el as HTMLSelectElement
          const selectedOptions = [...select.selectedOptions]
          const selectedValues = selectedOptions
            .filter((opt) => opt.selected)
            .map((opt) => opt.value)
          signals.setValue(signalName, selectedValues)
        } else {
          // assume it's a comma-separated string
          const v = JSON.stringify(input.value.split(','))
          signals.setValue(signalName, v)
        }
      } else {
        throw dsErr('UnsupportedSignalType', {
          current: typeof current,
        })
      }
    }

    for (const event of updateEvents) {
      el.addEventListener(event, el2sig)
    }
    const elSigClean = effect(() => setFromSignal())

    return () => {
      elSigClean()
      for (const event of updateEvents) {
        el.removeEventListener(event, el2sig)
      }
    }
  },
}
