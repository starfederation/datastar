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
    const input = el as HTMLInputElement
    const signalName = key
      ? modifyCasing(key, mods)
      : trimDollarSignPrefix(value)
      
    const tnl = el.tagName.toLowerCase()
    const isInput = tnl.includes('input')
    const isSelect = tnl.includes('select')
    const type = el.getAttribute('type')
    const hasValueAttribute = el.hasAttribute('value')

    let signalDefault: string | boolean | number | File = ''
    const isCheckbox = isInput && type === 'checkbox'
    if (isCheckbox) {
      signalDefault = hasValueAttribute ? '' : false
    }
    const isNumber = isInput && type === 'number'
    if (isNumber) {
      signalDefault = 0
    }
    const isRadio = isInput && type === 'radio'
    if (isRadio) {
      const name = el.getAttribute('name')
      if (!name?.length) {
        el.setAttribute('name', signalName)
      }
    }
    // Can't set a default value for a file input, yet
    const isFile = isInput && type === 'file'

    const { signal, inserted } = signals.upsertIfMissing(
      signalName,
      signalDefault,
    )

    let arrayIndex = -1
    if (Array.isArray(signal.value)) {
      if (el.getAttribute('name') === null) {
        el.setAttribute('name', signalName)
      }
      arrayIndex = [
        ...document.querySelectorAll(`[name="${signalName}"]`),
      ].findIndex((el) => el === ctx.el)
    }
    const isArray = arrayIndex >= 0

    const signalArray = () => [...(signals.value(signalName) as any[])]

    const setFromSignal = () => {
      let value = signals.value(signalName)
      if (isArray && !isSelect) {
        // May be undefined if the array is shorter than the index
        value = (value as any)[arrayIndex] || signalDefault
      }
      const stringValue = `${value}`
      if (isCheckbox || isRadio) {
        if (typeof value === 'boolean') {
          input.checked = value
        } else {
          input.checked = stringValue === input.value
        }
      } else if (isSelect) {
        const select = el as HTMLSelectElement
        if (select.multiple) {
          if (!isArray) {
            throw runtimeErr('BindSelectMultiple', ctx)
          }
          for (const opt of select.options) {
            if (opt?.disabled) return
            const incoming = isNumber ? Number(opt.value) : opt.value
            opt.selected = (value as any[]).includes(incoming)
          }
        } else {
          select.value = stringValue
        }
      } else if (isFile) {
        // File input reading from a signal is not supported
      } else if ('value' in el) {
        el.value = stringValue
      } else {
        el.setAttribute('value', stringValue)
      }
    }

    const setFromElement = async () => {
      let currentValue = signals.value(signalName)
      if (isArray) {
        // Push as many default signal values onto the array as necessary to reach the index
        const currentArray = currentValue as any[]
        while (arrayIndex >= currentArray.length) {
          currentArray.push(signalDefault)
        }
        currentValue = currentArray[arrayIndex] || signalDefault
      }

      const update = (signalName: string, value: any) => {
        let newValue = value
        if (isArray && !isSelect) {
          newValue = signalArray()
          newValue[arrayIndex] = value
        }
        signals.setValue(signalName, newValue)
      }

      // Files are a special flower
      if (isFile) {
        const files = [...(input?.files || [])]
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
        update(signalName, allContents)
        update(`${signalName}Mimes`, allMimes)
        update(`${signalName}Names`, allNames)
        return
      }

      const value = input.value || ''
      let newValue: any

      if (isCheckbox) {
        const checked =
          input.checked || input.getAttribute('checked') === 'true'

        // We must check for an attribute value because a checked value defaults to `on`.
        if (hasValueAttribute) {
          newValue = checked ? value : ''
        } else {
          newValue = checked
        }
      } else if (isSelect) {
        const select = el as HTMLSelectElement
        const selectedOptions = [...select.selectedOptions]
        if (isArray) {
          newValue = selectedOptions
            .filter((opt) => opt.selected)
            .map((opt) => opt.value)
        } else {
          newValue = selectedOptions[0]?.value || signalDefault
        }
      } else if (typeof currentValue === 'boolean') {
        newValue = Boolean(value)
      } else if (typeof currentValue === 'number') {
        newValue = Number(value)
      } else {
        newValue = value || ''
      }

      update(signalName, newValue)
    }

    // If the signal was inserted, attempt to set the the signal value from the element.
    if (inserted) {
      setFromElement()
    }

    for (const event of updateEvents) {
      el.addEventListener(event, setFromElement)
    }

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
      setFromElement()
    }
    window.addEventListener('pageshow', onPageshow)

    const reset = effect(() => setFromSignal())

    return () => {
      reset()

      for (const event of updateEvents) {
        el.removeEventListener(event, setFromElement)
      }
      
      window.removeEventListener('pageshow', onPageshow)
    }
  },
}
