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

    let signalDefault: string | boolean | number | File = ''
    const isCheckbox = isInput && type === 'checkbox'
    if (isCheckbox) {
      signalDefault = false
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
      el.setAttribute('multiple', '')
      if (el.getAttribute('name') === null) {
        el.setAttribute('name', signalName)
      }
      arrayIndex = [
        ...document.querySelectorAll(`[name="${signalName}"]`),
      ].findIndex((el) => el === ctx.el)
    }
    const isArray = arrayIndex >= 0
    const signalArr = () => [...(signals.value(signalName) as any[])]

    const setFromSignal = () => {
      const hasValue = 'value' in el
      let v = signals.value(signalName)
      if (isArray && !isSelect) {
        // May be undefined if the array is shorter than the index
        v = (v as any)[arrayIndex] || signalDefault
      }
      const vStr = `${v}`
      if (isCheckbox || isRadio) {
        if (Array.isArray(v)) {
          input.checked = v.includes(input.value)
        } else if (typeof v === 'boolean') {
          input.checked = !!v
        } else {
          input.checked = vStr === input.value
        }
      } else if (isSelect) {
        const select = el as HTMLSelectElement
        if (select.multiple) {
          if (!isArray) throw runtimeErr('BindSelectMultiple', ctx)
          for (const opt of select.options) {
            if (opt?.disabled) return
            const incoming = isNumber ? Number(opt.value) : opt.value
            opt.selected = (v as any[]).includes(incoming)
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

    const el2sig = async () => {
      let current = signals.value(signalName)
      if (isArray) {
        const currentArray = current as any[]
        while (arrayIndex >= currentArray.length) {
          currentArray.push(signalDefault)
        }
        current = currentArray[arrayIndex] || signalDefault
      }
      const value = input.value || ''

      const update = (signalName: string, value: any) => {
        let v = value
        if (isArray && !isSelect) {
          v = signalArr()
          v[arrayIndex] = value
        }
        signals.setValue(signalName, v)
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

      let v: any

      if (isCheckbox) {
        const checked =
          input.checked || input.getAttribute('checked') === 'true'

        // We must test for the attribute value because a checked value defaults to `on`.
        const attributeValue = input.getAttribute('value')
        if (attributeValue) {
          v = checked ? value : false
        } else {
          v = checked
        }
      } else if (isSelect) {
        const select = el as HTMLSelectElement
        const selectedOptions = [...select.selectedOptions]
        if (isArray) {
          v = selectedOptions
            .filter((opt) => opt.selected)
            .map((opt) => opt.value)
        } else {
          v = selectedOptions[0]?.value || signalDefault
        }
      } else if (typeof current === 'boolean') {
        v = Boolean(value)
      } else if (typeof current === 'number') {
        v = Number(value)
      } else {
        v = value || ''
      }

      update(signalName, v)
    }

    // If the signal was inserted, attempt to set the the signal value from the element.
    if (inserted) {
      el2sig()
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
    window.addEventListener('pageshow', onPageshow)

    return () => {
      elSigClean()
      for (const event of updateEvents) {
        el.removeEventListener(event, el2sig)
      }
      window.removeEventListener('pageshow', onPageshow)
    }
  },
}
