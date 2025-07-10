// Icon: akar-icons:link-chain
// Slug: Bind attributes to expressions
// Description: Any attribute can be bound to an expression. The attribute will be updated reactively whenever the expression signal changes.

import { aliasify } from '../../../engine/engine'
import type { AttributePlugin } from '../../../engine/types'
import { pathToObj } from '../../../utils/paths'
import { modifyCasing, modifyScope } from '../../../utils/text'

const dataURIRegex = /^data:(?<mime>[^;]+);base64,(?<contents>.*)$/
const textType = /email|password|search|tel|text|url/
const numberType = /number|range/

export const Bind: AttributePlugin = {
  type: 'attribute',
  name: 'bind',
  keyReq: 'exclusive',
  valReq: 'exclusive',
  onLoad: ({
    el,
    key,
    mods,
    value,
    effect,
    mergePatch,
    runtimeErr,
    getPath,
    hasPath,
  }) => {
    let signalName = key ? modifyCasing(key, mods) : value
    signalName = modifyScope(signalName, el, mods)

    if (
      (el instanceof HTMLInputElement && textType.test(el.type)) ||
      el instanceof HTMLTextAreaElement
    ) {
      if (Array.isArray(hasPath(signalName) && getPath(signalName))) {
        // For array signals, determine this input's index based on DOM order
        const scopedModifier = mods.has('scoped') ? '__scoped' : ''
        const allBoundInputs = document.querySelectorAll(
          `[${aliasify('bind')}${scopedModifier}-${key}],[${aliasify('bind')}${scopedModifier}="${value}"]`,
        )

        let inputIndex = 0
        const pathObj: Record<string, string> = {}
        for (const input of allBoundInputs) {
          if (!hasPath(`${signalName}.${inputIndex}`)) {
            pathObj[`${signalName}.${inputIndex}`] = (
              input as HTMLInputElement
            ).value
          }

          if (el === input) {
            break
          }
          inputIndex++
        }
        mergePatch(pathToObj({}, pathObj))

        const syncSignal = () => {
          mergePatch(
            pathToObj({}, { [`${signalName}.${inputIndex}`]: el.value }),
          )
        }

        el.addEventListener('change', syncSignal)
        el.addEventListener('input', syncSignal)

        const cleanup = effect(
          () => (el.value = getPath(signalName)[inputIndex]),
        )

        return () => {
          cleanup()
          el.removeEventListener('change', syncSignal)
          el.removeEventListener('input', syncSignal)
        }
      }

      // Non-array signal handling
      mergePatch(pathToObj({}, { [signalName]: el.value }), { ifMissing: true })

      const syncSignal = () =>
        mergePatch(pathToObj({}, { [signalName]: el.value }))
      el.addEventListener('change', syncSignal)
      el.addEventListener('input', syncSignal)

      const cleanup = effect(() => (el.value = getPath(signalName)))

      return () => {
        cleanup()
        el.removeEventListener('change', syncSignal)
        el.removeEventListener('input', syncSignal)
      }
    }

    if (el instanceof HTMLInputElement) {
      if (el.type === 'checkbox') {
        if (Array.isArray(hasPath(signalName) && getPath(signalName))) {
          // For array signals, determine this input's index based on DOM order
          const scopedModifier = mods.has('scoped') ? '__scoped' : ''
          const allBoundInputs = document.querySelectorAll(
            `[${aliasify('bind')}${scopedModifier}-${key}],[${aliasify('bind')}${scopedModifier}="${value}"]`,
          )

          let inputIndex = 0
          const pathObj: Record<string, string | boolean> = {}
          for (const input of allBoundInputs) {
            if (!hasPath(`${signalName}.${inputIndex}`)) {
              const val = input.getAttribute('value')
              pathObj[`${signalName}.${inputIndex}`] = val
                ? (input as HTMLInputElement).checked
                  ? val
                  : ''
                : (input as HTMLInputElement).checked
            }

            if (el === input) {
              break
            }
            inputIndex++
          }
          mergePatch(pathToObj({}, pathObj))

          const syncSignal = () => {
            const val = el.getAttribute('value')
            mergePatch(
              pathToObj(
                {},
                {
                  [`${signalName}.${inputIndex}`]: val
                    ? el.checked
                      ? val
                      : ''
                    : el.checked,
                },
              ),
            )
          }

          el.addEventListener('change', syncSignal)
          el.addEventListener('input', syncSignal)

          const cleanup = effect(() => {
            const val = el.getAttribute('value')
            el.checked = val
              ? val === getPath(signalName)[inputIndex]
              : getPath(signalName)[inputIndex]
          })

          return () => {
            cleanup()
            el.removeEventListener('change', syncSignal)
            el.removeEventListener('input', syncSignal)
          }
        }

        const val = el.getAttribute('value')
        mergePatch(
          pathToObj(
            {},
            { [signalName]: val ? (el.checked ? val : '') : el.checked },
          ),
        )

        const syncSignal = () => {
          const val = el.getAttribute('value')
          mergePatch(
            pathToObj(
              {},
              { [signalName]: val ? (el.checked ? val : '') : el.checked },
            ),
          )
        }
        el.addEventListener('change', syncSignal)
        el.addEventListener('input', syncSignal)

        const cleanup = effect(() => {
          const val = el.getAttribute('value')
          el.checked = val ? val === getPath(signalName) : getPath(signalName)
        })

        return () => {
          cleanup()
          el.removeEventListener('change', syncSignal)
          el.removeEventListener('input', syncSignal)
        }
      }

      if (el.type === 'radio') {
        // Set up radio button name attribute if not present
        if (!el.getAttribute('name')?.length) {
          el.setAttribute('name', signalName)
        }

        mergePatch(pathToObj({}, { [signalName]: el.value }), {
          ifMissing: true,
        })

        const syncSignal = () =>
          el.checked && mergePatch(pathToObj({}, { [signalName]: el.value }))
        el.addEventListener('change', syncSignal)
        el.addEventListener('input', syncSignal)

        const cleanup = effect(
          () => (el.checked = el.value === getPath(signalName)),
        )

        return () => {
          cleanup()
          el.removeEventListener('change', syncSignal)
          el.removeEventListener('input', syncSignal)
        }
      }

      if (numberType.test(el.type)) {
        mergePatch(pathToObj({}, { [signalName]: +el.value }), {
          ifMissing: true,
        })

        const syncSignal = () =>
          mergePatch(pathToObj({}, { [signalName]: +el.value }))
        el.addEventListener('change', syncSignal)
        el.addEventListener('input', syncSignal)

        const cleanup = effect(() => (el.value = getPath(signalName)))

        return () => {
          cleanup()
          el.removeEventListener('change', syncSignal)
          el.removeEventListener('input', syncSignal)
        }
      }

      if (el.type === 'file') {
        const syncSignal = () => {
          const files = [...(el.files || [])]
          const allContents: string[] = []
          const allMimes: string[] = []
          const allNames: string[] = []
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
                    allContents.push(match.groups.contents)
                    allMimes.push(match.groups.mime)
                    allNames.push(f.name)
                  }
                  reader.onloadend = () => resolve()
                  reader.readAsDataURL(f)
                }),
            ),
          ).then(() => {
            mergePatch(
              pathToObj(
                {},
                {
                  [signalName]: allContents,
                  [`${signalName}Mimes`]: allMimes,
                  [`${signalName}Names`]: allNames,
                },
              ),
            )
          })
        }

        el.addEventListener('change', syncSignal)
        el.addEventListener('input', syncSignal)

        return () => {
          el.removeEventListener('change', syncSignal)
          el.removeEventListener('input', syncSignal)
        }
      }
      // else unsupported type
    }

    if (el instanceof HTMLSelectElement) {
      if (el.multiple) {
        // Multi-select handling
        mergePatch(
          pathToObj(
            {},
            {
              [signalName]: [...el.selectedOptions].map(
                (option) => option.value,
              ),
            },
          ),
          {
            ifMissing: true,
          },
        )

        const syncSignal = () =>
          mergePatch(
            pathToObj(
              {},
              {
                [signalName]: [...el.selectedOptions].map(
                  (option) => option.value,
                ),
              },
            ),
          )
        el.addEventListener('change', syncSignal)
        el.addEventListener('input', syncSignal)

        const cleanup = effect(() => {
          const signalValue: string[] = getPath(signalName)
          for (const option of el.options) {
            option.selected = signalValue.includes(option.value)
          }
        })

        return () => {
          cleanup()
          el.removeEventListener('change', syncSignal)
          el.removeEventListener('input', syncSignal)
        }
      }

      // Single-select handling
      mergePatch(
        pathToObj(
          {},
          {
            [signalName]: el.value,
          },
        ),
        {
          ifMissing: true,
        },
      )

      const syncSignal = () =>
        mergePatch(
          pathToObj(
            {},
            {
              [signalName]: el.value,
            },
          ),
        )
      el.addEventListener('change', syncSignal)
      el.addEventListener('input', syncSignal)

      const syncElement = () => (el.value = getPath(signalName))
      const cleanup = effect(syncElement)

      return () => {
        cleanup()
        el.removeEventListener('change', syncSignal)
        el.removeEventListener('input', syncSignal)
      }
    }

    mergePatch(
      pathToObj(
        {},
        {
          [signalName]: el.getAttribute('value'),
        },
      ),
      { ifMissing: true },
    )

    const observer = new MutationObserver(() => {
      mergePatch(
        pathToObj(
          {},
          {
            [signalName]: el.getAttribute('value'),
          },
        ),
      )
    })
    observer.observe(el, {
      attributeFilter: ['value'],
    })

    const cleanup = effect(() => el.setAttribute('value', getPath(signalName)))

    return () => {
      cleanup()
      observer.disconnect()
    }
  },
}
