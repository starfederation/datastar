// Icon: material-symbols:cloud-download
// Slug: Merge fragments into the DOM using a Server-Sent Event
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import {
  DefaultFragmentMergeMode,
  DefaultFragmentsSettleDurationMs,
  DefaultFragmentsUseViewTransitions,
  EventTypes,
  FragmentMergeModes,
} from '../../../../engine/consts'
import { initErr } from '../../../../engine/errors'
import {
  type HTMLorSVGElement,
  type InitContext,
  PluginType,
  type WatcherPlugin,
} from '../../../../engine/types'
import { elUniqId, walkDOM } from '../../../../utils/dom'
import { camel, isBoolString } from '../../../../utils/text'
import { delay } from '../../../../utils/timing'
import {
  docWithViewTransitionAPI,
  supportsViewTransitions,
} from '../../../../utils/view-transtions'
import { Idiomorph } from '../../../../vendored/idiomorph.esm'
import {
  SETTLING_CLASS,
  SWAPPING_CLASS,
  datastarSSEEventWatcher,
} from '../shared'

export const MergeFragments: WatcherPlugin = {
  type: PluginType.Watcher,
  name: EventTypes.MergeFragments,
  onGlobalInit: async (ctx) => {
    const fragmentContainer = document.createElement('template')
    datastarSSEEventWatcher(
      EventTypes.MergeFragments,
      ({
        fragments: fragmentsRaw = '<div></div>',
        selector = '',
        mergeMode = DefaultFragmentMergeMode,
        settleDuration:
          settleDurationRaw = `${DefaultFragmentsSettleDurationMs}`,
        useViewTransition:
          useViewTransitionRaw = `${DefaultFragmentsUseViewTransitions}`,
      }) => {
        const settleDuration = Number.parseInt(settleDurationRaw)
        const useViewTransition = isBoolString(useViewTransitionRaw)

        fragmentContainer.innerHTML = fragmentsRaw.trim()
        const fragments = [...fragmentContainer.content.children]
        for (const fragment of fragments) {
          if (!(fragment instanceof Element)) {
            throw initErr('NoFragmentsFound', ctx)
          }

          const selectorOrID = selector || `#${fragment.getAttribute('id')}`
          const targets = [...(document.querySelectorAll(selectorOrID) || [])]
          if (!targets.length) {
            throw initErr('NoTargetsFound', ctx, { selectorOrID })
          }

          if (useViewTransition && supportsViewTransitions) {
            docWithViewTransitionAPI.startViewTransition(() =>
              applyToTargets(ctx, mergeMode, settleDuration, fragment, targets),
            )
          } else {
            applyToTargets(ctx, mergeMode, settleDuration, fragment, targets)
          }
        }
      },
    )
  },
}

function applyToTargets(
  ctx: InitContext,
  mergeMode: string,
  settleDuration: number,
  fragment: Element,
  capturedTargets: Element[],
) {
  for (const initialTarget of capturedTargets) {
    initialTarget.classList.add(SWAPPING_CLASS)
    const originalHTML = initialTarget.outerHTML
    let modifiedTarget = initialTarget
    switch (mergeMode) {
      case FragmentMergeModes.Morph: {
        const toApply = new Map<Element, Array<string>>()

        const fragmentWithIDs = fragment.cloneNode(true) as HTMLorSVGElement
        walkDOM(fragmentWithIDs, (el) => {
          if (!el.id?.length && Object.keys(el.dataset).length) {
            el.id = elUniqId(el)
            console.log(el.id)
          }
        })

        const result = Idiomorph.morph(modifiedTarget, fragmentWithIDs, {
          restoreFocus: true,
          callbacks: {
            beforeAttributeUpdated: (
              argument: string,
              el: Element,
              mode: 'update' | 'remove',
            ): boolean => {
              if (mode === 'update' && argument.startsWith('data-')) {
                let elAddAttrs = toApply.get(el)
                if (!elAddAttrs) {
                  elAddAttrs = []
                  toApply.set(el, elAddAttrs)
                }
                const name = argument.slice('data-'.length)
                elAddAttrs.push(camel(name))
              }
              return true
            },
          },
        })
        if (result?.length) {
          modifiedTarget = result[0] as Element
          for (const [el, attrs] of toApply.entries()) {
            for (const attr of attrs) {
              ctx.applyAttributePlugin(el as HTMLorSVGElement, attr)
            }
          }
        }

        break
      }
      case FragmentMergeModes.Inner:
        // Replace the contents of the target element with the outer HTML of the response
        modifiedTarget.innerHTML = fragment.outerHTML
        break
      case FragmentMergeModes.Outer:
        // Replace the entire target element with the response
        modifiedTarget.replaceWith(fragment)
        break
      case FragmentMergeModes.Prepend:
        // Insert the response before the first child of the target element
        modifiedTarget.prepend(fragment)
        break
      case FragmentMergeModes.Append:
        // Insert the response after the last child of the target element
        modifiedTarget.append(fragment)
        break
      case FragmentMergeModes.Before:
        // Insert the response before the target element
        modifiedTarget.before(fragment)
        break
      case FragmentMergeModes.After:
        // Insert the response after the target element
        modifiedTarget.after(fragment)
        break
      case FragmentMergeModes.UpsertAttributes:
        // Upsert the attributes of the target element
        for (const attrName of fragment.getAttributeNames()) {
          const value = fragment.getAttribute(attrName)!
          modifiedTarget.setAttribute(attrName, value)
        }
        break
      default:
        throw initErr('InvalidMergeMode', ctx, { mergeMode })
    }

    const cl = modifiedTarget.classList
    cl?.add(SWAPPING_CLASS)

    delay(() => {
      initialTarget.classList.remove(SWAPPING_CLASS)
      cl?.remove(SWAPPING_CLASS)
    }, settleDuration)

    const revisedHTML = modifiedTarget.outerHTML

    if (cl && originalHTML !== revisedHTML) {
      cl.add(SETTLING_CLASS)
      delay(() => {
        cl.remove(SETTLING_CLASS)
      }, settleDuration)
    }
  }
}
