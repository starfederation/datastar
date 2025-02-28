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
import { attrHash, elUniqId, walkDOM } from '../../../../utils/dom'
import { isBoolString } from '../../../../utils/text'
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
    const modifiedTarget = initialTarget
    switch (mergeMode) {
      case FragmentMergeModes.Morph: {
        const fragmentWithIDs = fragment.cloneNode(true) as HTMLorSVGElement
        walkDOM(fragmentWithIDs, (el) => {
          if (!el.id?.length && Object.keys(el.dataset).length) {
            el.id = elUniqId(el)
          }
          // Rehash the cleanup functions for this element to ensure that plugins are cleaned up and reapplied after merging.
          const elTracking = ctx.removals.get(el.id)
          if (elTracking) {
            const newElTracking = new Map()
            for (const [key, cleanup] of elTracking) {
              const newKey = attrHash(key, key)
              newElTracking.set(newKey, cleanup)
              elTracking.delete(key)
            }
            ctx.removals.set(el.id, newElTracking)
          }
        })

        Idiomorph.morph(modifiedTarget, fragmentWithIDs)
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

    setTimeout(() => {
      initialTarget.classList.remove(SWAPPING_CLASS)
      cl?.remove(SWAPPING_CLASS)
    }, settleDuration)

    const revisedHTML = modifiedTarget.outerHTML

    if (cl && originalHTML !== revisedHTML) {
      cl.add(SETTLING_CLASS)
      setTimeout(() => {
        cl.remove(SETTLING_CLASS)
      }, settleDuration)
    }
  }
}
