// Icon: material-symbols:cloud-download
// Slug: Merge fragments into the DOM using a Server-Sent Event
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import {
  DefaultFragmentMergeMode,
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
import { datastarSSEEventWatcher } from '../shared'

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
        useViewTransition:
          useViewTransitionRaw = `${DefaultFragmentsUseViewTransitions}`,
      }) => {
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
              applyToTargets(ctx, mergeMode, fragment, targets),
            )
          } else {
            applyToTargets(ctx, mergeMode, fragment, targets)
          }
        }
      },
    )
  },
}

function applyToTargets(
  ctx: InitContext,
  mergeMode: string,
  fragment: Element,
  capturedTargets: Element[],
) {
  for (const target of capturedTargets) {
    // Clone the fragment to merge to avoid modifying the original and force browsers to merge the fragment into the DOM
    const fragmentToMerge = fragment.cloneNode(true) as HTMLorSVGElement

    switch (mergeMode) {
      case FragmentMergeModes.Morph: {
        walkDOM(fragmentToMerge, (el) => {
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

        Idiomorph.morph(target, fragmentToMerge)
        break
      }
      case FragmentMergeModes.Inner:
        // Replace the contents of the target element with the outer HTML of the response
        target.innerHTML = fragmentToMerge.outerHTML
        break
      case FragmentMergeModes.Outer:
        // Replace the entire target element with the response
        target.replaceWith(fragmentToMerge)
        break
      case FragmentMergeModes.Prepend:
        // Insert the response before the first child of the target element
        target.prepend(fragmentToMerge)
        break
      case FragmentMergeModes.Append:
        // Insert the response after the last child of the target element
        target.append(fragmentToMerge)
        break
      case FragmentMergeModes.Before:
        // Insert the response before the target element
        target.before(fragmentToMerge)
        break
      case FragmentMergeModes.After:
        // Insert the response after the target element
        target.after(fragmentToMerge)
        break
      case FragmentMergeModes.UpsertAttributes:
        // Upsert the attributes of the target element
        for (const attrName of fragmentToMerge.getAttributeNames()) {
          const value = fragmentToMerge.getAttribute(attrName)!
          target.setAttribute(attrName, value)
        }
        break
      default:
        throw initErr('InvalidMergeMode', ctx, { mergeMode })
    }
  }
}
