// Icon: material-symbols:cloud-download
// Slug: Patches elements into the DOM.
// Description: Patches elements into the DOM.

import {
  DefaultElementPatchMode,
  ElementPatchModeAfter,
  ElementPatchModeAppend,
  ElementPatchModeBefore,
  ElementPatchModeInner,
  ElementPatchModeOuter,
  ElementPatchModePrepend,
  ElementPatchModeRemove,
  ElementPatchModeReplace,
  EventTypePatchElements,
} from '../../../engine/consts'
import { aliasify } from '../../../engine/engine'
import type { InitContext, WatcherPlugin } from '../../../engine/types'
import { kebab } from '../../../utils/text'
import { supportsViewTransitions } from '../../../utils/view-transitions'
import { datastarSSEEventWatcher } from '../shared'

export const PatchElements: WatcherPlugin = {
  type: 'watcher',
  name: EventTypePatchElements,
  async onGlobalInit(ctx) {
    datastarSSEEventWatcher(EventTypePatchElements, (args) => {
      if (
        supportsViewTransitions &&
        args.useViewTransition?.trim() === 'true'
      ) {
        document.startViewTransition(() => onPatchElements(ctx, args))
      } else {
        onPatchElements(ctx, args)
      }
    })
  },
}

function onPatchElements(
  ctx: InitContext,
  {
    elements = '',
    selector,
    mode = DefaultElementPatchMode,
  }: Record<string, string>,
) {
  const { initErr } = ctx
  const elementsWithSvgsRemoved = elements.replace(
    /<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,
    '',
  )
  const hasHtml = /<\/html>/.test(elementsWithSvgsRemoved)
  const hasHead = /<\/head>/.test(elementsWithSvgsRemoved)
  const hasBody = /<\/body>/.test(elementsWithSvgsRemoved)

  const newDocument = new DOMParser().parseFromString(
    hasHtml || hasHead || hasBody
      ? elements
      : `<body><template>${elements}</template></body>`,
    'text/html',
  )

  let newContent = document.createDocumentFragment()
  if (hasHtml) {
    newContent.appendChild(newDocument.documentElement)
  } else if (hasHead && hasBody) {
    newContent.appendChild(newDocument.head)
    newContent.appendChild(newDocument.body)
  } else if (hasHead) {
    newContent.appendChild(newDocument.head)
  } else if (hasBody) {
    newContent.appendChild(newDocument.body)
  } else {
    newContent = newDocument.querySelector('template')!.content
  }

  if (
    !selector &&
    (mode === ElementPatchModeOuter || mode === ElementPatchModeReplace)
  ) {
    for (const child of newContent.children) {
      let target: Element
      if (child instanceof HTMLHtmlElement) {
        target = document.documentElement
      } else if (child instanceof HTMLBodyElement) {
        target = document.body
      } else if (child instanceof HTMLHeadElement) {
        target = document.head
      } else {
        target = document.getElementById(child.id)!
        if (!target) {
          console.error(
            initErr('NoTargetsFound', {
              id: child.id,
            }),
          )
          continue
        }
      }

      applyToTargets(ctx, mode, child, [target])
    }
  } else {
    const targets = document.querySelectorAll(selector)
    if (!targets.length) {
      console.error(
        initErr('NoTargetsFound', {
          selector: selector,
        }),
      )
      return
    }

    applyToTargets(ctx, mode, newContent, targets)
  }
}

const scripts = new WeakSet<HTMLScriptElement>()
for (const script of document.querySelectorAll('script')) {
  scripts.add(script)
}

function execute(target: Element): void {
  const elScripts =
    target instanceof HTMLScriptElement
      ? [target]
      : target.querySelectorAll('script')
  for (const old of elScripts) {
    if (!scripts.has(old)) {
      const script = document.createElement('script')
      for (const { name, value } of old.attributes) {
        script.setAttribute(name, value)
      }
      script.text = old.text
      old.replaceWith(script)
      scripts.add(script)
    }
  }
}

function applyToTargets(
  { initErr }: InitContext,
  mode: string,
  element: DocumentFragment | Element,
  capturedTargets: Iterable<Element>,
) {
  for (const target of capturedTargets) {
    const cloned = element.cloneNode(true) as Element
    if (mode === ElementPatchModeRemove) {
      target.remove()
    } else if (
      mode === ElementPatchModeOuter ||
      mode === ElementPatchModeInner
    ) {
      morph(target, cloned, mode)
      execute(target)
    } else {
      execute(cloned)
      if (mode === ElementPatchModeReplace) {
        target.replaceWith(cloned)
      } else if (mode === ElementPatchModePrepend) {
        target.prepend(cloned)
      } else if (mode === ElementPatchModeAppend) {
        target.append(cloned)
      } else if (mode === ElementPatchModeBefore) {
        target.before(cloned)
      } else if (mode === ElementPatchModeAfter) {
        target.after(cloned)
      } else {
        throw initErr('InvalidPatchMode', { mode })
      }
    }
  }
}

const oldIdTagNameMap = new Map<string, string>()
const ctxIdMap = new Map<Node, Set<string>>()
const ctxPersistentIds = new Set<string>()
const duplicateIds = new Set<string>()
const ctxPantry = document.createElement('div')
ctxPantry.hidden = true

function morph(
  oldElt: Element,
  newContent: DocumentFragment | Element,
  mode: typeof ElementPatchModeInner | typeof ElementPatchModeOuter,
): void {
  const ignore = aliasify('ignore-morph')
  if (
    (oldElt.hasAttribute(ignore) &&
      newContent instanceof HTMLElement &&
      newContent.hasAttribute(ignore)) ||
    oldElt.parentElement?.closest(`[${ignore}]`)
  ) {
    return
  }

  const normalizedElt = document.createElement('div')
  normalizedElt.append(newContent)
  document.body.insertAdjacentElement('afterend', ctxPantry)

  // Computes the set of IDs that persist between the two contents excluding duplicates
  const oldIdElements = oldElt.querySelectorAll('[id]')
  for (const { id, tagName } of oldIdElements) {
    if (oldIdTagNameMap.has(id)) {
      duplicateIds.add(id)
    } else {
      oldIdTagNameMap.set(id, tagName)
    }
  }
  if (oldElt.id) {
    if (oldIdTagNameMap.has(oldElt.id)) {
      duplicateIds.add(oldElt.id)
    } else {
      oldIdTagNameMap.set(oldElt.id, oldElt.tagName)
    }
  }

  ctxPersistentIds.clear()
  const newIdElements = normalizedElt.querySelectorAll('[id]')
  for (const { id, tagName } of newIdElements) {
    if (ctxPersistentIds.has(id)) {
      duplicateIds.add(id)
    } else if (oldIdTagNameMap.get(id) === tagName) {
      ctxPersistentIds.add(id)
    }
  }

  oldIdTagNameMap.clear()

  for (const id of duplicateIds) {
    ctxPersistentIds.delete(id)
  }

  duplicateIds.clear()

  // Computes a map of nodes to all IDs contained within that node (inclusive of the node).
  // This map can be used to ask if two nodes have intersecting sets of IDs,
  // which allows for a looser definition of "matching" than traditional ID matching,
  // and allows child nodes to contribute to a parent nodes matching.
  // const idMap = new Map<Node, Set<string>>()
  ctxIdMap.clear()

  populateIdMapWithTree(
    mode === 'outer' ? oldElt.parentElement! : oldElt,
    oldIdElements,
  )
  populateIdMapWithTree(normalizedElt, newIdElements)

  morphChildren(
    mode === 'outer' ? oldElt.parentElement! : oldElt,
    normalizedElt,
    mode === 'outer' ? (oldElt as Node) : null,
    oldElt.nextSibling,
  )

  ctxPantry.remove()
}

// This is the core algorithm for matching up children.
// The idea is to use ID sets to try to match up nodes as faithfully as possible.
// We greedily match, which allows us to keep the algorithm fast,
// but by using ID sets, we are able to better match up with content deeper in the DOM.
function morphChildren(
  oldParent: Element, // the old content that we are merging the new content into
  newParent: Element, // the parent element of the new content
  insertionPoint: Node | null = null, // // the point in the DOM we start morphing at (defaults to first child)
  endPoint: Node | null = null, // the point in the DOM we stop morphing at (defaults to after last child)
): void {
  // normalize
  if (
    oldParent instanceof HTMLTemplateElement &&
    newParent instanceof HTMLTemplateElement
  ) {
    // we can pretend the DocumentElement is an Element
    oldParent = oldParent.content as unknown as Element
    newParent = newParent.content as unknown as Element
  }
  insertionPoint ??= oldParent.firstChild

  // run through all the new content
  for (const newChild of newParent.childNodes) {
    // once we reach the end of the old parent content skip to the end and insert the rest
    if (insertionPoint && insertionPoint !== endPoint) {
      const bestMatch = findBestMatch(newChild, insertionPoint, endPoint)
      if (bestMatch) {
        // if the node to morph is not at the insertion point then remove/move up to it
        if (bestMatch !== insertionPoint) {
          let cursor: Node | null = insertionPoint
          // Remove nodes between the start and end nodes
          while (cursor && cursor !== bestMatch) {
            const tempNode = cursor
            cursor = cursor.nextSibling
            removeNode(tempNode)
          }
        }
        morphNode(bestMatch, newChild)
        insertionPoint = bestMatch.nextSibling
        continue
      }
    }

    // @ts-ignore
    const ncId = newChild.id
    // if the matching node is elsewhere in the original content
    if (newChild instanceof Element && ctxPersistentIds.has(ncId)) {
      // move it and all its children here and morph, will always be found
      // Search for an element by ID within the document and pantry, and move it using moveBefore.

      const movedChild = window[ncId] as unknown as Element

      // Removes an element from its ancestors' ID maps.
      // This is needed when an element is moved from the "future" via `moveBeforeId`.
      // Otherwise, its erstwhile ancestors could be mistakenly moved to the pantry rather than being deleted,
      // preventing their removal hooks from being called.
      let current = movedChild
      while ((current = current.parentNode as Element)) {
        const idSet = ctxIdMap.get(current)
        if (idSet) {
          idSet.delete(ncId)
          if (!idSet.size) {
            ctxIdMap.delete(current)
          }
        }
      }

      moveBefore(oldParent, movedChild, insertionPoint)
      morphNode(movedChild, newChild)
      insertionPoint = movedChild.nextSibling
      continue
    }

    // This performs the action of inserting a new node while handling situations where the node contains
    // elements with persistent IDs and possible state info we can still preserve by moving in and then morphing
    if (ctxIdMap.has(newChild)) {
      // node has children with IDs with possible state so create a dummy elt of same type and apply full morph algorithm
      const newEmptyChild = document.createElement(
        (newChild as Element).tagName,
      )
      oldParent.insertBefore(newEmptyChild, insertionPoint)
      morphNode(newEmptyChild, newChild)
      insertionPoint = newEmptyChild.nextSibling
    } else {
      // optimization: no id state to preserve so we can just insert a clone of the newChild and its descendants
      const newClonedChild = document.importNode(newChild, true) // importNode to not mutate newParent
      oldParent.insertBefore(newClonedChild, insertionPoint)
      insertionPoint = newClonedChild.nextSibling
    }
  }

  // remove any remaining old nodes that didn't match up with new content
  while (insertionPoint && insertionPoint !== endPoint) {
    const tempNode = insertionPoint
    insertionPoint = insertionPoint.nextSibling
    removeNode(tempNode)
  }
}

// Scans forward from the startPoint to the endPoint looking for a match for the node.
// It looks for an id set match first, then a soft match.
// We abort soft matching if we find two future soft matches, to reduce churn.
function findBestMatch(
  node: Node,
  startPoint: Node | null,
  endPoint: Node | null,
): Node | null {
  let bestMatch = null
  let nextSibling = node.nextSibling
  let siblingSoftMatchCount = 0
  let displaceMatchCount = 0

  // Max ID matches we are willing to displace in our search
  const nodeMatchCount = ctxIdMap.get(node)?.size || 0

  let cursor = startPoint
  while (cursor && cursor !== endPoint) {
    // soft matching is a prerequisite for id set matching
    if (isSoftMatch(cursor, node)) {
      let isIdSetMatch = false
      const oldSet = ctxIdMap.get(cursor)
      const newSet = ctxIdMap.get(node)

      if (newSet && oldSet) {
        for (const id of oldSet) {
          // a potential match is an id in the new and old nodes that
          // has not already been merged into the DOM
          // But the newNode content we call this on has not been
          // merged yet and we don't allow duplicate IDs so it is simple
          if (newSet.has(id)) {
            isIdSetMatch = true
            break
          }
        }
      }

      if (isIdSetMatch) {
        return cursor // found an id set match, we're done!
      }

      // we haven’t yet saved a soft match fallback
      // the current soft match will hard match something else in the future, leave it
      if (!bestMatch && !ctxIdMap.has(cursor)) {
        // optimization: if node can't id set match, we can just return the soft match immediately
        if (!nodeMatchCount) {
          return cursor
        }
        // save this as the fallback if we get through the loop without finding a hard match
        bestMatch = cursor
      }
    }
    // check for IDs we may be displaced when matching
    displaceMatchCount += ctxIdMap.get(cursor)?.size || 0
    if (displaceMatchCount > nodeMatchCount) {
      // if we are going to displace more IDs than the node contains then
      // we do not have a good candidate for an ID match, so return
      break
    }

    if (bestMatch === null && nextSibling && isSoftMatch(cursor, nextSibling)) {
      // The next new node has a soft match with this node, so
      // increment the count of future soft matches
      siblingSoftMatchCount++
      nextSibling = nextSibling.nextSibling

      // If there are two future soft matches, block soft matching for this node to allow
      // future siblings to soft match. This is to reduce churn in the DOM when an element
      // is prepended.
      if (siblingSoftMatchCount >= 2) {
        bestMatch = undefined
      }
    }

    // if the current node contains active element, stop looking for better future matches,
    // because if one is found, this node will be moved to the pantry, re-parenting it and thus losing focus
    if (cursor.contains(document.activeElement)) break

    cursor = cursor.nextSibling
  }

  return bestMatch || null
}

function isSoftMatch(oldNode: Node, newNode: Node): boolean {
  // ok to cast: if one is not element, `id` and `tagName` will be null and we'll just compare that.
  const oldId = (oldNode as Element).id
  return (
    oldNode.nodeType === newNode.nodeType &&
    (oldNode as Element).tagName === (newNode as Element).tagName &&
    // If oldElt has an `id` with possible state and it doesn’t match newElt.id then avoid morphing.
    // We'll still match an anonymous node with an IDed newElt, though, because if it got this far,
    // its not persistent, and new nodes can't have any hidden state.
    (!oldId || oldId === (newNode as Element).id)
  )
}

// Gets rid of an unwanted DOM node; strategy depends on nature of its reuse:
// - Persistent nodes will be moved to the pantry for later reuse
// - Other nodes will have their hooks called, and then are removed
function removeNode(node: Node) {
  // are we going to id set match this later?
  if (ctxIdMap.has(node)) {
    // skip callbacks and move to pantry
    moveBefore(ctxPantry, node, null)
  } else {
    // remove for realsies
    node.parentNode?.removeChild(node)
  }
}

// Moves an element before another element within the same parent.
// Uses the proposed `moveBefore` API if available (and working), otherwise falls back to `insertBefore`.
// This is essentially a forward-compat wrapper.
const moveBefore: (parentNode: Node, node: Node, after: Node | null) => void =
  // @ts-ignore
  removeNode.call.bind(ctxPantry.moveBefore ?? ctxPantry.insertBefore)

function morphNode(
  oldNode: Node, // root node to merge content into
  newNode: Node, // new content to merge
): Node {
  // syncs the oldNode to the newNode, copying over all attributes and
  // inner element state from the newNode to the oldNode
  const type = newNode.nodeType

  // if is an element type, sync the attributes from the
  // new node into the new node
  if (type === 1 /* element type */) {
    const ignore = aliasify('ignore-morph')
    if (
      (oldNode as Element).hasAttribute(ignore) &&
      (newNode as Element).hasAttribute(ignore)
    ) {
      return oldNode
    }

    //  many bothans died to bring us this information:
    //  https://github.com/patrick-steele-idem/morphdom/blob/master/src/specialElHandlers.js
    //  https://github.com/choojs/nanomorph/blob/master/lib/morph.js#L113
    if (
      oldNode instanceof HTMLInputElement &&
      newNode instanceof HTMLInputElement &&
      newNode.type !== 'file'
    ) {
      // https://github.com/bigskysoftware/idiomorph/issues/27
      // | old input value | new input value  | behaviour                              |
      // | --------------- | ---------------- | -------------------------------------- |
      // | `null`          | `null`           | preserve old input value               |
      // | some value      | the same value   | preserve old input value               |
      // | some value      | `null`           | set old input value to `""`            |
      // | `null`          | some value       | set old input value to new input value |
      // | some value      | some other value | set old input value to new input value |
      if (newNode.getAttribute('value') !== oldNode.getAttribute('value')) {
        oldNode.value = newNode.getAttribute('value') ?? ''
      }
    } else if (
      oldNode instanceof HTMLTextAreaElement &&
      newNode instanceof HTMLTextAreaElement
    ) {
      const newValue = newNode.value
      if (newValue !== oldNode.value) {
        oldNode.value = newValue
      }
      if (oldNode.firstChild && oldNode.firstChild.nodeValue !== newValue) {
        oldNode.firstChild.nodeValue = newValue
      }
    }

    const preserveAttrs = (
      (newNode as HTMLElement).getAttribute(aliasify('preserve-attr')) ?? ''
    ).split(' ')

    for (const { name, value } of (newNode as Element).attributes) {
      if (
        (oldNode as Element).getAttribute(name) !== value &&
        !preserveAttrs.includes(kebab(name))
      ) {
        ;(oldNode as Element).setAttribute(name, value)
      }
    }

    const oldAttrs = (oldNode as Element).attributes
    for (let i = oldAttrs.length - 1; i >= 0; i--) {
      const { name } = oldAttrs[i]
      if (
        !(newNode as Element).hasAttribute(name) &&
        !preserveAttrs.includes(kebab(name))
      ) {
        ;(oldNode as Element).removeAttribute(name)
      }
    }
  }

  if (type === 8 /* comment */ || type === 3 /* text */) {
    if (oldNode.nodeValue !== newNode.nodeValue) {
      oldNode.nodeValue = newNode.nodeValue
    }
  }

  if (!oldNode.isEqualNode(newNode)) {
    morphChildren(oldNode as Element, newNode as Element)
  }
  return oldNode
}

// A bottom-up algorithm that populates a map of Element -> IdSet.
// The ID set for a given element is the set of all IDs contained within its subtree.
// As an optimization, we filter these IDs through the given list of persistent IDs,
// because we don't need to bother considering IDed elements that won't be in the new content.
function populateIdMapWithTree(
  root: Element | null,
  elements: Element[] | NodeListOf<Element>,
) {
  for (const elt of elements) {
    if (ctxPersistentIds.has(elt.id)) {
      let current: Element | null = elt
      // walk up the parent hierarchy of that element, adding the ID of element to the parent's ID set
      while (current && current !== root) {
        let idSet = ctxIdMap.get(current)
        // if the ID set doesn’t exist, create it and insert it in the map
        if (!idSet) {
          idSet = new Set()
          ctxIdMap.set(current, idSet)
        }
        idSet.add(elt.id)
        current = current.parentElement
      }
    }
  }
}
