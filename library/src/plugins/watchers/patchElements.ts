// Icon: material-symbols:cloud-download
// Slug: Patches elements into the DOM.
// Description: Patches elements into the DOM.

import { watcher } from '@engine'
import type { WatcherContext } from '@engine/types'
import { isHTMLOrSVG } from '@utils/dom'
import { aliasify } from '@utils/text'
import { supportsViewTransitions } from '@utils/view-transitions'

const isValidType = <T extends readonly string[]>(
  arr: T,
  value: string,
): value is T[number] => (arr as readonly string[]).includes(value)

const PATCH_MODES = [
  'remove',
  'outer',
  'inner',
  'replace',
  'prepend',
  'append',
  'before',
  'after',
] as const
type PatchElementsMode = (typeof PATCH_MODES)[number]

const NAMESPACES = ['html', 'svg', 'mathml'] as const
type Namespace = (typeof NAMESPACES)[number]

type PatchElementsArgs = {
  elements: string
  mode: PatchElementsMode
  selector: string
  useViewTransition: boolean
  namespace: Namespace
}

watcher({
  name: 'datastar-patch-elements',
  apply(
    ctx,
    {
      elements = '',
      selector = '',
      mode = 'outer',
      useViewTransition = '',
      namespace = 'html',
    },
  ) {
    if (!isValidType(PATCH_MODES, mode)) {
      throw ctx.error('PatchElementsInvalidMode', { mode })
    }

    if (!selector && mode !== 'outer' && mode !== 'replace') {
      throw ctx.error('PatchElementsExpectedSelector')
    }

    if (!isValidType(NAMESPACES, namespace)) {
      throw ctx.error('PatchElementsInvalidNamespace', { namespace })
    }

    const args2: PatchElementsArgs = {
      mode,
      selector,
      elements,
      useViewTransition: useViewTransition.trim() === 'true',
      namespace,
    }

    if (supportsViewTransitions && useViewTransition) {
      document.startViewTransition(() => onPatchElements(ctx, args2))
    } else {
      onPatchElements(ctx, args2)
    }
  },
})

const onPatchElements = (
  { error }: WatcherContext,
  { elements, selector, mode, namespace }: PatchElementsArgs,
) => {
  const elementsWithSvgsRemoved = elements.replace(
    /<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,
    '',
  )
  const hasHtml = /<\/html>/.test(elementsWithSvgsRemoved)
  const hasHead = /<\/head>/.test(elementsWithSvgsRemoved)
  const hasBody = /<\/body>/.test(elementsWithSvgsRemoved)

  const wrapperTag =
    namespace === 'svg' ? 'svg' : namespace === 'mathml' ? 'math' : ''
  const wrappedEls = wrapperTag
    ? `<${wrapperTag}>${elements}</${wrapperTag}>`
    : elements

  const newDocument = new DOMParser().parseFromString(
    hasHtml || hasHead || hasBody
      ? elements
      : `<body><template>${wrappedEls}</template></body>`,
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
  } else if (wrapperTag) {
    const wrapperEl = newDocument
      .querySelector('template')!
      .content.querySelector(wrapperTag)!
    for (const child of wrapperEl.childNodes) {
      newContent.appendChild(child)
    }
  } else {
    newContent = newDocument.querySelector('template')!.content
  }

  if (!selector && (mode === 'outer' || mode === 'replace')) {
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
          console.warn(error('PatchElementsNoTargetsFound'), {
            element: { id: child.id },
          })
          continue
        }
      }

      applyToTargets(mode as PatchElementsMode, child, [target])
    }
  } else {
    const targets = document.querySelectorAll(selector)
    if (!targets.length) {
      console.warn(error('PatchElementsNoTargetsFound'), { selector })
      return
    }

    applyToTargets(mode as PatchElementsMode, newContent, targets)
  }
}

const scripts = new WeakSet<HTMLScriptElement>()
for (const script of document.querySelectorAll('script')) {
  scripts.add(script)
}

const execute = (target: Element): void => {
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

const applyPatchMode = (
  targets: Iterable<Element>,
  element: DocumentFragment | Element,
  action: string,
) => {
  for (const target of targets) {
    const cloned = element.cloneNode(true) as Element
    execute(cloned)
    // @ts-expect-error
    target[action](cloned)
  }
}

const applyToTargets = (
  mode: PatchElementsMode,
  element: DocumentFragment | Element,
  targets: Iterable<Element>,
) => {
  switch (mode) {
    case 'remove':
      for (const target of targets) {
        target.remove()
      }
      break
    case 'outer':
    case 'inner':
      for (const target of targets) {
        morph(target, element.cloneNode(true) as Element, mode)
        execute(target)
      }
      break
    case 'replace':
      applyPatchMode(targets, element, 'replaceWith')
      break
    case 'prepend':
    case 'append':
    case 'before':
    case 'after':
      applyPatchMode(targets, element, mode)
  }
}

const ctxIdMap = new Map<Node, Set<string>>()
const ctxPersistentIds = new Set<string>()
const oldIdTagNameMap = new Map<string, string>()
const duplicateIds = new Set<string>()
const ctxPantry = document.createElement('div')
ctxPantry.hidden = true

const aliasedIgnoreMorph = aliasify('ignore-morph')
const aliasedIgnoreMorphAttr = `[${aliasedIgnoreMorph}]`
const morph = (
  oldElt: Element | ShadowRoot,
  newContent: DocumentFragment | Element,
  mode: 'outer' | 'inner' = 'outer',
): void => {
  if (
    (isHTMLOrSVG(oldElt) &&
      isHTMLOrSVG(newContent) &&
      oldElt.hasAttribute(aliasedIgnoreMorph) &&
      newContent.hasAttribute(aliasedIgnoreMorph)) ||
    oldElt.parentElement?.closest(aliasedIgnoreMorphAttr)
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
  if (oldElt instanceof Element && oldElt.id) {
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

  for (const id of duplicateIds) {
    ctxPersistentIds.delete(id)
  }

  oldIdTagNameMap.clear()
  duplicateIds.clear()
  ctxIdMap.clear()

  const parent = mode === 'outer' ? oldElt.parentElement! : oldElt
  populateIdMapWithTree(parent, oldIdElements)
  populateIdMapWithTree(normalizedElt, newIdElements)

  morphChildren(
    parent,
    normalizedElt,
    mode === 'outer' ? oldElt : null,
    oldElt.nextSibling,
  )

  ctxPantry.remove()
}

// This is the core algorithm for matching up children.
// The idea is to use ID sets to try to match up nodes as faithfully as possible.
// We greedily match, which allows us to keep the algorithm fast,
// but by using ID sets, we are able to better match up with content deeper in the DOM.
const morphChildren = (
  oldParent: Element | ShadowRoot, // the old content that we are merging the new content into
  newParent: Element, // the parent element of the new content
  insertionPoint: Node | null = null, // the point in the DOM we start morphing at (defaults to first child)
  endPoint: Node | null = null, // the point in the DOM we stop morphing at (defaults to after last child)
): void => {
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

    // if the matching node is elsewhere in the original content
    if (newChild instanceof Element && ctxPersistentIds.has(newChild.id)) {
      // move it and all its children here and morph, will always be found
      // Search for an element by ID within the document and pantry, and move it using moveBefore.
      const movedChild = document.getElementById(newChild.id) as Element

      // Removes an element from its ancestors' ID maps.
      // This is needed when an element is moved from the "future" via `moveBeforeId`.
      // Otherwise, its erstwhile ancestors could be mistakenly moved to the pantry rather than being deleted,
      // preventing their removal hooks from being called.
      let current = movedChild
      while ((current = current.parentNode as Element)) {
        const idSet = ctxIdMap.get(current)
        if (idSet) {
          idSet.delete(newChild.id)
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
      const namespaceURI = (newChild as Element).namespaceURI
      const tagName = (newChild as Element).tagName
      const newEmptyChild =
        namespaceURI && namespaceURI !== 'http://www.w3.org/1999/xhtml'
          ? document.createElementNS(namespaceURI, tagName)
          : document.createElement(tagName)
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
const findBestMatch = (
  node: Node,
  startPoint: Node | null,
  endPoint: Node | null,
): Node | null => {
  let bestMatch: Node | null | undefined = null
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

    cursor = cursor.nextSibling
  }

  return bestMatch || null
}

// ok to cast: if one is not element, `id` and `tagName` will be null and we'll just compare that.
const isSoftMatch = (oldNode: Node, newNode: Node): boolean =>
  oldNode.nodeType === newNode.nodeType &&
  (oldNode as Element).tagName === (newNode as Element).tagName &&
  // If oldElt has an `id` with possible state and it doesn’t match newElt.id then avoid morphing.
  // We'll still match an anonymous node with an IDed newElt, though, because if it got this far,
  // its not persistent, and new nodes can't have any hidden state.
  (!(oldNode as Element).id ||
    (oldNode as Element).id === (newNode as Element).id)

// Gets rid of an unwanted DOM node; strategy depends on nature of its reuse:
// - Persistent nodes will be moved to the pantry for later reuse
// - Other nodes will have their hooks called, and then are removed
const removeNode = (node: Node): void => {
  // are we going to id set match this later?
  ctxIdMap.has(node)
    ? // skip callbacks and move to pantry
      moveBefore(ctxPantry, node, null)
    : // remove for realsies
      node.parentNode?.removeChild(node)
}

// Moves an element before another element within the same parent.
// Uses the proposed `moveBefore` API if available (and working), otherwise falls back to `insertBefore`.
// This is essentially a forward-compat wrapper.
const moveBefore: (parentNode: Node, node: Node, after: Node | null) => void =
  // @ts-expect-error
  removeNode.call.bind(ctxPantry.moveBefore ?? ctxPantry.insertBefore)

const aliasedPreserveAttr = aliasify('preserve-attr')

// syncs the oldNode to the newNode, copying over all attributes and
// inner element state from the newNode to the oldNode
const morphNode = (
  oldNode: Node, // root node to merge content into
  newNode: Node, // new content to merge
): Node => {
  const type = newNode.nodeType

  // if is an element type, sync the attributes from the
  // new node into the new node
  if (type === 1 /* element type */) {
    const oldElt = oldNode as Element
    const newElt = newNode as Element
    const shouldScopeChildren = oldElt.hasAttribute('data-scope-children')
    if (
      oldElt.hasAttribute(aliasedIgnoreMorph) &&
      newElt.hasAttribute(aliasedIgnoreMorph)
    ) {
      return oldNode
    }

    //  many bothans died to bring us this information:
    //  https://github.com/patrick-steele-idem/morphdom/blob/master/src/specialElHandlers.js
    //  https://github.com/choojs/nanomorph/blob/master/lib/morph.js#L113
    if (
      oldElt instanceof HTMLInputElement &&
      newElt instanceof HTMLInputElement &&
      newElt.type !== 'file'
    ) {
      // https://github.com/bigskysoftware/idiomorph/issues/27
      // | old input value | new input value  | behaviour                              |
      // | --------------- | ---------------- | -------------------------------------- |
      // | `null`          | `null`           | preserve old input value               |
      // | some value      | the same value   | preserve old input value               |
      // | some value      | `null`           | set old input value to `""`            |
      // | `null`          | some value       | set old input value to new input value |
      // | some value      | some other value | set old input value to new input value |
      if (newElt.getAttribute('value') !== oldElt.getAttribute('value')) {
        oldElt.value = newElt.getAttribute('value') ?? ''
      }
    } else if (
      oldElt instanceof HTMLTextAreaElement &&
      newElt instanceof HTMLTextAreaElement
    ) {
      if (newElt.value !== oldElt.value) {
        oldElt.value = newElt.value
      }
      if (oldElt.firstChild && oldElt.firstChild.nodeValue !== newElt.value) {
        oldElt.firstChild.nodeValue = newElt.value
      }
    }

    const preserveAttrs = (
      (newNode as HTMLElement).getAttribute(aliasedPreserveAttr) ?? ''
    ).split(' ')

    for (const { name, value } of newElt.attributes) {
      if (
        oldElt.getAttribute(name) !== value &&
        !preserveAttrs.includes(name)
      ) {
        oldElt.setAttribute(name, value)
      }
    }

    for (let i = oldElt.attributes.length - 1; i >= 0; i--) {
      const { name } = oldElt.attributes[i]!
      if (!newElt.hasAttribute(name) && !preserveAttrs.includes(name)) {
        oldElt.removeAttribute(name)
      }
    }

    // Preserve the scope marker even if the incoming markup doesn't carry it.
    if (shouldScopeChildren && !oldElt.hasAttribute('data-scope-children')) {
      oldElt.setAttribute('data-scope-children', '')
    }

    if (!oldElt.isEqualNode(newElt)) {
      morphChildren(oldElt, newElt)
    }

    if (shouldScopeChildren) {
      oldElt.dispatchEvent(
        new CustomEvent('datastar:scope-children', { bubbles: false }),
      )
    }
  }

  if (type === 8 /* comment */ || type === 3 /* text */) {
    if (oldNode.nodeValue !== newNode.nodeValue) {
      oldNode.nodeValue = newNode.nodeValue
    }
  }

  return oldNode
}

// A bottom-up algorithm that populates a map of Element -> IdSet.
// The ID set for a given element is the set of all IDs contained within its subtree.
// As an optimization, we filter these IDs through the given list of persistent IDs,
// because we don't need to bother considering IDed elements that won't be in the new content.
const populateIdMapWithTree = (
  root: Element | ShadowRoot | null,
  elements: Iterable<Element>,
): void => {
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
