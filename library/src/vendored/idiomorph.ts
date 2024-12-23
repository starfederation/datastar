import { FragmentMergeModes } from '~/engine/consts'
import { dsErr } from '~/engine/errors'

const generatedByIdiomorphId = new WeakSet()

//=============================================================================
// Core Morphing Algorithm - morph, morphNormalizedContent, morphOldNodeTo, morphChildren
//=============================================================================
export function idiomorph(
  oldNode: Document | Element,
  newContent: string | Element,
  config = {},
) {
  if (oldNode instanceof Document) {
    oldNode = oldNode.documentElement
  }

  let parsedContent: Element
  if (typeof newContent === 'string') {
    parsedContent = parseContent(newContent) as Element
  } else {
    parsedContent = newContent
  }

  const normalizedContent = normalizeContent(parsedContent)
  const ctx = createMorphContext(oldNode, normalizedContent, config)

  return morphNormalizedContent(oldNode, normalizedContent, ctx)
}

function morphNormalizedContent(
  oldNode: Element,
  normalizedNewContent: Element,
  ctx: any,
) {
  if (ctx.head.block) {
    const oldHead = oldNode.querySelector('head')
    const newHead = normalizedNewContent.querySelector('head')
    if (oldHead && newHead) {
      const promises = handleHeadElement(newHead, oldHead, ctx)
      // when head promises resolve, call morph again, ignoring the head tag
      Promise.all(promises).then(() => {
        morphNormalizedContent(
          oldNode,
          normalizedNewContent,
          Object.assign(ctx, {
            head: {
              block: false,
              ignore: true,
            },
          }),
        )
      })
      return
    }
  }

  if (ctx.morphStyle === 'innerHTML') {
    // innerHTML, so we are only updating the children
    morphChildren(normalizedNewContent, oldNode, ctx)
    return oldNode.children
  }
  if (ctx.morphStyle === 'outerHTML' || ctx.morphStyle == null) {
    // otherwise find the best element match in the new content, morph that, and merge its siblings
    // into either side of the best match
    const bestMatch = findBestNodeMatch(normalizedNewContent, oldNode, ctx)
    if (!bestMatch) {
      throw dsErr('NoBestMatchFound', {
        old: oldNode,
        new: normalizedNewContent,
      })
    }

    // stash the siblings that will need to be inserted on either side of the best match
    const previousSibling = bestMatch?.previousSibling as Element
    const nextSibling = bestMatch?.nextSibling as Element

    // morph it
    const morphedNode = morphOldNodeTo(oldNode, bestMatch, ctx)

    if (bestMatch) {
      // if there was a best match, merge the siblings in too and return the
      // whole bunch
      return insertSiblings(previousSibling, morphedNode, nextSibling)
    }

    // otherwise nothing was added to the DOM
    return []
  }
  throw dsErr('InvalidMorphStyle', { style: ctx.morphStyle })
}

/**
 * @param oldNode root node to merge content into
 * @param newContent new content to merge
 * @param ctx the merge context
 * @returns {Element} the element that ended up in the DOM
 */
function morphOldNodeTo(oldNode: Element, newContent: Element, ctx: any) {
  if (ctx.ignoreActive && oldNode === document.activeElement) {
    // don't morph focused element
  } else if (newContent == null) {
    if (ctx.callbacks.beforeNodeRemoved(oldNode) === false) return

    oldNode.remove()
    ctx.callbacks.afterNodeRemoved(oldNode)
    return
  } else if (!isSoftMatch(oldNode, newContent)) {
    if (ctx.callbacks.beforeNodeRemoved(oldNode) === false) return
    if (ctx.callbacks.beforeNodeAdded(newContent) === false) return

    if (!oldNode.parentElement) {
      throw dsErr('NoParentElementFound', { oldNode })
    }
    oldNode.parentElement.replaceChild(newContent, oldNode)
    ctx.callbacks.afterNodeAdded(newContent)
    ctx.callbacks.afterNodeRemoved(oldNode)
    return newContent
  } else {
    if (ctx.callbacks.beforeNodeMorphed(oldNode, newContent) === false) {
      return
    }

    if (oldNode instanceof HTMLHeadElement && ctx.head.ignore) {
      // ignore the head element
    } else if (
      newContent instanceof HTMLHeadElement &&
      oldNode instanceof HTMLHeadElement &&
      ctx.head.style !== FragmentMergeModes.Morph
    ) {
      handleHeadElement(newContent, oldNode, ctx)
    } else {
      syncNodeFrom(newContent, oldNode)
      morphChildren(newContent, oldNode, ctx)
    }
    ctx.callbacks.afterNodeMorphed(oldNode, newContent)
    return oldNode
  }
}

/**
 * This is the core algorithm for matching up children.  The idea is to use id sets to try to match up
 * nodes as faithfully as possible.  We greedily match, which allows us to keep the algorithm fast, but
 * by using id sets, we are able to better match up with content deeper in the DOM.
 *
 * Basic algorithm is, for each node in the new content:
 *
 * - if we have reached the end of the old parent, append the new content
 * - if the new content has an id set match with the current insertion point, morph
 * - search for an id set match
 * - if id set match found, morph
 * - otherwise search for a "soft" match
 * - if a soft match is found, morph
 * - otherwise, prepend the new node before the current insertion point
 *
 * The two search algorithms terminate if competing node matches appear to outweigh what can be achieved
 * with the current node.  See findIdSetMatch() and findSoftMatch() for details.
 *
 * @param {Element} newParent the parent element of the new content
 * @param {Element } oldParent the old content that we are merging the new content into
 * @param ctx the merge context
 */
function morphChildren(newParent: Element, oldParent: Element, ctx: any) {
  let nextNewChild = newParent.firstChild as Element | null
  let insertionPoint = oldParent.firstChild as Element | null
  let newChild: any

  // run through all the new content
  while (nextNewChild) {
    newChild = nextNewChild
    nextNewChild = newChild.nextSibling as Element | null

    // if we are at the end of the exiting parent's children, just append
    if (insertionPoint == null) {
      if (ctx.callbacks.beforeNodeAdded(newChild) === false) return

      oldParent.appendChild(newChild)
      ctx.callbacks.afterNodeAdded(newChild)
      removeIdsFromConsideration(ctx, newChild)
      continue
    }

    // if the current node has an id set match then morph
    if (isIdSetMatch(newChild, insertionPoint, ctx)) {
      morphOldNodeTo(insertionPoint, newChild, ctx)
      insertionPoint = insertionPoint.nextSibling as Element | null
      removeIdsFromConsideration(ctx, newChild)
      continue
    }

    // otherwise search forward in the existing old children for an id set match
    const idSetMatch = findIdSetMatch(
      newParent,
      oldParent,
      newChild,
      insertionPoint,
      ctx,
    )

    // if we found a potential match, remove the nodes until that point and morph
    if (idSetMatch) {
      insertionPoint = removeNodesBetween(insertionPoint, idSetMatch, ctx)
      morphOldNodeTo(idSetMatch, newChild, ctx)
      removeIdsFromConsideration(ctx, newChild)
      continue
    }

    // no id set match found, so scan forward for a soft match for the current node
    const softMatch = findSoftMatch(newParent, newChild, insertionPoint, ctx)

    // if we found a soft match for the current node, morph
    if (softMatch) {
      insertionPoint = removeNodesBetween(insertionPoint, softMatch, ctx)
      morphOldNodeTo(softMatch, newChild, ctx)
      removeIdsFromConsideration(ctx, newChild)
      continue
    }

    // abandon all hope of morphing, just insert the new child before the insertion point
    // and move on
    if (ctx.callbacks.beforeNodeAdded(newChild) === false) return

    oldParent.insertBefore(newChild, insertionPoint)
    ctx.callbacks.afterNodeAdded(newChild)
    removeIdsFromConsideration(ctx, newChild)
  }

  // remove any remaining old nodes that didn't match up with new content
  while (insertionPoint !== null) {
    const tempNode = insertionPoint
    insertionPoint = insertionPoint.nextSibling as Element | null
    removeNode(tempNode, ctx)
  }
}

//=============================================================================
// Attribute Syncing Code
//=============================================================================

/**
 * syncs a given node with another node, copying over all attributes and
 * inner element state from the 'from' node to the 'to' node
 *
 * @param {Element} from the element to copy attributes & state from
 * @param {Element} to the element to copy attributes & state to
 */
function syncNodeFrom(from: Element, to: Element) {
  const type = from.nodeType

  // if is an element type, sync the attributes from the
  // new node into the new node
  if (type === 1 /* element type */) {
    for (const fromAttribute of from.attributes) {
      const toAttribute = to.getAttribute(fromAttribute.name)
      if (toAttribute !== fromAttribute.value) {
        to.setAttribute(fromAttribute.name, fromAttribute.value)
      }
    }
    for (const toAttribute of to.attributes) {
      if (!from.hasAttribute(toAttribute.name)) {
        to.removeAttribute(toAttribute.name)
      }
    }
  }

  // sync text nodes
  if (type === Node.COMMENT_NODE || type === Node.TEXT_NODE) {
    if (to.nodeValue !== from.nodeValue) {
      to.nodeValue = from.nodeValue
    }
  }

  // NB: many bothans died to bring us information:
  //
  // https://github.com/patrick-steele-idem/morphdom/blob/master/src/specialElHandlers.js
  // https://github.com/choojs/nanomorph/blob/master/lib/morph.jsL113

  // sync input value
  if (
    from instanceof HTMLInputElement &&
    to instanceof HTMLInputElement &&
    from.type !== 'file'
  ) {
    to.value = from.value || ''
    syncAttribute(from, to, 'value')

    // sync boolean attributes
    syncAttribute(from, to, 'checked')
    syncAttribute(from, to, 'disabled')
  } else if (from instanceof HTMLOptionElement) {
    syncAttribute(from, to, 'selected')
  } else if (
    from instanceof HTMLTextAreaElement &&
    to instanceof HTMLTextAreaElement
  ) {
    const fromValue = from.value
    const toValue = to.value
    if (fromValue !== toValue) {
      to.value = fromValue
    }
    if (to.firstChild && to.firstChild.nodeValue !== fromValue) {
      to.firstChild.nodeValue = fromValue
    }
  }
}

function syncAttribute(from: Element, to: Element, attributeName: string) {
  const fAttr = from.getAttribute(attributeName)
  const tAttr = to.getAttribute(attributeName)

  if (fAttr !== tAttr) {
    if (fAttr) {
      to.setAttribute(attributeName, fAttr)
    } else {
      to.removeAttribute(attributeName)
    }
  }
}

//=============================================================================
// the HEAD tag can be handled specially, either w/ a 'merge' or 'append' style
//=============================================================================
function handleHeadElement(
  newHeadTag: HTMLHeadElement,
  currentHead: HTMLHeadElement,
  ctx: any,
) {
  const added = []
  const removed = []
  const preserved = []
  const nodesToAppend = []

  const headMergeStyle = ctx.head.style

  // put all new head elements into a Map, by their outerHTML
  const srcToNewHeadNodes = new Map()
  for (const newHeadChild of newHeadTag.children) {
    srcToNewHeadNodes.set(newHeadChild.outerHTML, newHeadChild)
  }

  // for each elt in the current head
  for (const currentHeadElt of currentHead.children) {
    // If the current head element is in the map
    const inNewContent = srcToNewHeadNodes.has(currentHeadElt.outerHTML)
    const isReAppended = ctx.head.shouldReAppend(currentHeadElt)
    const isPreserved = ctx.head.shouldPreserve(currentHeadElt)
    if (inNewContent || isPreserved) {
      if (isReAppended) {
        // remove the current version and let the new version replace it and re-execute
        removed.push(currentHeadElt)
      } else {
        // this element already exists and should not be re-appended, so remove it from
        // the new content map, preserving it in the DOM
        srcToNewHeadNodes.delete(currentHeadElt.outerHTML)
        preserved.push(currentHeadElt)
      }
    } else {
      if (headMergeStyle === FragmentMergeModes.Append) {
        // we are appending and this existing element is not new content
        // so if and only if it is marked for re-append do we do anything
        if (isReAppended) {
          removed.push(currentHeadElt)
          nodesToAppend.push(currentHeadElt)
        }
      } else {
        // if this is a merge, we remove this content since it is not in the new head
        if (ctx.head.shouldRemove(currentHeadElt) !== false) {
          removed.push(currentHeadElt)
        }
      }
    }
  }

  // Push the remaining new head elements in the Map into the
  // nodes to append to the head tag
  nodesToAppend.push(...srcToNewHeadNodes.values())

  const promises = []
  for (const newNode of nodesToAppend) {
    const newElt = document
      .createRange()
      .createContextualFragment(newNode.outerHTML).firstChild as Element | null
    if (!newElt) {
      throw dsErr('NewElementCouldNotBeCreated', { newNode })
    }
    if (ctx.callbacks.beforeNodeAdded(newElt)) {
      if (newElt.hasAttribute('href') || newElt.hasAttribute('src')) {
        let resolver: (value: unknown) => void
        const promise = new Promise((resolve) => {
          resolver = resolve
        })
        newElt.addEventListener('load', () => {
          resolver(undefined)
        })
        promises.push(promise)
      }
      currentHead.appendChild(newElt)
      ctx.callbacks.afterNodeAdded(newElt)
      added.push(newElt)
    }
  }

  // remove all removed elements, after we have appended the new elements to avoid
  // additional network requests for things like style sheets
  for (const removedElement of removed) {
    if (ctx.callbacks.beforeNodeRemoved(removedElement) !== false) {
      currentHead.removeChild(removedElement)
      ctx.callbacks.afterNodeRemoved(removedElement)
    }
  }

  ctx.head.afterHeadMorphed(currentHead, {
    added: added,
    kept: preserved,
    removed: removed,
  })
  return promises
}

//=============================================================================
// Misc
//=============================================================================
function noOp() {}

function createMorphContext(
  oldNode: Element,
  newContent: Element,
  config: any,
) {
  return {
    target: oldNode,
    newContent: newContent,
    config: config,
    morphStyle: config.morphStyle,
    ignoreActive: config.ignoreActive,
    idMap: createIdMap(oldNode, newContent),
    deadIds: new Set(),
    callbacks: Object.assign(
      {
        beforeNodeAdded: noOp,
        afterNodeAdded: noOp,
        beforeNodeMorphed: noOp,
        afterNodeMorphed: noOp,
        beforeNodeRemoved: noOp,
        afterNodeRemoved: noOp,
      },
      config.callbacks,
    ),
    head: Object.assign(
      {
        style: 'merge',
        shouldPreserve: (elt: Element) =>
          elt.getAttribute('im-preserve') === 'true',
        shouldReAppend: (elt: Element) =>
          elt.getAttribute('im-re-append') === 'true',
        shouldRemove: noOp,
        afterHeadMorphed: noOp,
      },
      config.head,
    ),
  }
}

function isIdSetMatch(node1: Element, node2: Element, ctx: any) {
  if (!node1 || !node2) return false

  if (node1.nodeType === node2.nodeType && node1.tagName === node2.tagName) {
    if (node1?.id?.length && node1.id === node2.id) return true

    return getIdIntersectionCount(ctx, node1, node2) > 0
  }
  return false
}

function isSoftMatch(node1: Element, node2: Element) {
  if (!node1 || !node2) return false

  return node1.nodeType === node2.nodeType && node1.tagName === node2.tagName
}

function removeNodesBetween(
  startInclusive: Element,
  endExclusive: Element,
  ctx: any,
) {
  while (startInclusive !== endExclusive) {
    const tempNode = startInclusive
    startInclusive = startInclusive?.nextSibling as Element
    if (!tempNode) {
      throw dsErr('NoTemporaryNodeFound', { startInclusive, endExclusive })
    }
    removeNode(tempNode, ctx)
  }
  removeIdsFromConsideration(ctx, endExclusive)
  return endExclusive.nextSibling as Element | null
}

//=============================================================================
// Scans forward from the insertionPoint in the old parent looking for a potential id match
// for the newChild.  We stop if we find a potential id match for the new child OR
// if the number of potential id matches we are discarding is greater than the
// potential id matches for the new child
//=============================================================================
function findIdSetMatch(
  newContent: Element,
  oldParent: Element,
  newChild: Element,
  insertionPoint: Element,
  ctx: any,
) {
  // max id matches we are willing to discard in our search
  const newChildPotentialIdCount = getIdIntersectionCount(
    ctx,
    newChild,
    oldParent,
  )

  let potentialMatch: Element | null = null

  // only search forward if there is a possibility of an id match
  if (newChildPotentialIdCount > 0) {
    potentialMatch = insertionPoint
    // if there is a possibility of an id match, scan forward
    // keep track of the potential id match count we are discarding (the
    // newChildPotentialIdCount must be greater than this to make it likely
    // worth it)
    let otherMatchCount = 0
    while (potentialMatch != null) {
      // If we have an id match, return the current potential match
      if (isIdSetMatch(newChild, potentialMatch, ctx)) {
        return potentialMatch
      }

      // computer the other potential matches of this new content
      otherMatchCount += getIdIntersectionCount(ctx, potentialMatch, newContent)
      if (otherMatchCount > newChildPotentialIdCount) {
        // if we have more potential id matches in _other_ content, we
        // do not have a good candidate for an id match, so return null
        return null
      }

      // advanced to the next old content child
      potentialMatch = potentialMatch.nextSibling as Element | null
    }
  }
  return potentialMatch
}

//=============================================================================
// Scans forward from the insertionPoint in the old parent looking for a potential soft match
// for the newChild.  We stop if we find a potential soft match for the new child OR
// if we find a potential id match in the old parents children OR if we find two
// potential soft matches for the next two pieces of new content
//=============================================================================
function findSoftMatch(
  newContent: Element,
  newChild: Element,
  insertionPoint: Element,
  ctx: any,
) {
  let potentialSoftMatch = insertionPoint as Element | null
  let nextSibling = newChild.nextSibling as Element | null

  let siblingSoftMatchCount = 0

  while (potentialSoftMatch && nextSibling) {
    if (getIdIntersectionCount(ctx, potentialSoftMatch, newContent) > 0) {
      // the current potential soft match has a potential id set match with the remaining new
      // content so bail out of looking
      return null
    }

    // if we have a soft match with the current node, return it
    if (isSoftMatch(newChild, potentialSoftMatch)) {
      return potentialSoftMatch
    }

    if (isSoftMatch(nextSibling, potentialSoftMatch)) {
      // the next new node has a soft match with this node, so
      // increment the count of future soft matches
      siblingSoftMatchCount++
      nextSibling = nextSibling.nextSibling as Element | null

      // If there are two future soft matches, bail to allow the siblings to soft match
      // so that we don't consume future soft matches for the sake of the current node
      if (siblingSoftMatchCount >= 2) {
        return null
      }
    }

    // advanced to the next old content child
    potentialSoftMatch = potentialSoftMatch.nextSibling as Element | null
  }

  return potentialSoftMatch
}

const parser = new DOMParser()
function parseContent(newContent: string) {
  // remove svgs to avoid false-positive matches on head, etc.
  const contentWithSvgsRemoved = newContent.replace(
    /<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,
    '',
  )

  // if the newContent contains a html, head or body tag, we can simply parse it w/o wrapping
  if (
    contentWithSvgsRemoved.match(/<\/html>/) ||
    contentWithSvgsRemoved.match(/<\/head>/) ||
    contentWithSvgsRemoved.match(/<\/body>/)
  ) {
    const content = parser.parseFromString(newContent, 'text/html')
    // if it is a full HTML document, return the document itself as the parent container
    if (contentWithSvgsRemoved.match(/<\/html>/)) {
      generatedByIdiomorphId.add(content)
      return content
    }
    // otherwise return the html element as the parent container
    const Element = content.firstChild
    if (Element) {
      generatedByIdiomorphId.add(Element)
      return Element as Element
    }
    return null
  }
  // if it is partial HTML, wrap it in a template tag to provide a parent element and also to help
  // deal with touchy tags like tr, tbody, etc.
  const responseDoc = parser.parseFromString(
    `<body><template>${newContent}</template></body>`,
    'text/html',
  )
  const content = responseDoc.body.querySelector('template')?.content
  if (!content) {
    throw dsErr('NoContentFound', { newContent })
  }
  generatedByIdiomorphId.add(content)
  return content
}

function normalizeContent(newContent: Element) {
  if (newContent == null) {
    // noinspection UnnecessaryLocalVariableJS
    const dummyParent = document.createElement('div')
    return dummyParent
  }
  if (generatedByIdiomorphId.has(newContent)) {
    // the template tag created by idiomorph parsing can serve as a dummy parent
    return newContent
  }
  if (newContent instanceof Node) {
    // a single node is added as a child to a dummy parent
    const dummyParent = document.createElement('div')
    dummyParent.append(newContent)
    return dummyParent
  }
  // all nodes in the array or Element collection are consolidated under
  // a single dummy parent element
  const dummyParent = document.createElement('div')
  for (const elt of [...newContent]) {
    dummyParent.append(elt)
  }
  return dummyParent
}

function insertSiblings(
  previousSibling?: Element,
  morphedNode?: Element,
  nextSibling?: Element,
) {
  const stack = []
  const added = []
  while (previousSibling) {
    stack.push(previousSibling)
    previousSibling = previousSibling.previousSibling as Element
  }
  while (stack.length > 0) {
    const node = stack.pop()!
    added.push(node) // push added preceding siblings on in order and insert
    morphedNode?.parentElement?.insertBefore(node, morphedNode)
  }
  added.push(morphedNode)
  while (nextSibling) {
    stack.push(nextSibling)
    added.push(nextSibling) // here we are going in order, so push on as we scan, rather than add
    nextSibling = nextSibling.nextSibling as Element
  }
  while (stack.length) {
    morphedNode?.parentElement?.insertBefore(
      stack.pop()!,
      morphedNode.nextSibling,
    )
  }
  return added
}

function findBestNodeMatch(newContent: Element, oldNode: Element, ctx: any) {
  let currentElement = newContent.firstChild as Element | null
  let bestElement = currentElement
  let score = 0
  while (currentElement) {
    const newScore = scoreElement(currentElement, oldNode, ctx)
    if (newScore > score) {
      bestElement = currentElement
      score = newScore
    }
    currentElement = currentElement.nextSibling as Element | null
  }
  return bestElement
}

function scoreElement(node1: Element, node2: Element, ctx: any) {
  if (isSoftMatch(node1, node2)) {
    return 0.5 + getIdIntersectionCount(ctx, node1, node2)
  }
  return 0
}

function removeNode(tempNode: Element, ctx: any) {
  removeIdsFromConsideration(ctx, tempNode)
  if (ctx.callbacks.beforeNodeRemoved(tempNode) === false) return

  tempNode.remove()
  ctx.callbacks.afterNodeRemoved(tempNode)
}

//=============================================================================
// ID Set Functions
//=============================================================================

function isIdInConsideration(ctx: any, id: string) {
  return !ctx.deadIds.has(id)
}

function idIsWithinNode(ctx: any, id: string, targetNode: Element) {
  return ctx.idMap.get(targetNode)?.has(id) || false
}

function removeIdsFromConsideration(ctx: any, node: Element) {
  const idSet = ctx.idMap.get(node)
  if (!idSet) return
  for (const id of idSet) {
    ctx.deadIds.add(id)
  }
}

function getIdIntersectionCount(ctx: any, node1: Element, node2: Element) {
  const sourceSet = ctx.idMap.get(node1)
  if (!sourceSet) return 0

  let matchCount = 0
  for (const id of sourceSet) {
    // a potential match is an id in the source and potentialIdsSet, but
    // that has not already been merged into the DOM
    if (isIdInConsideration(ctx, id) && idIsWithinNode(ctx, id, node2)) {
      ++matchCount
    }
  }
  return matchCount
}

/**
 * A bottom up algorithm that finds all elements with ids inside of the node
 * argument and populates id sets for those nodes and all their parents, generating
 * a set of ids contained within all nodes for the entire hierarchy in the DOM
 *
 * @param node {Element}
 * @param {Map<Node, Set<String>>} idMap
 */
function populateIdMapForNode(node: Element, idMap: Map<Element, Set<string>>) {
  const nodeParent = node.parentElement
  // find all elements with an id property
  const idElements = node.querySelectorAll('[id]')
  for (const elt of idElements) {
    let current = elt as Element | null
    // walk up the parent hierarchy of that element, adding the id
    // of element to the parent's id set
    while (current !== nodeParent && !!current) {
      let idSet = idMap.get(current)
      // if the id set doesn't exist, create it and insert it in the  map
      if (idSet == null) {
        idSet = new Set()
        idMap.set(current, idSet)
      }
      idSet.add(elt.id)
      current = current.parentElement
    }
  }
}

/**
 * This function computes a map of nodes to all ids contained within that node (inclusive of the
 * node).  This map can be used to ask if two nodes have intersecting sets of ids, which allows
 * for a looser definition of "matching" than tradition id matching, and allows child nodes
 * to contribute to a parent nodes matching.
 *
 * @param {Element} oldContent  the old content that will be morphed
 * @param {Element} newContent  the new content to morph to
 * @returns {Map<Node, Set<String>>} a map of nodes to id sets for the
 */
function createIdMap(oldContent: Element, newContent: Element) {
  const idMap = new Map()
  populateIdMapForNode(oldContent, idMap)
  populateIdMapForNode(newContent, idMap)
  return idMap
}
