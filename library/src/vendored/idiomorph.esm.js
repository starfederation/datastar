/**
 * @typedef {object} ConfigHead
 *
 * @property {'merge' | 'append' | 'morph' | 'none'} [style]
 * @property {boolean} [block]
 * @property {boolean} [ignore]
 * @property {function(Element): boolean} [shouldPreserve]
 * @property {function(Element): boolean} [shouldReAppend]
 * @property {function(Element): boolean} [shouldRemove]
 * @property {function(Element, {added: Node[], kept: Element[], removed: Element[]}): void} [afterHeadMorphed]
 */

/**
 * @typedef {object} ConfigCallbacks
 *
 * @property {function(Node): boolean} [beforeNodeAdded]
 * @property {function(Node): void} [afterNodeAdded]
 * @property {function(Element, Node): boolean} [beforeNodeMorphed]
 * @property {function(Element, Node): void} [afterNodeMorphed]
 * @property {function(Element): boolean} [beforeNodeRemoved]
 * @property {function(Element): void} [afterNodeRemoved]
 * @property {function(string, Element, "update" | "remove"): boolean} [beforeAttributeUpdated]
 * @property {function(Element): boolean} [beforeNodePantried]
 */

/**
 * @typedef {object} Config
 *
 * @property {'outerHTML' | 'innerHTML'} [morphStyle]
 * @property {boolean} [ignoreActive]
 * @property {boolean} [ignoreActiveValue]
 * @property {ConfigCallbacks} [callbacks]
 * @property {ConfigHead} [head]
 */

/**
 * @typedef {function} NoOp
 *
 * @returns {void}
 */

/**
 * @typedef {object} ConfigHeadInternal
 *
 * @property {'merge' | 'append' | 'morph' | 'none'} style
 * @property {boolean} [block]
 * @property {boolean} [ignore]
 * @property {(function(Element): boolean) | NoOp} shouldPreserve
 * @property {(function(Element): boolean) | NoOp} shouldReAppend
 * @property {(function(Element): boolean) | NoOp} shouldRemove
 * @property {(function(Element, {added: Node[], kept: Element[], removed: Element[]}): void) | NoOp} afterHeadMorphed
 */

/**
 * @typedef {object} ConfigCallbacksInternal
 *
 * @property {(function(Node): boolean) | NoOp} beforeNodeAdded
 * @property {(function(Node): void) | NoOp} afterNodeAdded
 * @property {(function(Node, Node): boolean) | NoOp} beforeNodeMorphed
 * @property {(function(Node, Node): void) | NoOp} afterNodeMorphed
 * @property {(function(Node): boolean) | NoOp} beforeNodeRemoved
 * @property {(function(Node): void) | NoOp} afterNodeRemoved
 * @property {(function(string, Element, "update" | "remove"): boolean) | NoOp} beforeAttributeUpdated
 * @property {(function(Node): boolean) | NoOp} beforeNodePantried
 */

/**
 * @typedef {object} ConfigInternal
 *
 * @property {'outerHTML' | 'innerHTML'} morphStyle
 * @property {boolean} [ignoreActive]
 * @property {boolean} [ignoreActiveValue]
 * @property {ConfigCallbacksInternal} callbacks
 * @property {ConfigHeadInternal} head
 * @property {boolean} [twoPass]
 */

/**
 * @typedef {Function} Morph
 *
 * @param {Element | Document} oldNode
 * @param {Element | Node | HTMLCollection | Node[] | string | null} newContent
 * @param {Config} [config]
 * @returns {undefined | Node[]}
 */

// base IIFE to define idiomorph
/**
 *
 * @type {{defaults: ConfigInternal, morph: Morph}}
 */
var Idiomorph = (function () {
  "use strict";

  /**
   * @typedef {object} MorphContext
   *
   * @property {Node} target
   * @property {Node} newContent
   * @property {ConfigInternal} config
   * @property {ConfigInternal['morphStyle']} morphStyle
   * @property {ConfigInternal['ignoreActive']} ignoreActive
   * @property {ConfigInternal['ignoreActiveValue']} ignoreActiveValue
   * @property {Map<Node, Set<string>>} idMap
   * @property {Set<string>} persistentIds
   * @property {Set<string>} deadIds
   * @property {ConfigInternal['callbacks']} callbacks
   * @property {ConfigInternal['head']} head
   * @property {HTMLDivElement} pantry
   */

  //=============================================================================
  // AND NOW IT BEGINS...
  //=============================================================================

  /**
   *
   * @type {Set<string>}
   */
  let EMPTY_SET = new Set();

  /**
   * Default configuration values, updatable by users now
   * @type {ConfigInternal}
   */
  let defaults = {
    morphStyle: "outerHTML",
    callbacks: {
      beforeNodeAdded: noOp,
      afterNodeAdded: noOp,
      beforeNodeMorphed: noOp,
      afterNodeMorphed: noOp,
      beforeNodeRemoved: noOp,
      afterNodeRemoved: noOp,
      beforeAttributeUpdated: noOp,
      beforeNodePantried: noOp,
    },
    head: {
      style: "merge",
      shouldPreserve: function (elt) {
        return elt.getAttribute("im-preserve") === "true";
      },
      shouldReAppend: function (elt) {
        return elt.getAttribute("im-re-append") === "true";
      },
      shouldRemove: noOp,
      afterHeadMorphed: noOp,
    },
  };

  /**
   * =============================================================================
   * Core Morphing Algorithm - morph, morphNormalizedContent, morphOldNodeTo, morphChildren
   * =============================================================================
   *
   * @param {Element | Document} oldNode
   * @param {Element | Node | HTMLCollection | Node[] | string | null} newContent
   * @param {Config} [config]
   * @returns {undefined | Node[]}
   */
  function morph(oldNode, newContent, config = {}) {
    if (oldNode instanceof Document) {
      oldNode = oldNode.documentElement;
    }

    if (typeof newContent === "string") {
      newContent = parseContent(newContent);
    }

    let normalizedContent = normalizeContent(newContent);

    let ctx = createMorphContext(oldNode, normalizedContent, config);

    return morphNormalizedContent(oldNode, normalizedContent, ctx);
  }

  /**
   *
   * @param {Element} oldNode
   * @param {Element} normalizedNewContent
   * @param {MorphContext} ctx
   * @returns {undefined | Node[]}
   */
  function morphNormalizedContent(oldNode, normalizedNewContent, ctx) {
    if (ctx.head.block) {
      let oldHead = oldNode.querySelector("head");
      let newHead = normalizedNewContent.querySelector("head");
      if (oldHead && newHead) {
        let promises = handleHeadElement(newHead, oldHead, ctx);
        // when head promises resolve, call morph again, ignoring the head tag
        Promise.all(promises).then(function () {
          morphNormalizedContent(
            oldNode,
            normalizedNewContent,
            Object.assign(ctx, {
              head: {
                block: false,
                ignore: true,
              },
            }),
          );
        });
        return;
      }
    }

    if (ctx.morphStyle === "innerHTML") {
      // innerHTML, so we are only updating the children
      morphChildren(normalizedNewContent, oldNode, ctx);
      if (ctx.config.twoPass) {
        restoreFromPantry(oldNode, ctx);
      }
      return Array.from(oldNode.children);
    } else if (ctx.morphStyle === "outerHTML" || ctx.morphStyle == null) {
      // otherwise find the best element match in the new content, morph that, and merge its siblings
      // into either side of the best match
      let bestMatch = findBestNodeMatch(normalizedNewContent, oldNode, ctx);

      // stash the siblings that will need to be inserted on either side of the best match
      let previousSibling = bestMatch?.previousSibling ?? null;
      let nextSibling = bestMatch?.nextSibling ?? null;

      // morph it
      let morphedNode = morphOldNodeTo(oldNode, bestMatch, ctx);

      if (bestMatch) {
        // if there was a best match, merge the siblings in too and return the
        // whole bunch
        if (morphedNode) {
          const elements = insertSiblings(
            previousSibling,
            morphedNode,
            nextSibling,
          );
          if (ctx.config.twoPass) {
            restoreFromPantry(morphedNode.parentNode, ctx);
          }
          return elements;
        }
      } else {
        // otherwise nothing was added to the DOM
        return [];
      }
    } else {
      throw "Do not understand how to morph style " + ctx.morphStyle;
    }
  }

  /**
   * @param {Node} possibleActiveElement
   * @param {MorphContext} ctx
   * @returns {boolean}
   */
  // TODO: ignoreActive and ignoreActiveValue are marked as optional since they are not
  //   initialised in the default config object. As a result the && in the function body may
  //   return undefined instead of boolean. Either expand the type of the return value to
  //   include undefined or wrap the ctx.ignoreActiveValue into a Boolean()
  function ignoreValueOfActiveElement(possibleActiveElement, ctx) {
    return (
      !!ctx.ignoreActiveValue &&
      possibleActiveElement === document.activeElement &&
      possibleActiveElement !== document.body
    );
  }

  /**
   * @param {Node} oldNode root node to merge content into
   * @param {Node | null} newContent new content to merge
   * @param {MorphContext} ctx the merge context
   * @returns {Node | null} the element that ended up in the DOM
   */
  function morphOldNodeTo(oldNode, newContent, ctx) {
    if (ctx.ignoreActive && oldNode === document.activeElement) {
      // don't morph focused element
    } else if (newContent == null) {
      if (ctx.callbacks.beforeNodeRemoved(oldNode) === false) return oldNode;

      oldNode.parentNode?.removeChild(oldNode);
      ctx.callbacks.afterNodeRemoved(oldNode);
      return null;
    } else if (!isSoftMatch(oldNode, newContent)) {
      if (ctx.callbacks.beforeNodeRemoved(oldNode) === false) return oldNode;
      if (ctx.callbacks.beforeNodeAdded(newContent) === false) return oldNode;

      oldNode.parentNode?.replaceChild(newContent, oldNode);
      ctx.callbacks.afterNodeAdded(newContent);
      ctx.callbacks.afterNodeRemoved(oldNode);
      return newContent;
    } else {
      if (ctx.callbacks.beforeNodeMorphed(oldNode, newContent) === false)
        return oldNode;

      if (oldNode instanceof HTMLHeadElement && ctx.head.ignore) {
        // ignore the head element
      } else if (
        oldNode instanceof HTMLHeadElement &&
        ctx.head.style !== "morph"
      ) {
        // ok to cast: if newContent wasn't also a <head>, it would've got caught in the `!isSoftMatch` branch above
        handleHeadElement(
          /** @type {HTMLHeadElement} */ (newContent),
          oldNode,
          ctx,
        );
      } else {
        syncNodeFrom(newContent, oldNode, ctx);
        if (!ignoreValueOfActiveElement(oldNode, ctx)) {
          morphChildren(newContent, oldNode, ctx);
        }
      }
      ctx.callbacks.afterNodeMorphed(oldNode, newContent);
      return oldNode;
    }
    return null;
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
   * @param {Node} newParent the parent element of the new content
   * @param {Node} oldParent the old content that we are merging the new content into
   * @param {MorphContext} ctx the merge context
   * @returns {void}
   */
  function morphChildren(newParent, oldParent, ctx) {
    if (
      newParent instanceof HTMLTemplateElement &&
      oldParent instanceof HTMLTemplateElement
    ) {
      newParent = newParent.content;
      oldParent = oldParent.content;
    }

    /**
     *
     * @type {Node | null}
     */
    let nextNewChild = newParent.firstChild;
    /**
     *
     * @type {Node | null}
     */
    let insertionPoint = oldParent.firstChild;
    let newChild;

    // run through all the new content
    while (nextNewChild) {
      newChild = nextNewChild;
      nextNewChild = newChild.nextSibling;

      // if we are at the end of the exiting parent's children, just append
      if (insertionPoint == null) {
        // skip add callbacks when we're going to be restoring this from the pantry in the second pass
        if (
          ctx.config.twoPass &&
          ctx.persistentIds.has(/** @type {Element} */ (newChild).id)
        ) {
          oldParent.appendChild(newChild);
        } else {
          if (ctx.callbacks.beforeNodeAdded(newChild) === false) continue;
          oldParent.appendChild(newChild);
          ctx.callbacks.afterNodeAdded(newChild);
        }
        removeIdsFromConsideration(ctx, newChild);
        continue;
      }

      // if the current node has an id set match then morph
      if (isIdSetMatch(newChild, insertionPoint, ctx)) {
        morphOldNodeTo(insertionPoint, newChild, ctx);
        insertionPoint = insertionPoint.nextSibling;
        removeIdsFromConsideration(ctx, newChild);
        continue;
      }

      // otherwise search forward in the existing old children for an id set match
      let idSetMatch = findIdSetMatch(
        newParent,
        oldParent,
        newChild,
        insertionPoint,
        ctx,
      );

      // if we found a potential match, remove the nodes until that point and morph
      if (idSetMatch) {
        insertionPoint = removeNodesBetween(insertionPoint, idSetMatch, ctx);
        morphOldNodeTo(idSetMatch, newChild, ctx);
        removeIdsFromConsideration(ctx, newChild);
        continue;
      }

      // no id set match found, so scan forward for a soft match for the current node
      let softMatch = findSoftMatch(
        newParent,
        oldParent,
        newChild,
        insertionPoint,
        ctx,
      );

      // if we found a soft match for the current node, morph
      if (softMatch) {
        insertionPoint = removeNodesBetween(insertionPoint, softMatch, ctx);
        morphOldNodeTo(softMatch, newChild, ctx);
        removeIdsFromConsideration(ctx, newChild);
        continue;
      }

      // abandon all hope of morphing, just insert the new child before the insertion point
      // and move on

      // skip add callbacks when we're going to be restoring this from the pantry in the second pass
      if (
        ctx.config.twoPass &&
        ctx.persistentIds.has(/** @type {Element} */ (newChild).id)
      ) {
        oldParent.insertBefore(newChild, insertionPoint);
      } else {
        if (ctx.callbacks.beforeNodeAdded(newChild) === false) continue;
        oldParent.insertBefore(newChild, insertionPoint);
        ctx.callbacks.afterNodeAdded(newChild);
      }
      removeIdsFromConsideration(ctx, newChild);
    }

    // remove any remaining old nodes that didn't match up with new content
    while (insertionPoint !== null) {
      let tempNode = insertionPoint;
      insertionPoint = insertionPoint.nextSibling;
      removeNode(tempNode, ctx);
    }
  }

  //=============================================================================
  // Attribute Syncing Code
  //=============================================================================

  /**
   * @param {string} attr the attribute to be mutated
   * @param {Element} to the element that is going to be updated
   * @param {"update" | "remove"} updateType
   * @param {MorphContext} ctx the merge context
   * @returns {boolean} true if the attribute should be ignored, false otherwise
   */
  function ignoreAttribute(attr, to, updateType, ctx) {
    if (
      attr === "value" &&
      ctx.ignoreActiveValue &&
      to === document.activeElement
    ) {
      return true;
    }
    return ctx.callbacks.beforeAttributeUpdated(attr, to, updateType) === false;
  }

  /**
   * syncs a given node with another node, copying over all attributes and
   * inner element state from the 'from' node to the 'to' node
   *
   * @param {Node} from the element to copy attributes & state from
   * @param {Node} to the element to copy attributes & state to
   * @param {MorphContext} ctx the merge context
   */
  function syncNodeFrom(from, to, ctx) {
    let type = from.nodeType;

    // if is an element type, sync the attributes from the
    // new node into the new node
    if (type === 1 /* element type */) {
      const fromEl = /** @type {Element} */ (from);
      const toEl = /** @type {Element} */ (to);
      const fromAttributes = fromEl.attributes;
      const toAttributes = toEl.attributes;
      for (const fromAttribute of fromAttributes) {
        if (ignoreAttribute(fromAttribute.name, toEl, "update", ctx)) {
          continue;
        }
        if (toEl.getAttribute(fromAttribute.name) !== fromAttribute.value) {
          toEl.setAttribute(fromAttribute.name, fromAttribute.value);
        }
      }
      // iterate backwards to avoid skipping over items when a delete occurs
      for (let i = toAttributes.length - 1; 0 <= i; i--) {
        const toAttribute = toAttributes[i];

        // toAttributes is a live NamedNodeMap, so iteration+mutation is unsafe
        // e.g. custom element attribute callbacks can remove other attributes
        if (!toAttribute) continue;

        if (!fromEl.hasAttribute(toAttribute.name)) {
          if (ignoreAttribute(toAttribute.name, toEl, "remove", ctx)) {
            continue;
          }
          toEl.removeAttribute(toAttribute.name);
        }
      }
    }

    // sync text nodes
    if (type === 8 /* comment */ || type === 3 /* text */) {
      if (to.nodeValue !== from.nodeValue) {
        to.nodeValue = from.nodeValue;
      }
    }

    if (!ignoreValueOfActiveElement(to, ctx)) {
      // sync input values
      syncInputValue(from, to, ctx);
    }
  }

  /**
   * @param {Element} from element to sync the value from
   * @param {Element} to element to sync the value to
   * @param {string} attributeName the attribute name
   * @param {MorphContext} ctx the merge context
   */
  function syncBooleanAttribute(from, to, attributeName, ctx) {
    // TODO: prefer set/getAttribute here
    if (!(from instanceof Element && to instanceof Element)) return;
    // @ts-ignore this function is only used on boolean attrs that are reflected as dom properties
    const fromLiveValue = from[attributeName],
      toLiveValue = to[attributeName];
    if (fromLiveValue !== toLiveValue) {
      let ignoreUpdate = ignoreAttribute(attributeName, to, "update", ctx);
      if (!ignoreUpdate) {
        // update attribute's associated DOM property
        // @ts-ignore this function is only used on boolean attrs that are reflected as dom properties
        to[attributeName] = from[attributeName];
      }
      if (fromLiveValue) {
        if (!ignoreUpdate) {
          // TODO: do we really want this? tests say so but it feels wrong
          to.setAttribute(attributeName, fromLiveValue);
        }
      } else {
        if (!ignoreAttribute(attributeName, to, "remove", ctx)) {
          to.removeAttribute(attributeName);
        }
      }
    }
  }

  /**
   * NB: many bothans died to bring us information:
   *
   *  https://github.com/patrick-steele-idem/morphdom/blob/master/src/specialElHandlers.js
   *  https://github.com/choojs/nanomorph/blob/master/lib/morph.jsL113
   *
   * @param {Node} from the element to sync the input value from
   * @param {Node} to the element to sync the input value to
   * @param {MorphContext} ctx the merge context
   */
  function syncInputValue(from, to, ctx) {
    if (
      from instanceof HTMLInputElement &&
      to instanceof HTMLInputElement &&
      from.type !== "file"
    ) {
      let fromValue = from.value;
      let toValue = to.value;

      // sync boolean attributes
      syncBooleanAttribute(from, to, "checked", ctx);
      syncBooleanAttribute(from, to, "disabled", ctx);

      if (!from.hasAttribute("value")) {
        if (!ignoreAttribute("value", to, "remove", ctx)) {
          to.value = "";
          to.removeAttribute("value");
        }
      } else if (fromValue !== toValue) {
        if (!ignoreAttribute("value", to, "update", ctx)) {
          to.setAttribute("value", fromValue);
          to.value = fromValue;
        }
      }
      // TODO: QUESTION(1cg): this used to only check `from` unlike the other branches -- why?
      // did I break something?
    } else if (
      from instanceof HTMLOptionElement &&
      to instanceof HTMLOptionElement
    ) {
      syncBooleanAttribute(from, to, "selected", ctx);
    } else if (
      from instanceof HTMLTextAreaElement &&
      to instanceof HTMLTextAreaElement
    ) {
      let fromValue = from.value;
      let toValue = to.value;
      if (ignoreAttribute("value", to, "update", ctx)) {
        return;
      }
      if (fromValue !== toValue) {
        to.value = fromValue;
      }
      if (to.firstChild && to.firstChild.nodeValue !== fromValue) {
        to.firstChild.nodeValue = fromValue;
      }
    }
  }

  /**
   * =============================================================================
   *  The HEAD tag can be handled specially, either w/ a 'merge' or 'append' style
   * =============================================================================
   * @param {Element} newHeadTag
   * @param {Element} currentHead
   * @param {MorphContext} ctx
   * @returns {Promise<void>[]}
   */
  function handleHeadElement(newHeadTag, currentHead, ctx) {
    /**
     * @type {Node[]}
     */
    let added = [];
    /**
     * @type {Element[]}
     */
    let removed = [];
    /**
     * @type {Element[]}
     */
    let preserved = [];
    /**
     * @type {Element[]}
     */
    let nodesToAppend = [];

    let headMergeStyle = ctx.head.style;

    // put all new head elements into a Map, by their outerHTML
    let srcToNewHeadNodes = new Map();
    for (const newHeadChild of newHeadTag.children) {
      srcToNewHeadNodes.set(newHeadChild.outerHTML, newHeadChild);
    }

    // for each elt in the current head
    for (const currentHeadElt of currentHead.children) {
      // If the current head element is in the map
      let inNewContent = srcToNewHeadNodes.has(currentHeadElt.outerHTML);
      let isReAppended = ctx.head.shouldReAppend(currentHeadElt);
      let isPreserved = ctx.head.shouldPreserve(currentHeadElt);
      if (inNewContent || isPreserved) {
        if (isReAppended) {
          // remove the current version and let the new version replace it and re-execute
          removed.push(currentHeadElt);
        } else {
          // this element already exists and should not be re-appended, so remove it from
          // the new content map, preserving it in the DOM
          srcToNewHeadNodes.delete(currentHeadElt.outerHTML);
          preserved.push(currentHeadElt);
        }
      } else {
        if (headMergeStyle === "append") {
          // we are appending and this existing element is not new content
          // so if and only if it is marked for re-append do we do anything
          if (isReAppended) {
            removed.push(currentHeadElt);
            nodesToAppend.push(currentHeadElt);
          }
        } else {
          // if this is a merge, we remove this content since it is not in the new head
          if (ctx.head.shouldRemove(currentHeadElt) !== false) {
            removed.push(currentHeadElt);
          }
        }
      }
    }

    // Push the remaining new head elements in the Map into the
    // nodes to append to the head tag
    nodesToAppend.push(...srcToNewHeadNodes.values());
    log("to append: ", nodesToAppend);

    let promises = [];
    for (const newNode of nodesToAppend) {
      log("adding: ", newNode);
      // TODO: This could theoretically be null, based on type
      let newElt = /** @type {ChildNode} */ (
        document.createRange().createContextualFragment(newNode.outerHTML)
          .firstChild
      );
      log(newElt);
      if (ctx.callbacks.beforeNodeAdded(newElt) !== false) {
        if (
          ("href" in newElt && newElt.href) ||
          ("src" in newElt && newElt.src)
        ) {
          /** @type {(result?: any) => void} */ let resolve;
          let promise = new Promise(function (_resolve) {
            resolve = _resolve;
          });
          newElt.addEventListener("load", function () {
            resolve();
          });
          promises.push(promise);
        }
        currentHead.appendChild(newElt);
        ctx.callbacks.afterNodeAdded(newElt);
        added.push(newElt);
      }
    }

    // remove all removed elements, after we have appended the new elements to avoid
    // additional network requests for things like style sheets
    for (const removedElement of removed) {
      if (ctx.callbacks.beforeNodeRemoved(removedElement) !== false) {
        currentHead.removeChild(removedElement);
        ctx.callbacks.afterNodeRemoved(removedElement);
      }
    }

    ctx.head.afterHeadMorphed(currentHead, {
      added: added,
      kept: preserved,
      removed: removed,
    });
    return promises;
  }

  //=============================================================================
  // Misc
  //=============================================================================

  /**
   * @param {any[]} _args
   */
  function log(..._args) {
    //console.log(args);
  }

  function noOp() {}

  /**
   * Deep merges the config object and the Idiomoroph.defaults object to
   * produce a final configuration object
   * @param {Config} config
   * @returns {ConfigInternal}
   */
  function mergeDefaults(config) {
    /**
     * @type {ConfigInternal}
     */
    let finalConfig = Object.assign({}, defaults);

    // copy top level stuff into final config
    Object.assign(finalConfig, config);

    // copy callbacks into final config (do this to deep merge the callbacks)
    finalConfig.callbacks = Object.assign(
      {},
      defaults.callbacks,
      config.callbacks,
    );

    // copy head config into final config  (do this to deep merge the head)
    finalConfig.head = Object.assign({}, defaults.head, config.head);

    return finalConfig;
  }

  /**
   *
   * @param {Element} oldNode
   * @param {Element} newContent
   * @param {Config} config
   * @returns {MorphContext}
   */
  function createMorphContext(oldNode, newContent, config) {
    const mergedConfig = mergeDefaults(config);
    return {
      target: oldNode,
      newContent: newContent,
      config: mergedConfig,
      morphStyle: mergedConfig.morphStyle,
      ignoreActive: mergedConfig.ignoreActive,
      ignoreActiveValue: mergedConfig.ignoreActiveValue,
      idMap: createIdMap(oldNode, newContent),
      deadIds: new Set(),
      persistentIds: mergedConfig.twoPass
        ? createPersistentIds(oldNode, newContent)
        : new Set(),
      pantry: mergedConfig.twoPass
        ? createPantry()
        : document.createElement("div"),
      callbacks: mergedConfig.callbacks,
      head: mergedConfig.head,
    };
  }

  function createPantry() {
    const pantry = document.createElement("div");
    pantry.hidden = true;
    document.body.insertAdjacentElement("afterend", pantry);
    return pantry;
  }

  /**
   *
   * @param {Node | null} node1
   * @param {Node | null} node2
   * @param {MorphContext} ctx
   * @returns {boolean}
   */
  // TODO: The function handles this as if it's Element or null, but the function is called in
  //   places where the arguments may be just a Node, not an Element
  function isIdSetMatch(node1, node2, ctx) {
    if (node1 == null || node2 == null) {
      return false;
    }
    if (
      node1 instanceof Element &&
      node2 instanceof Element &&
      node1.tagName === node2.tagName
    ) {
      if (node1.id !== "" && node1.id === node2.id) {
        return true;
      } else {
        return getIdIntersectionCount(ctx, node1, node2) > 0;
      }
    }
    return false;
  }

  /**
   *
   * @param {Node | null} oldNode
   * @param {Node | null} newNode
   * @returns {boolean}
   */
  function isSoftMatch(oldNode, newNode) {
    if (oldNode == null || newNode == null) {
      return false;
    }
    // ok to cast: if one is not element, `id` or `tagName` will be undefined and we'll compare that
    // If oldNode has an `id` with possible state and it doesn't match newNode.id then avoid morphing
    if (
      /** @type {Element} */ (oldNode).id &&
      /** @type {Element} */ (oldNode).id !==
        /** @type {Element} */ (newNode).id
    ) {
      return false;
    }
    return (
      oldNode.nodeType === newNode.nodeType &&
      /** @type {Element} */ (oldNode).tagName ===
        /** @type {Element} */ (newNode).tagName
    );
  }

  /**
   *
   * @param {Node} startInclusive
   * @param {Node} endExclusive
   * @param {MorphContext} ctx
   * @returns {Node | null}
   */
  function removeNodesBetween(startInclusive, endExclusive, ctx) {
    /** @type {Node | null} */ let cursor = startInclusive;
    while (cursor !== endExclusive) {
      let tempNode = /** @type {Node} */ (cursor);
      // TODO: Prefer assigning to a new variable here or expand the type of startInclusive
      //  to be Node | null
      cursor = tempNode.nextSibling;
      removeNode(tempNode, ctx);
    }
    removeIdsFromConsideration(ctx, endExclusive);
    return endExclusive.nextSibling;
  }

  /**
   * =============================================================================
   *  Scans forward from the insertionPoint in the old parent looking for a potential id match
   *  for the newChild.  We stop if we find a potential id match for the new child OR
   *  if the number of potential id matches we are discarding is greater than the
   *  potential id matches for the new child
   * =============================================================================
   * @param {Node} newContent
   * @param {Node} oldParent
   * @param {Node} newChild
   * @param {Node} insertionPoint
   * @param {MorphContext} ctx
   * @returns {null | Node}
   */
  function findIdSetMatch(
    newContent,
    oldParent,
    newChild,
    insertionPoint,
    ctx,
  ) {
    // max id matches we are willing to discard in our search
    let newChildPotentialIdCount = getIdIntersectionCount(
      ctx,
      newChild,
      oldParent,
    );

    /**
     * @type {Node | null}
     */
    let potentialMatch = null;

    // only search forward if there is a possibility of an id match
    if (newChildPotentialIdCount > 0) {
      // TODO: This is ghosting the potentialMatch variable outside of this block.
      //   Probably an error
      potentialMatch = insertionPoint;
      // if there is a possibility of an id match, scan forward
      // keep track of the potential id match count we are discarding (the
      // newChildPotentialIdCount must be greater than this to make it likely
      // worth it)
      let otherMatchCount = 0;
      while (potentialMatch != null) {
        // If we have an id match, return the current potential match
        if (isIdSetMatch(newChild, potentialMatch, ctx)) {
          return potentialMatch;
        }

        // computer the other potential matches of this new content
        otherMatchCount += getIdIntersectionCount(
          ctx,
          potentialMatch,
          newContent,
        );
        if (otherMatchCount > newChildPotentialIdCount) {
          // if we have more potential id matches in _other_ content, we
          // do not have a good candidate for an id match, so return null
          return null;
        }

        // advanced to the next old content child
        potentialMatch = potentialMatch.nextSibling;
      }
    }
    return potentialMatch;
  }

  /**
   * =============================================================================
   *  Scans forward from the insertionPoint in the old parent looking for a potential soft match
   *  for the newChild.  We stop if we find a potential soft match for the new child OR
   *  if we find a potential id match in the old parents children OR if we find two
   *  potential soft matches for the next two pieces of new content
   * =============================================================================
   * @param {Node} newContent
   * @param {Node} oldParent
   * @param {Node} newChild
   * @param {Node} insertionPoint
   * @param {MorphContext} ctx
   * @returns {null | Node}
   */
  function findSoftMatch(newContent, oldParent, newChild, insertionPoint, ctx) {
    /**
     * @type {Node | null}
     */
    let potentialSoftMatch = insertionPoint;
    /**
     * @type {Node | null}
     */
    let nextSibling = newChild.nextSibling;
    let siblingSoftMatchCount = 0;

    while (potentialSoftMatch != null) {
      if (getIdIntersectionCount(ctx, potentialSoftMatch, newContent) > 0) {
        // the current potential soft match has a potential id set match with the remaining new
        // content so bail out of looking
        return null;
      }

      // if we have a soft match with the current node, return it
      if (isSoftMatch(potentialSoftMatch, newChild)) {
        return potentialSoftMatch;
      }

      if (isSoftMatch(potentialSoftMatch, nextSibling)) {
        // the next new node has a soft match with this node, so
        // increment the count of future soft matches
        siblingSoftMatchCount++;
        // ok to cast: if it was null it couldn't be a soft match
        nextSibling = /** @type {Node} */ (nextSibling).nextSibling;

        // If there are two future soft matches, bail to allow the siblings to soft match
        // so that we don't consume future soft matches for the sake of the current node
        if (siblingSoftMatchCount >= 2) {
          return null;
        }
      }

      // advanced to the next old content child
      potentialSoftMatch = potentialSoftMatch.nextSibling;
    }

    return potentialSoftMatch;
  }

  /** @type {WeakSet<Node>} */
  const generatedByIdiomorph = new WeakSet();

  /**
   *
   * @param {string} newContent
   * @returns {Node | null | DocumentFragment}
   */
  function parseContent(newContent) {
    let parser = new DOMParser();

    // remove svgs to avoid false-positive matches on head, etc.
    let contentWithSvgsRemoved = newContent.replace(
      /<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,
      "",
    );

    // if the newContent contains a html, head or body tag, we can simply parse it w/o wrapping
    if (
      contentWithSvgsRemoved.match(/<\/html>/) ||
      contentWithSvgsRemoved.match(/<\/head>/) ||
      contentWithSvgsRemoved.match(/<\/body>/)
    ) {
      let content = parser.parseFromString(newContent, "text/html");
      // if it is a full HTML document, return the document itself as the parent container
      if (contentWithSvgsRemoved.match(/<\/html>/)) {
        generatedByIdiomorph.add(content);
        return content;
      } else {
        // otherwise return the html element as the parent container
        let htmlElement = content.firstChild;
        if (htmlElement) {
          generatedByIdiomorph.add(htmlElement);
          return htmlElement;
        } else {
          return null;
        }
      }
    } else {
      // if it is partial HTML, wrap it in a template tag to provide a parent element and also to help
      // deal with touchy tags like tr, tbody, etc.
      let responseDoc = parser.parseFromString(
        "<body><template>" + newContent + "</template></body>",
        "text/html",
      );
      let content = /** @type {HTMLTemplateElement} */ (
        responseDoc.body.querySelector("template")
      ).content;
      generatedByIdiomorph.add(content);
      return content;
    }
  }

  /**
   *
   * @param {null | Node | HTMLCollection | Node[] | Document & {generatedByIdiomorph:boolean}} newContent
   * @returns {Element}
   */
  function normalizeContent(newContent) {
    if (newContent == null) {
      // noinspection UnnecessaryLocalVariableJS
      const dummyParent = document.createElement("div");
      return dummyParent;
    } else if (generatedByIdiomorph.has(/** @type {Element} */ (newContent))) {
      // the template tag created by idiomorph parsing can serve as a dummy parent
      return /** @type {Element} */ (newContent);
    } else if (newContent instanceof Node) {
      // a single node is added as a child to a dummy parent
      const dummyParent = document.createElement("div");
      dummyParent.append(newContent);
      return dummyParent;
    } else {
      // all nodes in the array or HTMLElement collection are consolidated under
      // a single dummy parent element
      const dummyParent = document.createElement("div");
      for (const elt of [...newContent]) {
        dummyParent.append(elt);
      }
      return dummyParent;
    }
  }

  /**
   *
   * @param {Node | null} previousSibling
   * @param {Node} morphedNode
   * @param {Node | null} nextSibling
   * @returns {Node[]}
   */
  function insertSiblings(previousSibling, morphedNode, nextSibling) {
    /**
     * @type {Node[]}
     */
    let stack = [];
    /**
     * @type {Node[]}
     */
    let added = [];
    while (previousSibling != null) {
      stack.push(previousSibling);
      previousSibling = previousSibling.previousSibling;
    }
    // Base the loop on the node variable, so that you do not need runtime checks for
    // undefined value inside the loop
    let node = stack.pop();
    while (node !== undefined) {
      added.push(node); // push added preceding siblings on in order and insert
      morphedNode.parentElement?.insertBefore(node, morphedNode);
      node = stack.pop();
    }
    added.push(morphedNode);
    while (nextSibling != null) {
      stack.push(nextSibling);
      added.push(nextSibling); // here we are going in order, so push on as we scan, rather than add
      nextSibling = nextSibling.nextSibling;
    }
    while (stack.length > 0) {
      const node = /** @type {Node} */ (stack.pop());
      morphedNode.parentElement?.insertBefore(node, morphedNode.nextSibling);
    }
    return added;
  }

  /**
   *
   * @param {Element} newContent
   * @param {Element} oldNode
   * @param {MorphContext} ctx
   * @returns {Node | null}
   */
  function findBestNodeMatch(newContent, oldNode, ctx) {
    /**
     * @type {Node | null}
     */
    let currentElement;
    currentElement = newContent.firstChild;
    /**
     * @type {Node | null}
     */
    let bestElement = currentElement;
    let score = 0;
    while (currentElement) {
      let newScore = scoreElement(currentElement, oldNode, ctx);
      if (newScore > score) {
        bestElement = currentElement;
        score = newScore;
      }
      currentElement = currentElement.nextSibling;
    }
    return bestElement;
  }

  /**
   *
   * @param {Node | null} node1
   * @param {Element} node2
   * @param {MorphContext} ctx
   * @returns {number}
   */
  // TODO: The function handles node1 and node2 as if they are Elements but the function is
  //   called in places where node1 and node2 may be just Nodes, not Elements
  function scoreElement(node1, node2, ctx) {
    if (isSoftMatch(node2, node1)) {
      // ok to cast: isSoftMatch performs a null check
      return (
        0.5 + getIdIntersectionCount(ctx, /** @type {Node} */ (node1), node2)
      );
    }
    return 0;
  }

  /**
   *
   * @param {Node} tempNode
   * @param {MorphContext} ctx
   */
  // TODO: The function handles tempNode as if it's Element but the function is called in
  //   places where tempNode may be just a Node, not an Element
  function removeNode(tempNode, ctx) {
    removeIdsFromConsideration(ctx, tempNode);
    // skip remove callbacks when we're going to be restoring this from the pantry in the second pass
    if (
      ctx.config.twoPass &&
      hasPersistentIdNodes(ctx, tempNode) &&
      tempNode instanceof Element
    ) {
      moveToPantry(tempNode, ctx);
    } else {
      if (ctx.callbacks.beforeNodeRemoved(tempNode) === false) return;
      tempNode.parentNode?.removeChild(tempNode);
      ctx.callbacks.afterNodeRemoved(tempNode);
    }
  }

  /**
   *
   * @param {Node} node
   * @param {MorphContext} ctx
   */
  function moveToPantry(node, ctx) {
    if (ctx.callbacks.beforeNodePantried(node) === false) return;

    Array.from(node.childNodes).forEach((child) => {
      moveToPantry(child, ctx);
    });

    // After processing children, process the current node
    if (ctx.persistentIds.has(/** @type {Element} */ (node).id)) {
      // @ts-ignore - use proposed moveBefore feature
      if (ctx.pantry.moveBefore) {
        // @ts-ignore - use proposed moveBefore feature
        ctx.pantry.moveBefore(node, null);
      } else {
        ctx.pantry.insertBefore(node, null);
      }
    } else {
      if (ctx.callbacks.beforeNodeRemoved(node) === false) return;
      node.parentNode?.removeChild(node);
      ctx.callbacks.afterNodeRemoved(node);
    }
  }

  /**
   *
   * @param {Node | null} root
   * @param {MorphContext} ctx
   */
  function restoreFromPantry(root, ctx) {
    if (root instanceof Element) {
      Array.from(ctx.pantry.children)
        .reverse()
        .forEach((element) => {
          const matchElement = root.querySelector(`#${element.id}`);
          if (matchElement) {
            // @ts-ignore - use proposed moveBefore feature
            if (matchElement.parentElement?.moveBefore) {
              // @ts-ignore - use proposed moveBefore feature
              matchElement.parentElement.moveBefore(element, matchElement);
              while (matchElement.hasChildNodes()) {
                // @ts-ignore - use proposed moveBefore feature
                element.moveBefore(matchElement.firstChild, null);
              }
            } else {
              matchElement.before(element);
              while (matchElement.firstChild) {
                element.insertBefore(matchElement.firstChild, null);
              }
            }
            if (
              ctx.callbacks.beforeNodeMorphed(element, matchElement) !== false
            ) {
              syncNodeFrom(matchElement, element, ctx);
              ctx.callbacks.afterNodeMorphed(element, matchElement);
            }
            matchElement.remove();
          }
        });
      ctx.pantry.remove();
    }
  }

  //=============================================================================
  // ID Set Functions
  //=============================================================================

  /**
   *
   * @param {MorphContext} ctx
   * @param {string} id
   * @returns {boolean}
   */
  function isIdInConsideration(ctx, id) {
    return !ctx.deadIds.has(id);
  }

  /**
   *
   * @param {MorphContext} ctx
   * @param {string} id
   * @param {Node} targetNode
   * @returns {boolean}
   */
  function idIsWithinNode(ctx, id, targetNode) {
    let idSet = ctx.idMap.get(targetNode) || EMPTY_SET;
    return idSet.has(id);
  }

  /**
   *
   * @param {MorphContext} ctx
   * @param {Node} node
   * @returns {void}
   */
  function removeIdsFromConsideration(ctx, node) {
    let idSet = ctx.idMap.get(node) || EMPTY_SET;
    for (const id of idSet) {
      ctx.deadIds.add(id);
    }
  }

  /**
   *
   * @param {MorphContext} ctx
   * @param {Node} node
   * @returns {boolean}
   */
  function hasPersistentIdNodes(ctx, node) {
    for (const id of ctx.idMap.get(node) || EMPTY_SET) {
      if (ctx.persistentIds.has(id)) {
        return true;
      }
    }
    return false;
  }

  /**
   *
   * @param {MorphContext} ctx
   * @param {Node} node1
   * @param {Node} node2
   * @returns {number}
   */
  function getIdIntersectionCount(ctx, node1, node2) {
    let sourceSet = ctx.idMap.get(node1) || EMPTY_SET;
    let matchCount = 0;
    for (const id of sourceSet) {
      // a potential match is an id in the source and potentialIdsSet, but
      // that has not already been merged into the DOM
      if (isIdInConsideration(ctx, id) && idIsWithinNode(ctx, id, node2)) {
        ++matchCount;
      }
    }
    return matchCount;
  }

  /**
   * @param {Element} content
   * @returns {Element[]}
   */
  function nodesWithIds(content) {
    let nodes = Array.from(content.querySelectorAll("[id]"));
    if (content.id) {
      nodes.push(content);
    }
    return nodes;
  }

  /**
   * A bottom up algorithm that finds all elements with ids in the node
   * argument and populates id sets for those nodes and all their parents, generating
   * a set of ids contained within all nodes for the entire hierarchy in the DOM
   *
   * @param {Element} node
   * @param {Map<Node, Set<string>>} idMap
   */
  function populateIdMapForNode(node, idMap) {
    let nodeParent = node.parentElement;
    for (const elt of nodesWithIds(node)) {
      /**
       * @type {Element|null}
       */
      let current = elt;
      // walk up the parent hierarchy of that element, adding the id
      // of element to the parent's id set
      while (current !== nodeParent && current != null) {
        let idSet = idMap.get(current);
        // if the id set doesn't exist, create it and insert it in the  map
        if (idSet == null) {
          idSet = new Set();
          idMap.set(current, idSet);
        }
        idSet.add(elt.id);
        current = current.parentElement;
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
   * @returns {Map<Node, Set<string>>} a map of nodes to id sets for the
   */
  function createIdMap(oldContent, newContent) {
    /**
     *
     * @type {Map<Node, Set<string>>}
     */
    let idMap = new Map();
    populateIdMapForNode(oldContent, idMap);
    populateIdMapForNode(newContent, idMap);
    return idMap;
  }

  /**
   * @param {Element} oldContent  the old content that will be morphed
   * @param {Element} newContent  the new content to morph to
   * @returns {Set<string>} the id set of all persistent nodes that exist in both old and new content
   */
  function createPersistentIds(oldContent, newContent) {
    const toIdTagName = (node) => node.tagName + "#" + node.id;
    const oldIdSet = new Set(nodesWithIds(oldContent).map(toIdTagName));

    let matchIdSet = new Set();
    for (const newNode of nodesWithIds(newContent)) {
      if (oldIdSet.has(toIdTagName(newNode))) {
        matchIdSet.add(newNode.id);
      }
    }
    return matchIdSet;
  }

  //=============================================================================
  // This is what ends up becoming the Idiomorph global object
  //=============================================================================
  return {
    morph,
    defaults,
  };
})();

export {Idiomorph};
