export type ConfigHead = {
    style?: "merge" | "append" | "morph" | "none";
    block?: boolean;
    ignore?: boolean;
    shouldPreserve?: (arg0: Element) => boolean;
    shouldReAppend?: (arg0: Element) => boolean;
    shouldRemove?: (arg0: Element) => boolean;
    afterHeadMorphed?: (arg0: Element, arg1: {
        added: Node[];
        kept: Element[];
        removed: Element[];
    }) => void;
};
export type ConfigCallbacks = {
    beforeNodeAdded?: (arg0: Node) => boolean;
    afterNodeAdded?: (arg0: Node) => void;
    beforeNodeMorphed?: (arg0: Element, arg1: Node) => boolean;
    afterNodeMorphed?: (arg0: Element, arg1: Node) => void;
    beforeNodeRemoved?: (arg0: Element) => boolean;
    afterNodeRemoved?: (arg0: Element) => void;
    beforeAttributeUpdated?: (arg0: string, arg1: Element, arg2: "update" | "remove") => boolean;
    beforeNodePantried?: (arg0: Element) => boolean;
};
export type Config = {
    morphStyle?: "outerHTML" | "innerHTML";
    ignoreActive?: boolean;
    ignoreActiveValue?: boolean;
    callbacks?: ConfigCallbacks;
    head?: ConfigHead;
};
export type NoOp = Function;
export type ConfigHeadInternal = {
    style: "merge" | "append" | "morph" | "none";
    block?: boolean;
    ignore?: boolean;
    shouldPreserve: ((arg0: Element) => boolean) | NoOp;
    shouldReAppend: ((arg0: Element) => boolean) | NoOp;
    shouldRemove: ((arg0: Element) => boolean) | NoOp;
    afterHeadMorphed: ((arg0: Element, arg1: {
        added: Node[];
        kept: Element[];
        removed: Element[];
    }) => void) | NoOp;
};
export type ConfigCallbacksInternal = {
    beforeNodeAdded: ((arg0: Node) => boolean) | NoOp;
    afterNodeAdded: ((arg0: Node) => void) | NoOp;
    beforeNodeMorphed: ((arg0: Node, arg1: Node) => boolean) | NoOp;
    afterNodeMorphed: ((arg0: Node, arg1: Node) => void) | NoOp;
    beforeNodeRemoved: ((arg0: Node) => boolean) | NoOp;
    afterNodeRemoved: ((arg0: Node) => void) | NoOp;
    beforeAttributeUpdated: ((arg0: string, arg1: Element, arg2: "update" | "remove") => boolean) | NoOp;
    beforeNodePantried: ((arg0: Node) => boolean) | NoOp;
};
export type ConfigInternal = {
    morphStyle: "outerHTML" | "innerHTML";
    ignoreActive?: boolean;
    ignoreActiveValue?: boolean;
    callbacks: ConfigCallbacksInternal;
    head: ConfigHeadInternal;
    twoPass?: boolean;
};
export type Morph = Function;
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
/**
 *
 * @type {{defaults: ConfigInternal, morph: Morph}}
 */
export var Idiomorph: {
    defaults: ConfigInternal;
    morph: Morph;
};
