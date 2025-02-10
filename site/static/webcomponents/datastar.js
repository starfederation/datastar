/**
 * @license
 * Copyright 2019 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const I = globalThis, K = I.ShadowRoot && (I.ShadyCSS === void 0 || I.ShadyCSS.nativeShadow) && "adoptedStyleSheets" in Document.prototype && "replace" in CSSStyleSheet.prototype, pt = Symbol(), tt = /* @__PURE__ */ new WeakMap();
let yt = class {
  constructor(t, e, s) {
    if (this._$cssResult$ = !0, s !== pt) throw Error("CSSResult is not constructable. Use `unsafeCSS` or `css` instead.");
    this.cssText = t, this.t = e;
  }
  get styleSheet() {
    let t = this.o;
    const e = this.t;
    if (K && t === void 0) {
      const s = e !== void 0 && e.length === 1;
      s && (t = tt.get(e)), t === void 0 && ((this.o = t = new CSSStyleSheet()).replaceSync(this.cssText), s && tt.set(e, t));
    }
    return t;
  }
  toString() {
    return this.cssText;
  }
};
const mt = (i) => new yt(typeof i == "string" ? i : i + "", void 0, pt), Et = (i, t) => {
  if (K) i.adoptedStyleSheets = t.map((e) => e instanceof CSSStyleSheet ? e : e.styleSheet);
  else for (const e of t) {
    const s = document.createElement("style"), r = I.litNonce;
    r !== void 0 && s.setAttribute("nonce", r), s.textContent = e.cssText, i.appendChild(s);
  }
}, et = K ? (i) => i : (i) => i instanceof CSSStyleSheet ? ((t) => {
  let e = "";
  for (const s of t.cssRules) e += s.cssText;
  return mt(e);
})(i) : i;
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const { is: St, defineProperty: bt, getOwnPropertyDescriptor: wt, getOwnPropertyNames: Ct, getOwnPropertySymbols: Pt, getPrototypeOf: xt } = Object, f = globalThis, st = f.trustedTypes, Ut = st ? st.emptyScript : "", B = f.reactiveElementPolyfillSupport, x = (i, t) => i, L = { toAttribute(i, t) {
  switch (t) {
    case Boolean:
      i = i ? Ut : null;
      break;
    case Object:
    case Array:
      i = i == null ? i : JSON.stringify(i);
  }
  return i;
}, fromAttribute(i, t) {
  let e = i;
  switch (t) {
    case Boolean:
      e = i !== null;
      break;
    case Number:
      e = i === null ? null : Number(i);
      break;
    case Object:
    case Array:
      try {
        e = JSON.parse(i);
      } catch {
        e = null;
      }
  }
  return e;
} }, Z = (i, t) => !St(i, t), it = { attribute: !0, type: String, converter: L, reflect: !1, hasChanged: Z };
Symbol.metadata ?? (Symbol.metadata = Symbol("metadata")), f.litPropertyMetadata ?? (f.litPropertyMetadata = /* @__PURE__ */ new WeakMap());
class S extends HTMLElement {
  static addInitializer(t) {
    this._$Ei(), (this.l ?? (this.l = [])).push(t);
  }
  static get observedAttributes() {
    return this.finalize(), this._$Eh && [...this._$Eh.keys()];
  }
  static createProperty(t, e = it) {
    if (e.state && (e.attribute = !1), this._$Ei(), this.elementProperties.set(t, e), !e.noAccessor) {
      const s = Symbol(), r = this.getPropertyDescriptor(t, s, e);
      r !== void 0 && bt(this.prototype, t, r);
    }
  }
  static getPropertyDescriptor(t, e, s) {
    const { get: r, set: n } = wt(this.prototype, t) ?? { get() {
      return this[e];
    }, set(o) {
      this[e] = o;
    } };
    return { get() {
      return r == null ? void 0 : r.call(this);
    }, set(o) {
      const a = r == null ? void 0 : r.call(this);
      n.call(this, o), this.requestUpdate(t, a, s);
    }, configurable: !0, enumerable: !0 };
  }
  static getPropertyOptions(t) {
    return this.elementProperties.get(t) ?? it;
  }
  static _$Ei() {
    if (this.hasOwnProperty(x("elementProperties"))) return;
    const t = xt(this);
    t.finalize(), t.l !== void 0 && (this.l = [...t.l]), this.elementProperties = new Map(t.elementProperties);
  }
  static finalize() {
    if (this.hasOwnProperty(x("finalized"))) return;
    if (this.finalized = !0, this._$Ei(), this.hasOwnProperty(x("properties"))) {
      const e = this.properties, s = [...Ct(e), ...Pt(e)];
      for (const r of s) this.createProperty(r, e[r]);
    }
    const t = this[Symbol.metadata];
    if (t !== null) {
      const e = litPropertyMetadata.get(t);
      if (e !== void 0) for (const [s, r] of e) this.elementProperties.set(s, r);
    }
    this._$Eh = /* @__PURE__ */ new Map();
    for (const [e, s] of this.elementProperties) {
      const r = this._$Eu(e, s);
      r !== void 0 && this._$Eh.set(r, e);
    }
    this.elementStyles = this.finalizeStyles(this.styles);
  }
  static finalizeStyles(t) {
    const e = [];
    if (Array.isArray(t)) {
      const s = new Set(t.flat(1 / 0).reverse());
      for (const r of s) e.unshift(et(r));
    } else t !== void 0 && e.push(et(t));
    return e;
  }
  static _$Eu(t, e) {
    const s = e.attribute;
    return s === !1 ? void 0 : typeof s == "string" ? s : typeof t == "string" ? t.toLowerCase() : void 0;
  }
  constructor() {
    super(), this._$Ep = void 0, this.isUpdatePending = !1, this.hasUpdated = !1, this._$Em = null, this._$Ev();
  }
  _$Ev() {
    var t;
    this._$ES = new Promise((e) => this.enableUpdating = e), this._$AL = /* @__PURE__ */ new Map(), this._$E_(), this.requestUpdate(), (t = this.constructor.l) == null || t.forEach((e) => e(this));
  }
  addController(t) {
    var e;
    (this._$EO ?? (this._$EO = /* @__PURE__ */ new Set())).add(t), this.renderRoot !== void 0 && this.isConnected && ((e = t.hostConnected) == null || e.call(t));
  }
  removeController(t) {
    var e;
    (e = this._$EO) == null || e.delete(t);
  }
  _$E_() {
    const t = /* @__PURE__ */ new Map(), e = this.constructor.elementProperties;
    for (const s of e.keys()) this.hasOwnProperty(s) && (t.set(s, this[s]), delete this[s]);
    t.size > 0 && (this._$Ep = t);
  }
  createRenderRoot() {
    const t = this.shadowRoot ?? this.attachShadow(this.constructor.shadowRootOptions);
    return Et(t, this.constructor.elementStyles), t;
  }
  connectedCallback() {
    var t;
    this.renderRoot ?? (this.renderRoot = this.createRenderRoot()), this.enableUpdating(!0), (t = this._$EO) == null || t.forEach((e) => {
      var s;
      return (s = e.hostConnected) == null ? void 0 : s.call(e);
    });
  }
  enableUpdating(t) {
  }
  disconnectedCallback() {
    var t;
    (t = this._$EO) == null || t.forEach((e) => {
      var s;
      return (s = e.hostDisconnected) == null ? void 0 : s.call(e);
    });
  }
  attributeChangedCallback(t, e, s) {
    this._$AK(t, s);
  }
  _$EC(t, e) {
    var n;
    const s = this.constructor.elementProperties.get(t), r = this.constructor._$Eu(t, s);
    if (r !== void 0 && s.reflect === !0) {
      const o = (((n = s.converter) == null ? void 0 : n.toAttribute) !== void 0 ? s.converter : L).toAttribute(e, s.type);
      this._$Em = t, o == null ? this.removeAttribute(r) : this.setAttribute(r, o), this._$Em = null;
    }
  }
  _$AK(t, e) {
    var n;
    const s = this.constructor, r = s._$Eh.get(t);
    if (r !== void 0 && this._$Em !== r) {
      const o = s.getPropertyOptions(r), a = typeof o.converter == "function" ? { fromAttribute: o.converter } : ((n = o.converter) == null ? void 0 : n.fromAttribute) !== void 0 ? o.converter : L;
      this._$Em = r, this[r] = a.fromAttribute(e, o.type), this._$Em = null;
    }
  }
  requestUpdate(t, e, s) {
    if (t !== void 0) {
      if (s ?? (s = this.constructor.getPropertyOptions(t)), !(s.hasChanged ?? Z)(this[t], e)) return;
      this.P(t, e, s);
    }
    this.isUpdatePending === !1 && (this._$ES = this._$ET());
  }
  P(t, e, s) {
    this._$AL.has(t) || this._$AL.set(t, e), s.reflect === !0 && this._$Em !== t && (this._$Ej ?? (this._$Ej = /* @__PURE__ */ new Set())).add(t);
  }
  async _$ET() {
    this.isUpdatePending = !0;
    try {
      await this._$ES;
    } catch (e) {
      Promise.reject(e);
    }
    const t = this.scheduleUpdate();
    return t != null && await t, !this.isUpdatePending;
  }
  scheduleUpdate() {
    return this.performUpdate();
  }
  performUpdate() {
    var s;
    if (!this.isUpdatePending) return;
    if (!this.hasUpdated) {
      if (this.renderRoot ?? (this.renderRoot = this.createRenderRoot()), this._$Ep) {
        for (const [n, o] of this._$Ep) this[n] = o;
        this._$Ep = void 0;
      }
      const r = this.constructor.elementProperties;
      if (r.size > 0) for (const [n, o] of r) o.wrapped !== !0 || this._$AL.has(n) || this[n] === void 0 || this.P(n, this[n], o);
    }
    let t = !1;
    const e = this._$AL;
    try {
      t = this.shouldUpdate(e), t ? (this.willUpdate(e), (s = this._$EO) == null || s.forEach((r) => {
        var n;
        return (n = r.hostUpdate) == null ? void 0 : n.call(r);
      }), this.update(e)) : this._$EU();
    } catch (r) {
      throw t = !1, this._$EU(), r;
    }
    t && this._$AE(e);
  }
  willUpdate(t) {
  }
  _$AE(t) {
    var e;
    (e = this._$EO) == null || e.forEach((s) => {
      var r;
      return (r = s.hostUpdated) == null ? void 0 : r.call(s);
    }), this.hasUpdated || (this.hasUpdated = !0, this.firstUpdated(t)), this.updated(t);
  }
  _$EU() {
    this._$AL = /* @__PURE__ */ new Map(), this.isUpdatePending = !1;
  }
  get updateComplete() {
    return this.getUpdateComplete();
  }
  getUpdateComplete() {
    return this._$ES;
  }
  shouldUpdate(t) {
    return !0;
  }
  update(t) {
    this._$Ej && (this._$Ej = this._$Ej.forEach((e) => this._$EC(e, this[e]))), this._$EU();
  }
  updated(t) {
  }
  firstUpdated(t) {
  }
}
S.elementStyles = [], S.shadowRootOptions = { mode: "open" }, S[x("elementProperties")] = /* @__PURE__ */ new Map(), S[x("finalized")] = /* @__PURE__ */ new Map(), B == null || B({ ReactiveElement: S }), (f.reactiveElementVersions ?? (f.reactiveElementVersions = [])).push("2.0.4");
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const U = globalThis, Y = U.trustedTypes, rt = Y ? Y.createPolicy("lit-html", { createHTML: (i) => i }) : void 0, ut = "$lit$", _ = `lit$${Math.random().toFixed(9).slice(2)}$`, $t = "?" + _, Nt = `<${$t}>`, m = document, M = () => m.createComment(""), H = (i) => i === null || typeof i != "object" && typeof i != "function", Q = Array.isArray, Ot = (i) => Q(i) || typeof (i == null ? void 0 : i[Symbol.iterator]) == "function", V = `[ 	
\f\r]`, P = /<(?:(!--|\/[^a-zA-Z])|(\/?[a-zA-Z][^>\s]*)|(\/?$))/g, nt = /-->/g, ot = />/g, v = RegExp(`>|${V}(?:([^\\s"'>=/]+)(${V}*=${V}*(?:[^ 	
\f\r"'\`<>=]|("|')|))|$)`, "g"), ht = /'/g, at = /"/g, _t = /^(?:script|style|textarea|title)$/i, Tt = (i) => (t, ...e) => ({ _$litType$: i, strings: t, values: e }), Mt = Tt(1), b = Symbol.for("lit-noChange"), d = Symbol.for("lit-nothing"), ct = /* @__PURE__ */ new WeakMap(), g = m.createTreeWalker(m, 129);
function ft(i, t) {
  if (!Q(i) || !i.hasOwnProperty("raw")) throw Error("invalid template strings array");
  return rt !== void 0 ? rt.createHTML(t) : t;
}
const Ht = (i, t) => {
  const e = i.length - 1, s = [];
  let r, n = t === 2 ? "<svg>" : t === 3 ? "<math>" : "", o = P;
  for (let a = 0; a < e; a++) {
    const h = i[a];
    let l, p, c = -1, u = 0;
    for (; u < h.length && (o.lastIndex = u, p = o.exec(h), p !== null); ) u = o.lastIndex, o === P ? p[1] === "!--" ? o = nt : p[1] !== void 0 ? o = ot : p[2] !== void 0 ? (_t.test(p[2]) && (r = RegExp("</" + p[2], "g")), o = v) : p[3] !== void 0 && (o = v) : o === v ? p[0] === ">" ? (o = r ?? P, c = -1) : p[1] === void 0 ? c = -2 : (c = o.lastIndex - p[2].length, l = p[1], o = p[3] === void 0 ? v : p[3] === '"' ? at : ht) : o === at || o === ht ? o = v : o === nt || o === ot ? o = P : (o = v, r = void 0);
    const $ = o === v && i[a + 1].startsWith("/>") ? " " : "";
    n += o === P ? h + Nt : c >= 0 ? (s.push(l), h.slice(0, c) + ut + h.slice(c) + _ + $) : h + _ + (c === -2 ? a : $);
  }
  return [ft(i, n + (i[e] || "<?>") + (t === 2 ? "</svg>" : t === 3 ? "</math>" : "")), s];
};
class R {
  constructor({ strings: t, _$litType$: e }, s) {
    let r;
    this.parts = [];
    let n = 0, o = 0;
    const a = t.length - 1, h = this.parts, [l, p] = Ht(t, e);
    if (this.el = R.createElement(l, s), g.currentNode = this.el.content, e === 2 || e === 3) {
      const c = this.el.content.firstChild;
      c.replaceWith(...c.childNodes);
    }
    for (; (r = g.nextNode()) !== null && h.length < a; ) {
      if (r.nodeType === 1) {
        if (r.hasAttributes()) for (const c of r.getAttributeNames()) if (c.endsWith(ut)) {
          const u = p[o++], $ = r.getAttribute(c).split(_), D = /([.?@])?(.*)/.exec(u);
          h.push({ type: 1, index: n, name: D[2], strings: $, ctor: D[1] === "." ? zt : D[1] === "?" ? kt : D[1] === "@" ? Dt : j }), r.removeAttribute(c);
        } else c.startsWith(_) && (h.push({ type: 6, index: n }), r.removeAttribute(c));
        if (_t.test(r.tagName)) {
          const c = r.textContent.split(_), u = c.length - 1;
          if (u > 0) {
            r.textContent = Y ? Y.emptyScript : "";
            for (let $ = 0; $ < u; $++) r.append(c[$], M()), g.nextNode(), h.push({ type: 2, index: ++n });
            r.append(c[u], M());
          }
        }
      } else if (r.nodeType === 8) if (r.data === $t) h.push({ type: 2, index: n });
      else {
        let c = -1;
        for (; (c = r.data.indexOf(_, c + 1)) !== -1; ) h.push({ type: 7, index: n }), c += _.length - 1;
      }
      n++;
    }
  }
  static createElement(t, e) {
    const s = m.createElement("template");
    return s.innerHTML = t, s;
  }
}
function w(i, t, e = i, s) {
  var o, a;
  if (t === b) return t;
  let r = s !== void 0 ? (o = e._$Co) == null ? void 0 : o[s] : e._$Cl;
  const n = H(t) ? void 0 : t._$litDirective$;
  return (r == null ? void 0 : r.constructor) !== n && ((a = r == null ? void 0 : r._$AO) == null || a.call(r, !1), n === void 0 ? r = void 0 : (r = new n(i), r._$AT(i, e, s)), s !== void 0 ? (e._$Co ?? (e._$Co = []))[s] = r : e._$Cl = r), r !== void 0 && (t = w(i, r._$AS(i, t.values), r, s)), t;
}
class Rt {
  constructor(t, e) {
    this._$AV = [], this._$AN = void 0, this._$AD = t, this._$AM = e;
  }
  get parentNode() {
    return this._$AM.parentNode;
  }
  get _$AU() {
    return this._$AM._$AU;
  }
  u(t) {
    const { el: { content: e }, parts: s } = this._$AD, r = ((t == null ? void 0 : t.creationScope) ?? m).importNode(e, !0);
    g.currentNode = r;
    let n = g.nextNode(), o = 0, a = 0, h = s[0];
    for (; h !== void 0; ) {
      if (o === h.index) {
        let l;
        h.type === 2 ? l = new k(n, n.nextSibling, this, t) : h.type === 1 ? l = new h.ctor(n, h.name, h.strings, this, t) : h.type === 6 && (l = new It(n, this, t)), this._$AV.push(l), h = s[++a];
      }
      o !== (h == null ? void 0 : h.index) && (n = g.nextNode(), o++);
    }
    return g.currentNode = m, r;
  }
  p(t) {
    let e = 0;
    for (const s of this._$AV) s !== void 0 && (s.strings !== void 0 ? (s._$AI(t, s, e), e += s.strings.length - 2) : s._$AI(t[e])), e++;
  }
}
class k {
  get _$AU() {
    var t;
    return ((t = this._$AM) == null ? void 0 : t._$AU) ?? this._$Cv;
  }
  constructor(t, e, s, r) {
    this.type = 2, this._$AH = d, this._$AN = void 0, this._$AA = t, this._$AB = e, this._$AM = s, this.options = r, this._$Cv = (r == null ? void 0 : r.isConnected) ?? !0;
  }
  get parentNode() {
    let t = this._$AA.parentNode;
    const e = this._$AM;
    return e !== void 0 && (t == null ? void 0 : t.nodeType) === 11 && (t = e.parentNode), t;
  }
  get startNode() {
    return this._$AA;
  }
  get endNode() {
    return this._$AB;
  }
  _$AI(t, e = this) {
    t = w(this, t, e), H(t) ? t === d || t == null || t === "" ? (this._$AH !== d && this._$AR(), this._$AH = d) : t !== this._$AH && t !== b && this._(t) : t._$litType$ !== void 0 ? this.$(t) : t.nodeType !== void 0 ? this.T(t) : Ot(t) ? this.k(t) : this._(t);
  }
  O(t) {
    return this._$AA.parentNode.insertBefore(t, this._$AB);
  }
  T(t) {
    this._$AH !== t && (this._$AR(), this._$AH = this.O(t));
  }
  _(t) {
    this._$AH !== d && H(this._$AH) ? this._$AA.nextSibling.data = t : this.T(m.createTextNode(t)), this._$AH = t;
  }
  $(t) {
    var n;
    const { values: e, _$litType$: s } = t, r = typeof s == "number" ? this._$AC(t) : (s.el === void 0 && (s.el = R.createElement(ft(s.h, s.h[0]), this.options)), s);
    if (((n = this._$AH) == null ? void 0 : n._$AD) === r) this._$AH.p(e);
    else {
      const o = new Rt(r, this), a = o.u(this.options);
      o.p(e), this.T(a), this._$AH = o;
    }
  }
  _$AC(t) {
    let e = ct.get(t.strings);
    return e === void 0 && ct.set(t.strings, e = new R(t)), e;
  }
  k(t) {
    Q(this._$AH) || (this._$AH = [], this._$AR());
    const e = this._$AH;
    let s, r = 0;
    for (const n of t) r === e.length ? e.push(s = new k(this.O(M()), this.O(M()), this, this.options)) : s = e[r], s._$AI(n), r++;
    r < e.length && (this._$AR(s && s._$AB.nextSibling, r), e.length = r);
  }
  _$AR(t = this._$AA.nextSibling, e) {
    var s;
    for ((s = this._$AP) == null ? void 0 : s.call(this, !1, !0, e); t && t !== this._$AB; ) {
      const r = t.nextSibling;
      t.remove(), t = r;
    }
  }
  setConnected(t) {
    var e;
    this._$AM === void 0 && (this._$Cv = t, (e = this._$AP) == null || e.call(this, t));
  }
}
class j {
  get tagName() {
    return this.element.tagName;
  }
  get _$AU() {
    return this._$AM._$AU;
  }
  constructor(t, e, s, r, n) {
    this.type = 1, this._$AH = d, this._$AN = void 0, this.element = t, this.name = e, this._$AM = r, this.options = n, s.length > 2 || s[0] !== "" || s[1] !== "" ? (this._$AH = Array(s.length - 1).fill(new String()), this.strings = s) : this._$AH = d;
  }
  _$AI(t, e = this, s, r) {
    const n = this.strings;
    let o = !1;
    if (n === void 0) t = w(this, t, e, 0), o = !H(t) || t !== this._$AH && t !== b, o && (this._$AH = t);
    else {
      const a = t;
      let h, l;
      for (t = n[0], h = 0; h < n.length - 1; h++) l = w(this, a[s + h], e, h), l === b && (l = this._$AH[h]), o || (o = !H(l) || l !== this._$AH[h]), l === d ? t = d : t !== d && (t += (l ?? "") + n[h + 1]), this._$AH[h] = l;
    }
    o && !r && this.j(t);
  }
  j(t) {
    t === d ? this.element.removeAttribute(this.name) : this.element.setAttribute(this.name, t ?? "");
  }
}
class zt extends j {
  constructor() {
    super(...arguments), this.type = 3;
  }
  j(t) {
    this.element[this.name] = t === d ? void 0 : t;
  }
}
class kt extends j {
  constructor() {
    super(...arguments), this.type = 4;
  }
  j(t) {
    this.element.toggleAttribute(this.name, !!t && t !== d);
  }
}
class Dt extends j {
  constructor(t, e, s, r, n) {
    super(t, e, s, r, n), this.type = 5;
  }
  _$AI(t, e = this) {
    if ((t = w(this, t, e, 0) ?? d) === b) return;
    const s = this._$AH, r = t === d && s !== d || t.capture !== s.capture || t.once !== s.once || t.passive !== s.passive, n = t !== d && (s === d || r);
    r && this.element.removeEventListener(this.name, this, s), n && this.element.addEventListener(this.name, this, t), this._$AH = t;
  }
  handleEvent(t) {
    var e;
    typeof this._$AH == "function" ? this._$AH.call(((e = this.options) == null ? void 0 : e.host) ?? this.element, t) : this._$AH.handleEvent(t);
  }
}
class It {
  constructor(t, e, s) {
    this.element = t, this.type = 6, this._$AN = void 0, this._$AM = e, this.options = s;
  }
  get _$AU() {
    return this._$AM._$AU;
  }
  _$AI(t) {
    w(this, t);
  }
}
const q = U.litHtmlPolyfillSupport;
q == null || q(R, k), (U.litHtmlVersions ?? (U.litHtmlVersions = [])).push("3.2.1");
const Lt = (i, t, e) => {
  const s = (e == null ? void 0 : e.renderBefore) ?? t;
  let r = s._$litPart$;
  if (r === void 0) {
    const n = (e == null ? void 0 : e.renderBefore) ?? null;
    s._$litPart$ = r = new k(t.insertBefore(M(), n), n, void 0, e ?? {});
  }
  return r._$AI(i), r;
};
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
let N = class extends S {
  constructor() {
    super(...arguments), this.renderOptions = { host: this }, this._$Do = void 0;
  }
  createRenderRoot() {
    var e;
    const t = super.createRenderRoot();
    return (e = this.renderOptions).renderBefore ?? (e.renderBefore = t.firstChild), t;
  }
  update(t) {
    const e = this.render();
    this.hasUpdated || (this.renderOptions.isConnected = this.isConnected), super.update(t), this._$Do = Lt(e, this.renderRoot, this.renderOptions);
  }
  connectedCallback() {
    var t;
    super.connectedCallback(), (t = this._$Do) == null || t.setConnected(!0);
  }
  disconnectedCallback() {
    var t;
    super.disconnectedCallback(), (t = this._$Do) == null || t.setConnected(!1);
  }
  render() {
    return b;
  }
};
var dt;
N._$litElement$ = !0, N.finalized = !0, (dt = globalThis.litElementHydrateSupport) == null || dt.call(globalThis, { LitElement: N });
const X = globalThis.litElementPolyfillSupport;
X == null || X({ LitElement: N });
(globalThis.litElementVersions ?? (globalThis.litElementVersions = [])).push("4.1.1");
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const Yt = (i) => (t, e) => {
  e !== void 0 ? e.addInitializer(() => {
    customElements.define(i, t);
  }) : customElements.define(i, t);
};
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const Wt = { attribute: !0, type: String, converter: L, reflect: !1, hasChanged: Z }, jt = (i = Wt, t, e) => {
  const { kind: s, metadata: r } = e;
  let n = globalThis.litPropertyMetadata.get(r);
  if (n === void 0 && globalThis.litPropertyMetadata.set(r, n = /* @__PURE__ */ new Map()), n.set(e.name, i), s === "accessor") {
    const { name: o } = e;
    return { set(a) {
      const h = t.get.call(this);
      t.set.call(this, a), this.requestUpdate(o, h, i);
    }, init(a) {
      return a !== void 0 && this.P(o, void 0, i), a;
    } };
  }
  if (s === "setter") {
    const { name: o } = e;
    return function(a) {
      const h = this[o];
      t.call(this, a), this.requestUpdate(o, h, i);
    };
  }
  throw Error("Unsupported decorator location: " + s);
};
function C(i) {
  return (t, e) => typeof e == "object" ? jt(i, t, e) : ((s, r, n) => {
    const o = r.hasOwnProperty(n);
    return r.constructor.createProperty(n, o ? { ...s, wrapped: !0 } : s), o ? Object.getOwnPropertyDescriptor(r, n) : void 0;
  })(i, t, e);
}
/**
 * @license
 * Copyright 2020 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const Bt = (i) => i.strings === void 0;
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const Vt = { CHILD: 2 }, qt = (i) => (...t) => ({ _$litDirective$: i, values: t });
class Xt {
  constructor(t) {
  }
  get _$AU() {
    return this._$AM._$AU;
  }
  _$AT(t, e, s) {
    this._$Ct = t, this._$AM = e, this._$Ci = s;
  }
  _$AS(t, e) {
    return this.update(t, e);
  }
  update(t, e) {
    return this.render(...e);
  }
}
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const O = (i, t) => {
  var s;
  const e = i._$AN;
  if (e === void 0) return !1;
  for (const r of e) (s = r._$AO) == null || s.call(r, t, !1), O(r, t);
  return !0;
}, W = (i) => {
  let t, e;
  do {
    if ((t = i._$AM) === void 0) break;
    e = t._$AN, e.delete(i), i = t;
  } while ((e == null ? void 0 : e.size) === 0);
}, At = (i) => {
  for (let t; t = i._$AM; i = t) {
    let e = t._$AN;
    if (e === void 0) t._$AN = e = /* @__PURE__ */ new Set();
    else if (e.has(i)) break;
    e.add(i), Kt(t);
  }
};
function Ft(i) {
  this._$AN !== void 0 ? (W(this), this._$AM = i, At(this)) : this._$AM = i;
}
function Jt(i, t = !1, e = 0) {
  const s = this._$AH, r = this._$AN;
  if (r !== void 0 && r.size !== 0) if (t) if (Array.isArray(s)) for (let n = e; n < s.length; n++) O(s[n], !1), W(s[n]);
  else s != null && (O(s, !1), W(s));
  else O(this, i);
}
const Kt = (i) => {
  i.type == Vt.CHILD && (i._$AP ?? (i._$AP = Jt), i._$AQ ?? (i._$AQ = Ft));
};
class Zt extends Xt {
  constructor() {
    super(...arguments), this._$AN = void 0;
  }
  _$AT(t, e, s) {
    super._$AT(t, e, s), At(this), this.isConnected = t._$AU;
  }
  _$AO(t, e = !0) {
    var s, r;
    t !== this.isConnected && (this.isConnected = t, t ? (s = this.reconnected) == null || s.call(this) : (r = this.disconnected) == null || r.call(this)), e && (O(this, t), W(this));
  }
  setValue(t) {
    if (Bt(this._$Ct)) this._$Ct._$AI(t, this);
    else {
      const e = [...this._$Ct._$AH];
      e[this._$Ci] = t, this._$Ct._$AI(e, this, 0);
    }
  }
  disconnected() {
  }
  reconnected() {
  }
}
/**
 * @license
 * Copyright 2020 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const Qt = () => new Gt();
class Gt {
}
const F = /* @__PURE__ */ new WeakMap(), te = qt(class extends Zt {
  render(i) {
    return d;
  }
  update(i, [t]) {
    var s;
    const e = t !== this.Y;
    return e && this.Y !== void 0 && this.rt(void 0), (e || this.lt !== this.ct) && (this.Y = t, this.ht = (s = i.options) == null ? void 0 : s.host, this.rt(this.ct = i.element)), d;
  }
  rt(i) {
    if (this.isConnected || (i = void 0), typeof this.Y == "function") {
      const t = this.ht ?? globalThis;
      let e = F.get(t);
      e === void 0 && (e = /* @__PURE__ */ new WeakMap(), F.set(t, e)), e.get(this.Y) !== void 0 && this.Y.call(this.ht, void 0), e.set(this.Y, i), i !== void 0 && this.Y.call(this.ht, i);
    } else this.Y.value = i;
  }
  get lt() {
    var i, t;
    return typeof this.Y == "function" ? (i = F.get(this.ht ?? globalThis)) == null ? void 0 : i.get(this.Y) : (t = this.Y) == null ? void 0 : t.value;
  }
  disconnected() {
    this.lt === this.ct && this.rt(void 0);
  }
  reconnected() {
    this.rt(this.ct);
  }
});
var ee = Object.defineProperty, se = Object.getOwnPropertyDescriptor, vt = (i) => {
  throw TypeError(i);
}, E = (i, t, e, s) => {
  for (var r = s > 1 ? void 0 : s ? se(t, e) : t, n = i.length - 1, o; n >= 0; n--)
    (o = i[n]) && (r = (s ? o(t, e, r) : o(r)) || r);
  return s && r && ee(t, e, r), r;
}, ie = (i, t, e) => t.has(i) || vt("Cannot " + e), re = (i, t, e) => t.has(i) ? vt("Cannot add the same private member more than once") : t instanceof WeakSet ? t.add(i) : t.set(i, e), z = (i, t, e) => (ie(i, t, "access private method"), e), y, G, gt, J;
const lt = Math.PI * 2, T = Math.random;
let A = class extends N {
  constructor() {
    super(...arguments), re(this, y), this.numStars = 5e3, this.centerXPercentage = 25, this.centerYPercentage = 50, this.size = 2.5, this.speed = 10, this.streak = 50, this.stars = [], this.canvasRef = Qt(), this.ctx = null;
  }
  firstUpdated() {
    var t, e;
    const i = this.canvasRef.value;
    if (!i)
      throw new Error("Canvas not found");
    i.width = ((t = i.parentElement) == null ? void 0 : t.clientWidth) || window.innerWidth, i.height = ((e = i.parentElement) == null ? void 0 : e.clientHeight) || window.innerHeight, this.ctx = i.getContext("2d"), z(this, y, gt).call(this), z(this, y, J).call(this);
  }
  render() {
    return Mt`
        <canvas
            ${te(this.canvasRef)}
            style="width: 100%; height: 100%;">
        </canvas>
    `;
  }
};
y = /* @__PURE__ */ new WeakSet();
G = function() {
  if (!this.canvasRef.value) throw new Error("Canvas not found");
  const { width: i, height: t } = this.canvasRef.value, e = i * this.centerXPercentage / 100, s = t * this.centerYPercentage / 100;
  return { centerX: e, centerY: s };
};
gt = function() {
  if (!this.canvasRef.value) throw new Error("Canvas not found");
  const { width: i, height: t } = this.canvasRef.value, { centerX: e, centerY: s } = z(this, y, G).call(this);
  this.stars = [];
  for (let r = 0; r < this.numStars; r++)
    this.stars.push({
      x: T() * i - e,
      y: T() * t - s,
      z: T() * i
    });
};
J = function() {
  if (!this.ctx) throw new Error("Canvas not found");
  if (!this.canvasRef.value) throw new Error("Canvas not found");
  const { width: i, height: t } = this.canvasRef.value, { centerX: e, centerY: s } = z(this, y, G).call(this), r = getComputedStyle(this), n = r.getPropertyValue("color"), o = r.getPropertyValue("background-color");
  this.ctx.clearRect(0, 0, i, t), this.ctx.fillStyle = o, this.ctx.fillRect(0, 0, i, t), this.ctx.fillStyle = n;
  for (const a of this.stars) {
    const h = a.x / a.z * i + e, l = a.y / a.z * t + s, p = this.size * (1 - a.z / i);
    this.ctx.beginPath(), this.ctx.arc(h, l, p, -this.streak * lt, this.streak * lt), this.ctx.fill(), a.z -= this.speed, a.z <= 0 && (a.z = i, a.x = T() * i - e, a.y = T() * t - s);
  }
  requestAnimationFrame(() => z(this, y, J).call(this));
};
E([
  C({ type: Number })
], A.prototype, "numStars", 2);
E([
  C({ type: Number, attribute: "center-x" })
], A.prototype, "centerXPercentage", 2);
E([
  C({ type: Number, attribute: "center-y" })
], A.prototype, "centerYPercentage", 2);
E([
  C({ type: Number })
], A.prototype, "size", 2);
E([
  C({ type: Number })
], A.prototype, "speed", 2);
E([
  C({ type: Number })
], A.prototype, "streak", 2);
A = E([
  Yt("ds-starfield")
], A);
export {
  A as DatastarStarfield
};
//# sourceMappingURL=datastar.js.map
