// Datastar v1.0.0-beta.1
var z=/🖕JS_DS🚀/.source,k=z.slice(0,5),B=z.slice(4),P="datastar";var H="1.0.0-beta.1";var me={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ee=me.Morph;var y=(s=>(s[s.Attribute=1]="Attribute",s[s.Watcher=2]="Watcher",s[s.Action=3]="Action",s))(y||{});var ye="computed",X={type:1,name:ye,keyReq:1,valReq:1,onLoad:({key:n,signals:e,genRX:t})=>{let s=t();e.setComputed(n,s)}};var Z=n=>n.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,t)=>(t?"-":"")+e.toLowerCase()),Q=n=>n.replace(/(?:^\w|[A-Z]|\b\w)/g,(e,t)=>t===0?e.toLowerCase():e.toUpperCase()).replace(/\s+/g,""),ee=n=>new Function(`return Object.assign({}, ${n})`)();var te={type:1,name:"signals",removeOnLoad:!0,onLoad:n=>{let{key:e,value:t,genRX:s,signals:i,mods:o}=n,r=o.has("ifmissing");if(e!==""&&!r){let a=t===""?t:s()();i.setValue(e,a)}else{let a=ee(n.value);n.value=JSON.stringify(a);let f=s()();i.merge(f,r)}}};var ne={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var A=class{#e=0;#t;constructor(e=P){this.#t=e}with(e){if(typeof e=="string")for(let t of e.split(""))this.with(t.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}reset(){return this.#e=0,this}get value(){return this.#t+Math.abs(this.#e).toString(36)}};function se(n){if(n.id)return n.id;let e=new A,t=n;for(;t.parentNode;){if(t.id){e.with(t.id);break}if(t===t.ownerDocument.documentElement)e.with(t.tagName);else{for(let s=1,i=n;i.previousElementSibling;i=i.previousElementSibling,s++)e.with(s);t=t.parentNode}t=t.parentNode}return e.value}var ve=`${window.location.origin}/errors`;function G(n,e,t={}){let s=new Error;e=e[0].toUpperCase()+e.slice(1),s.name=`${P} ${n} error ${e}`;let i=Z(e).replaceAll("-","_"),o=new URLSearchParams({metadata:JSON.stringify(t)}).toString();return s.message=`for more info see ${ve}/${n}/${i}?${o}`,s}function g(n,e,t={}){return G("internal",e,Object.assign({from:n},t))}function ie(n,e,t={}){let s={plugin:{name:e.plugin.name,type:y[e.plugin.type]}};return G("init",n,Object.assign(s,t))}function m(n,e,t={}){let s={plugin:{name:e.plugin.name,type:y[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},raw:{key:e.rawKey,value:e.rawValue},expression:{key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return G("runtime",n,Object.assign(s,t))}var v="preact-signals",Se=Symbol.for("preact-signals"),h=1,E=2,O=4,w=8,V=16,T=32;function W(){M++}function K(){if(M>1){M--;return}let n,e=!1;for(;C!==void 0;){let t=C;for(C=void 0,q++;t!==void 0;){let s=t._nextBatchedEffect;if(t._nextBatchedEffect=void 0,t._flags&=~E,!(t._flags&w)&&oe(t))try{t._callback()}catch(i){e||(n=i,e=!0)}t=s}}if(q=0,M--,e)throw g(v,"BatchError, error",{error:n})}var l;var C,M=0,q=0,I=0;function re(n){if(l===void 0)return;let e=n._node;if(e===void 0||e._target!==l)return e={_version:0,_source:n,_prevSource:l._sources,_nextSource:void 0,_target:l,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},l._sources!==void 0&&(l._sources._nextSource=e),l._sources=e,n._node=e,l._flags&T&&n._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=l._sources,e._nextSource=void 0,l._sources._nextSource=e,l._sources=e),e}function c(n){this._value=n,this._version=0,this._node=void 0,this._targets=void 0}c.prototype.brand=Se;c.prototype._refresh=()=>!0;c.prototype._subscribe=function(n){this._targets!==n&&n._prevTarget===void 0&&(n._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=n),this._targets=n)};c.prototype._unsubscribe=function(n){if(this._targets!==void 0){let e=n._prevTarget,t=n._nextTarget;e!==void 0&&(e._nextTarget=t,n._prevTarget=void 0),t!==void 0&&(t._prevTarget=e,n._nextTarget=void 0),n===this._targets&&(this._targets=t)}};c.prototype.subscribe=function(n){return $(()=>{let e=this.value,t=l;l=void 0;try{n(e)}finally{l=t}})};c.prototype.valueOf=function(){return this.value};c.prototype.toString=function(){return`${this.value}`};c.prototype.toJSON=function(){return this.value};c.prototype.peek=function(){let n=l;l=void 0;try{return this.value}finally{l=n}};Object.defineProperty(c.prototype,"value",{get(){let n=re(this);return n!==void 0&&(n._version=this._version),this._value},set(n){if(n!==this._value){if(q>100)throw g(v,"SignalCycleDetected");this._value=n,this._version++,I++,W();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{K()}}}});function oe(n){for(let e=n._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function ae(n){for(let e=n._sources;e!==void 0;e=e._nextSource){let t=e._source._node;if(t!==void 0&&(e._rollbackNode=t),e._source._node=e,e._version=-1,e._nextSource===void 0){n._sources=e;break}}}function le(n){let e=n._sources,t;for(;e!==void 0;){let s=e._prevSource;e._version===-1?(e._source._unsubscribe(e),s!==void 0&&(s._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=s)):t=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=s}n._sources=t}function S(n){c.call(this,void 0),this._fn=n,this._sources=void 0,this._globalVersion=I-1,this._flags=O}S.prototype=new c;S.prototype._refresh=function(){if(this._flags&=~E,this._flags&h)return!1;if((this._flags&(O|T))===T||(this._flags&=~O,this._globalVersion===I))return!0;if(this._globalVersion=I,this._flags|=h,this._version>0&&!oe(this))return this._flags&=~h,!0;let n=l;try{ae(this),l=this;let e=this._fn();(this._flags&V||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~V,this._version++)}catch(e){this._value=e,this._flags|=V,this._version++}return l=n,le(this),this._flags&=~h,!0};S.prototype._subscribe=function(n){if(this._targets===void 0){this._flags|=O|T;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}c.prototype._subscribe.call(this,n)};S.prototype._unsubscribe=function(n){if(this._targets!==void 0&&(c.prototype._unsubscribe.call(this,n),this._targets===void 0)){this._flags&=~T;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};S.prototype._notify=function(){if(!(this._flags&E)){this._flags|=O|E;for(let n=this._targets;n!==void 0;n=n._nextTarget)n._target._notify()}};Object.defineProperty(S.prototype,"value",{get(){if(this._flags&h)throw g(v,"SignalCycleDetected");let n=re(this);if(this._refresh(),n!==void 0&&(n._version=this._version),this._flags&V)throw g(v,"GetComputedError",{value:this._value});return this._value}});function ue(n){return new S(n)}function ce(n){let e=n._cleanup;if(n._cleanup=void 0,typeof e=="function"){W();let t=l;l=void 0;try{e()}catch(s){throw n._flags&=~h,n._flags|=w,U(n),g(v,"CleanupEffectError",{error:s})}finally{l=t,K()}}}function U(n){for(let e=n._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);n._fn=void 0,n._sources=void 0,ce(n)}function be(n){if(l!==this)throw g(v,"EndEffectError");le(this),l=n,this._flags&=~h,this._flags&w&&U(this),K()}function D(n){this._fn=n,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=T}D.prototype._callback=function(){let n=this._start();try{if(this._flags&w||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{n()}};D.prototype._start=function(){if(this._flags&h)throw g(v,"SignalCycleDetected");this._flags|=h,this._flags&=~w,ce(this),ae(this),W();let n=l;return l=this,be.bind(this,n)};D.prototype._notify=function(){this._flags&E||(this._flags|=E,this._nextBatchedEffect=C,C=this)};D.prototype._dispose=function(){this._flags|=w,this._flags&h||U(this)};function $(n){let e=new D(n);try{e._callback()}catch(t){throw e._dispose(),t}return e._dispose.bind(e)}var fe="namespacedSignals";function de(n,e=!1){let t={};for(let s in n)if(Object.hasOwn(n,s)){if(e&&s.startsWith("_"))continue;let i=n[s];i instanceof c?t[s]=i.value:t[s]=de(i)}return t}function pe(n,e,t=!1){for(let s in e)if(Object.hasOwn(e,s)){if(s.match(/\_\_+/))throw g(fe,"InvalidSignalKey",{key:s});let i=e[s];if(i instanceof Object&&!Array.isArray(i))n[s]||(n[s]={}),pe(n[s],i,t);else{if(Object.hasOwn(n,s)){if(t)continue;let r=n[s];if(r instanceof c){r.value=i;continue}}n[s]=new c(i)}}}function ge(n,e){for(let t in n)if(Object.hasOwn(n,t)){let s=n[t];s instanceof c?e(t,s):ge(s,(i,o)=>{e(`${t}.${i}`,o)})}}function xe(n,...e){let t={};for(let s of e){let i=s.split("."),o=n,r=t;for(let d=0;d<i.length-1;d++){let f=i[d];if(!o[f])return{};r[f]||(r[f]={}),o=o[f],r=r[f]}let a=i[i.length-1];r[a]=o[a]}return t}var F=class{#e={};exists(e){return!!this.signal(e)}signal(e){let t=e.split("."),s=this.#e;for(let r=0;r<t.length-1;r++){let a=t[r];if(!s[a])return null;s=s[a]}let i=t[t.length-1],o=s[i];if(!o)throw g(fe,"SignalNotFound",{path:e});return o}setSignal(e,t){let s=e.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let a=s[r];i[a]||(i[a]={}),i=i[a]}let o=s[s.length-1];i[o]=t}setComputed(e,t){let s=ue(()=>t());this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,t){let s=this.upsertIfMissing(e,t);s.value=t}upsertIfMissing(e,t){let s=e.split("."),i=this.#e;for(let d=0;d<s.length-1;d++){let f=s[d];i[f]||(i[f]={}),i=i[f]}let o=s[s.length-1],r=i[o];if(r instanceof c)return r;let a=new c(t);return i[o]=a,a}remove(...e){for(let t of e){let s=t.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let a=s[r];if(!i[a])return;i=i[a]}let o=s[s.length-1];delete i[o]}}merge(e,t=!1){pe(this.#e,e,t)}subset(...e){return xe(this.values(),...e)}walk(e){ge(this.#e,e)}paths(){let e=new Array;return this.walk(t=>e.push(t)),e}values(e=!1){return de(this.#e,e)}JSON(e=!0,t=!1){let s=this.values(t);return e?JSON.stringify(s,null,2):JSON.stringify(s)}toString(){return this.JSON()}};var L=class{#e=new F;#t=[];#s={};#o=[];#n=new Map;get signals(){return this.#e}get version(){return H}load(...e){for(let t of e){let s=this,i={get signals(){return s.#e},effect:r=>$(r),actions:this.#s,apply:this.apply.bind(this),cleanup:this.#i.bind(this),plugin:t},o;switch(t.type){case 2:{let r=t;this.#o.push(r),o=r.onGlobalInit;break}case 3:{this.#s[t.name]=t;break}case 1:{let r=t;this.#t.push(r),o=r.onGlobalInit;break}default:throw ie("InvalidPluginType",i)}o&&o(i)}this.#t.sort((t,s)=>{let i=s.name.length-t.name.length;return i!==0?i:t.name.localeCompare(s.name)})}apply(e){this.#r(e,t=>{if(this.#i(t),!(!t.dataset||"starIgnore"in t.dataset))for(let s of Object.keys(t.dataset)){let i=this.#t.find(u=>s.startsWith(u.name));if(!i)continue;t.id.length||(t.id=se(t));let[o,...r]=s.slice(i.name.length).split(/\_\_+/),a=o.length>0;if(a){let u=o.slice(1);o=o.startsWith("-")?u:o[0].toLowerCase()+u}let d=`${t.dataset[s]}`||"",f=d.length>0,j=this,p={get signals(){return j.#e},effect:u=>$(u),apply:this.apply.bind(this),cleanup:this.#i.bind(this),actions:this.#s,genRX:()=>this.#a(p,...i.argNames||[]),plugin:i,el:t,rawKey:s,key:o,value:d,mods:new Map},N=i.keyReq||0;if(a){if(N===2)throw m(`${i.name}KeyNotAllowed`,p)}else if(N===1)throw m(`${i.name}KeyRequired`,p);let b=i.valReq||0;if(f){if(b===2)throw m(`${i.name}ValueNotAllowed`,p)}else if(b===1)throw m(`${i.name}ValueRequired`,p);if(N===3||b===3){if(a&&f)throw m(`${i.name}KeyAndValueProvided`,p);if(!a&&!f)throw m(`${i.name}KeyOrValueRequired`,p)}for(let u of r){let[_,...x]=u.split(".");p.mods.set(Q(_),new Set(x.map(_e=>_e.toLowerCase())))}let R=i.onLoad(p);R&&(this.#n.has(t)||this.#n.set(t,{id:t.id,fns:[]}),this.#n.get(t)?.fns.push(R)),i?.removeOnLoad&&delete t.dataset[s]}})}#a(e,...t){let s=e.value.split(/;|\n/).map(u=>u.trim()).filter(u=>u!==""),i=s.length-1;s[i].startsWith("return")||(s[i]=`return (${s[i]});`);let r=s.join(`;
`).trim(),a=new Map,d=new RegExp(`(?:${k})(.*?)(?:${B})`,"gm");for(let u of r.matchAll(d)){let _=u[1],x=new A("dsEscaped").with(_).value;a.set(x,_),r=r.replace(k+_+B,x)}let f=/@(\w*)\(/gm,j=r.matchAll(f),p=new Set;for(let u of j)p.add(u[1]);let N=new RegExp(`@(${Object.keys(this.#s).join("|")})\\(`,"gm");r=r.replaceAll(N,"ctx.actions.$1.fn(ctx,");let b=e.signals.paths();if(b.length){let u=new RegExp(`\\$(${b.join("|")})(\\W|$)`,"gm");r=r.replaceAll(u,"ctx.signals.signal('$1').value$2")}for(let[u,_]of a)r=r.replace(u,_);let R=`return (()=> {
${r}
})()`;e.fnContent=R;try{let u=new Function("ctx",...t,R);return(..._)=>{try{return u(e,..._)}catch(x){throw m("ExecuteExpression",e,{error:x.message})}}}catch(u){throw m("GenerateExpression",e,{error:u.message})}}#r(e,t){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;t(e);let s=e.firstElementChild;for(;s;)this.#r(s,t),s=s.nextElementSibling}#i(e){let t=this.#n.get(e);if(t){for(let s of t.fns)s();this.#n.delete(e)}}};var he=new L;he.load(ne,te,X);var J=he;J.apply(document.body);var ot=J;export{ot as Datastar};
//# sourceMappingURL=datastar-core.js.map
