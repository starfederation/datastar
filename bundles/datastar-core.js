// Datastar v1.0.0-beta.1
var H=/🖕JS_DS🚀/.source,P=H.slice(0,5),G=H.slice(4),M="datastar";var Y="1.0.0-beta.1";var me={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ee=me.Morph;var v=(s=>(s[s.Attribute=1]="Attribute",s[s.Watcher=2]="Watcher",s[s.Action=3]="Action",s))(v||{});var ye="computed",Z={type:1,name:ye,keyReq:1,valReq:1,onLoad:({key:n,signals:e,genRX:t})=>{let s=t();e.setComputed(n,s)}};var Q=n=>n.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,t)=>(t?"-":"")+e.toLowerCase()),ee=n=>n.replace(/(?:^\w|[A-Z]|\b\w)/g,(e,t)=>t===0?e.toLowerCase():e.toUpperCase()).replace(/\s+/g,""),te=n=>new Function(`return Object.assign({}, ${n})`)();var ne={type:1,name:"signals",removeOnLoad:!0,onLoad:n=>{let{key:e,value:t,genRX:s,signals:i,mods:o}=n,r=o.has("ifmissing");if(e!==""&&!r){let a=t===""?t:s()();i.setValue(e,a)}else{let a=te(n.value);n.value=JSON.stringify(a);let c=s()();i.merge(c,r)}}};var se={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var C=class{#e=0;#t;constructor(e=M){this.#t=e}with(e){if(typeof e=="string")for(let t of e.split(""))this.with(t.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}reset(){return this.#e=0,this}get value(){return this.#t+Math.abs(this.#e).toString(36)}};function ie(n){if(n.id)return n.id;let e=new C,t=n;for(;t.parentNode;){if(t.id){e.with(t.id);break}if(t===t.ownerDocument.documentElement)e.with(t.tagName);else{for(let s=1,i=n;i.previousElementSibling;i=i.previousElementSibling,s++)e.with(s);t=t.parentNode}t=t.parentNode}return e.value}var ve=`${window.location.origin}/errors`;function q(n,e,t={}){let s=new Error;e=e[0].toUpperCase()+e.slice(1),s.name=`${M} ${n} error`;let i=Q(e).replaceAll("-","_"),o=new URLSearchParams({metadata:JSON.stringify(t)}).toString(),r=JSON.stringify(t,null,2);return s.message=`${e}
More info: ${ve}/${n}/${i}?${o}
Context: ${r}`,s}function h(n,e,t={}){return q("internal",e,Object.assign({from:n},t))}function re(n,e,t={}){let s={plugin:{name:e.plugin.name,type:v[e.plugin.type]}};return q("init",n,Object.assign(s,t))}function y(n,e,t={}){let s={plugin:{name:e.plugin.name,type:v[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return q("runtime",n,Object.assign(s,t))}var S="preact-signals",Se=Symbol.for("preact-signals"),_=1,E=2,D=4,w=8,V=16,T=32;function W(){I++}function J(){if(I>1){I--;return}let n,e=!1;for(;O!==void 0;){let t=O;for(O=void 0,K++;t!==void 0;){let s=t._nextBatchedEffect;if(t._nextBatchedEffect=void 0,t._flags&=~E,!(t._flags&w)&&ae(t))try{t._callback()}catch(i){e||(n=i,e=!0)}t=s}}if(K=0,I--,e)throw h(S,"BatchError, error",{error:n})}var l;var O,I=0,K=0,$=0;function oe(n){if(l===void 0)return;let e=n._node;if(e===void 0||e._target!==l)return e={_version:0,_source:n,_prevSource:l._sources,_nextSource:void 0,_target:l,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},l._sources!==void 0&&(l._sources._nextSource=e),l._sources=e,n._node=e,l._flags&T&&n._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=l._sources,e._nextSource=void 0,l._sources._nextSource=e,l._sources=e),e}function u(n){this._value=n,this._version=0,this._node=void 0,this._targets=void 0}u.prototype.brand=Se;u.prototype._refresh=()=>!0;u.prototype._subscribe=function(n){this._targets!==n&&n._prevTarget===void 0&&(n._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=n),this._targets=n)};u.prototype._unsubscribe=function(n){if(this._targets!==void 0){let e=n._prevTarget,t=n._nextTarget;e!==void 0&&(e._nextTarget=t,n._prevTarget=void 0),t!==void 0&&(t._prevTarget=e,n._nextTarget=void 0),n===this._targets&&(this._targets=t)}};u.prototype.subscribe=function(n){return F(()=>{let e=this.value,t=l;l=void 0;try{n(e)}finally{l=t}})};u.prototype.valueOf=function(){return this.value};u.prototype.toString=function(){return`${this.value}`};u.prototype.toJSON=function(){return this.value};u.prototype.peek=function(){let n=l;l=void 0;try{return this.value}finally{l=n}};Object.defineProperty(u.prototype,"value",{get(){let n=oe(this);return n!==void 0&&(n._version=this._version),this._value},set(n){if(n!==this._value){if(K>100)throw h(S,"SignalCycleDetected");this._value=n,this._version++,$++,W();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{J()}}}});function ae(n){for(let e=n._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function le(n){for(let e=n._sources;e!==void 0;e=e._nextSource){let t=e._source._node;if(t!==void 0&&(e._rollbackNode=t),e._source._node=e,e._version=-1,e._nextSource===void 0){n._sources=e;break}}}function ue(n){let e=n._sources,t;for(;e!==void 0;){let s=e._prevSource;e._version===-1?(e._source._unsubscribe(e),s!==void 0&&(s._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=s)):t=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=s}n._sources=t}function b(n){u.call(this,void 0),this._fn=n,this._sources=void 0,this._globalVersion=$-1,this._flags=D}b.prototype=new u;b.prototype._refresh=function(){if(this._flags&=~E,this._flags&_)return!1;if((this._flags&(D|T))===T||(this._flags&=~D,this._globalVersion===$))return!0;if(this._globalVersion=$,this._flags|=_,this._version>0&&!ae(this))return this._flags&=~_,!0;let n=l;try{le(this),l=this;let e=this._fn();(this._flags&V||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~V,this._version++)}catch(e){this._value=e,this._flags|=V,this._version++}return l=n,ue(this),this._flags&=~_,!0};b.prototype._subscribe=function(n){if(this._targets===void 0){this._flags|=D|T;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}u.prototype._subscribe.call(this,n)};b.prototype._unsubscribe=function(n){if(this._targets!==void 0&&(u.prototype._unsubscribe.call(this,n),this._targets===void 0)){this._flags&=~T;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};b.prototype._notify=function(){if(!(this._flags&E)){this._flags|=D|E;for(let n=this._targets;n!==void 0;n=n._nextTarget)n._target._notify()}};Object.defineProperty(b.prototype,"value",{get(){if(this._flags&_)throw h(S,"SignalCycleDetected");let n=oe(this);if(this._refresh(),n!==void 0&&(n._version=this._version),this._flags&V)throw h(S,"GetComputedError",{value:this._value});return this._value}});function ce(n){return new b(n)}function fe(n){let e=n._cleanup;if(n._cleanup=void 0,typeof e=="function"){W();let t=l;l=void 0;try{e()}catch(s){throw n._flags&=~_,n._flags|=w,U(n),h(S,"CleanupEffectError",{error:s})}finally{l=t,J()}}}function U(n){for(let e=n._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);n._fn=void 0,n._sources=void 0,fe(n)}function be(n){if(l!==this)throw h(S,"EndEffectError");ue(this),l=n,this._flags&=~_,this._flags&w&&U(this),J()}function k(n){this._fn=n,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=T}k.prototype._callback=function(){let n=this._start();try{if(this._flags&w||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{n()}};k.prototype._start=function(){if(this._flags&_)throw h(S,"SignalCycleDetected");this._flags|=_,this._flags&=~w,fe(this),le(this),W();let n=l;return l=this,be.bind(this,n)};k.prototype._notify=function(){this._flags&E||(this._flags|=E,this._nextBatchedEffect=O,O=this)};k.prototype._dispose=function(){this._flags|=w,this._flags&_||U(this)};function F(n){let e=new k(n);try{e._callback()}catch(t){throw e._dispose(),t}return e._dispose.bind(e)}var de="namespacedSignals";function pe(n,e=!1){let t={};for(let s in n)if(Object.hasOwn(n,s)){if(e&&s.startsWith("_"))continue;let i=n[s];i instanceof u?t[s]=i.value:t[s]=pe(i)}return t}function ge(n,e,t=!1){for(let s in e)if(Object.hasOwn(e,s)){if(s.match(/\_\_+/))throw h(de,"InvalidSignalKey",{key:s});let i=e[s];if(i instanceof Object&&!Array.isArray(i))n[s]||(n[s]={}),ge(n[s],i,t);else{if(Object.hasOwn(n,s)){if(t)continue;let r=n[s];if(r instanceof u){r.value=i;continue}}n[s]=new u(i)}}}function he(n,e){for(let t in n)if(Object.hasOwn(n,t)){let s=n[t];s instanceof u?e(t,s):he(s,(i,o)=>{e(`${t}.${i}`,o)})}}function xe(n,...e){let t={};for(let s of e){let i=s.split("."),o=n,r=t;for(let f=0;f<i.length-1;f++){let c=i[f];if(!o[c])return{};r[c]||(r[c]={}),o=o[c],r=r[c]}let a=i[i.length-1];r[a]=o[a]}return t}var L=class{#e={};exists(e){return!!this.signal(e)}signal(e){let t=e.split("."),s=this.#e;for(let r=0;r<t.length-1;r++){let a=t[r];if(!s[a])return null;s=s[a]}let i=t[t.length-1],o=s[i];if(!o)throw h(de,"SignalNotFound",{path:e});return o}setSignal(e,t){let s=e.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let a=s[r];i[a]||(i[a]={}),i=i[a]}let o=s[s.length-1];i[o]=t}setComputed(e,t){let s=ce(()=>t());this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,t){let s=this.upsertIfMissing(e,t);s.value=t}upsertIfMissing(e,t){let s=e.split("."),i=this.#e;for(let f=0;f<s.length-1;f++){let c=s[f];i[c]||(i[c]={}),i=i[c]}let o=s[s.length-1],r=i[o];if(r instanceof u)return r;let a=new u(t);return i[o]=a,a}remove(...e){for(let t of e){let s=t.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let a=s[r];if(!i[a])return;i=i[a]}let o=s[s.length-1];delete i[o]}}merge(e,t=!1){ge(this.#e,e,t)}subset(...e){return xe(this.values(),...e)}walk(e){he(this.#e,e)}paths(){let e=new Array;return this.walk(t=>e.push(t)),e}values(e=!1){return pe(this.#e,e)}JSON(e=!0,t=!1){let s=this.values(t);return e?JSON.stringify(s,null,2):JSON.stringify(s)}toString(){return this.JSON()}};var j=class{#e=new L;#t=[];#s={};#o=[];#n=new Map;get signals(){return this.#e}get version(){return Y}load(...e){for(let t of e){let s=this,i={get signals(){return s.#e},effect:r=>F(r),actions:this.#s,apply:this.apply.bind(this),cleanup:this.#i.bind(this),plugin:t},o;switch(t.type){case 2:{let r=t;this.#o.push(r),o=r.onGlobalInit;break}case 3:{this.#s[t.name]=t;break}case 1:{let r=t;this.#t.push(r),o=r.onGlobalInit;break}default:throw re("InvalidPluginType",i)}o&&o(i)}this.#t.sort((t,s)=>{let i=s.name.length-t.name.length;return i!==0?i:t.name.localeCompare(s.name)})}apply(e){this.#r(e,t=>{if(this.#i(t),!(!t.dataset||"starIgnore"in t.dataset))for(let s of Object.keys(t.dataset)){let i=this.#t.find(g=>s.startsWith(g.name));if(!i)continue;t.id.length||(t.id=ie(t));let[o,...r]=s.slice(i.name.length).split(/\_\_+/),a=o.length>0;if(a){let g=o.slice(1);o=o.startsWith("-")?g:o[0].toLowerCase()+g}let f=`${t.dataset[s]}`||"",c=f.length>0,B=this,p={get signals(){return B.#e},effect:g=>F(g),apply:this.apply.bind(this),cleanup:this.#i.bind(this),actions:this.#s,genRX:()=>this.#a(p,...i.argNames||[]),plugin:i,el:t,rawKey:s,key:o,value:f,mods:new Map},N=i.keyReq||0;if(a){if(N===2)throw y(`${i.name}KeyNotAllowed`,p)}else if(N===1)throw y(`${i.name}KeyRequired`,p);let R=i.valReq||0;if(c){if(R===2)throw y(`${i.name}ValueNotAllowed`,p)}else if(R===1)throw y(`${i.name}ValueRequired`,p);if(N===3||R===3){if(a&&c)throw y(`${i.name}KeyAndValueProvided`,p);if(!a&&!c)throw y(`${i.name}KeyOrValueRequired`,p)}for(let g of r){let[d,...m]=g.split(".");p.mods.set(ee(d),new Set(m.map(x=>x.toLowerCase())))}let A=i.onLoad(p);A&&(this.#n.has(t)||this.#n.set(t,{id:t.id,fns:[]}),this.#n.get(t)?.fns.push(A)),i?.removeOnLoad&&delete t.dataset[s]}})}#a(e,...t){let s=/(?:\/(?:\\\/|[^\/])*\/|"(?:\\"|[^\"])*"|'(?:\\'|[^'])*'|`(?:\\`|[^`])*`|[^;\n])+/gm,i=e.value.trim().match(s),o=i.length-1,r=i[o];r.startsWith("return")||(i[o]=`return (${r});`);let a=i.join(";"),f=new Map,c=new RegExp(`(?:${P})(.*?)(?:${G})`,"gm");for(let d of a.matchAll(c)){let m=d[1],x=new C("dsEscaped").with(m).value;f.set(x,m),a=a.replace(P+m+G,x)}let B=/@(\w*)\(/gm,p=a.matchAll(B),N=new Set;for(let d of p)N.add(d[1]);let R=new RegExp(`@(${Object.keys(this.#s).join("|")})\\(`,"gm");a=a.replaceAll(R,"ctx.actions.$1.fn(ctx,");let A=e.signals.paths();if(A.length){let d=new RegExp(`\\$(${A.join("|")})(\\W|$)`,"gm");a=a.replaceAll(d,"ctx.signals.signal('$1').value$2")}for(let[d,m]of f)a=a.replace(d,m);let g=`return (()=> {
${a}
})()`;e.fnContent=g;try{let d=new Function("ctx",...t,g);return(...m)=>{try{return d(e,...m)}catch(x){throw y("ExecuteExpression",e,{error:x.message})}}}catch(d){throw y("GenerateExpression",e,{error:d.message})}}#r(e,t){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;t(e);let s=e.firstElementChild;for(;s;)this.#r(s,t),s=s.nextElementSibling}#i(e){let t=this.#n.get(e);if(t){for(let s of t.fns)s();this.#n.delete(e)}}};var _e=new j;_e.load(se,ne,Z);var z=_e;z.apply(document.body);var ot=z;export{ot as Datastar};
//# sourceMappingURL=datastar-core.js.map
