// Datastar v1.0.0-beta.2
var U=/🖕JS_DS🚀/.source,D=U.slice(0,5),j=U.slice(4),k="datastar";var z="1.0.0-beta.2";var me={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ee=me.Morph;var y=(s=>(s[s.Attribute=1]="Attribute",s[s.Watcher=2]="Watcher",s[s.Action=3]="Action",s))(y||{});var ye="computed",Y={type:1,name:ye,keyReq:1,valReq:1,onLoad:({key:t,signals:e,genRX:n})=>{let s=n();e.setComputed(t,s)}};var X=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),Z=t=>t.replace(/(?:^\w|[A-Z]|\b\w)/g,(e,n)=>n===0?e.toLowerCase():e.toUpperCase()).replace(/\s+/g,""),Q=t=>new Function(`return Object.assign({}, ${t})`)();var ee={type:1,name:"signals",removeOnLoad:!0,onLoad:t=>{let{key:e,value:n,genRX:s,signals:i,mods:o}=t,r=o.has("ifmissing");if(e!==""&&!r){let l=n===""?n:s()();i.setValue(e,l)}else{let l=Q(t.value);t.value=JSON.stringify(l);let f=s()();i.merge(f,r)}}};var te={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var R=class{#e=0;#t;constructor(e=k){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}reset(){return this.#e=0,this}get value(){return this.#t+Math.abs(this.#e).toString(36)}};function ne(t){if(t.id)return t.id;let e=new R,n=t;for(;n.parentNode;){if(n.id){e.with(n.id);break}if(n===n.ownerDocument.documentElement)e.with(n.tagName);else for(let s=1,i=t;i.previousElementSibling;i=i.previousElementSibling,s++)e.with(s);n=n.parentNode}return e.value}var ve=`${window.location.origin}/errors`;function B(t,e,n={}){let s=new Error;e=e[0].toUpperCase()+e.slice(1),s.name=`${k} ${t} error`;let i=X(e).replaceAll("-","_"),o=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),r=JSON.stringify(n,null,2);return s.message=`${e}
More info: ${ve}/${t}/${i}?${o}
Context: ${r}`,s}function h(t,e,n={}){return B("internal",e,Object.assign({from:t},n))}function se(t,e,n={}){let s={plugin:{name:e.plugin.name,type:y[e.plugin.type]}};return B("init",t,Object.assign(s,n))}function m(t,e,n={}){let s={plugin:{name:e.plugin.name,type:y[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return B("runtime",t,Object.assign(s,n))}var v="preact-signals",Se=Symbol.for("preact-signals"),_=1,T=2,C=4,N=8,P=16,w=32;function q(){M++}function K(){if(M>1){M--;return}let t,e=!1;for(;A!==void 0;){let n=A;for(A=void 0,G++;n!==void 0;){let s=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~T,!(n._flags&N)&&re(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=s}}if(G=0,M--,e)throw h(v,"BatchError, error",{error:t})}var a;var A,M=0,G=0,V=0;function ie(t){if(a===void 0)return;let e=t._node;if(e===void 0||e._target!==a)return e={_version:0,_source:t,_prevSource:a._sources,_nextSource:void 0,_target:a,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},a._sources!==void 0&&(a._sources._nextSource=e),a._sources=e,t._node=e,a._flags&w&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=a._sources,e._nextSource=void 0,a._sources._nextSource=e,a._sources=e),e}function u(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}u.prototype.brand=Se;u.prototype._refresh=()=>!0;u.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};u.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};u.prototype.subscribe=function(t){return I(()=>{let e=this.value,n=a;a=void 0;try{t(e)}finally{a=n}})};u.prototype.valueOf=function(){return this.value};u.prototype.toString=function(){return`${this.value}`};u.prototype.toJSON=function(){return this.value};u.prototype.peek=function(){let t=a;a=void 0;try{return this.value}finally{a=t}};Object.defineProperty(u.prototype,"value",{get(){let t=ie(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(G>100)throw h(v,"SignalCycleDetected");this._value=t,this._version++,V++,q();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{K()}}}});function re(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function oe(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ae(t){let e=t._sources,n;for(;e!==void 0;){let s=e._prevSource;e._version===-1?(e._source._unsubscribe(e),s!==void 0&&(s._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=s)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=s}t._sources=n}function S(t){u.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=V-1,this._flags=C}S.prototype=new u;S.prototype._refresh=function(){if(this._flags&=~T,this._flags&_)return!1;if((this._flags&(C|w))===w||(this._flags&=~C,this._globalVersion===V))return!0;if(this._globalVersion=V,this._flags|=_,this._version>0&&!re(this))return this._flags&=~_,!0;let t=a;try{oe(this),a=this;let e=this._fn();(this._flags&P||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~P,this._version++)}catch(e){this._value=e,this._flags|=P,this._version++}return a=t,ae(this),this._flags&=~_,!0};S.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=C|w;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}u.prototype._subscribe.call(this,t)};S.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(u.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~w;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};S.prototype._notify=function(){if(!(this._flags&T)){this._flags|=C|T;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(S.prototype,"value",{get(){if(this._flags&_)throw h(v,"SignalCycleDetected");let t=ie(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&P)throw h(v,"GetComputedError",{value:this._value});return this._value}});function le(t){return new S(t)}function ue(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){q();let n=a;a=void 0;try{e()}catch(s){throw t._flags&=~_,t._flags|=N,W(t),h(v,"CleanupEffectError",{error:s})}finally{a=n,K()}}}function W(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,ue(t)}function be(t){if(a!==this)throw h(v,"EndEffectError");ae(this),a=t,this._flags&=~_,this._flags&N&&W(this),K()}function O(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=w}O.prototype._callback=function(){let t=this._start();try{if(this._flags&N||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};O.prototype._start=function(){if(this._flags&_)throw h(v,"SignalCycleDetected");this._flags|=_,this._flags&=~N,ue(this),oe(this),q();let t=a;return a=this,be.bind(this,t)};O.prototype._notify=function(){this._flags&T||(this._flags|=T,this._nextBatchedEffect=A,A=this)};O.prototype._dispose=function(){this._flags|=N,this._flags&_||W(this)};function I(t){let e=new O(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var ce="namespacedSignals";function fe(t,e=!1){let n={};for(let s in t)if(Object.hasOwn(t,s)){if(e&&s.startsWith("_"))continue;let i=t[s];i instanceof u?n[s]=i.value:n[s]=fe(i)}return n}function de(t,e,n=!1){for(let s in e)if(Object.hasOwn(e,s)){if(s.match(/\_\_+/))throw h(ce,"InvalidSignalKey",{key:s});let i=e[s];if(i instanceof Object&&!Array.isArray(i))t[s]||(t[s]={}),de(t[s],i,n);else{if(Object.hasOwn(t,s)){if(n)continue;let r=t[s];if(r instanceof u){r.value=i;continue}}t[s]=new u(i)}}}function pe(t,e){for(let n in t)if(Object.hasOwn(t,n)){let s=t[n];s instanceof u?e(n,s):pe(s,(i,o)=>{e(`${n}.${i}`,o)})}}function xe(t,...e){let n={};for(let s of e){let i=s.split("."),o=t,r=n;for(let p=0;p<i.length-1;p++){let f=i[p];if(!o[f])return{};r[f]||(r[f]={}),o=o[f],r=r[f]}let l=i[i.length-1];r[l]=o[l]}return n}var $=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),s=this.#e;for(let r=0;r<n.length-1;r++){let l=n[r];if(!s[l])return null;s=s[l]}let i=n[n.length-1],o=s[i];if(!o)throw h(ce,"SignalNotFound",{path:e});return o}setSignal(e,n){let s=e.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let l=s[r];i[l]||(i[l]={}),i=i[l]}let o=s[s.length-1];i[o]=n}setComputed(e,n){let s=le(()=>n());this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,n){let s=this.upsertIfMissing(e,n);s.value=n}upsertIfMissing(e,n){let s=e.split("."),i=this.#e;for(let p=0;p<s.length-1;p++){let f=s[p];i[f]||(i[f]={}),i=i[f]}let o=s[s.length-1],r=i[o];if(r instanceof u)return r;let l=new u(n);return i[o]=l,l}remove(...e){for(let n of e){let s=n.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let l=s[r];if(!i[l])return;i=i[l]}let o=s[s.length-1];delete i[o]}}merge(e,n=!1){de(this.#e,e,n)}subset(...e){return xe(this.values(),...e)}walk(e){pe(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return fe(this.#e,e)}JSON(e=!0,n=!1){let s=this.values(n);return e?JSON.stringify(s,null,2):JSON.stringify(s)}toString(){return this.JSON()}};var F=class{#e=new $;#t=[];#s={};#o=[];#n=new Map;get signals(){return this.#e}get version(){return z}load(...e){for(let n of e){let s=this,i={get signals(){return s.#e},effect:r=>I(r),actions:this.#s,apply:this.apply.bind(this),cleanup:this.#i.bind(this),plugin:n},o;switch(n.type){case 2:{let r=n;this.#o.push(r),o=r.onGlobalInit;break}case 3:{this.#s[n.name]=n;break}case 1:{let r=n;this.#t.push(r),o=r.onGlobalInit;break}default:throw se("InvalidPluginType",i)}o&&o(i)}this.#t.sort((n,s)=>{let i=s.name.length-n.name.length;return i!==0?i:n.name.localeCompare(s.name)})}apply(e){this.#r(e,n=>{this.#i(n);for(let s of Object.keys(n.dataset)){let i=this.#t.find(c=>s.startsWith(c.name));if(!i)continue;n.id.length||(n.id=ne(n));let[o,...r]=s.slice(i.name.length).split(/\_\_+/),l=o.length>0;if(l){let c=o.slice(1);o=o.startsWith("-")?c:o[0].toLowerCase()+c}let p=`${n.dataset[s]}`||"",f=p.length>0,L=this,g={get signals(){return L.#e},effect:c=>I(c),apply:this.apply.bind(this),cleanup:this.#i.bind(this),actions:this.#s,genRX:()=>this.#a(g,...i.argNames||[]),plugin:i,el:n,rawKey:s,key:o,value:p,mods:new Map},b=i.keyReq||0;if(l){if(b===2)throw m(`${i.name}KeyNotAllowed`,g)}else if(b===1)throw m(`${i.name}KeyRequired`,g);let x=i.valReq||0;if(f){if(x===2)throw m(`${i.name}ValueNotAllowed`,g)}else if(x===1)throw m(`${i.name}ValueRequired`,g);if(b===3||x===3){if(l&&f)throw m(`${i.name}KeyAndValueProvided`,g);if(!l&&!f)throw m(`${i.name}KeyOrValueRequired`,g)}for(let c of r){let[E,...he]=c.split(".");g.mods.set(Z(E),new Set(he.map(_e=>_e.toLowerCase())))}let d=i.onLoad(g);d&&(this.#n.has(n)||this.#n.set(n,{id:n.id,fns:[]}),this.#n.get(n)?.fns.push(d)),i?.removeOnLoad&&delete n.dataset[s]}})}#a(e,...n){let s="",i=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,o=e.value.trim().match(i);if(o){let d=o.length-1,c=o[d].trim();c.startsWith("return")||(o[d]=`return (${c});`),s=o.join(`;
`)}let r=new Map,l=new RegExp(`(?:${D})(.*?)(?:${j})`,"gm");for(let d of s.matchAll(l)){let c=d[1],E=new R("dsEscaped").with(c).value;r.set(E,c),s=s.replace(D+c+j,E)}let p=/@(\w*)\(/gm,f=s.matchAll(p),L=new Set;for(let d of f)L.add(d[1]);let g=new RegExp(`@(${Object.keys(this.#s).join("|")})\\(`,"gm");s=s.replaceAll(g,"ctx.actions.$1.fn(ctx,");let b=e.signals.paths();if(b.length){let d=new RegExp(`\\$(${b.join("|")})(\\W|$)`,"gm");s=s.replaceAll(d,"ctx.signals.signal('$1').value$2")}for(let[d,c]of r)s=s.replace(d,c);let x=`return (()=> {
${s}
})()`;e.fnContent=x;try{let d=new Function("ctx",...n,x);return(...c)=>{try{return d(e,...c)}catch(E){throw m("ExecuteExpression",e,{error:E.message})}}}catch(d){throw m("GenerateExpression",e,{error:d.message})}}#r(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;let s=e.dataset;if("starIgnore"in s)return null;"starIgnore__self"in s||n(e);let i=e.firstElementChild;for(;i;)this.#r(i,n),i=i.nextElementSibling}#i(e){let n=this.#n.get(e);if(n){for(let s of n.fns)s();this.#n.delete(e)}}};var ge=new F;ge.load(te,ee,Y);var J=ge;J.apply(document.body);var ot=J;export{ot as Datastar};
//# sourceMappingURL=datastar-core.js.map
