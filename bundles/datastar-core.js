// Datastar v0.22.0
var J=/🖕JS_DS🚀/.source,D=J.slice(0,5),$=J.slice(4),L="datastar";var H="0.22.0";var _e={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ee=_e.Morph;var Re=/🖕JS_DS🚀/.source,me="computed",z={type:1,name:me,keyReq:1,valReq:1,onLoad:({key:t,signals:e,genRX:n})=>{let s=n();e.setComputed(t,s)}};var Y=t=>t.replace(/(?:^\w|[A-Z]|\b\w)/g,(e,n)=>n===0?e.toLowerCase():e.toUpperCase()).replace(/\s+/g,""),X=t=>new Function(`return Object.assign({}, ${t})`)();var Z={type:1,name:"signals",valReq:1,removeOnLoad:!0,onLoad:t=>{let{key:e,genRX:n,signals:s,mods:i}=t,o=i.has("ifmissing");if(e!==""&&!o)s.setValue(e,n()());else{let r=X(t.value);t.value=JSON.stringify(r);let u=n()();s.merge(u,o)}}};var Q={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var R=class{#e=0;#t;constructor(e=L){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}reset(){return this.#e=0,this}get value(){return this.#t+Math.abs(this.#e).toString(36)}};function ee(t){if(t.id)return t.id;let e=new R,n=t;for(;n.parentNode;){if(n.id){e.with(n.id);break}if(n===n.ownerDocument.documentElement)e.with(n.tagName);else{for(let s=1,i=t;i.previousElementSibling;i=i.previousElementSibling,s++)e.with(s);n=n.parentNode}n=n.parentNode}return L+e.value}var ve="https://data-star.dev/errors";var f=(t,e)=>{let n=new Error;t=t.charAt(0).toUpperCase()+t.slice(1),n.name=`error ${t}`;let s=`${ve}/${t}?${new URLSearchParams(e)}`;return n.message=`for more info see ${s}`,n};var ye=Symbol.for("preact-signals"),h=1,E=2,O=4,w=8,M=16,T=32;function q(){k++}function U(){if(k>1){k--;return}let t,e=!1;for(;A!==void 0;){let n=A;for(A=void 0,G++;n!==void 0;){let s=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~E,!(n._flags&w)&&ne(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=s}}if(G=0,k--,e)throw f("BatchError, error",{error:t})}var a;var A,k=0,G=0,P=0;function te(t){if(a===void 0)return;let e=t._node;if(e===void 0||e._target!==a)return e={_version:0,_source:t,_prevSource:a._sources,_nextSource:void 0,_target:a,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},a._sources!==void 0&&(a._sources._nextSource=e),a._sources=e,t._node=e,a._flags&T&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=a._sources,e._nextSource=void 0,a._sources._nextSource=e,a._sources=e),e}function d(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}d.prototype.brand=ye;d.prototype._refresh=()=>!0;d.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};d.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};d.prototype.subscribe=function(t){return V(()=>{let e=this.value,n=a;a=void 0;try{t(e)}finally{a=n}})};d.prototype.valueOf=function(){return this.value};d.prototype.toString=function(){return`${this.value}`};d.prototype.toJSON=function(){return this.value};d.prototype.peek=function(){let t=a;a=void 0;try{return this.value}finally{a=t}};Object.defineProperty(d.prototype,"value",{get(){let t=te(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(G>100)throw f("SignalCycleDetected");this._value=t,this._version++,P++,q();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{U()}}}});function ne(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function se(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ie(t){let e=t._sources,n;for(;e!==void 0;){let s=e._prevSource;e._version===-1?(e._source._unsubscribe(e),s!==void 0&&(s._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=s)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=s}t._sources=n}function y(t){d.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=P-1,this._flags=O}y.prototype=new d;y.prototype._refresh=function(){if(this._flags&=~E,this._flags&h)return!1;if((this._flags&(O|T))===T||(this._flags&=~O,this._globalVersion===P))return!0;if(this._globalVersion=P,this._flags|=h,this._version>0&&!ne(this))return this._flags&=~h,!0;let t=a;try{se(this),a=this;let e=this._fn();(this._flags&M||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~M,this._version++)}catch(e){this._value=e,this._flags|=M,this._version++}return a=t,ie(this),this._flags&=~h,!0};y.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=O|T;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}d.prototype._subscribe.call(this,t)};y.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(d.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~T;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};y.prototype._notify=function(){if(!(this._flags&E)){this._flags|=O|E;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(y.prototype,"value",{get(){if(this._flags&h)throw f("SignalCycleDetected");let t=te(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&M)throw f("GetComputedError",{value:this._value});return this._value}});function re(t){return new y(t)}function oe(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){q();let n=a;a=void 0;try{e()}catch(s){throw t._flags&=~h,t._flags|=w,K(t),f("CleanupEffectError",{error:s})}finally{a=n,U()}}}function K(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,oe(t)}function Se(t){if(a!==this)throw f("EndEffectError");ie(this),a=t,this._flags&=~h,this._flags&w&&K(this),U()}function C(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=T}C.prototype._callback=function(){let t=this._start();try{if(this._flags&w||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};C.prototype._start=function(){if(this._flags&h)throw f("SignalCycleDetected");this._flags|=h,this._flags&=~w,oe(this),se(this),q();let t=a;return a=this,Se.bind(this,t)};C.prototype._notify=function(){this._flags&E||(this._flags|=E,this._nextBatchedEffect=A,A=this)};C.prototype._dispose=function(){this._flags|=w,this._flags&h||K(this)};function V(t){let e=new C(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}function ae(t,e=!1){let n={};for(let s in t)if(Object.hasOwn(t,s)){if(e&&s.startsWith("_"))continue;let i=t[s];i instanceof d?n[s]=i.value:n[s]=ae(i)}return n}function le(t,e,n=!1){for(let s in e)if(Object.hasOwn(e,s)){if(s.match(/\_\_+/))throw f("InvalidSignalKey",{key:s});let i=e[s];if(i instanceof Object&&!Array.isArray(i))t[s]||(t[s]={}),le(t[s],i,n);else{if(Object.hasOwn(t,s)){if(n)continue;let r=t[s];if(r instanceof d){r.value=i;continue}}t[s]=new d(i)}}}function ue(t,e){for(let n in t)if(Object.hasOwn(t,n)){let s=t[n];s instanceof d?e(n,s):ue(s,(i,o)=>{e(`${n}.${i}`,o)})}}function xe(t,...e){let n={};for(let s of e){let i=s.split("."),o=t,r=n;for(let u=0;u<i.length-1;u++){let p=i[u];if(!o[p])return{};r[p]||(r[p]={}),o=o[p],r=r[p]}let l=i[i.length-1];r[l]=o[l]}return n}var F=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),s=this.#e;for(let r=0;r<n.length-1;r++){let l=n[r];if(!s[l])return null;s=s[l]}let i=n[n.length-1],o=s[i];if(!o)throw f("SignalNotFound",{path:e});return o}setSignal(e,n){let s=e.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let l=s[r];i[l]||(i[l]={}),i=i[l]}let o=s[s.length-1];i[o]=n}setComputed(e,n){let s=re(()=>n());this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,n){let s=this.upsert(e,n);s.value=n}upsert(e,n){let s=e.split("."),i=this.#e;for(let u=0;u<s.length-1;u++){let p=s[u];i[p]||(i[p]={}),i=i[p]}let o=s[s.length-1],r=i[o];if(r)return(r.value===null||r.value===void 0)&&(r.value=n),r;let l=new d(n);return i[o]=l,l}remove(...e){for(let n of e){let s=n.split("."),i=this.#e;for(let r=0;r<s.length-1;r++){let l=s[r];if(!i[l])return;i=i[l]}let o=s[s.length-1];delete i[o]}}merge(e,n=!1){le(this.#e,e,n)}subset(...e){return xe(this.values(),...e)}walk(e){ue(this.#e,e)}values(e=!1){return ae(this.#e,e)}JSON(e=!0,n=!1){let s=this.values(n);return e?JSON.stringify(s,null,2):JSON.stringify(s)}toString(){return this.JSON()}};var be=new RegExp(`(?:${D})(.*?)(?:${$})`,"gm"),I=class{#e=new F;#t=[];#r=[];#s={};#a=[];#n=new Map;get signals(){return this.#e}get version(){return H}load(...e){for(let n of e){let s;switch(n.type){case 0:{this.#r.push(n);break}case 2:{let i=n;this.#a.push(i),s=i.onGlobalInit;break}case 3:{this.#s[n.name]=n;break}case 1:{let i=n;this.#t.push(i),s=i.onGlobalInit;break}default:throw f("InvalidPluginType",{name:n.name,type:n.type})}if(s){let i=this;s({get signals(){return i.#e},effect:o=>V(o),actions:this.#s,apply:this.apply.bind(this),cleanup:this.#i.bind(this)})}}this.apply(document.body)}apply(e){let n=new Set;this.#t.forEach((s,i)=>{this.#o(e,o=>{if(!("starIgnore"in o.dataset)){i||this.#i(o);for(let r in o.dataset){if(!r.startsWith(s.name))continue;let l=r.slice(s.name.length),[u,...p]=l.split(/\_\_+/),S=u.length>0;S&&(u.startsWith("-_")?u=u.slice(1):u=u[0].toLowerCase()+u.slice(1));let x=`${o.dataset[r]}`||"",m=x.length>0,b=s.keyReq||0;if(S){if(b===2)throw f(`${s.name}KeyNotAllowed`,{key:u})}else if(b===1)throw f(`${s.name}KeyRequired`);let c=s.valReq||0;if(m){if(c===2)throw f(`${s.name}ValueNotAllowed`,{rawValue:x})}else if(c===1)throw f(`${s.name}ValueRequired`);if(b===3||c===3){if(S&&m)throw f(`${s.name}KeyAndValueProvided`);if(!S&&!m)throw f(`${s.name}KeyOrValueRequired`)}o.id.length||(o.id=ee(o));let g=new Map;for(let v of p){let[pe,...ge]=v.split(".");g.set(Y(pe),new Set(ge.map(he=>he.toLowerCase())))}let _=this,N={get signals(){return _.#e},effect:v=>V(v),apply:_.apply.bind(_),cleanup:_.#i.bind(_),actions:_.#s,genRX:()=>this.#l(N,...s.argNames||[]),el:o,rawKey:r,rawValue:x,key:u,value:x,mods:g};n.clear();let de=[...s.macros?.pre||[],...this.#r,...s.macros?.post||[]];for(let v of de)n.has(v)||(n.add(v),N.value=v.fn(N,N.value));let W=s.onLoad(N);W&&(this.#n.has(o)||this.#n.set(o,{id:o.id,set:new Set}),this.#n.get(o)?.set.add(W)),s?.removeOnLoad&&delete o.dataset[r]}}})})}#l(e,...n){let s=e.value.split(/;|\n/).map(c=>c.trim()).filter(c=>c!==""),i=s.length-1;s[i].startsWith("return")||(s[i]=`return (${s[i]});`);let r=s.join(`;
`).trim(),l=new Map;for(let c of r.matchAll(be)){let g=c[1],_=new R("dsEscaped").with(g).value;l.set(_,g),r=r.replace(D+g+$,_)}let u=/@(\w*)\(/gm,p=r.matchAll(u),S=new Set;for(let c of p)S.add(c[1]);let x=new RegExp(`@(${Object.keys(this.#s).join("|")})\\(`,"gm");r=r.replaceAll(x,"ctx.actions.$1.fn(ctx,");let m=new Array;if(e.signals.walk(c=>m.push(c)),m.length){let c=new RegExp(`\\$(${m.join("|")})`,"gm");r=r.replaceAll(c,"ctx.signals.signal('$1').value")}for(let[c,g]of l)r=r.replace(c,g);let b=`return (()=> { ${r} })()`;try{let c=new Function("ctx",...n,b);return(...g)=>c(e,...g)}catch(c){throw f("GeneratingExpressionFailed",{error:c,fnContent:b})}}#o(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;n(e);let s=e.firstElementChild;for(;s;)this.#o(s,n),s=s.nextElementSibling}#i(e){let n=this.#n.get(e);if(n){for(let s of n.set)s();this.#n.delete(e)}}};var ce=new I;ce.load(Q,Z,z);var fe=ce;var it=fe;export{it as Datastar};
//# sourceMappingURL=datastar-core.js.map
