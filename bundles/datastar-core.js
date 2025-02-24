// Datastar v1.0.0-beta.7
var Q=/🖕JS_DS🚀/.source,P=Q.slice(0,5),H=Q.slice(4),b="datastar";var Ee={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Oe=Ee.Morph;var v=(n=>(n[n.Attribute=1]="Attribute",n[n.Watcher=2]="Watcher",n[n.Action=3]="Action",n))(v||{});var ee=`${b}-signals`;var B=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,s)=>(s?"-":"")+e.toLowerCase()),O=t=>B(t).replace(/-./g,e=>e[1].toUpperCase()),W=t=>B(t).replace(/-/g,"_"),Te=t=>O(t).replace(/^./,e=>e[0].toUpperCase()),te=t=>new Function(`return Object.assign({}, ${t})`)();var we={kebab:B,snake:W,pascal:Te};function I(t,e){for(let s of e.get("case")||[]){let n=we[s];n&&(t=n(t))}return t}var Ae="computed",se={type:1,name:Ae,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:s,genRX:n})=>{t=I(t,e);let r=n();s.setComputed(t,r)}};var re={type:1,name:"signals",removeOnLoad:()=>!0,onLoad:t=>{let{key:e,mods:s,signals:n,value:r,genRX:i}=t,o=s.has("ifmissing");if(e!==""){let a=I(e,s),l=r===""?r:i()();o?n.upsertIfMissing(a,l):n.setValue(a,l)}else{let a=te(t.value);t.value=JSON.stringify(a);let c=i()();n.merge(c,o)}}};var ie={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var E=class{#e=0;#t;constructor(e=b){this.#t=e}with(e){if(typeof e=="string")for(let s of e.split(""))this.with(s.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=(this.#e<<5)-this.#e+e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function oe(t){if(t.id)return t.id;let e=new E,s=t;for(;s;){if(e.with(s.tagName||""),s.id){e.with(s.id);break}let n=s?.parentNode;n&&e.with([...n.children].indexOf(s)),s=n}return e.string}function ae(t,e){return new E().with(t).with(e).value}function q(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let s=t.dataset;if("starIgnore"in s)return null;"starIgnore__self"in s||e(t);let n=t.firstElementChild;for(;n;)q(n,e),n=n.nextElementSibling}var Ne="https://data-star.dev/errors";function J(t,e,s={}){let n=new Error;n.name=`${b} ${t} error`;let r=W(e),i=new URLSearchParams({metadata:JSON.stringify(s)}).toString(),o=JSON.stringify(s,null,2);return n.message=`${e}
More info: ${Ne}/${t}/${r}?${i}
Context: ${o}`,n}function m(t,e,s={}){return J("internal",e,Object.assign({from:t},s))}function le(t,e,s={}){let n={plugin:{name:e.plugin.name,type:v[e.plugin.type]}};return J("init",t,Object.assign(n,s))}function y(t,e,s={}){let n={plugin:{name:e.plugin.name,type:v[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return J("runtime",t,Object.assign(n,s))}var T="preact-signals",Re=Symbol.for("preact-signals"),_=1,w=2,D=4,N=8,$=16,A=32;function z(){L++}function Y(){if(L>1){L--;return}let t,e=!1;for(;k!==void 0;){let s=k;for(k=void 0,U++;s!==void 0;){let n=s._nextBatchedEffect;if(s._nextBatchedEffect=void 0,s._flags&=~w,!(s._flags&N)&&ce(s))try{s._callback()}catch(r){e||(t=r,e=!0)}s=n}}if(U=0,L--,e)throw t}var u;var k,L=0,U=0,F=0;function ue(t){if(u===void 0)return;let e=t._node;if(e===void 0||e._target!==u)return e={_version:0,_source:t,_prevSource:u._sources,_nextSource:void 0,_target:u,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},u._sources!==void 0&&(u._sources._nextSource=e),u._sources=e,t._node=e,u._flags&A&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=u._sources,e._nextSource=void 0,u._sources._nextSource=e,u._sources=e),e}function f(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}f.prototype.brand=Re;f.prototype._refresh=()=>!0;f.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};f.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,s=t._nextTarget;e!==void 0&&(e._nextTarget=s,t._prevTarget=void 0),s!==void 0&&(s._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=s)}};f.prototype.subscribe=function(t){return G(()=>{let e=this.value,s=u;u=void 0;try{t(e)}finally{u=s}})};f.prototype.valueOf=function(){return this.value};f.prototype.toString=function(){return`${this.value}`};f.prototype.toJSON=function(){return this.value};f.prototype.peek=function(){let t=u;u=void 0;try{return this.value}finally{u=t}};Object.defineProperty(f.prototype,"value",{get(){let t=ue(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(U>100)throw m(T,"SignalCycleDetected");let e=this._value,s=t;this._value=t,this._version++,F++,z();try{for(let n=this._targets;n!==void 0;n=n._nextTarget)n._target._notify()}finally{Y()}this?._onChange({old:e,revised:s})}}});function ce(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function fe(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let s=e._source._node;if(s!==void 0&&(e._rollbackNode=s),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function de(t){let e=t._sources,s;for(;e!==void 0;){let n=e._prevSource;e._version===-1?(e._source._unsubscribe(e),n!==void 0&&(n._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=n)):s=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=n}t._sources=s}function S(t){f.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=F-1,this._flags=D}S.prototype=new f;S.prototype._refresh=function(){if(this._flags&=~w,this._flags&_)return!1;if((this._flags&(D|A))===A||(this._flags&=~D,this._globalVersion===F))return!0;if(this._globalVersion=F,this._flags|=_,this._version>0&&!ce(this))return this._flags&=~_,!0;let t=u;try{fe(this),u=this;let e=this._fn();(this._flags&$||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~$,this._version++)}catch(e){this._value=e,this._flags|=$,this._version++}return u=t,de(this),this._flags&=~_,!0};S.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=D|A;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}f.prototype._subscribe.call(this,t)};S.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(f.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~A;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};S.prototype._notify=function(){if(!(this._flags&w)){this._flags|=D|w;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(S.prototype,"value",{get(){if(this._flags&_)throw m(T,"SignalCycleDetected");let t=ue(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&$)throw m(T,"GetComputedError",{value:this._value});return this._value}});function pe(t){return new S(t)}function ge(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){z();let s=u;u=void 0;try{e()}catch(n){throw t._flags&=~_,t._flags|=N,X(t),m(T,"CleanupEffectError",{error:n})}finally{u=s,Y()}}}function X(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,ge(t)}function Ce(t){if(u!==this)throw m(T,"EndEffectError");de(this),u=t,this._flags&=~_,this._flags&N&&X(this),Y()}function V(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=A}V.prototype._callback=function(){let t=this._start();try{if(this._flags&N||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};V.prototype._start=function(){if(this._flags&_)throw m(T,"SignalCycleDetected");this._flags|=_,this._flags&=~N,ge(this),fe(this),z();let t=u;return u=this,Ce.bind(this,t)};V.prototype._notify=function(){this._flags&w||(this._flags|=w,this._nextBatchedEffect=k,k=this)};V.prototype._dispose=function(){this._flags|=N,this._flags&_||X(this)};function G(t){let e=new V(t);try{e._callback()}catch(s){throw e._dispose(),s}return e._dispose.bind(e)}var he="namespacedSignals",R=t=>{document.dispatchEvent(new CustomEvent(ee,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function me(t,e=!1){let s={};for(let n in t)if(Object.hasOwn(t,n)){if(e&&n.startsWith("_"))continue;let r=t[n];r instanceof f?s[n]=r.value:s[n]=me(r)}return s}function _e(t,e,s=!1){let n={added:[],removed:[],updated:[]};for(let r in e)if(Object.hasOwn(e,r)){if(r.match(/\_\_+/))throw m(he,"InvalidSignalKey",{key:r});let i=e[r];if(i instanceof Object&&!Array.isArray(i)){t[r]||(t[r]={});let o=_e(t[r],i,s);n.added.push(...o.added.map(a=>`${r}.${a}`)),n.removed.push(...o.removed.map(a=>`${r}.${a}`)),n.updated.push(...o.updated.map(a=>`${r}.${a}`))}else{if(Object.hasOwn(t,r)){if(s)continue;let l=t[r];if(l instanceof f){let c=l.value;l.value=i,c!==i&&n.updated.push(r);continue}}let a=new f(i);a._onChange=()=>{R({updated:[r]})},t[r]=a,n.added.push(r)}}return n}function ye(t,e){for(let s in t)if(Object.hasOwn(t,s)){let n=t[s];n instanceof f?e(s,n):ye(n,(r,i)=>{e(`${s}.${r}`,i)})}}function Me(t,...e){let s={};for(let n of e){let r=n.split("."),i=t,o=s;for(let l=0;l<r.length-1;l++){let c=r[l];if(!i[c])return{};o[c]||(o[c]={}),i=i[c],o=o[c]}let a=r[r.length-1];o[a]=i[a]}return s}var j=class{#e={};exists(e){return!!this.signal(e)}signal(e){let s=e.split("."),n=this.#e;for(let o=0;o<s.length-1;o++){let a=s[o];if(!n[a])return null;n=n[a]}let r=s[s.length-1],i=n[r];if(!i)throw m(he,"SignalNotFound",{path:e});return i}setSignal(e,s){let n=e.split("."),r=this.#e;for(let o=0;o<n.length-1;o++){let a=n[o];r[a]||(r[a]={}),r=r[a]}let i=n[n.length-1];r[i]=s}setComputed(e,s){let n=pe(()=>s());this.setSignal(e,n)}value(e){return this.signal(e)?.value}setValue(e,s){let{signal:n}=this.upsertIfMissing(e,s),r=n.value;n.value=s,r!==s&&R({updated:[e]})}upsertIfMissing(e,s){let n=e.split("."),r=this.#e;for(let l=0;l<n.length-1;l++){let c=n[l];r[c]||(r[c]={}),r=r[c]}let i=n[n.length-1],o=r[i];if(o instanceof f)return{signal:o,inserted:!1};let a=new f(s);return a._onChange=()=>{R({updated:[e]})},r[i]=a,R({added:[e]}),{signal:a,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let s=Array();for(let n of e){let r=n.split("."),i=this.#e;for(let a=0;a<r.length-1;a++){let l=r[a];if(!i[l])return;i=i[l]}let o=r[r.length-1];delete i[o],s.push(n)}R({removed:s})}merge(e,s=!1){let n=_e(this.#e,e,s);(n.added.length||n.removed.length||n.updated.length)&&R(n)}subset(...e){return Me(this.values(),...e)}walk(e){ye(this.#e,e)}paths(){let e=new Array;return this.walk(s=>e.push(s)),e}values(e=!1){return me(this.#e,e)}JSON(e=!0,s=!1){let n=this.values(s);return e?JSON.stringify(n,null,2):JSON.stringify(n)}toString(){return this.JSON()}};var K=class{constructor(){this.aliasPrefix="";this.#e=new j;this.#t=[];this.#s={};this.#i=[];this.#r=null;this.#n=new Map}#e;#t;#s;#i;#r;#n;get signals(){return this.#e}load(...e){let s=this;for(let n of e){let r={get signals(){return s.#e},effect:o=>G(o),actions:this.#s,plugin:n,apply:s.apply.bind(s)},i;switch(n.type){case 2:{let o=n;this.#i.push(o),i=o.onGlobalInit;break}case 3:{this.#s[n.name]=n;break}case 1:{let o=n;this.#t.push(o),i=o.onGlobalInit;break}default:throw le("InvalidPluginType",r)}i&&i(r)}this.#t.sort((n,r)=>{let i=r.name.length-n.name.length;return i!==0?i:n.name.localeCompare(r.name)})}apply(e=document.documentElement){q(e,s=>{let n=new Array,r=this.#n.get(s)||new Map,i=new Map([...r]),o=new Map;for(let a of Object.keys(s.dataset)){if(!a.startsWith(this.aliasPrefix))break;let l=s.dataset[a]||"",c=ae(a,l);o.set(a,c),r.has(c)?i.delete(c):n.push(a)}for(let[a,l]of i)l();for(let a of n){let l=o.get(a);this.#a(s,a,l)}}),this.#o()}#o(){this.#r||(this.#r=new MutationObserver(e=>{let s=new Set,n=new Set;for(let{target:r,type:i,addedNodes:o,removedNodes:a}of e)switch(i){case"childList":{for(let l of a)s.add(l);for(let l of o)n.add(l)}break;case"attributes":{n.add(r);break}}for(let r of s){let i=this.#n.get(r);if(i){for(let[o,a]of i)a(),i.delete(o);i.size===0&&this.#n.delete(r)}}for(let r of n)this.apply(r)}),this.#r.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}#a(e,s,n){let r=O(s.slice(this.aliasPrefix.length)),i=this.#t.find(p=>r.startsWith(p.name));if(!i)return;e.id.length||(e.id=oe(e));let[o,...a]=r.slice(i.name.length).split(/\_\_+/),l=o.length>0;l&&(o=O(o));let c=e.dataset[s]||"",C=c.length>0,M=this,h={get signals(){return M.#e},apply:M.apply.bind(M),effect:p=>G(p),actions:this.#s,genRX:()=>this.#l(h,...i.argNames||[]),plugin:i,el:e,rawKey:r,key:o,value:c,mods:new Map},x=i.keyReq||0;if(l){if(x===2)throw y(`${i.name}KeyNotAllowed`,h)}else if(x===1)throw y(`${i.name}KeyRequired`,h);let d=i.valReq||0;if(C){if(d===2)throw y(`${i.name}ValueNotAllowed`,h)}else if(d===1)throw y(`${i.name}ValueRequired`,h);if(x===3||d===3){if(l&&C)throw y(`${i.name}KeyAndValueProvided`,h);if(!l&&!C)throw y(`${i.name}KeyOrValueRequired`,h)}for(let p of a){let[Se,...xe]=p.split(".");h.mods.set(O(Se),new Set(xe.map(be=>be.toLowerCase())))}let g=i.onLoad(h);if(g){let p=this.#n.get(e);p||(p=new Map,this.#n.set(e,p)),p.set(n,g)}}#l(e,...s){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=e.value.trim().match(r);if(i){let d=i.length-1,g=i[d].trim();g.startsWith("return")||(i[d]=`return (${g});`),n=i.join(`;
`)}let o=new Map,a=new RegExp(`(?:${P})(.*?)(?:${H})`,"gm");for(let d of n.matchAll(a)){let g=d[1],p=new E("dsEscaped").with(g).string;o.set(p,g),n=n.replace(P+g+H,p)}let l=/@(\w*)\(/gm,c=n.matchAll(l),C=new Set;for(let d of c)C.add(d[1]);let M=new RegExp(`@(${Object.keys(this.#s).join("|")})\\(`,"gm");n=n.replaceAll(M,"ctx.actions.$1.fn(ctx,");let h=e.signals.paths();if(h.length){let d=new RegExp(`\\$(${h.join("|")})(\\W|$)`,"gm");n=n.replaceAll(d,"ctx.signals.signal('$1').value$2")}for(let[d,g]of o)n=n.replace(d,g);let x=`return (()=> {
${n}
})()`;e.fnContent=x;try{let d=new Function("ctx",...s,x);return(...g)=>{try{return d(e,...g)}catch(p){throw y("ExecuteExpression",e,{error:p.message})}}}catch(d){throw y("GenerateExpression",e,{error:d.message})}}};var ve=new K;ve.load(ie,re,se);var Z=ve;Z.apply();var _t=Z;export{_t as Datastar};
//# sourceMappingURL=datastar-core.js.map
