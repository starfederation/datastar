// Datastar v1.0.0-beta.2
var Z=/🖕JS_DS🚀/.source,C=Z.slice(0,5),H=Z.slice(4),w="datastar";var be={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Re=be.Morph;var b=(s=>(s[s.Attribute=1]="Attribute",s[s.Watcher=2]="Watcher",s[s.Action=3]="Action",s))(b||{});var Q=`${w}-signals`;var Ee="computed",te={type:1,name:Ee,keyReq:1,valReq:1,onLoad:({key:t,signals:e,genRX:n})=>{let s=n();e.setComputed(t,s)}};var B=t=>t[0].toLowerCase()+t.slice(1),W=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),q=t=>W(t).replace(/-./g,e=>e[1].toUpperCase()),ne=t=>new Function(`return Object.assign({}, ${t})`)();var se={type:1,name:"signals",removeOnLoad:()=>!0,onLoad:t=>{let{key:e,value:n,genRX:s,signals:r,mods:i}=t,o=i.has("ifmissing");if(e!==""&&!o){let a=n===""?n:s()();r.setValue(e,a)}else{let a=ne(t.value);t.value=JSON.stringify(a);let p=s()();r.merge(p,o)}}};var re={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var O=class{#e=0;#t;constructor(e=w){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}reset(){return this.#e=0,this}get value(){return this.#t+Math.abs(this.#e).toString(36)}};function ie(t){if(t.id)return t.id;let e=new O,n=t;for(;n.parentNode;){if(n.id){e.with(n.id);break}if(n===n.ownerDocument.documentElement)e.with(n.tagName);else for(let s=1,r=t;r.previousElementSibling;r=r.previousElementSibling,s++)e.with(s);n=n.parentNode}return e.value}function oe(t,e,n=!1,s=!0){let r=-1,i=()=>r&&clearTimeout(r);return(...o)=>{i(),n&&!r&&t(...o),r=setTimeout(()=>{s&&t(...o),i()},e)}}var Te=`${window.location.origin}/errors`;function J(t,e,n={}){let s=new Error;e=e[0].toUpperCase()+e.slice(1),s.name=`${w} ${t} error`;let r=W(e).replaceAll("-","_"),i=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),o=JSON.stringify(n,null,2);return s.message=`${e}
More info: ${Te}/${t}/${r}?${i}
Context: ${o}`,s}function _(t,e,n={}){return J("internal",e,Object.assign({from:t},n))}function ae(t,e,n={}){let s={plugin:{name:e.plugin.name,type:b[e.plugin.type]}};return J("init",t,Object.assign(s,n))}function x(t,e,n={}){let s={plugin:{name:e.plugin.name,type:b[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return J("runtime",t,Object.assign(s,n))}var E="preact-signals",we=Symbol.for("preact-signals"),y=1,A=2,k=4,R=8,$=16,N=32;function z(){F++}function Y(){if(F>1){F--;return}let t,e=!1;for(;D!==void 0;){let n=D;for(D=void 0,U++;n!==void 0;){let s=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~A,!(n._flags&R)&&ue(n))try{n._callback()}catch(r){e||(t=r,e=!0)}n=s}}if(U=0,F--,e)throw _(E,"BatchError, error",{error:t})}var l;var D,F=0,U=0,I=0;function le(t){if(l===void 0)return;let e=t._node;if(e===void 0||e._target!==l)return e={_version:0,_source:t,_prevSource:l._sources,_nextSource:void 0,_target:l,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},l._sources!==void 0&&(l._sources._nextSource=e),l._sources=e,t._node=e,l._flags&N&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=l._sources,e._nextSource=void 0,l._sources._nextSource=e,l._sources=e),e}function f(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}f.prototype.brand=we;f.prototype._refresh=()=>!0;f.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};f.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};f.prototype.subscribe=function(t){return L(()=>{let e=this.value,n=l;l=void 0;try{t(e)}finally{l=n}})};f.prototype.valueOf=function(){return this.value};f.prototype.toString=function(){return`${this.value}`};f.prototype.toJSON=function(){return this.value};f.prototype.peek=function(){let t=l;l=void 0;try{return this.value}finally{l=t}};Object.defineProperty(f.prototype,"value",{get(){let t=le(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(U>100)throw _(E,"SignalCycleDetected");this._value=t,this._version++,I++,z();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{Y()}}}});function ue(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function ce(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function fe(t){let e=t._sources,n;for(;e!==void 0;){let s=e._prevSource;e._version===-1?(e._source._unsubscribe(e),s!==void 0&&(s._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=s)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=s}t._sources=n}function T(t){f.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=I-1,this._flags=k}T.prototype=new f;T.prototype._refresh=function(){if(this._flags&=~A,this._flags&y)return!1;if((this._flags&(k|N))===N||(this._flags&=~k,this._globalVersion===I))return!0;if(this._globalVersion=I,this._flags|=y,this._version>0&&!ue(this))return this._flags&=~y,!0;let t=l;try{ce(this),l=this;let e=this._fn();(this._flags&$||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~$,this._version++)}catch(e){this._value=e,this._flags|=$,this._version++}return l=t,fe(this),this._flags&=~y,!0};T.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=k|N;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}f.prototype._subscribe.call(this,t)};T.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(f.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~N;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};T.prototype._notify=function(){if(!(this._flags&A)){this._flags|=k|A;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(T.prototype,"value",{get(){if(this._flags&y)throw _(E,"SignalCycleDetected");let t=le(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&$)throw _(E,"GetComputedError",{value:this._value});return this._value}});function de(t){return new T(t)}function pe(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){z();let n=l;l=void 0;try{e()}catch(s){throw t._flags&=~y,t._flags|=R,X(t),_(E,"CleanupEffectError",{error:s})}finally{l=n,Y()}}}function X(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,pe(t)}function Ae(t){if(l!==this)throw _(E,"EndEffectError");fe(this),l=t,this._flags&=~y,this._flags&R&&X(this),Y()}function M(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=N}M.prototype._callback=function(){let t=this._start();try{if(this._flags&R||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};M.prototype._start=function(){if(this._flags&y)throw _(E,"SignalCycleDetected");this._flags|=y,this._flags&=~R,pe(this),ce(this),z();let t=l;return l=this,Ae.bind(this,t)};M.prototype._notify=function(){this._flags&A||(this._flags|=A,this._nextBatchedEffect=D,D=this)};M.prototype._dispose=function(){this._flags|=R,this._flags&y||X(this)};function L(t){let e=new M(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var ge="namespacedSignals",K=t=>{document.dispatchEvent(new CustomEvent(Q,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function he(t,e=!1){let n={};for(let s in t)if(Object.hasOwn(t,s)){if(e&&s.startsWith("_"))continue;let r=t[s];r instanceof f?n[s]=r.value:n[s]=he(r)}return n}function me(t,e,n=!1){let s={added:[],removed:[],updated:[]};for(let r in e)if(Object.hasOwn(e,r)){if(r.match(/\_\_+/))throw _(ge,"InvalidSignalKey",{key:r});let i=e[r];if(i instanceof Object&&!Array.isArray(i)){t[r]||(t[r]={});let o=me(t[r],i,n);s.added.push(...o.added.map(a=>`${r}.${a}`)),s.removed.push(...o.removed.map(a=>`${r}.${a}`)),s.updated.push(...o.updated.map(a=>`${r}.${a}`))}else{if(Object.hasOwn(t,r)){if(n)continue;let a=t[r];if(a instanceof f){let c=a.value;a.value=i,c!==i&&s.updated.push(r);continue}}t[r]=new f(i),s.added.push(r)}}return s}function _e(t,e){for(let n in t)if(Object.hasOwn(t,n)){let s=t[n];s instanceof f?e(n,s):_e(s,(r,i)=>{e(`${n}.${r}`,i)})}}function Ne(t,...e){let n={};for(let s of e){let r=s.split("."),i=t,o=n;for(let c=0;c<r.length-1;c++){let p=r[c];if(!i[p])return{};o[p]||(o[p]={}),i=i[p],o=o[p]}let a=r[r.length-1];o[a]=i[a]}return n}var G=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),s=this.#e;for(let o=0;o<n.length-1;o++){let a=n[o];if(!s[a])return null;s=s[a]}let r=n[n.length-1],i=s[r];if(!i)throw _(ge,"SignalNotFound",{path:e});return i}setSignal(e,n){let s=e.split("."),r=this.#e;for(let o=0;o<s.length-1;o++){let a=s[o];r[a]||(r[a]={}),r=r[a]}let i=s[s.length-1];r[i]=n}setComputed(e,n){let s=de(()=>n());this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,n){let s=this.upsertIfMissing(e,n),r=s.value;s.value=n,r!==n&&K({updated:[e]})}upsertIfMissing(e,n){let s=e.split("."),r=this.#e;for(let c=0;c<s.length-1;c++){let p=s[c];r[p]||(r[p]={}),r=r[p]}let i=s[s.length-1],o=r[i];if(o instanceof f)return o;let a=new f(n);return r[i]=a,K({added:[e]}),a}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let s of e){let r=s.split("."),i=this.#e;for(let a=0;a<r.length-1;a++){let c=r[a];if(!i[c])return;i=i[c]}let o=r[r.length-1];delete i[o],n.push(s)}K({removed:n})}merge(e,n=!1){let s=me(this.#e,e,n);(s.added.length||s.removed.length||s.updated.length)&&K(s)}subset(...e){return Ne(this.values(),...e)}walk(e){_e(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return he(this.#e,e)}JSON(e=!0,n=!1){let s=this.values(n);return e?JSON.stringify(s,null,2):JSON.stringify(s)}toString(){return this.JSON()}};var ve=(t,e)=>`${t}${C}${e}`,j=class{constructor(){this.aliasPrefix="ds";this.#e=new G;this.#t=[];this.#s={};this.#l=[];this.#n=new Map;this.#u=oe(()=>{this.#r(document.body)},1);let e="data-";new MutationObserver(s=>{for(let{target:r,type:i,attributeName:o,oldValue:a,addedNodes:c,removedNodes:p}of s)switch(i){case"childList":{for(let S of p){let d=S,m=this.#n.get(d);if(m){for(let[v,u]of m)u();this.#n.delete(d)}}for(let S of c){let d=S;this.#r(d)}}break;case"attributes":{{let S=e+(this.aliasPrefix?this.aliasPrefix+"-":"");if(!o?.startsWith(S))break;let d=r,m=q(o.slice(e.length)),v=this.#o(m),u=this.#c(v);if(!u)return;let h=u.removeOnLoad;if(h&&h(v)===!0)break;if(a!==null&&d.dataset[m]!==a){let g=this.#n.get(d);if(g){let P=ve(m,a),V=g.get(P);V&&(V(),g.delete(P))}}this.#i(d,m)}break}}}).observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0})}#e;#t;#s;#l;#n;get signals(){return this.#e}load(...e){for(let n of e){let s=this,r={get signals(){return s.#e},effect:o=>L(o),actions:this.#s,plugin:n},i;switch(n.type){case 2:{let o=n;this.#l.push(o),i=o.onGlobalInit;break}case 3:{this.#s[n.name]=n;break}case 1:{let o=n;this.#t.push(o),i=o.onGlobalInit;break}default:throw ae("InvalidPluginType",r)}i&&i(r)}this.#t.sort((n,s)=>{let r=s.name.length-n.name.length;return r!==0?r:n.name.localeCompare(s.name)}),this.#u()}#u;#r(e){this.#a(e,n=>{let s=this.#n.get(n);if(s){for(let[,r]of s)r();this.#n.delete(n)}for(let r of Object.keys(n.dataset))this.#i(n,r)})}#i(e,n){let s=this.#o(n),r=this.#t.find(g=>s.startsWith(g.name));if(!r)return;e.id.length||(e.id=ie(e));let[i,...o]=s.slice(r.name.length).split(/\_\_+/),a=i.length>0;a&&(i=i.startsWith("-")?i.slice(1):B(i));let c=e.dataset[n]||"",p=c.length>0,S=this,d={get signals(){return S.#e},effect:g=>L(g),actions:this.#s,genRX:()=>this.#f(d,...r.argNames||[]),plugin:r,el:e,rawKey:s,key:i,value:c,mods:new Map},m=r.keyReq||0;if(a){if(m===2)throw x(`${r.name}KeyNotAllowed`,d)}else if(m===1)throw x(`${r.name}KeyRequired`,d);let v=r.valReq||0;if(p){if(v===2)throw x(`${r.name}ValueNotAllowed`,d)}else if(v===1)throw x(`${r.name}ValueRequired`,d);if(m===3||v===3){if(a&&p)throw x(`${r.name}KeyAndValueProvided`,d);if(!a&&!p)throw x(`${r.name}KeyOrValueRequired`,d)}for(let g of o){let[P,...V]=g.split(".");d.mods.set(q(P),new Set(V.map(xe=>xe.toLowerCase())))}let u=r.onLoad(d);if(u){let g=this.#n.get(e);g||(g=new Map,this.#n.set(e,g)),g.set(ve(s,c),u)}let h=r.removeOnLoad;h&&h(s)===!0&&delete e.dataset[n]}#o(e){return B(e.slice(this.aliasPrefix.length))}#c(e){return this.#t.find(n=>e.startsWith(n.name))}#f(e,...n){let s="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=e.value.trim().match(r);if(i){let u=i.length-1,h=i[u].trim();h.startsWith("return")||(i[u]=`return (${h});`),s=i.join(`;
`)}let o=new Map,a=new RegExp(`(?:${C})(.*?)(?:${H})`,"gm");for(let u of s.matchAll(a)){let h=u[1],g=new O("dsEscaped").with(h).value;o.set(g,h),s=s.replace(C+h+H,g)}let c=/@(\w*)\(/gm,p=s.matchAll(c),S=new Set;for(let u of p)S.add(u[1]);let d=new RegExp(`@(${Object.keys(this.#s).join("|")})\\(`,"gm");s=s.replaceAll(d,"ctx.actions.$1.fn(ctx,");let m=e.signals.paths();if(m.length){let u=new RegExp(`\\$(${m.join("|")})(\\W|$)`,"gm");s=s.replaceAll(u,"ctx.signals.signal('$1').value$2")}for(let[u,h]of o)s=s.replace(u,h);let v=`return (()=> {
${s}
})()`;e.fnContent=v;try{let u=new Function("ctx",...n,v);return(...h)=>{try{return u(e,...h)}catch(g){throw x("ExecuteExpression",e,{error:g.message})}}}catch(u){throw x("GenerateExpression",e,{error:u.message})}}#a(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;let s=e.dataset;if("starIgnore"in s)return null;"starIgnore__self"in s||n(e);let r=e.firstElementChild;for(;r;)this.#a(r,n),r=r.nextElementSibling}};var ye=new j;ye.load(re,se,te);var Se=ye;var ht=Se;export{ht as Datastar};
//# sourceMappingURL=datastar-core.js.map
