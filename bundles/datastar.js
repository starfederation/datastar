// Datastar v1.0.0-beta.5
var Ue=/🖕JS_DS🚀/.source,ie=Ue.slice(0,5),Ce=Ue.slice(4),O="datastar";var je="Datastar-Request",ve=300,Be=1e3,Ke="type module",ye=!1,Je=!1,ze=!0,W={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ye=W.Morph,k={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var M=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(M||{});var oe=`${O}-signals`;var B=t=>t.trim()==="true",se=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),j=t=>se(t).replace(/-./g,e=>e[1].toUpperCase()),Le=t=>se(t).replace(/-/g,"_"),vn=t=>j(t).replace(/^./,e=>e[0].toUpperCase()),be=t=>new Function(`return Object.assign({}, ${t})`)(),K=t=>t.startsWith("$")?t.slice(1):t,yn={kebab:se,snake:Le,pascal:vn};function D(t,e){for(let n of e.get("case")||[]){let r=yn[n];r&&(t=r(t))}return t}var bn="computed",Xe={type:1,name:bn,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=D(t,e);let i=r();n.setComputed(t,i)}};var Qe={type:1,name:"signals",removeOnLoad:()=>!0,onLoad:t=>{let{key:e,mods:n,signals:r,value:i,genRX:s}=t,l=n.has("ifmissing");if(e!==""){let u=D(e,n),m=i===""?i:s()();l?r.upsertIfMissing(u,m):r.setValue(u,m)}else{let u=be(t.value);t.value=JSON.stringify(u);let T=s()();r.merge(T,l)}}};var Ze={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ae=class{#e=0;#n;constructor(e=O){this.#n=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}get value(){return this.#n+Math.abs(this.#e).toString(36)}};function Se(t){if(t.id)return t.id;let e=new ae,n=t;for(;n;){if(n.id){e.with(n.id);break}let r=n?.parentNode;r?e.with([...r.children].indexOf(n)):e.with(n.tagName),n=r}return e.value}function le(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)le(r,e),r=r.nextElementSibling}function ue(t,e){return(...n)=>{setTimeout(()=>{t(...n)},e)}}function et(t,e,n=!1,r=!0){let i=-1,s=()=>i&&clearTimeout(i);return(...l)=>{s(),n&&!i&&t(...l),i=setTimeout(()=>{r&&t(...l),s()},e)}}function tt(t,e,n=!0,r=!1){let i=!1;return(...s)=>{i||(n&&t(...s),i=!0,setTimeout(()=>{i=!1,r&&t(...s)},e))}}var Sn="https://data-star.dev/errors";function Ie(t,e,n={}){let r=new Error;r.name=`${O} ${t} error`;let i=Le(e),s=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),l=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${Sn}/${t}/${i}?${s}
Context: ${l}`,r}function q(t,e,n={}){return Ie("internal",e,Object.assign({from:t},n))}function H(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]}};return Ie("init",t,Object.assign(r,n))}function N(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return Ie("runtime",t,Object.assign(r,n))}var X="preact-signals",Tn=Symbol.for("preact-signals"),$=1,Q=2,fe=4,ee=8,Te=16,Z=32;function Ve(){Ee++}function Oe(){if(Ee>1){Ee--;return}let t,e=!1;for(;ce!==void 0;){let n=ce;for(ce=void 0,De++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~Q,!(n._flags&ee)&&rt(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(De=0,Ee--,e)throw t}var P;var ce,Ee=0,De=0,Ae=0;function nt(t){if(P===void 0)return;let e=t._node;if(e===void 0||e._target!==P)return e={_version:0,_source:t,_prevSource:P._sources,_nextSource:void 0,_target:P,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},P._sources!==void 0&&(P._sources._nextSource=e),P._sources=e,t._node=e,P._flags&Z&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=P._sources,e._nextSource=void 0,P._sources._nextSource=e,P._sources=e),e}function C(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}C.prototype.brand=Tn;C.prototype._refresh=()=>!0;C.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};C.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};C.prototype.subscribe=function(t){return _e(()=>{let e=this.value,n=P;P=void 0;try{t(e)}finally{P=n}})};C.prototype.valueOf=function(){return this.value};C.prototype.toString=function(){return`${this.value}`};C.prototype.toJSON=function(){return this.value};C.prototype.peek=function(){let t=P;P=void 0;try{return this.value}finally{P=t}};Object.defineProperty(C.prototype,"value",{get(){let t=nt(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(De>100)throw q(X,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Ae++,Ve();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{Oe()}this?._onChange({old:e,revised:n})}}});function rt(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function it(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ot(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function z(t){C.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Ae-1,this._flags=fe}z.prototype=new C;z.prototype._refresh=function(){if(this._flags&=~Q,this._flags&$)return!1;if((this._flags&(fe|Z))===Z||(this._flags&=~fe,this._globalVersion===Ae))return!0;if(this._globalVersion=Ae,this._flags|=$,this._version>0&&!rt(this))return this._flags&=~$,!0;let t=P;try{it(this),P=this;let e=this._fn();(this._flags&Te||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~Te,this._version++)}catch(e){this._value=e,this._flags|=Te,this._version++}return P=t,ot(this),this._flags&=~$,!0};z.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=fe|Z;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}C.prototype._subscribe.call(this,t)};z.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(C.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~Z;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};z.prototype._notify=function(){if(!(this._flags&Q)){this._flags|=fe|Q;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(z.prototype,"value",{get(){if(this._flags&$)throw q(X,"SignalCycleDetected");let t=nt(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&Te)throw q(X,"GetComputedError",{value:this._value});return this._value}});function st(t){return new z(t)}function at(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){Ve();let n=P;P=void 0;try{e()}catch(r){throw t._flags&=~$,t._flags|=ee,ke(t),q(X,"CleanupEffectError",{error:r})}finally{P=n,Oe()}}}function ke(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,at(t)}function En(t){if(P!==this)throw q(X,"EndEffectError");ot(this),P=t,this._flags&=~$,this._flags&ee&&ke(this),Oe()}function de(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=Z}de.prototype._callback=function(){let t=this._start();try{if(this._flags&ee||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};de.prototype._start=function(){if(this._flags&$)throw q(X,"SignalCycleDetected");this._flags|=$,this._flags&=~ee,at(this),it(this),Ve();let t=P;return P=this,En.bind(this,t)};de.prototype._notify=function(){this._flags&Q||(this._flags|=Q,this._nextBatchedEffect=ce,ce=this)};de.prototype._dispose=function(){this._flags|=ee,this._flags&$||ke(this)};function _e(t){let e=new de(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var lt="namespacedSignals",te=t=>{document.dispatchEvent(new CustomEvent(oe,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function ut(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof C?n[r]=i.value:n[r]=ut(i)}return n}function ct(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw q(lt,"InvalidSignalKey",{key:i});let s=e[i];if(s instanceof Object&&!Array.isArray(s)){t[i]||(t[i]={});let l=ct(t[i],s,n);r.added.push(...l.added.map(u=>`${i}.${u}`)),r.removed.push(...l.removed.map(u=>`${i}.${u}`)),r.updated.push(...l.updated.map(u=>`${i}.${u}`))}else{if(Object.hasOwn(t,i)){if(n)continue;let m=t[i];if(m instanceof C){let T=m.value;m.value=s,T!==s&&r.updated.push(i);continue}}let u=new C(s);u._onChange=()=>{te({updated:[i]})},t[i]=u,r.added.push(i)}}return r}function ft(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof C?e(n,r):ft(r,(i,s)=>{e(`${n}.${i}`,s)})}}function An(t,...e){let n={};for(let r of e){let i=r.split("."),s=t,l=n;for(let m=0;m<i.length-1;m++){let T=i[m];if(!s[T])return{};l[T]||(l[T]={}),s=s[T],l=l[T]}let u=i[i.length-1];l[u]=s[u]}return n}var xe=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let l=0;l<n.length-1;l++){let u=n[l];if(!r[u])return null;r=r[u]}let i=n[n.length-1],s=r[i];if(!s)throw q(lt,"SignalNotFound",{path:e});return s}setSignal(e,n){let r=e.split("."),i=this.#e;for(let l=0;l<r.length-1;l++){let u=r[l];i[u]||(i[u]={}),i=i[u]}let s=r[r.length-1];i[s]=n}setComputed(e,n){let r=st(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let r=this.upsertIfMissing(e,n),i=r.value;r.value=n,i!==n&&te({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let m=0;m<r.length-1;m++){let T=r[m];i[T]||(i[T]={}),i=i[T]}let s=r[r.length-1],l=i[s];if(l instanceof C)return l;let u=new C(n);return u._onChange=()=>{te({updated:[e]})},i[s]=u,te({added:[e]}),u}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let i=r.split("."),s=this.#e;for(let u=0;u<i.length-1;u++){let m=i[u];if(!s[m])return;s=s[m]}let l=i[i.length-1];delete s[l],n.push(r)}te({removed:n})}merge(e,n=!1){let r=ct(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&te(r)}subset(...e){return An(this.values(),...e)}walk(e){ft(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return ut(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var dt=(t,e)=>`${t}${ie}${e}`,Re=class{constructor(){this.aliasPrefix="";this.#e=new xe;this.#n=[];this.#r={};this.#a=[];this.#o=null;this.#t=new Map;this.#l=ue(()=>{this.#s(document.body),this.#u()},1)}#e;#n;#r;#a;#o;#t;get signals(){return this.#e}load(...e){let n=this;for(let r of e){let i={get signals(){return n.#e},effect:l=>_e(l),actions:this.#r,plugin:r,applyAttributePlugin:n.#i.bind(n)},s;switch(r.type){case 2:{let l=r;this.#a.push(l),s=l.onGlobalInit;break}case 3:{this.#r[r.name]=r;break}case 1:{let l=r;this.#n.push(l),s=l.onGlobalInit;break}default:throw H("InvalidPluginType",i)}s&&s(i)}this.#n.sort((r,i)=>{let s=i.name.length-r.name.length;return s!==0?s:r.name.localeCompare(i.name)}),this.#l()}#l;#s(e){le(e,n=>{let r=this.#t.get(n);if(r){for(let[,i]of r)i();this.#t.delete(n)}for(let i of Object.keys(n.dataset))this.#i(n,i)})}#u(){this.#o||(this.#o=new MutationObserver(e=>{for(let{target:n,type:r,attributeName:i,oldValue:s,addedNodes:l,removedNodes:u}of e)switch(r){case"childList":{for(let m of u){let T=m,b=this.#t.get(T);if(b){for(let[w,v]of b)v();this.#t.delete(T)}}for(let m of l){let T=m;this.#s(T)}}break;case"attributes":{{let m="data-",T=m+(this.aliasPrefix?`${this.aliasPrefix}-`:"");if(!i?.startsWith(T))break;let b=n,w=j(i.slice(m.length));if(s!==null&&b.dataset[w]!==s){let v=this.#t.get(b);if(v){let E=dt(w,s),S=v.get(E);S&&(S(),v.delete(E))}}w in b.dataset&&this.#i(b,w)}break}}}),this.#o.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}#i(e,n){let r=n.slice(this.aliasPrefix.length),i=this.#n.find(h=>r.startsWith(h.name));if(!i)return;let s=this.#t.get(e);if(s)for(let[h,o]of s)h.startsWith(n)&&(o(),s.delete(h));e.id.length||(e.id=Se(e));let[l,...u]=r.slice(i.name.length).split(/\_\_+/),m=l.length>0;m&&(l=j(l));let T=e.dataset[n]||"",b=T.length>0,w=this,v={get signals(){return w.#e},applyAttributePlugin:w.#i.bind(w),effect:h=>_e(h),actions:this.#r,genRX:()=>this.#c(v,...i.argNames||[]),plugin:i,el:e,rawKey:r,key:l,value:T,mods:new Map},E=i.keyReq||0;if(m){if(E===2)throw N(`${i.name}KeyNotAllowed`,v)}else if(E===1)throw N(`${i.name}KeyRequired`,v);let S=i.valReq||0;if(b){if(S===2)throw N(`${i.name}ValueNotAllowed`,v)}else if(S===1)throw N(`${i.name}ValueRequired`,v);if(E===3||S===3){if(m&&b)throw N(`${i.name}KeyAndValueProvided`,v);if(!m&&!b)throw N(`${i.name}KeyOrValueRequired`,v)}for(let h of u){let[o,...d]=h.split(".");v.mods.set(j(o),new Set(d.map(c=>c.toLowerCase())))}let A=i.onLoad(v);if(A){let h=this.#t.get(e);h||(h=new Map,this.#t.set(e,h)),h.set(dt(n,T),A)}let y=i.removeOnLoad;y&&y(r)===!0&&delete e.dataset[n]}#c(e,...n){let r="",i=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,s=e.value.trim().match(i);if(s){let S=s.length-1,A=s[S].trim();A.startsWith("return")||(s[S]=`return (${A});`),r=s.join(`;
`)}let l=new Map,u=new RegExp(`(?:${ie})(.*?)(?:${Ce})`,"gm");for(let S of r.matchAll(u)){let A=S[1],y=new ae("dsEscaped").with(A).value;l.set(y,A),r=r.replace(ie+A+Ce,y)}let m=/@(\w*)\(/gm,T=r.matchAll(m),b=new Set;for(let S of T)b.add(S[1]);let w=new RegExp(`@(${Object.keys(this.#r).join("|")})\\(`,"gm");r=r.replaceAll(w,"ctx.actions.$1.fn(ctx,");let v=e.signals.paths();if(v.length){let S=new RegExp(`\\$(${v.join("|")})(\\W|$)`,"gm");r=r.replaceAll(S,"ctx.signals.signal('$1').value$2")}for(let[S,A]of l)r=r.replace(S,A);let E=`return (()=> {
${r}
})()`;e.fnContent=E;try{let S=new Function("ctx",...n,E);return(...A)=>{try{return S(e,...A)}catch(y){throw N("ExecuteExpression",e,{error:y.message})}}}catch(S){throw N("GenerateExpression",e,{error:S.message})}}};var pt=new Re;pt.load(Ze,Qe,Xe);var Fe=pt;async function _n(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function xn(t){let e,n,r,i=!1;return function(l){e===void 0?(e=l,n=0,r=-1):e=wn(e,l);let u=e.length,m=0;for(;n<u;){i&&(e[n]===10&&(m=++n),i=!1);let T=-1;for(;n<u&&T===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-m);break;case 13:i=!0;case 10:T=n;break}if(T===-1)break;t(e.subarray(m,T),r),m=n,r=-1}m===u?e=void 0:m!==0&&(e=e.subarray(m),n-=m)}}function Rn(t,e,n){let r=mt(),i=new TextDecoder;return function(l,u){if(l.length===0)n?.(r),r=mt();else if(u>0){let m=i.decode(l.subarray(0,u)),T=u+(l[u+1]===32?2:1),b=i.decode(l.subarray(T));switch(m){case"data":r.data=r.data?`${r.data}
${b}`:b;break;case"event":r.event=b;break;case"id":t(r.id=b);break;case"retry":{let w=Number.parseInt(b,10);Number.isNaN(w)||e(r.retry=w);break}}}}}function wn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function mt(){return{data:"",event:"",id:"",retry:void 0}}var Mn="text/event-stream",gt="last-event-id";function ht(t,e,{signal:n,headers:r,onopen:i,onmessage:s,onclose:l,onerror:u,openWhenHidden:m,fetch:T,retryInterval:b=1e3,retryScaler:w=2,retryMaxWaitMs:v=3e4,retryMaxCount:E=10,...S}){return new Promise((A,y)=>{let h=0,o={...r};o.accept||(o.accept=Mn);let d;function c(){d.abort(),document.hidden||R()}m||document.addEventListener("visibilitychange",c);let a=0;function g(){document.removeEventListener("visibilitychange",c),window.clearTimeout(a),d.abort()}n?.addEventListener("abort",()=>{g(),A()});let f=T??window.fetch,p=i??function(){};async function R(){d=new AbortController;try{let x=await f(e,{...S,headers:o,signal:d.signal});await p(x),await _n(x.body,xn(Rn(_=>{_?o[gt]=_:delete o[gt]},_=>{b=_},s))),l?.(),g(),A()}catch(x){if(!d.signal.aborted)try{let _=u?.(x)??b;window.clearTimeout(a),a=window.setTimeout(R,_),b*=w,b=Math.min(b,v),h++,h>=E?(g(),y(N("SseMaxRetries",t,{retryMaxCount:E}))):console.error(`Datastar failed to reach ${S.method}: ${e.toString()} retry in ${_}ms`)}catch(_){g(),y(_)}}}R()})}var ne=`${O}-sse`,He=`${O}-settling`,Y=`${O}-swapping`,we="started",Me="finished",vt="error",yt="retrying";function G(t,e){document.addEventListener(ne,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function pe(t,e){document.dispatchEvent(new CustomEvent(ne,{detail:{type:t,argsRaw:e}}))}var bt=t=>`${t}`.includes("text/event-stream"),U=async(t,e,n,r)=>{let{el:{id:i},el:s,signals:l}=t,{headers:u,contentType:m,includeLocal:T,selector:b,openWhenHidden:w,retryInterval:v,retryScaler:E,retryMaxWaitMs:S,retryMaxCount:A,abort:y}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:Be,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),h=e.toLowerCase(),o=()=>{};try{if(pe(we,{elId:i}),!n?.length)throw N("SseNoUrlProvided",t,{action:h});let d={};d[je]=!0,m==="json"&&(d["Content-Type"]="application/json");let c=Object.assign({},d,u),a={method:e,headers:c,openWhenHidden:w,retryInterval:v,retryScaler:E,retryMaxWaitMs:S,retryMaxCount:A,signal:y,onopen:async p=>{if(p.status>=400){let R=p.status.toString();pe(vt,{status:R})}},onmessage:p=>{if(!p.event.startsWith(O))return;let R=p.event,x={},_=p.data.split(`
`);for(let V of _){let L=V.indexOf(" "),Ge=V.slice(0,L),he=x[Ge];he||(he=[],x[Ge]=he);let hn=V.slice(L+1).trim();he.push(hn)}let I={};for(let[V,L]of Object.entries(x))I[V]=L.join(`
`);pe(R,I)},onerror:p=>{if(bt(p))throw N("InvalidContentType",t,{url:n});p&&(console.error(p.message),pe(yt,{message:p.message}))}},g=new URL(n,window.location.origin),f=new URLSearchParams(g.search);if(m==="json"){let p=l.JSON(!1,!T);e==="GET"?f.set(O,p):a.body=p}else if(m==="form"){let p=b?document.querySelector(b):s.closest("form");if(p===null)throw b?N("SseFormNotFound",t,{action:h,selector:b}):N("SseClosestFormNotFound",t,{action:h});if(s!==p){let x=_=>_.preventDefault();p.addEventListener("submit",x),o=()=>p.removeEventListener("submit",x)}if(!p.checkValidity()){p.reportValidity(),o();return}let R=new FormData(p);if(e==="GET"){let x=new URLSearchParams(R);for(let[_,I]of x)f.set(_,I)}else a.body=R}else throw N("SseInvalidContentType",t,{action:h,contentType:m});g.search=f.toString();try{await ht(t,g.toString(),a)}catch(p){if(!bt(p))throw N("SseFetchFailed",t,{method:e,url:n,error:p})}}finally{pe(Me,{elId:i}),o()}};var St={type:3,name:"delete",fn:async(t,e,n)=>U(t,"DELETE",e,{...n})};var Tt={type:3,name:"get",fn:async(t,e,n)=>U(t,"GET",e,{...n})};var Et={type:3,name:"patch",fn:async(t,e,n)=>U(t,"PATCH",e,{...n})};var At={type:3,name:"post",fn:async(t,e,n)=>U(t,"POST",e,{...n})};var _t={type:3,name:"put",fn:async(t,e,n)=>U(t,"PUT",e,{...n})};var xt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let s=e?D(e,n):K(i),l=r.upsertIfMissing(s,!1),u=m=>{let{type:T,argsRaw:{elId:b}}=m.detail;if(b===t.id)switch(T){case we:l.value=!0;break;case Me:l.value=!1;break}};return document.addEventListener(ne,u),()=>{document.removeEventListener(ne,u)}}};var Rt={type:2,name:k.ExecuteScript,onGlobalInit:async t=>{G(k.ExecuteScript,({autoRemove:e=`${ze}`,attributes:n=Ke,script:r})=>{let i=B(e);if(!r?.length)throw H("NoScriptProvided",t);let s=document.createElement("script");for(let l of n.split(`
`)){let u=l.indexOf(" "),m=u?l.slice(0,u):l,T=u?l.slice(u):"";s.setAttribute(m.trim(),T.trim())}s.text=r,document.head.appendChild(s),i&&s.remove()})}};var me=document,J=!!me.startViewTransition;var wt=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:v=>v.getAttribute("im-preserve")==="true",shouldReAppend:v=>v.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!1};function n(v,E,S={}){v=b(v);let A=w(E),y=T(v,A,S),h=i(y,()=>u(y,v,A,o=>o.morphStyle==="innerHTML"?(s(o,v,A),Array.from(v.childNodes)):r(o,v,A)));return y.pantry.remove(),h}function r(v,E,S){let A=w(E),y=Array.from(A.childNodes),h=y.indexOf(E),o=y.length-(h+1);return s(v,A,S,E,E.nextSibling),y=Array.from(A.childNodes),y.slice(h,y.length-o)}function i(v,E){if(!v.config.restoreFocus)return E();let S=document.activeElement;if(!(S instanceof HTMLInputElement||S instanceof HTMLTextAreaElement))return E();let{id:A,selectionStart:y,selectionEnd:h}=S,o=E();return A&&A!==document.activeElement?.id&&(S=v.target.querySelector(`#${A}`),S?.focus()),S&&y&&h&&S.setSelectionRange(y,h),o}let s=function(){function v(c,a,g,f=null,p=null){a instanceof HTMLTemplateElement&&g instanceof HTMLTemplateElement&&(a=a.content,g=g.content),f||=a.firstChild;for(let R of g.childNodes){if(f&&f!=p){let _=S(c,R,f,p);if(_){_!==f&&y(c,f,_),l(_,R,c),f=_.nextSibling;continue}}if(R instanceof Element&&c.persistentIds.has(R.id)){let _=h(a,R.id,f,c);l(_,R,c),f=_.nextSibling;continue}let x=E(a,R,f,c);x&&(f=x.nextSibling)}for(;f&&f!=p;){let R=f;f=f.nextSibling,A(c,R)}}function E(c,a,g,f){if(f.callbacks.beforeNodeAdded(a)===!1)return null;if(f.idMap.has(a)&&a instanceof Element){let p=document.createElement(a.tagName);return c.insertBefore(p,g),l(p,a,f),f.callbacks.afterNodeAdded(p),p}else{let p=document.importNode(a,!0);return c.insertBefore(p,g),f.callbacks.afterNodeAdded(p),p}}let S=function(){function c(f,p,R,x){let _=null,I=p.nextSibling,V=0,L=R;for(;L&&L!=x;){if(g(L,p)){if(a(f,L,p))return L;_===null&&(f.idMap.has(L)||(_=L))}_===null&&I&&g(L,I)&&(V++,I=I.nextSibling,V>=2&&(_=void 0)),L=L.nextSibling}return _||null}function a(f,p,R){let x=f.idMap.get(p),_=f.idMap.get(R);if(!_||!x)return!1;for(let I of x)if(_.has(I))return!0;return!1}function g(f,p){let R=f,x=p;return R.nodeType===x.nodeType&&R.tagName===x.tagName&&(!R.id||R.id===x.id)}return c}();function A(c,a){if(c.idMap.has(a)&&a instanceof Element)d(c.pantry,a,null);else{if(c.callbacks.beforeNodeRemoved(a)===!1)return;a.parentNode?.removeChild(a),c.callbacks.afterNodeRemoved(a)}}function y(c,a,g){let f=a;for(;f&&f!==g;){let p=f;f=f.nextSibling,A(c,p)}return f}function h(c,a,g,f){let p=f.target.querySelector(`#${a}`)||f.pantry.querySelector(`#${a}`);return o(p,f),d(c,p,g),p}function o(c,a){let g=c.id;for(;c=c.parentNode;){let f=a.idMap.get(c);f&&(f.delete(g),f.size||a.idMap.delete(c))}}function d(c,a,g){if(c.moveBefore)try{c.moveBefore(a,g)}catch{c.insertBefore(a,g)}else c.insertBefore(a,g)}return v}(),l=function(){function v(o,d,c){return c.ignoreActive&&o===document.activeElement?null:(c.callbacks.beforeNodeMorphed(o,d)===!1||(o instanceof HTMLHeadElement&&c.head.ignore||(o instanceof HTMLHeadElement&&c.head.style!=="morph"?m(o,d,c):(E(o,d,c),h(o,c)||s(c,o,d))),c.callbacks.afterNodeMorphed(o,d)),o)}function E(o,d,c){let a=d.nodeType;if(a===1){let g=o,f=d,p=g.attributes,R=f.attributes;for(let x of R)y(x.name,g,"update",c)||g.getAttribute(x.name)!==x.value&&g.setAttribute(x.name,x.value);for(let x=p.length-1;0<=x;x--){let _=p[x];if(_&&!f.hasAttribute(_.name)){if(y(_.name,g,"remove",c))continue;g.removeAttribute(_.name)}}h(g,c)||S(g,f,c)}(a===8||a===3)&&o.nodeValue!==d.nodeValue&&(o.nodeValue=d.nodeValue)}function S(o,d,c){if(o instanceof HTMLInputElement&&d instanceof HTMLInputElement&&d.type!=="file"){let a=d.value,g=o.value;A(o,d,"checked",c),A(o,d,"disabled",c),d.hasAttribute("value")?g!==a&&(y("value",o,"update",c)||(o.setAttribute("value",a),o.value=a)):y("value",o,"remove",c)||(o.value="",o.removeAttribute("value"))}else if(o instanceof HTMLOptionElement&&d instanceof HTMLOptionElement)A(o,d,"selected",c);else if(o instanceof HTMLTextAreaElement&&d instanceof HTMLTextAreaElement){let a=d.value,g=o.value;if(y("value",o,"update",c))return;a!==g&&(o.value=a),o.firstChild&&o.firstChild.nodeValue!==a&&(o.firstChild.nodeValue=a)}}function A(o,d,c,a){let g=d[c],f=o[c];if(g!==f){let p=y(c,o,"update",a);p||(o[c]=d[c]),g?p||o.setAttribute(c,""):y(c,o,"remove",a)||o.removeAttribute(c)}}function y(o,d,c,a){return o==="value"&&a.ignoreActiveValue&&d===document.activeElement?!0:a.callbacks.beforeAttributeUpdated(o,d,c)===!1}function h(o,d){return!!d.ignoreActiveValue&&o===document.activeElement&&o!==document.body}return v}();function u(v,E,S,A){if(v.head.block){let y=E.querySelector("head"),h=S.querySelector("head");if(y&&h){let o=m(y,h,v);return Promise.all(o).then(()=>{let d=Object.assign(v,{head:{block:!1,ignore:!0}});return A(d)})}}return A(v)}function m(v,E,S){let A=[],y=[],h=[],o=[],d=new Map;for(let a of E.children)d.set(a.outerHTML,a);for(let a of v.children){let g=d.has(a.outerHTML),f=S.head.shouldReAppend(a),p=S.head.shouldPreserve(a);g||p?f?y.push(a):(d.delete(a.outerHTML),h.push(a)):S.head.style==="append"?f&&(y.push(a),o.push(a)):S.head.shouldRemove(a)!==!1&&y.push(a)}o.push(...d.values());let c=[];for(let a of o){let g=document.createRange().createContextualFragment(a.outerHTML).firstChild;if(S.callbacks.beforeNodeAdded(g)!==!1){if("href"in g&&g.href||"src"in g&&g.src){let f,p=new Promise(function(R){f=R});g.addEventListener("load",function(){f()}),c.push(p)}v.appendChild(g),S.callbacks.afterNodeAdded(g),A.push(g)}}for(let a of y)S.callbacks.beforeNodeRemoved(a)!==!1&&(v.removeChild(a),S.callbacks.afterNodeRemoved(a));return S.head.afterHeadMorphed(v,{added:A,kept:h,removed:y}),c}let T=function(){function v(o,d,c){let a=E(c),{persistentIds:g,idMap:f}=h(o,d),p=a.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(p))throw`Do not understand how to morph style ${p}`;return{target:o,newContent:d,config:a,morphStyle:p,ignoreActive:a.ignoreActive,ignoreActiveValue:a.ignoreActiveValue,restoreFocus:a.restoreFocus,idMap:f,persistentIds:g,pantry:S(),callbacks:a.callbacks,head:a.head}}function E(o){let d=Object.assign({},e);return Object.assign(d,o),d.callbacks=Object.assign({},e.callbacks,o.callbacks),d.head=Object.assign({},e.head,o.head),d}function S(){let o=document.createElement("div");return o.hidden=!0,document.body.insertAdjacentElement("afterend",o),o}function A(o){let d=Array.from(o.querySelectorAll("[id]"));return o.id&&d.push(o),d}function y(o,d,c,a){for(let g of d)if(c.has(g.id)){let f=g;for(;f&&f!==o;){let p=a.get(f);p==null&&(p=new Set,a.set(f,p)),p.add(g.id),f=f.parentElement}}}function h(o,d){let c=new Map,a=new Set,g=A(o);for(let x of g){let _=x.id;c.has(_)?a.add(_):c.set(_,x.tagName)}let f=new Set,p=A(d);for(let x of p){let _=x.id,I=c.get(_);(f.has(_)||I&&I!==x.tagName)&&(a.add(_),f.delete(_)),I===x.tagName&&!a.has(_)&&f.add(_)}let R=new Map;return y(o.parentElement,p,f,R),y(d.parentElement,g,f,R),{persistentIds:f,idMap:R}}return v}(),{normalizeElement:b,normalizeParent:w}=function(){let v=new WeakSet;function E(y){return y instanceof Document?y.documentElement:y}function S(y){if(y==null)return document.createElement("div");if(typeof y=="string")return S(A(y));if(v.has(y))return y;if(y instanceof Node){if(y.parentNode)return y.parentNode;{let h=document.createElement("div");return h.append(y),h}}else{let h=document.createElement("div");for(let o of[...y])h.append(o);return h}}function A(y){let h=new DOMParser,o=y.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(o.match(/<\/html>/)||o.match(/<\/head>/)||o.match(/<\/body>/)){let d=h.parseFromString(y,"text/html");if(o.match(/<\/html>/))return v.add(d),d;{let c=d.firstChild;return c&&v.add(c),c}}else{let c=h.parseFromString("<body><template>"+y+"</template></body>","text/html").body.querySelector("template").content;return v.add(c),c}}return{normalizeElement:E,normalizeParent:S}}();return{morph:n,defaults:e}}();var Nt={type:2,name:k.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");G(k.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=Ye,settleDuration:s=`${ve}`,useViewTransition:l=`${ye}`})=>{let u=Number.parseInt(s),m=B(l);e.innerHTML=n.trim();let T=[...e.content.children];for(let b of T){if(!(b instanceof Element))throw H("NoFragmentsFound",t);let w=r||`#${b.getAttribute("id")}`,v=[...document.querySelectorAll(w)||[]];if(!v.length)throw H("NoTargetsFound",t,{selectorOrID:w});m&&J?me.startViewTransition(()=>Mt(t,i,u,b,v)):Mt(t,i,u,b,v)}})}};function Mt(t,e,n,r,i){for(let s of i){s.classList.add(Y);let l=s.outerHTML,u=s;switch(e){case W.Morph:{let b=new Map,w=r.cloneNode(!0);le(w,E=>{!E.id?.length&&Object.keys(E.dataset).length&&(E.id=Se(E),console.log(E.id))});let v=wt.morph(u,w,{restoreFocus:!0,callbacks:{beforeAttributeUpdated:(E,S,A)=>{if(A==="update"&&E.startsWith("data-")){let y=b.get(S);y||(y=[],b.set(S,y));let h=E.slice(5);y.push(j(h))}return!0}}});if(v?.length){u=v[0];for(let[E,S]of b.entries())for(let A of S)t.applyAttributePlugin(E,A)}break}case W.Inner:u.innerHTML=r.outerHTML;break;case W.Outer:u.replaceWith(r);break;case W.Prepend:u.prepend(r);break;case W.Append:u.append(r);break;case W.Before:u.before(r);break;case W.After:u.after(r);break;case W.UpsertAttributes:for(let b of r.getAttributeNames()){let w=r.getAttribute(b);u.setAttribute(b,w)}break;default:throw H("InvalidMergeMode",t,{mergeMode:e})}let m=u.classList;m?.add(Y),setTimeout(()=>{s.classList.remove(Y),m?.remove(Y)},n);let T=u.outerHTML;m&&l!==T&&(m.add(He),setTimeout(()=>{m.remove(He)},n))}}var Pt={type:2,name:k.MergeSignals,onGlobalInit:async t=>{G(k.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${Je}`})=>{let{signals:r}=t,i=B(n);r.merge(be(e),i)})}};var Ct={type:2,name:k.RemoveFragments,onGlobalInit:async t=>{G(k.RemoveFragments,({selector:e,settleDuration:n=`${ve}`,useViewTransition:r=`${ye}`})=>{if(!e.length)throw H("NoSelectorProvided",t);let i=Number.parseInt(n),s=B(r),l=document.querySelectorAll(e),u=()=>{for(let m of l)m.classList.add(Y);setTimeout(()=>{for(let m of l)m.remove()},i)};s&&J?me.startViewTransition(()=>u()):u()})}};var Lt={type:2,name:k.RemoveSignals,onGlobalInit:async t=>{G(k.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw H("NoPathsProvided",t);t.signals.remove(...n)})}};var It={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw N("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var Dt={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw N("CustomValidityInvalidElement",t);let i=n();return r(()=>{let s=i();if(typeof s!="string")throw N("CustomValidityInvalidExpression",t,{result:s});e.setCustomValidity(s)})}};var Vt="once",Ot="half",kt="full",Ft={type:1,name:"intersects",keyReq:2,mods:new Set([Vt,Ot,kt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(kt)?i.threshold=1:n.has(Ot)&&(i.threshold=.5);let s=r(),l=new IntersectionObserver(u=>{for(let m of u)m.isIntersecting&&(s(),n.has(Vt)&&(l.disconnect(),delete t.dataset[e]))},i);return l.observe(t),()=>l.disconnect()}};var Ht="session",Wt={type:1,name:"persist",mods:new Set([Ht]),onLoad:({key:t,effect:e,mods:n,signals:r,value:i})=>{t=D(t,n),t===""&&(t=O);let s=n.has(Ht)?sessionStorage:localStorage,l=i.split(/\s+/).filter(T=>T!=="");l=l.map(T=>K(T));let u=()=>{let T=s.getItem(t)||"{}",b=JSON.parse(T);r.merge(b)},m=()=>{let T;l.length?T=r.subset(...l):T=r.values(),s.setItem(t,JSON.stringify(T))};return u(),e(()=>{m()})}};var qt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,s=new URL(r,i).toString();window.history.replaceState({},"",s)})}};var Ne="smooth",We="instant",qe="auto",$t="hstart",Gt="hcenter",Ut="hend",jt="hnearest",Bt="vstart",Kt="vcenter",Jt="vend",zt="vnearest",Nn="focus",Pe="center",Yt="start",Xt="end",Qt="nearest",Zt={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ne,We,qe,$t,Gt,Ut,jt,Bt,Kt,Jt,zt,Nn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:Ne,block:Pe,inline:Pe};if(n.has(Ne)&&(i.behavior=Ne),n.has(We)&&(i.behavior=We),n.has(qe)&&(i.behavior=qe),n.has($t)&&(i.inline=Yt),n.has(Gt)&&(i.inline=Pe),n.has(Ut)&&(i.inline=Xt),n.has(jt)&&(i.inline=Qt),n.has(Bt)&&(i.block=Yt),n.has(Kt)&&(i.block=Pe),n.has(Jt)&&(i.block=Xt),n.has(zt)&&(i.block=Qt),!(e instanceof HTMLElement||e instanceof SVGElement))throw N("ScrollIntoViewInvalidElement",t);return e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r],()=>{}}};var en="none",tn="display",nn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===en&&t.removeProperty(tn):t.setProperty(tn,en)})}};var rn="view-transition",on={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===rn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=rn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!J){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let s=e.style;s.viewTransitionName=i})}};var sn={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let i=r();return e===""?n(async()=>{let s=i();for(let[l,u]of Object.entries(s))t.setAttribute(l,u)}):(e=se(e),n(async()=>{let s=!1;try{s=i()}catch{}let l;typeof s=="string"?l=s:l=JSON.stringify(s),!l||l==="false"||l==="null"||l==="undefined"?t.removeAttribute(e):t.setAttribute(e,l)}))}};var Pn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,an=["change","input","keydown"],ln={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:i,value:s,effect:l}=t,u=n?D(n,r):K(s),m=()=>{},T=()=>{},b=e.tagName.toLowerCase(),w="",v=b.includes("input"),E=e.getAttribute("type"),S=b.includes("checkbox")||v&&E==="checkbox";S&&(w=!1),v&&E==="number"&&(w=0);let y=b.includes("select"),h=b.includes("radio")||v&&E==="radio",o=v&&E==="file";h&&(e.getAttribute("name")?.length||e.setAttribute("name",u)),i.upsertIfMissing(u,w),m=()=>{let a="value"in e,g=i.value(u),f=`${g}`;if(S||h){let p=e;S?p.checked=!!g||g==="true":h&&(p.checked=f===p.value)}else if(!o)if(y){let p=e;if(p.multiple)for(let R of p.options){if(R?.disabled)return;Array.isArray(g)||typeof g=="string"?R.selected=g.includes(R.value):typeof g=="number"?R.selected=g===Number(R.value):R.selected=g}else p.value=f}else a?e.value=f:e.setAttribute("value",f)},T=async()=>{if(o){let f=[...e?.files||[]],p=[],R=[],x=[];await Promise.all(f.map(_=>new Promise(I=>{let V=new FileReader;V.onload=()=>{if(typeof V.result!="string")throw N("InvalidFileResultType",t,{resultType:typeof V.result});let L=V.result.match(Pn);if(!L?.groups)throw N("InvalidDataUri",t,{result:V.result});p.push(L.groups.contents),R.push(L.groups.mime),x.push(_.name)},V.onloadend=()=>I(void 0),V.readAsDataURL(_)}))),i.setValue(u,p),i.setValue(`${u}Mimes`,R),i.setValue(`${u}Names`,x);return}let a=i.value(u),g=e||e;if(typeof a=="number"){let f=Number(g.value||g.getAttribute("value"));i.setValue(u,f)}else if(typeof a=="string"){let f=g.value||g.getAttribute("value")||"";i.setValue(u,f)}else if(typeof a=="boolean")if(S){let f=g.checked||g.getAttribute("checked")==="true";i.setValue(u,f)}else{let f=!!(g.value||g.getAttribute("value"));i.setValue(u,f)}else if(!(typeof a>"u"))if(Array.isArray(a))if(y){let R=[...e.selectedOptions].filter(x=>x.selected).map(x=>x.value);i.setValue(u,R)}else{let f=JSON.stringify(g.value.split(","));i.setValue(u,f)}else throw N("BindUnsupportedSignalType",t,{signalType:typeof a})};for(let a of an)e.addEventListener(a,T);let d=l(()=>m()),c=a=>{a.persisted&&T()};return window.addEventListener("pageshow",c),()=>{d();for(let a of an)e.removeEventListener(a,T);window.removeEventListener("pageshow",c)}}};var un={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:i})=>{let s=t.classList,l=i();return r(()=>{if(e===""){let u=l();for(let[m,T]of Object.entries(u)){let b=m.split(/\s+/);T?s.add(...b):s.remove(...b)}}else e=D(e,n),l()?s.add(e):s.remove(e)})}};function ge(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function re(t,e,n=!1){return t?t.has(e.toLowerCase()):n}var Cn="evt",$e="signalsChange",Ln=$e.length,cn={type:1,name:"on",keyReq:1,valReq:1,argNames:[Cn],removeOnLoad:t=>t.startsWith("onLoad"),onLoad:({el:t,key:e,mods:n,rawKey:r,signals:i,value:s,effect:l,genRX:u})=>{let m=u(),T=t;n.has("window")&&(T=window);let b=h=>{h&&((n.has("prevent")||e==="submit")&&h.preventDefault(),n.has("stop")&&h.stopPropagation()),m(h)},w=n.get("delay");if(w){let h=ge(w);b=ue(b,h)}let v=n.get("debounce");if(v){let h=ge(v),o=re(v,"leading",!1),d=!re(v,"notrail",!1);b=et(b,h,o,d)}let E=n.get("throttle");if(E){let h=ge(E),o=!re(E,"noleading",!1),d=re(E,"trail",!1);b=tt(b,h,o,d)}if(n.has("viewtransition")&&J){let h=b;b=(...o)=>document.startViewTransition(()=>h(...o))}let S={capture:!0,passive:!1,once:!1};if(n.has("capture")||(S.capture=!1),n.has("passive")&&(S.passive=!0),n.has("once")&&(S.once=!0),e==="load")return ue(()=>b(),0)(),()=>{};if(e==="interval"){let h=1e3,o=n.get("duration");o&&(h=ge(o),re(o,"leading",!1)&&(t.dataset[r.replace(".leading","")]=s,delete t.dataset[r],b()));let d=setInterval(b,h);return()=>{clearInterval(d)}}if(e==="raf"){let h,o=()=>{b(),h=requestAnimationFrame(o)};return h=requestAnimationFrame(o),()=>{h&&cancelAnimationFrame(h)}}if(e.startsWith($e)){if(e===$e){b();let d=c=>b(c);return document.addEventListener(oe,d),()=>{document.removeEventListener(oe,d)}}let h=D(j(e.slice(Ln)),n),o=new Map;return i.walk((d,c)=>{d.startsWith(h)&&o.set(c,c.value)}),l(()=>{for(let[d,c]of o)c!==d.value&&(b(),o.set(d,d.value))})}if(n.has("outside")){T=document;let h=b;b=d=>{let c=d?.target;t.contains(c)||h(d)}}let y=D(e,n);return T.addEventListener(y,b,S),()=>{T.removeEventListener(y,b)}}};var fn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let s=e?D(e,n):K(i);r.setValue(s,t)}};var dn={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,i=r();return e instanceof HTMLElement||N("TextInvalidElement",t),n(()=>{let s=i(t);e.textContent=`${s}`})}};var{round:In,max:Dn,min:Vn}=Math,pn={type:3,name:"fit",fn:(t,e,n,r,i,s,l=!1,u=!1)=>{let m=(e-n)/(r-n)*(s-i)+i;return u&&(m=In(m)),l&&(m=Dn(i,Vn(s,m))),m}};var mn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var gn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Fe.load(sn,ln,un,cn,fn,nn,dn,xt,Tt,At,_t,Et,St,Nt,Pt,Ct,Lt,Rt,It,Dt,Ft,Wt,qt,Zt,on,pn,mn,gn);var rs=Fe;export{rs as Datastar};
//# sourceMappingURL=datastar.js.map
