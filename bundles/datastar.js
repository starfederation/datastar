// Datastar v1.0.0-beta.2
var je=/🖕JS_DS🚀/.source,re=je.slice(0,5),Ie=je.slice(4),O="datastar";var Be="Datastar-Request",Ge="1.0.0-beta.2",de=300;var Ke="type module",me=!1,Je=!1,ze=!0,V={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Xe=V.Morph,I={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var g=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(g||{});var Pn="computed",Ye={type:1,name:Pn,keyReq:1,valReq:1,onLoad:({key:t,signals:e,genRX:n})=>{let r=n();e.setComputed(t,r)}};var U=t=>t.trim()==="true",W=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),De=t=>W(t).replace(/-./g,e=>e[1].toUpperCase()),ge=t=>new Function(`return Object.assign({}, ${t})`)(),j=t=>t.startsWith("$")?t.slice(1):t;var Qe={type:1,name:"signals",removeOnLoad:!0,onLoad:t=>{let{key:e,value:n,genRX:r,signals:i,mods:o}=t,s=o.has("ifmissing");if(e!==""&&!s){let a=n===""?n:r()();i.setValue(e,a)}else{let a=ge(t.value);t.value=JSON.stringify(a);let c=r()();i.merge(c,s)}}};var Ze={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ie=class{#e=0;#n;constructor(e=O){this.#n=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}reset(){return this.#e=0,this}get value(){return this.#n+Math.abs(this.#e).toString(36)}};function et(t){if(t.id)return t.id;let e=new ie,n=t;for(;n.parentNode;){if(n.id){e.with(n.id);break}if(n===n.ownerDocument.documentElement)e.with(n.tagName);else{for(let r=1,i=t;i.previousElementSibling;i=i.previousElementSibling,r++)e.with(r);n=n.parentNode}n=n.parentNode}return e.value}function tt(t,e){let n=new MutationObserver(r=>{for(let i of r)for(let o of i.removedNodes)if(o===t){n.disconnect(),e();return}});n.observe(t.parentNode,{childList:!0})}var Mn=`${window.location.origin}/errors`;function Le(t,e,n={}){let r=new Error;e=e[0].toUpperCase()+e.slice(1),r.name=`${O} ${t} error`;let i=W(e).replaceAll("-","_"),o=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),s=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${Mn}/${t}/${i}?${o}
Context: ${s}`,r}function M(t,e,n={}){return Le("internal",e,Object.assign({from:t},n))}function k(t,e,n={}){let r={plugin:{name:e.plugin.name,type:g[e.plugin.type]}};return Le("init",t,Object.assign(r,n))}function v(t,e,n={}){let r={plugin:{name:e.plugin.name,type:g[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return Le("runtime",t,Object.assign(r,n))}var B="preact-signals",Nn=Symbol.for("preact-signals"),F=1,z=2,se=4,Y=8,he=16,X=32;function Ve(){ye++}function ke(){if(ye>1){ye--;return}let t,e=!1;for(;oe!==void 0;){let n=oe;for(oe=void 0,Oe++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~z,!(n._flags&Y)&&rt(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(Oe=0,ye--,e)throw M(B,"BatchError, error",{error:t})}var E;var oe,ye=0,Oe=0,ve=0;function nt(t){if(E===void 0)return;let e=t._node;if(e===void 0||e._target!==E)return e={_version:0,_source:t,_prevSource:E._sources,_nextSource:void 0,_target:E,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},E._sources!==void 0&&(E._sources._nextSource=e),E._sources=e,t._node=e,E._flags&X&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=E._sources,e._nextSource=void 0,E._sources._nextSource=e,E._sources=e),e}function R(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}R.prototype.brand=Nn;R.prototype._refresh=()=>!0;R.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};R.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};R.prototype.subscribe=function(t){return be(()=>{let e=this.value,n=E;E=void 0;try{t(e)}finally{E=n}})};R.prototype.valueOf=function(){return this.value};R.prototype.toString=function(){return`${this.value}`};R.prototype.toJSON=function(){return this.value};R.prototype.peek=function(){let t=E;E=void 0;try{return this.value}finally{E=t}};Object.defineProperty(R.prototype,"value",{get(){let t=nt(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(Oe>100)throw M(B,"SignalCycleDetected");this._value=t,this._version++,ve++,Ve();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{ke()}}}});function rt(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function it(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ot(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function G(t){R.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=ve-1,this._flags=se}G.prototype=new R;G.prototype._refresh=function(){if(this._flags&=~z,this._flags&F)return!1;if((this._flags&(se|X))===X||(this._flags&=~se,this._globalVersion===ve))return!0;if(this._globalVersion=ve,this._flags|=F,this._version>0&&!rt(this))return this._flags&=~F,!0;let t=E;try{it(this),E=this;let e=this._fn();(this._flags&he||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~he,this._version++)}catch(e){this._value=e,this._flags|=he,this._version++}return E=t,ot(this),this._flags&=~F,!0};G.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=se|X;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}R.prototype._subscribe.call(this,t)};G.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(R.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~X;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};G.prototype._notify=function(){if(!(this._flags&z)){this._flags|=se|z;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(G.prototype,"value",{get(){if(this._flags&F)throw M(B,"SignalCycleDetected");let t=nt(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&he)throw M(B,"GetComputedError",{value:this._value});return this._value}});function st(t){return new G(t)}function at(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){Ve();let n=E;E=void 0;try{e()}catch(r){throw t._flags&=~F,t._flags|=Y,Fe(t),M(B,"CleanupEffectError",{error:r})}finally{E=n,ke()}}}function Fe(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,at(t)}function Cn(t){if(E!==this)throw M(B,"EndEffectError");ot(this),E=t,this._flags&=~F,this._flags&Y&&Fe(this),ke()}function ae(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=X}ae.prototype._callback=function(){let t=this._start();try{if(this._flags&Y||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};ae.prototype._start=function(){if(this._flags&F)throw M(B,"SignalCycleDetected");this._flags|=F,this._flags&=~Y,at(this),it(this),Ve();let t=E;return E=this,Cn.bind(this,t)};ae.prototype._notify=function(){this._flags&z||(this._flags|=z,this._nextBatchedEffect=oe,oe=this)};ae.prototype._dispose=function(){this._flags|=Y,this._flags&F||Fe(this)};function be(t){let e=new ae(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var lt="namespacedSignals";function ut(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof R?n[r]=i.value:n[r]=ut(i)}return n}function ct(t,e,n=!1){for(let r in e)if(Object.hasOwn(e,r)){if(r.match(/\_\_+/))throw M(lt,"InvalidSignalKey",{key:r});let i=e[r];if(i instanceof Object&&!Array.isArray(i))t[r]||(t[r]={}),ct(t[r],i,n);else{if(Object.hasOwn(t,r)){if(n)continue;let s=t[r];if(s instanceof R){s.value=i;continue}}t[r]=new R(i)}}}function ft(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof R?e(n,r):ft(r,(i,o)=>{e(`${n}.${i}`,o)})}}function In(t,...e){let n={};for(let r of e){let i=r.split("."),o=t,s=n;for(let u=0;u<i.length-1;u++){let c=i[u];if(!o[c])return{};s[c]||(s[c]={}),o=o[c],s=s[c]}let a=i[i.length-1];s[a]=o[a]}return n}var Se=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let s=0;s<n.length-1;s++){let a=n[s];if(!r[a])return null;r=r[a]}let i=n[n.length-1],o=r[i];if(!o)throw M(lt,"SignalNotFound",{path:e});return o}setSignal(e,n){let r=e.split("."),i=this.#e;for(let s=0;s<r.length-1;s++){let a=r[s];i[a]||(i[a]={}),i=i[a]}let o=r[r.length-1];i[o]=n}setComputed(e,n){let r=st(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let r=this.upsertIfMissing(e,n);r.value=n}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let u=0;u<r.length-1;u++){let c=r[u];i[c]||(i[c]={}),i=i[c]}let o=r[r.length-1],s=i[o];if(s instanceof R)return s;let a=new R(n);return i[o]=a,a}remove(...e){if(!e.length){this.#e={};return}for(let n of e){let r=n.split("."),i=this.#e;for(let s=0;s<r.length-1;s++){let a=r[s];if(!i[a])return;i=i[a]}let o=r[r.length-1];delete i[o]}}merge(e,n=!1){ct(this.#e,e,n)}subset(...e){return In(this.values(),...e)}walk(e){ft(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return ut(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var pt=(t,e)=>`${t}${re}${e}`,Ee=class{#e=new Se;#n=[];#r={};#s=[];#t=new Map;constructor(){let e="data-";new MutationObserver(r=>{for(let{target:i,type:o,attributeName:s,oldValue:a,addedNodes:u,removedNodes:c}of r){let l=f=>f;switch(o){case"childList":{if(c.length)for(let f of c){let d=l(f),b=this.#t.get(d);if(b){for(let[h,w]of b)w();this.#t.delete(d)}}if(u.length)for(let f of u){let d=l(f);this.apply(d)}}break;case"attributes":{{if(!s?.startsWith(e))break;let f=l(i),d=De(s.slice(e.length));if(a!==null&&f.dataset[d]!==a){let b=this.#t.get(f);if(b){let h=pt(d,a),w=b.get(h);w&&(w(),b.delete(h))}}this.#i(f,d)}break}}}}).observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0})}get signals(){return this.#e}get version(){return Ge}load(...e){for(let n of e){let r=this,i={get signals(){return r.#e},effect:s=>be(s),actions:this.#r,apply:this.apply.bind(this),plugin:n},o;switch(n.type){case 2:{let s=n;this.#s.push(s),o=s.onGlobalInit;break}case 3:{this.#r[n.name]=n;break}case 1:{let s=n;this.#n.push(s),o=s.onGlobalInit;break}default:throw k("InvalidPluginType",i)}o&&o(i)}this.#n.sort((n,r)=>{let i=r.name.length-n.name.length;return i!==0?i:n.name.localeCompare(r.name)})}apply(e){this.#o(e,n=>{let r=this.#t.get(n);if(r){for(let[,i]of r)i();this.#t.delete(n)}for(let i of Object.keys(n.dataset))this.#i(n,i)})}#i(e,n){let r=this.#n.find(h=>n.startsWith(h.name));if(!r)return;e.id.length||(e.id=et(e));let[i,...o]=n.slice(r.name.length).split(/\_\_+/),s=i.length>0;if(s){let h=i.slice(1);i=i.startsWith("-")?h:i[0].toLowerCase()+h}let a=`${e.dataset[n]}`||"",u=a.length>0,c=this,l={get signals(){return c.#e},effect:h=>be(h),apply:this.apply.bind(this),actions:this.#r,genRX:()=>this.#a(l,...r.argNames||[]),plugin:r,el:e,rawKey:n,key:i,value:a,mods:new Map},f=r.keyReq||0;if(s){if(f===2)throw v(`${r.name}KeyNotAllowed`,l)}else if(f===1)throw v(`${r.name}KeyRequired`,l);let d=r.valReq||0;if(u){if(d===2)throw v(`${r.name}ValueNotAllowed`,l)}else if(d===1)throw v(`${r.name}ValueRequired`,l);if(f===3||d===3){if(s&&u)throw v(`${r.name}KeyAndValueProvided`,l);if(!s&&!u)throw v(`${r.name}KeyOrValueRequired`,l)}for(let h of o){let[w,...p]=h.split(".");l.mods.set(De(w),new Set(p.map(m=>m.toLowerCase())))}let b=r.onLoad(l);if(b){let h=this.#t.get(e);h||(h=new Map,this.#t.set(e,h)),h.set(pt(n,a),b)}r?.removeOnLoad&&delete e.dataset[n]}#a(e,...n){let r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=e.value.trim().match(r)||[],o=i.length-1,s=i[o];s.startsWith("return")||(i[o]=`return (${s});`);let a=i.join(`;
`),u=new Map,c=new RegExp(`(?:${re})(.*?)(?:${Ie})`,"gm");for(let p of a.matchAll(c)){let m=p[1],_=new ie("dsEscaped").with(m).value;u.set(_,m),a=a.replace(re+m+Ie,_)}let l=/@(\w*)\(/gm,f=a.matchAll(l),d=new Set;for(let p of f)d.add(p[1]);let b=new RegExp(`@(${Object.keys(this.#r).join("|")})\\(`,"gm");a=a.replaceAll(b,"ctx.actions.$1.fn(ctx,");let h=e.signals.paths();if(h.length){let p=new RegExp(`\\$(${h.join("|")})(\\W|$)`,"gm");a=a.replaceAll(p,"ctx.signals.signal('$1').value$2")}for(let[p,m]of u)a=a.replace(p,m);let w=`return (()=> {
${a}
})()`;e.fnContent=w;try{let p=new Function("ctx",...n,w);return(...m)=>{try{return p(e,...m)}catch(_){throw v("ExecuteExpression",e,{error:_.message})}}}catch(p){throw v("GenerateExpression",e,{error:p.message})}}#o(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;let r=e.dataset;if("starIgnore"in r)return null;"starIgnore__self"in r||n(e);let i=e.firstElementChild;for(;i;)this.#o(i,n),i=i.nextElementSibling}};var dt=new Ee;dt.load(Ze,Qe,Ye);var Te=dt;async function Dn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function Ln(t){let e,n,r,i=!1;return function(s){e===void 0?(e=s,n=0,r=-1):e=Vn(e,s);let a=e.length,u=0;for(;n<a;){i&&(e[n]===10&&(u=++n),i=!1);let c=-1;for(;n<a&&c===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-u);break;case 13:i=!0;case 10:c=n;break}if(c===-1)break;t(e.subarray(u,c),r),u=n,r=-1}u===a?e=void 0:u!==0&&(e=e.subarray(u),n-=u)}}function On(t,e,n){let r=mt(),i=new TextDecoder;return function(s,a){if(s.length===0)n?.(r),r=mt();else if(a>0){let u=i.decode(s.subarray(0,a)),c=a+(s[a+1]===32?2:1),l=i.decode(s.subarray(c));switch(u){case"data":r.data=r.data?`${r.data}
${l}`:l;break;case"event":r.event=l;break;case"id":t(r.id=l);break;case"retry":{let f=Number.parseInt(l,10);Number.isNaN(f)||e(r.retry=f);break}}}}}function Vn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function mt(){return{data:"",event:"",id:"",retry:void 0}}var kn="text/event-stream",gt="last-event-id";function ht(t,e,{signal:n,headers:r,onopen:i,onmessage:o,onclose:s,onerror:a,openWhenHidden:u,fetch:c,retryInterval:l=1e3,retryScaler:f=2,retryMaxWaitMs:d=3e4,retryMaxCount:b=10,...h}){return new Promise((w,p)=>{let m=0,_={...r};_.accept||(_.accept=kn);let S;function T(){S.abort(),document.hidden||L()}u||document.addEventListener("visibilitychange",T);let A=0;function P(){document.removeEventListener("visibilitychange",T),window.clearTimeout(A),S.abort()}n?.addEventListener("abort",()=>{P(),w()});let N=c??window.fetch,y=i??function(){};async function L(){S=new AbortController;try{let C=await N(e,{...h,headers:_,signal:S.signal});await y(C),await Dn(C.body,Ln(On(x=>{x?_[gt]=x:delete _[gt]},x=>{l=x},o))),s?.(),P(),w()}catch(C){if(!S.signal.aborted)try{let x=a?.(C)??l;window.clearTimeout(A),A=window.setTimeout(L,x),l*=f,l=Math.min(l,d),m++,m>=b?(P(),p(v("SseMaxRetries",t,{retryMaxCount:b}))):console.error(`Datastar failed to reach ${h.method}: ${e.toString()} retry in ${x}ms`)}catch(x){P(),p(x)}}}L()})}var Q=`${O}-sse`,He=`${O}-settling`,K=`${O}-swapping`,Ae="started",_e="finished",yt="error";function H(t,e){document.addEventListener(Q,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function xe(t,e){document.dispatchEvent(new CustomEvent(Q,{detail:{type:t,argsRaw:e}}))}var vt=t=>`${t}`.includes("text/event-stream"),q=async(t,e,n,r)=>{let{el:{id:i},el:o,signals:s}=t,{headers:a,contentType:u,includeLocal:c,selector:l,openWhenHidden:f,retryInterval:d,retryScaler:b,retryMaxWaitMs:h,retryMaxCount:w,abort:p}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:1e3,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),m=e.toLowerCase(),_=()=>{};try{if(xe(Ae,{elId:i}),!n?.length)throw v("SseNoUrlProvided",t,{action:m});let S={};S[Be]=!0,u==="json"&&(S["Content-Type"]="application/json");let T=Object.assign({},S,a),A={method:e,headers:T,openWhenHidden:f,retryInterval:d,retryScaler:b,retryMaxWaitMs:h,retryMaxCount:w,signal:p,onopen:async y=>{if(y.status>=400){let L=y.status.toString();xe(yt,{status:L})}},onmessage:y=>{if(!y.event.startsWith(O))return;let L=y.event,C={},x=y.data.split(`
`);for(let ne of x){let fe=ne.indexOf(" "),Ue=ne.slice(0,fe),pe=C[Ue];pe||(pe=[],C[Ue]=pe);let Rn=ne.slice(fe+1).trim();pe.push(Rn)}let $={};for(let[ne,fe]of Object.entries(C))$[ne]=fe.join(`
`);xe(L,$)},onerror:y=>{if(vt(y))throw v("InvalidContentType",t,{url:n});y&&console.error(y.message)}},P=new URL(n,window.location.origin),N=new URLSearchParams(P.search);if(u==="json"){let y=s.JSON(!1,!c);e==="GET"?N.set(O,y):A.body=y}else if(u==="form"){let y=l?document.querySelector(l):o.closest("form");if(y===null)throw l?v("SseFormNotFound",t,{action:m,selector:l}):v("SseClosestFormNotFound",t,{action:m});if(o!==y){let C=x=>x.preventDefault();y.addEventListener("submit",C),_=()=>y.removeEventListener("submit",C)}if(!y.checkValidity()){y.reportValidity(),_();return}let L=new FormData(y);if(e==="GET"){let C=new URLSearchParams(L);for(let[x,$]of C)N.set(x,$)}else A.body=L}else throw v("SseInvalidContentType",t,{action:m,contentType:u});P.search=N.toString();try{await ht(t,P.toString(),A)}catch(y){if(!vt(y))throw v("SseFetchFailed",t,{method:e,url:n,error:y})}}finally{xe(_e,{elId:i}),_()}};var bt={type:3,name:"delete",fn:async(t,e,n)=>q(t,"DELETE",e,{...n})};var St={type:3,name:"get",fn:async(t,e,n)=>q(t,"GET",e,{...n})};var Et={type:3,name:"patch",fn:async(t,e,n)=>q(t,"PATCH",e,{...n})};var Tt={type:3,name:"post",fn:async(t,e,n)=>q(t,"POST",e,{...n})};var At={type:3,name:"put",fn:async(t,e,n)=>q(t,"PUT",e,{...n})};var _t={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({value:t,signals:e,el:n,key:r})=>{let i=r||j(t),o=e.upsertIfMissing(i,!1),s=a=>{let{type:u,argsRaw:{elId:c}}=a.detail;if(c===n.id)switch(u){case Ae:o.value=!0;break;case _e:o.value=!1;break}};return document.addEventListener(Q,s),()=>{document.removeEventListener(Q,s)}}};var xt={type:2,name:I.ExecuteScript,onGlobalInit:async t=>{H(I.ExecuteScript,({autoRemove:e=`${ze}`,attributes:n=Ke,script:r})=>{let i=U(e);if(!r?.length)throw k("NoScriptProvided",t);let o=document.createElement("script");for(let s of n.split(`
`)){let a=s.indexOf(" "),u=a?s.slice(0,a):s,c=a?s.slice(a):"";o.setAttribute(u.trim(),c.trim())}o.text=r,document.head.appendChild(o),i&&o.remove()})}};var le=document,Z=!!le.startViewTransition;var ee="idiomorph",Re=new WeakSet;function Nt(t,e,n={}){t instanceof Document&&(t=t.documentElement);let r;typeof e=="string"?r=$n(e):r=e;let i=Un(r),o=Hn(t,i,n);return Ct(t,i,o)}function Ct(t,e,n){if(n.head.block){let r=t.querySelector("head"),i=e.querySelector("head");if(r&&i){let o=Dt(i,r,n);Promise.all(o).then(()=>{Ct(t,e,Object.assign(n,{head:{block:!1,ignore:!0}}))});return}}if(n.morphStyle==="innerHTML")return It(e,t,n),t.children;if(n.morphStyle==="outerHTML"||n.morphStyle==null){let r=Bn(e,t,n);if(!r)throw M(ee,"NoBestMatchFound",{old:t,new:e});let i=r?.previousSibling,o=r?.nextSibling,s=Pe(t,r,n);return r?jn(i,s,o):[]}throw M(ee,"InvalidMorphStyle",{style:n.morphStyle})}function Pe(t,e,n){if(!(n.ignoreActive&&t===document.activeElement))if(e==null){t.remove();return}else{if(Me(t,e))return t instanceof HTMLHeadElement&&n.head.ignore||(e instanceof HTMLHeadElement&&t instanceof HTMLHeadElement&&n.head.style!==V.Morph?Dt(e,t,n):(Fn(e,t),It(e,t,n))),t;if(!t.parentElement)throw M(ee,"NoParentElementFound",{oldNode:t});return t.parentElement.replaceChild(e,t),e}}function It(t,e,n){let r=t.firstChild,i=e.firstChild,o;for(;r;){if(o=r,r=o.nextSibling,i==null){e.appendChild(o),J(n,o);continue}if(Lt(o,i,n)){Pe(i,o,n),i=i.nextSibling,J(n,o);continue}let s=qn(t,e,o,i,n);if(s){i=Rt(i,s,n),Pe(s,o,n),J(n,o);continue}let a=Wn(t,o,i,n);if(a){i=Rt(i,a,n),Pe(a,o,n),J(n,o);continue}e.insertBefore(o,i),J(n,o)}for(;i!==null;){let s=i;i=i.nextSibling,Ot(s,n)}}function Fn(t,e){let n=t.nodeType;if(n===1){for(let r of t.attributes)e.getAttribute(r.name)!==r.value&&e.setAttribute(r.name,r.value);for(let r of e.attributes)t.hasAttribute(r.name)||e.removeAttribute(r.name)}if((n===Node.COMMENT_NODE||n===Node.TEXT_NODE)&&e.nodeValue!==t.nodeValue&&(e.nodeValue=t.nodeValue),t instanceof HTMLInputElement&&e instanceof HTMLInputElement&&t.type!=="file")e.value=t.value||"",we(t,e,"value"),we(t,e,"checked"),we(t,e,"disabled");else if(t instanceof HTMLOptionElement)we(t,e,"selected");else if(t instanceof HTMLTextAreaElement&&e instanceof HTMLTextAreaElement){let r=t.value,i=e.value;r!==i&&(e.value=r),e.firstChild&&e.firstChild.nodeValue!==r&&(e.firstChild.nodeValue=r)}}function we(t,e,n){let r=t.getAttribute(n),i=e.getAttribute(n);r!==i&&(r?e.setAttribute(n,r):e.removeAttribute(n))}function Dt(t,e,n){let r=[],i=[],o=[],s=[],a=n.head.style,u=new Map;for(let l of t.children)u.set(l.outerHTML,l);for(let l of e.children){let f=u.has(l.outerHTML),d=n.head.shouldReAppend(l),b=n.head.shouldPreserve(l);f||b?d?i.push(l):(u.delete(l.outerHTML),o.push(l)):a===V.Append?d&&(i.push(l),s.push(l)):n.head.shouldRemove(l)!==!1&&i.push(l)}s.push(...u.values());let c=[];for(let l of s){let f=document.createRange().createContextualFragment(l.outerHTML).firstChild;if(!f)throw M(ee,"NewElementCouldNotBeCreated",{newNode:l});if(f.hasAttribute("href")||f.hasAttribute("src")){let d,b=new Promise(h=>{d=h});f.addEventListener("load",()=>{d(void 0)}),c.push(b)}e.appendChild(f),r.push(f)}for(let l of i)e.removeChild(l);return n.head.afterHeadMorphed(e,{added:r,kept:o,removed:i}),c}function wt(){}function Hn(t,e,n){return{target:t,newContent:e,config:n,morphStyle:n.morphStyle,ignoreActive:n.ignoreActive,idMap:zn(t,e),deadIds:new Set,head:Object.assign({style:"merge",shouldPreserve:r=>r.getAttribute("im-preserve")==="true",shouldReAppend:r=>r.getAttribute("im-re-append")==="true",shouldRemove:wt,afterHeadMorphed:wt},n.head)}}function Lt(t,e,n){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName?t?.id?.length&&t.id===e.id?!0:ue(n,t,e)>0:!1}function Me(t,e){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName}function Rt(t,e,n){for(;t!==e;){let r=t;if(t=t?.nextSibling,!r)throw M(ee,"NoTemporaryNodeFound",{startInclusive:t,endExclusive:e});Ot(r,n)}return J(n,e),e.nextSibling}function qn(t,e,n,r,i){let o=ue(i,n,e),s=null;if(o>0){s=r;let a=0;for(;s!=null;){if(Lt(n,s,i))return s;if(a+=ue(i,s,t),a>o)return null;s=s.nextSibling}}return s}function Wn(t,e,n,r){let i=n,o=e.nextSibling,s=0;for(;i&&o;){if(ue(r,i,t)>0)return null;if(Me(e,i))return i;if(Me(o,i)&&(s++,o=o.nextSibling,s>=2))return null;i=i.nextSibling}return i}var Pt=new DOMParser;function $n(t){let e=t.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(e.match(/<\/html>/)||e.match(/<\/head>/)||e.match(/<\/body>/)){let i=Pt.parseFromString(t,"text/html");if(e.match(/<\/html>/))return Re.add(i),i;let o=i.firstChild;return o?(Re.add(o),o):null}let r=Pt.parseFromString(`<body><template>${t}</template></body>`,"text/html").body.querySelector("template")?.content;if(!r)throw M(ee,"NoContentFound",{newContent:t});return Re.add(r),r}function Un(t){if(t==null)return document.createElement("div");if(Re.has(t))return t;if(t instanceof Node){let n=document.createElement("div");return n.append(t),n}let e=document.createElement("div");for(let n of[...t])e.append(n);return e}function jn(t,e,n){let r=[],i=[];for(;t;)r.push(t),t=t.previousSibling;for(;r.length>0;){let o=r.pop();i.push(o),e?.parentElement?.insertBefore(o,e)}for(i.push(e);n;)r.push(n),i.push(n),n=n.nextSibling;for(;r.length;)e?.parentElement?.insertBefore(r.pop(),e.nextSibling);return i}function Bn(t,e,n){let r=t.firstChild,i=r,o=0;for(;r;){let s=Gn(r,e,n);s>o&&(i=r,o=s),r=r.nextSibling}return i}function Gn(t,e,n){return Me(t,e)?.5+ue(n,t,e):0}function Ot(t,e){J(e,t),t.remove()}function Kn(t,e){return!t.deadIds.has(e)}function Jn(t,e,n){return t.idMap.get(n)?.has(e)||!1}function J(t,e){let n=t.idMap.get(e);if(n)for(let r of n)t.deadIds.add(r)}function ue(t,e,n){let r=t.idMap.get(e);if(!r)return 0;let i=0;for(let o of r)Kn(t,o)&&Jn(t,o,n)&&++i;return i}function Mt(t,e){let n=t.parentElement,r=t.querySelectorAll("[id]");for(let i of r){let o=i;for(;o!==n&&o;){let s=e.get(o);s==null&&(s=new Set,e.set(o,s)),s.add(i.id),o=o.parentElement}}}function zn(t,e){let n=new Map;return Mt(t,n),Mt(e,n),n}var kt={type:2,name:I.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");H(I.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=Xe,settleDuration:o=`${de}`,useViewTransition:s=`${me}`})=>{let a=Number.parseInt(o),u=U(s);e.innerHTML=n.trim();let c=[...e.content.children];for(let l of c){if(!(l instanceof Element))throw k("NoFragmentsFound",t);let f=r||`#${l.getAttribute("id")}`,d=[...document.querySelectorAll(f)||[]];if(!d.length)throw k("NoTargetsFound",t,{selectorOrID:f});Z&&u?le.startViewTransition(()=>Vt(t,i,a,l,d)):Vt(t,i,a,l,d)}})}};function Vt(t,e,n,r,i){for(let o of i){o.classList.add(K);let s=o.outerHTML,a=o;switch(e){case V.Morph:{let l=Nt(a,r);if(!l?.length)throw k("MorphFailed",t);a=l[0];break}case V.Inner:a.innerHTML=r.outerHTML;break;case V.Outer:a.replaceWith(r);break;case V.Prepend:a.prepend(r);break;case V.Append:a.append(r);break;case V.Before:a.before(r);break;case V.After:a.after(r);break;case V.UpsertAttributes:for(let l of r.getAttributeNames()){let f=r.getAttribute(l);a.setAttribute(l,f)}break;default:throw k("InvalidMergeMode",t,{mergeMode:e})}let u=a.classList;u.add(K),setTimeout(()=>{o.classList.remove(K),u.remove(K)},n);let c=a.outerHTML;s!==c&&(u.add(He),setTimeout(()=>{u.remove(He)},n))}}var Ft={type:2,name:I.MergeSignals,onGlobalInit:async t=>{H(I.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${Je}`})=>{let{signals:r}=t,i=U(n);r.merge(ge(e),i),t.apply(document.body)})}};var Ht={type:2,name:I.RemoveFragments,onGlobalInit:async t=>{H(I.RemoveFragments,({selector:e,settleDuration:n=`${de}`,useViewTransition:r=`${me}`})=>{if(!e.length)throw k("NoSelectorProvided",t);let i=Number.parseInt(n),o=U(r),s=document.querySelectorAll(e),a=()=>{for(let u of s)u.classList.add(K);setTimeout(()=>{for(let u of s)u.remove()},i)};Z&&o?le.startViewTransition(()=>a()):a()})}};var qt={type:2,name:I.RemoveSignals,onGlobalInit:async t=>{H(I.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw k("NoPathsProvided",t);t.signals.remove(...n),t.apply(document.body)})}};var Wt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw v("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var $t={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement))throw v("CustomValidityInvalidElement",t);let i=n();return r(()=>{let o=i();if(typeof o!="string")throw v("CustomValidityInvalidExpression",t,{result:o});e.setCustomValidity(o)})}};var Ut="once",jt="half",Bt="full",Gt={type:1,name:"intersects",keyReq:2,mods:new Set([Ut,jt,Bt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(Bt)?i.threshold=1:n.has(jt)&&(i.threshold=.5);let o=r(),s=new IntersectionObserver(a=>{for(let u of a)u.isIntersecting&&(o(),n.has(Ut)&&(s.disconnect(),delete t.dataset[e]))},i);return s.observe(t),()=>s.disconnect()}};var Kt="session",Jt={type:1,name:"persist",mods:new Set([Kt]),onLoad:({key:t,value:e,signals:n,effect:r,mods:i})=>{t===""&&(t=O);let o=i.has(Kt)?sessionStorage:localStorage,s=e.split(/\s+/).filter(c=>c!=="");s=s.map(c=>j(c));let a=()=>{let c=o.getItem(t)||"{}",l=JSON.parse(c);n.merge(l)},u=()=>{let c;s.length?c=n.subset(...s):c=n.values(),o.setItem(t,JSON.stringify(c))};return a(),r(()=>{u()})}};var zt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,o=new URL(r,i).toString();window.history.replaceState({},"",o)})}};var Ne="smooth",qe="instant",We="auto",Xt="hstart",Yt="hcenter",Qt="hend",Zt="hnearest",en="vstart",tn="vcenter",nn="vend",rn="vnearest",Xn="focus",Ce="center",on="start",sn="end",an="nearest",ln={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ne,qe,We,Xt,Yt,Qt,Zt,en,tn,nn,rn,Xn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:Ne,block:Ce,inline:Ce};if(n.has(Ne)&&(i.behavior=Ne),n.has(qe)&&(i.behavior=qe),n.has(We)&&(i.behavior=We),n.has(Xt)&&(i.inline=on),n.has(Yt)&&(i.inline=Ce),n.has(Qt)&&(i.inline=sn),n.has(Zt)&&(i.inline=an),n.has(en)&&(i.block=on),n.has(tn)&&(i.block=Ce),n.has(nn)&&(i.block=sn),n.has(rn)&&(i.block=an),!(e instanceof HTMLElement||e instanceof SVGElement))throw v("ScrollIntoViewInvalidElement",t);return e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r],()=>{}}};var un="none",cn="display",fn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===un&&t.removeProperty(cn):t.setProperty(cn,un)})}};var pn="view-transition",dn={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===pn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=pn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!Z){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var mn={type:1,name:"attr",valReq:1,onLoad:({el:t,genRX:e,key:n,effect:r})=>{let i=e();return n===""?r(async()=>{let o=i();for(let[s,a]of Object.entries(o))t.setAttribute(s,a)}):(n=W(n),r(async()=>{let o=!1;try{o=i()}catch{}let s;typeof o=="string"?s=o:s=JSON.stringify(o),!s||s==="false"||s==="null"||s==="undefined"?t.removeAttribute(n):t.setAttribute(n,s)}))}};var Yn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,gn=["change","input","keydown"],hn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,value:n,key:r,signals:i,effect:o}=t,s=r||j(n),a=()=>{},u=()=>{},c=e.tagName.toLowerCase(),l="",f=c.includes("input"),d=e.getAttribute("type"),b=c.includes("checkbox")||f&&d==="checkbox";b&&(l=!1),f&&d==="number"&&(l=0);let w=c.includes("select"),p=c.includes("radio")||f&&d==="radio",m=f&&d==="file";p&&(e.getAttribute("name")?.length||e.setAttribute("name",s)),i.upsertIfMissing(s,l),a=()=>{let S="value"in e,T=i.value(s),A=`${T}`;if(b||p){let P=e;b?P.checked=!!T||T==="true":p&&(P.checked=A===P.value)}else if(!m)if(w){let P=e;if(P.multiple)for(let N of P.options){if(N?.disabled)return;Array.isArray(T)||typeof T=="string"?N.selected=T.includes(N.value):typeof T=="number"?N.selected=T===Number(N.value):N.selected=T}else P.value=A}else S?e.value=A:e.setAttribute("value",A)},u=async()=>{if(m){let A=[...e?.files||[]],P=[],N=[],y=[];await Promise.all(A.map(L=>new Promise(C=>{let x=new FileReader;x.onload=()=>{if(typeof x.result!="string")throw v("InvalidFileResultType",t,{resultType:typeof x.result});let $=x.result.match(Yn);if(!$?.groups)throw v("InvalidDataUri",t,{result:x.result});P.push($.groups.contents),N.push($.groups.mime),y.push(L.name)},x.onloadend=()=>C(void 0),x.readAsDataURL(L)}))),i.setValue(s,P),i.setValue(`${s}Mimes`,N),i.setValue(`${s}Names`,y);return}let S=i.value(s),T=e||e;if(typeof S=="number"){let A=Number(T.value||T.getAttribute("value"));i.setValue(s,A)}else if(typeof S=="string"){let A=T.value||T.getAttribute("value")||"";i.setValue(s,A)}else if(typeof S=="boolean")if(b){let A=T.checked||T.getAttribute("checked")==="true";i.setValue(s,A)}else{let A=!!(T.value||T.getAttribute("value"));i.setValue(s,A)}else if(!(typeof S>"u"))if(Array.isArray(S))if(w){let N=[...e.selectedOptions].filter(y=>y.selected).map(y=>y.value);i.setValue(s,N)}else{let A=JSON.stringify(T.value.split(","));i.setValue(s,A)}else throw v("BindUnsupportedSignalType",t,{signalType:typeof S})};for(let S of gn)e.addEventListener(S,u);let _=o(()=>a());return()=>{_();for(let S of gn)e.removeEventListener(S,u)}}};var yn={type:1,name:"class",valReq:1,onLoad:({key:t,el:e,genRX:n,effect:r})=>{let i=e.classList,o=n();return r(()=>{if(t===""){let s=o();for(let[a,u]of Object.entries(s)){let c=a.split(/\s+/);u?i.add(...c):i.remove(...c)}}else{let s=o(),a=W(t);s?i.add(a):i.remove(a)}})}};function ce(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function te(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function vn(t,e){return(...n)=>{setTimeout(()=>{t(...n)},e)}}function bn(t,e,n=!1,r=!0){let i=-1,o=()=>i&&clearTimeout(i);return(...s)=>{o(),n&&!i&&t(...s),i=setTimeout(()=>{r&&t(...s),o()},e)}}function Sn(t,e,n=!0,r=!1){let i=!1;return(...o)=>{i||(n&&t(...o),i=!0,setTimeout(()=>{i=!1,r&&t(...o)},e))}}var $e=new Map,Qn="evt",En={type:1,name:"on",keyReq:1,valReq:1,argNames:[Qn],onLoad:({el:t,rawKey:e,key:n,value:r,genRX:i,mods:o,signals:s,effect:a})=>{let u=i(),c=t;o.has("window")&&(c=window);let l=p=>{p&&((o.has("prevent")||n==="submit")&&p.preventDefault(),o.has("stop")&&p.stopPropagation()),u(p)},f=o.get("delay");if(f){let p=ce(f);l=vn(l,p)}let d=o.get("debounce");if(d){let p=ce(d),m=te(d,"leading",!1),_=!te(d,"notrail",!1);l=bn(l,p,m,_)}let b=o.get("throttle");if(b){let p=ce(b),m=!te(b,"noleading",!1),_=te(b,"trail",!1);l=Sn(l,p,m,_)}let h={capture:!0,passive:!1,once:!1};o.has("capture")||(h.capture=!1),o.has("passive")&&(h.passive=!0),o.has("once")&&(h.once=!0);let w=W(n).toLowerCase();switch(w){case"load":return l(),delete t.dataset[e],()=>{};case"interval":{let p=1e3,m=o.get("duration");m&&(p=ce(m),te(m,"leading",!1)&&(t.dataset[e.replace(".leading","")]=r,delete t.dataset[e],l()));let _=setInterval(l,p);return()=>{clearInterval(_)}}case"raf":{let p,m=()=>{l(),p=requestAnimationFrame(m)};return p=requestAnimationFrame(m),()=>{p&&cancelAnimationFrame(p)}}case"signals-change":return tt(t,()=>{$e.delete(t.id)}),a(()=>{let p=o.has("remote"),m=s.JSON(!1,p);($e.get(t.id)||"")!==m&&($e.set(t.id,m),l())});default:{if(o.has("outside")){c=document;let m=l;l=S=>{let T=S?.target;t.contains(T)||m(S)}}return c.addEventListener(w,l,h),()=>{c.removeEventListener(w,l)}}}}};var Tn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,value:n,signals:r})=>{let i=e||j(n);return r.setValue(i,t),()=>r.setValue(i,null)}};var An={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t,i=n();return e instanceof HTMLElement||v("TextInvalidElement",t),r(()=>{let o=i(t);e.textContent=`${o}`})}};var{round:Zn,max:er,min:tr}=Math,_n={type:3,name:"fit",fn:(t,e,n,r,i,o,s=!1,a=!1)=>{let u=(e-n)/(r-n)*(o-i)+i;return a&&(u=Zn(u)),s&&(u=er(i,tr(o,u))),u}};var xn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var wn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Te.load(mn,hn,yn,En,Tn,fn,An,_t,St,Tt,At,Et,bt,kt,Ft,Ht,qt,xt,Wt,$t,Gt,Jt,zt,ln,dn,_n,xn,wn);Te.apply(document.body);var Ss=Te;export{Ss as Datastar};
//# sourceMappingURL=datastar.js.map
