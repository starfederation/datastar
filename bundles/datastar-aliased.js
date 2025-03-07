// Datastar v1.0.0-beta.9
var rt=/🖕JS_DS🚀/.source,Se=rt.slice(0,5),Fe=rt.slice(4),H="datastar";var it="Datastar-Request",Te=300,ot=1e3,st="type module",Ae=!1,at=!1,lt=!0,U={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},ut=U.Morph,F={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var M=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(M||{});var fe=`${H}-signals`;var z=t=>t.trim()==="true",de=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),Z=t=>de(t).replace(/-./g,e=>e[1].toUpperCase()),qe=t=>de(t).replace(/-/g,"_"),Cn=t=>Z(t).replace(/^./,e=>e[0].toUpperCase()),Ee=t=>new Function(`return Object.assign({}, ${t})`)(),Y=t=>t.startsWith("$")?t.slice(1):t,Pn={kebab:de,snake:qe,pascal:Cn};function k(t,e){for(let n of e.get("case")||[]){let r=Pn[n];r&&(t=r(t))}return t}var In="computed",ct={type:1,name:In,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=k(t,e);let i=r();n.setComputed(t,i)}};var ft={type:1,name:"signals",onLoad:t=>{let{key:e,mods:n,signals:r,value:i,genRX:o}=t,a=n.has("ifmissing");if(e!==""){let s=k(e,n),l=i===""?i:o()();a?r.upsertIfMissing(s,l):r.setValue(s,l)}else{let s=Ee(t.value);t.value=JSON.stringify(s);let T=o()();r.merge(T,a)}}};var dt={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var re=class{#e=0;#t;constructor(e=H){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function _e(t){if(t.id)return t.id;let e=new re,n=t;for(;n;){if(e.with(n.tagName||""),n.id){e.with(n.id);break}let r=n?.parentNode;r&&e.with([...r.children].indexOf(n)),n=r}return e.string}function Re(t,e){return new re().with(t).with(e).value}function pe(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)pe(r,e),r=r.nextElementSibling}var Ln="https://data-star.dev/errors";function We(t,e,n={}){let r=new Error;r.name=`${H} ${t} error`;let i=qe(e),o=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),a=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${Ln}/${t}/${i}?${o}
Context: ${a}`,r}function B(t,e,n={}){return We("internal",e,Object.assign({from:t},n))}function W(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]}};return We("init",t,Object.assign(r,n))}function C(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return We("runtime",t,Object.assign(r,n))}var ie="preact-signals",Dn=Symbol.for("preact-signals"),j=1,oe=2,ge=4,ae=8,xe=16,se=32;function Ge(){we++}function Ue(){if(we>1){we--;return}let t,e=!1;for(;me!==void 0;){let n=me;for(me=void 0,$e++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~oe,!(n._flags&ae)&&mt(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if($e=0,we--,e)throw t}var P;var me,we=0,$e=0,Me=0;function pt(t){if(P===void 0)return;let e=t._node;if(e===void 0||e._target!==P)return e={_version:0,_source:t,_prevSource:P._sources,_nextSource:void 0,_target:P,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},P._sources!==void 0&&(P._sources._nextSource=e),P._sources=e,t._node=e,P._flags&se&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=P._sources,e._nextSource=void 0,P._sources._nextSource=e,P._sources=e),e}function L(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}L.prototype.brand=Dn;L.prototype._refresh=()=>!0;L.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};L.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};L.prototype.subscribe=function(t){return Ne(()=>{let e=this.value,n=P;P=void 0;try{t(e)}finally{P=n}})};L.prototype.valueOf=function(){return this.value};L.prototype.toString=function(){return`${this.value}`};L.prototype.toJSON=function(){return this.value};L.prototype.peek=function(){let t=P;P=void 0;try{return this.value}finally{P=t}};Object.defineProperty(L.prototype,"value",{get(){let t=pt(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if($e>100)throw B(ie,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Me++,Ge();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{Ue()}this?._onChange({old:e,revised:n})}}});function mt(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function gt(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ht(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function ee(t){L.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Me-1,this._flags=ge}ee.prototype=new L;ee.prototype._refresh=function(){if(this._flags&=~oe,this._flags&j)return!1;if((this._flags&(ge|se))===se||(this._flags&=~ge,this._globalVersion===Me))return!0;if(this._globalVersion=Me,this._flags|=j,this._version>0&&!mt(this))return this._flags&=~j,!0;let t=P;try{gt(this),P=this;let e=this._fn();(this._flags&xe||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~xe,this._version++)}catch(e){this._value=e,this._flags|=xe,this._version++}return P=t,ht(this),this._flags&=~j,!0};ee.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=ge|se;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}L.prototype._subscribe.call(this,t)};ee.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(L.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~se;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};ee.prototype._notify=function(){if(!(this._flags&oe)){this._flags|=ge|oe;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(ee.prototype,"value",{get(){if(this._flags&j)throw B(ie,"SignalCycleDetected");let t=pt(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&xe)throw B(ie,"GetComputedError",{value:this._value});return this._value}});function vt(t){return new ee(t)}function yt(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){Ge();let n=P;P=void 0;try{e()}catch(r){throw t._flags&=~j,t._flags|=ae,Be(t),B(ie,"CleanupEffectError",{error:r})}finally{P=n,Ue()}}}function Be(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,yt(t)}function Vn(t){if(P!==this)throw B(ie,"EndEffectError");ht(this),P=t,this._flags&=~j,this._flags&ae&&Be(this),Ue()}function he(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=se}he.prototype._callback=function(){let t=this._start();try{if(this._flags&ae||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};he.prototype._start=function(){if(this._flags&j)throw B(ie,"SignalCycleDetected");this._flags|=j,this._flags&=~ae,yt(this),gt(this),Ge();let t=P;return P=this,Vn.bind(this,t)};he.prototype._notify=function(){this._flags&oe||(this._flags|=oe,this._nextBatchedEffect=me,me=this)};he.prototype._dispose=function(){this._flags|=ae,this._flags&j||Be(this)};function Ne(t){let e=new he(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var bt="namespacedSignals",le=t=>{document.dispatchEvent(new CustomEvent(fe,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function St(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof L?n[r]=i.value:n[r]=St(i)}return n}function Tt(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw B(bt,"InvalidSignalKey",{key:i});let o=e[i];if(o instanceof Object&&!Array.isArray(o)){t[i]||(t[i]={});let a=Tt(t[i],o,n);r.added.push(...a.added.map(s=>`${i}.${s}`)),r.removed.push(...a.removed.map(s=>`${i}.${s}`)),r.updated.push(...a.updated.map(s=>`${i}.${s}`))}else{if(Object.hasOwn(t,i)){if(n)continue;let l=t[i];if(l instanceof L){let T=l.value;l.value=o,T!==o&&r.updated.push(i);continue}}let s=new L(o);s._onChange=()=>{le({updated:[i]})},t[i]=s,r.added.push(i)}}return r}function At(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof L?e(n,r):At(r,(i,o)=>{e(`${n}.${i}`,o)})}}function kn(t,...e){let n={};for(let r of e){let i=r.split("."),o=t,a=n;for(let l=0;l<i.length-1;l++){let T=i[l];if(!o[T])return{};a[T]||(a[T]={}),o=o[T],a=a[T]}let s=i[i.length-1];a[s]=o[s]}return n}var Ce=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let a=0;a<n.length-1;a++){let s=n[a];if(!r[s])return null;r=r[s]}let i=n[n.length-1],o=r[i];if(!o)throw B(bt,"SignalNotFound",{path:e});return o}setSignal(e,n){let r=e.split("."),i=this.#e;for(let a=0;a<r.length-1;a++){let s=r[a];i[s]||(i[s]={}),i=i[s]}let o=r[r.length-1];i[o]=n}setComputed(e,n){let r=vt(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let{signal:r}=this.upsertIfMissing(e,n),i=r.value;r.value=n,i!==n&&le({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let l=0;l<r.length-1;l++){let T=r[l];i[T]||(i[T]={}),i=i[T]}let o=r[r.length-1],a=i[o];if(a instanceof L)return{signal:a,inserted:!1};let s=new L(n);return s._onChange=()=>{le({updated:[e]})},i[o]=s,le({added:[e]}),{signal:s,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let i=r.split("."),o=this.#e;for(let s=0;s<i.length-1;s++){let l=i[s];if(!o[l])return;o=o[l]}let a=i[i.length-1];delete o[a],n.push(r)}le({removed:n})}merge(e,n=!1){let r=Tt(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&le(r)}subset(...e){return kn(this.values(),...e)}walk(e){At(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return St(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var Et=new Ce,Ke=[],Pe={},On=[],te=new Map,je=null,Je="";function ze(t){Je=t}function Ie(...t){for(let e of t){let n={plugin:e,signals:Et,effect:i=>Ne(i),actions:Pe,removals:te,applyToElement:Le},r;switch(e.type){case 2:{let i=e;On.push(i),r=i.onGlobalInit;break}case 3:{Pe[e.name]=e;break}case 1:{let i=e;Ke.push(i),r=i.onGlobalInit;break}default:throw W("InvalidPluginType",n)}r&&r(n)}Ke.sort((e,n)=>{let r=n.name.length-e.name.length;return r!==0?r:e.name.localeCompare(n.name)})}function Ye(){queueMicrotask(()=>{Le(document.documentElement),Hn()})}function Le(t){pe(t,e=>{let n=new Array,r=te.get(e.id)||new Map,i=new Map([...r]),o=new Map;for(let a of Object.keys(e.dataset)){if(!a.startsWith(Je))break;let s=e.dataset[a]||"",l=Re(a,s);o.set(a,l),r.has(l)?i.delete(l):n.push(a)}for(let[a,s]of i)s();for(let a of n){let s=o.get(a);Fn(e,a,s)}})}function Hn(){je||(je=new MutationObserver(t=>{let e=new Set,n=new Set;for(let{target:r,type:i,addedNodes:o,removedNodes:a}of t)switch(i){case"childList":{for(let s of a)e.add(s);for(let s of o)n.add(s)}break;case"attributes":{n.add(r);break}}for(let r of e){let i=te.get(r.id);if(i){for(let[o,a]of i)a(),i.delete(o);i.size===0&&te.delete(r.id)}}for(let r of n)Le(r)}),je.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function Fn(t,e,n){let r=Z(e.slice(Je.length)),i=Ke.find(h=>r.startsWith(h.name));if(!i)return;t.id.length||(t.id=_e(t));let[o,...a]=r.slice(i.name.length).split(/\_\_+/),s=o.length>0;s&&(o=Z(o));let l=t.dataset[e]||"",T=l.length>0,A={signals:Et,applyToElement:Le,effect:h=>Ne(h),actions:Pe,removals:te,genRX:()=>qn(A,...i.argNames||[]),plugin:i,el:t,rawKey:r,key:o,value:l,mods:new Map},w=i.keyReq||0;if(s){if(w===2)throw C(`${i.name}KeyNotAllowed`,A)}else if(w===1)throw C(`${i.name}KeyRequired`,A);let b=i.valReq||0;if(T){if(b===2)throw C(`${i.name}ValueNotAllowed`,A)}else if(b===1)throw C(`${i.name}ValueRequired`,A);if(w===3||b===3){if(s&&T)throw C(`${i.name}KeyAndValueProvided`,A);if(!s&&!T)throw C(`${i.name}KeyOrValueRequired`,A)}for(let h of a){let[S,...g]=h.split(".");A.mods.set(Z(S),new Set(g.map(u=>u.toLowerCase())))}let _=i.onLoad(A)??(()=>{}),y=te.get(t.id);y||(y=new Map,te.set(t.id,y)),y.set(n,_)}function qn(t,...e){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=t.value.trim().match(r);if(i){let _=i.length-1,y=i[_].trim();y.startsWith("return")||(i[_]=`return (${y});`),n=i.join(`;
`)}let o=new Map,a=new RegExp(`(?:${Se})(.*?)(?:${Fe})`,"gm");for(let _ of n.matchAll(a)){let y=_[1],h=new re("dsEscaped").with(y).string;o.set(h,y),n=n.replace(Se+y+Fe,h)}let s=/@(\w*)\(/gm,l=n.matchAll(s),T=new Set;for(let _ of l)T.add(_[1]);let A=new RegExp(`@(${Object.keys(Pe).join("|")})\\(`,"gm");n=n.replaceAll(A,"ctx.actions.$1.fn(ctx,");let w=t.signals.paths();if(w.length){let _=new RegExp(`\\$(${w.join("|")})(\\W|$)`,"gm");n=n.replaceAll(_,"ctx.signals.signal('$1').value$2")}for(let[_,y]of o)n=n.replace(_,y);let b=`return (()=> {
${n}
})()`;t.fnContent=b;try{let _=new Function("ctx",...e,b);return(...y)=>{try{return _(t,...y)}catch(h){throw C("ExecuteExpression",t,{error:h.message})}}}catch(_){throw C("GenerateExpression",t,{error:_.message})}}Ie(dt,ft,ct);async function Wn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function $n(t){let e,n,r,i=!1;return function(a){e===void 0?(e=a,n=0,r=-1):e=Un(e,a);let s=e.length,l=0;for(;n<s;){i&&(e[n]===10&&(l=++n),i=!1);let T=-1;for(;n<s&&T===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-l);break;case 13:i=!0;case 10:T=n;break}if(T===-1)break;t(e.subarray(l,T),r),l=n,r=-1}l===s?e=void 0:l!==0&&(e=e.subarray(l),n-=l)}}function Gn(t,e,n){let r=_t(),i=new TextDecoder;return function(a,s){if(a.length===0)n?.(r),r=_t();else if(s>0){let l=i.decode(a.subarray(0,s)),T=s+(a[s+1]===32?2:1),A=i.decode(a.subarray(T));switch(l){case"data":r.data=r.data?`${r.data}
${A}`:A;break;case"event":r.event=A;break;case"id":t(r.id=A);break;case"retry":{let w=Number.parseInt(A,10);Number.isNaN(w)||e(r.retry=w);break}}}}}function Un(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function _t(){return{data:"",event:"",id:"",retry:void 0}}var Bn="text/event-stream",Rt="last-event-id";function xt(t,e,{signal:n,headers:r,onopen:i,onmessage:o,onclose:a,onerror:s,openWhenHidden:l,fetch:T,retryInterval:A=1e3,retryScaler:w=2,retryMaxWaitMs:b=3e4,retryMaxCount:_=10,...y}){return new Promise((h,S)=>{let g=0,u={...r};u.accept||(u.accept=Bn);let p;function f(){p.abort(),document.hidden||E()}l||document.addEventListener("visibilitychange",f);let c=0;function v(){document.removeEventListener("visibilitychange",f),window.clearTimeout(c),p.abort()}n?.addEventListener("abort",()=>{v(),h()});let m=T??window.fetch,d=i??function(){};async function E(){p=new AbortController;try{let x=await m(e,{...y,headers:u,signal:p.signal});await d(x),await Wn(x.body,$n(Gn(R=>{R?u[Rt]=R:delete u[Rt]},R=>{A=R},o))),a?.(),v(),h()}catch(x){if(!p.signal.aborted)try{let R=s?.(x)??A;window.clearTimeout(c),c=window.setTimeout(E,R),A*=w,A=Math.min(A,b),g++,g>=_?(v(),S(C("SseMaxRetries",t,{retryMaxCount:_}))):console.error(`Datastar failed to reach ${y.method}: ${e.toString()} retry in ${R}ms`)}catch(R){v(),S(R)}}}E()})}var ue=`${H}-sse`,Xe=`${H}-settling`,ne=`${H}-swapping`,De="started",Ve="finished",wt="error",Mt="retrying";function K(t,e){document.addEventListener(ue,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function ve(t,e){document.dispatchEvent(new CustomEvent(ue,{detail:{type:t,argsRaw:e}}))}var Nt=t=>`${t}`.includes("text/event-stream"),J=async(t,e,n,r)=>{let{el:{id:i},el:o,signals:a}=t,{headers:s,contentType:l,includeLocal:T,selector:A,openWhenHidden:w,retryInterval:b,retryScaler:_,retryMaxWaitMs:y,retryMaxCount:h,abort:S}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:ot,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),g=e.toLowerCase(),u=()=>{};try{if(ve(De,{elId:i}),!n?.length)throw C("SseNoUrlProvided",t,{action:g});let p={};p[it]=!0,l==="json"&&(p["Content-Type"]="application/json");let f=Object.assign({},p,s),c={method:e,headers:f,openWhenHidden:w,retryInterval:b,retryScaler:_,retryMaxWaitMs:y,retryMaxCount:h,signal:S,onopen:async d=>{if(d.status>=400){let E=d.status.toString();ve(wt,{status:E})}},onmessage:d=>{if(!d.event.startsWith(H))return;let E=d.event,x={},R=d.data.split(`
`);for(let D of R){let I=D.indexOf(" "),O=D.slice(0,I),V=x[O];V||(V=[],x[O]=V);let $=D.slice(I+1);V.push($)}let N={};for(let[D,I]of Object.entries(x))N[D]=I.join(`
`);ve(E,N)},onerror:d=>{if(Nt(d))throw C("InvalidContentType",t,{url:n});d&&(console.error(d.message),ve(Mt,{message:d.message}))}},v=new URL(n,window.location.origin),m=new URLSearchParams(v.search);if(l==="json"){let d=a.JSON(!1,!T);e==="GET"?m.set(H,d):c.body=d}else if(l==="form"){let d=A?document.querySelector(A):o.closest("form");if(d===null)throw A?C("SseFormNotFound",t,{action:g,selector:A}):C("SseClosestFormNotFound",t,{action:g});if(o!==d){let x=R=>R.preventDefault();d.addEventListener("submit",x),u=()=>d.removeEventListener("submit",x)}if(!d.checkValidity()){d.reportValidity(),u();return}let E=new FormData(d);if(e==="GET"){let x=new URLSearchParams(E);for(let[R,N]of x)m.set(R,N)}else c.body=E}else throw C("SseInvalidContentType",t,{action:g,contentType:l});v.search=m.toString();try{await xt(t,v.toString(),c)}catch(d){if(!Nt(d))throw C("SseFetchFailed",t,{method:e,url:n,error:d})}}finally{ve(Ve,{elId:i}),u()}};var Ct={type:3,name:"delete",fn:async(t,e,n)=>J(t,"DELETE",e,{...n})};var Pt={type:3,name:"get",fn:async(t,e,n)=>J(t,"GET",e,{...n})};var It={type:3,name:"patch",fn:async(t,e,n)=>J(t,"PATCH",e,{...n})};var Lt={type:3,name:"post",fn:async(t,e,n)=>J(t,"POST",e,{...n})};var Dt={type:3,name:"put",fn:async(t,e,n)=>J(t,"PUT",e,{...n})};var Vt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?k(e,n):Y(i),{signal:a}=r.upsertIfMissing(o,!1),s=l=>{let{type:T,argsRaw:{elId:A}}=l.detail;if(A===t.id)switch(T){case De:a.value=!0;break;case Ve:a.value=!1;break}};return document.addEventListener(ue,s),()=>{document.removeEventListener(ue,s)}}};var kt={type:2,name:F.ExecuteScript,onGlobalInit:async t=>{K(F.ExecuteScript,({autoRemove:e=`${lt}`,attributes:n=st,script:r})=>{let i=z(e);if(!r?.length)throw W("NoScriptProvided",t);let o=document.createElement("script");for(let a of n.split(`
`)){let s=a.indexOf(" "),l=s?a.slice(0,s):a,T=s?a.slice(s):"";o.setAttribute(l.trim(),T.trim())}o.text=r,document.head.appendChild(o),i&&o.remove()})}};var ye=document,X=!!ye.startViewTransition;var Ot=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:b=>b.getAttribute("im-preserve")==="true",shouldReAppend:b=>b.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!0};function n(b,_,y={}){b=A(b);let h=w(_),S=T(b,h,y),g=i(S,()=>s(S,b,h,u=>u.morphStyle==="innerHTML"?(o(u,b,h),Array.from(b.childNodes)):r(u,b,h)));return S.pantry.remove(),g}function r(b,_,y){let h=w(_),S=Array.from(h.childNodes),g=S.indexOf(_),u=S.length-(g+1);return o(b,h,y,_,_.nextSibling),S=Array.from(h.childNodes),S.slice(g,S.length-u)}function i(b,_){if(!b.config.restoreFocus)return _();let y=document.activeElement;if(!(y instanceof HTMLInputElement||y instanceof HTMLTextAreaElement))return _();let{id:h,selectionStart:S,selectionEnd:g}=y,u=_();return h&&h!==document.activeElement?.id&&(y=b.target.querySelector(`#${h}`),y?.focus()),y&&!y.selectionEnd&&g&&y.setSelectionRange(S,g),u}let o=function(){function b(f,c,v,m=null,d=null){c instanceof HTMLTemplateElement&&v instanceof HTMLTemplateElement&&(c=c.content,v=v.content),m||=c.firstChild;for(let E of v.childNodes){if(m&&m!=d){let R=y(f,E,m,d);if(R){R!==m&&S(f,m,R),a(R,E,f),m=R.nextSibling;continue}}if(E instanceof Element&&f.persistentIds.has(E.id)){let R=g(c,E.id,m,f);a(R,E,f),m=R.nextSibling;continue}let x=_(c,E,m,f);x&&(m=x.nextSibling)}for(;m&&m!=d;){let E=m;m=m.nextSibling,h(f,E)}}function _(f,c,v,m){if(m.callbacks.beforeNodeAdded(c)===!1)return null;if(m.idMap.has(c)){let d=document.createElement(c.tagName);return f.insertBefore(d,v),a(d,c,m),m.callbacks.afterNodeAdded(d),d}else{let d=document.importNode(c,!0);return f.insertBefore(d,v),m.callbacks.afterNodeAdded(d),d}}let y=function(){function f(m,d,E,x){let R=null,N=d.nextSibling,D=0,I=E;for(;I&&I!=x;){if(v(I,d)){if(c(m,I,d))return I;R===null&&(m.idMap.has(I)||(R=I))}if(R===null&&N&&v(I,N)&&(D++,N=N.nextSibling,D>=2&&(R=void 0)),I.contains(document.activeElement))break;I=I.nextSibling}return R||null}function c(m,d,E){let x=m.idMap.get(d),R=m.idMap.get(E);if(!R||!x)return!1;for(let N of x)if(R.has(N))return!0;return!1}function v(m,d){let E=m,x=d;return E.nodeType===x.nodeType&&E.tagName===x.tagName&&(!E.id||E.id===x.id)}return f}();function h(f,c){if(f.idMap.has(c))p(f.pantry,c,null);else{if(f.callbacks.beforeNodeRemoved(c)===!1)return;c.parentNode?.removeChild(c),f.callbacks.afterNodeRemoved(c)}}function S(f,c,v){let m=c;for(;m&&m!==v;){let d=m;m=m.nextSibling,h(f,d)}return m}function g(f,c,v,m){let d=m.target.querySelector(`#${c}`)||m.pantry.querySelector(`#${c}`);return u(d,m),p(f,d,v),d}function u(f,c){let v=f.id;for(;f=f.parentNode;){let m=c.idMap.get(f);m&&(m.delete(v),m.size||c.idMap.delete(f))}}function p(f,c,v){if(f.moveBefore)try{f.moveBefore(c,v)}catch{f.insertBefore(c,v)}else f.insertBefore(c,v)}return b}(),a=function(){function b(u,p,f){return f.ignoreActive&&u===document.activeElement?null:(f.callbacks.beforeNodeMorphed(u,p)===!1||(u instanceof HTMLHeadElement&&f.head.ignore||(u instanceof HTMLHeadElement&&f.head.style!=="morph"?l(u,p,f):(_(u,p,f),g(u,f)||o(f,u,p))),f.callbacks.afterNodeMorphed(u,p)),u)}function _(u,p,f){let c=p.nodeType;if(c===1){let v=u,m=p,d=v.attributes,E=m.attributes;for(let x of E)S(x.name,v,"update",f)||v.getAttribute(x.name)!==x.value&&v.setAttribute(x.name,x.value);for(let x=d.length-1;0<=x;x--){let R=d[x];if(R&&!m.hasAttribute(R.name)){if(S(R.name,v,"remove",f))continue;v.removeAttribute(R.name)}}g(v,f)||y(v,m,f)}(c===8||c===3)&&u.nodeValue!==p.nodeValue&&(u.nodeValue=p.nodeValue)}function y(u,p,f){if(u instanceof HTMLInputElement&&p instanceof HTMLInputElement&&p.type!=="file"){let c=p.value,v=u.value;h(u,p,"checked",f),h(u,p,"disabled",f),p.hasAttribute("value")?v!==c&&(S("value",u,"update",f)||(u.setAttribute("value",c),u.value=c)):S("value",u,"remove",f)||(u.value="",u.removeAttribute("value"))}else if(u instanceof HTMLOptionElement&&p instanceof HTMLOptionElement)h(u,p,"selected",f);else if(u instanceof HTMLTextAreaElement&&p instanceof HTMLTextAreaElement){let c=p.value,v=u.value;if(S("value",u,"update",f))return;c!==v&&(u.value=c),u.firstChild&&u.firstChild.nodeValue!==c&&(u.firstChild.nodeValue=c)}}function h(u,p,f,c){let v=p[f],m=u[f];if(v!==m){let d=S(f,u,"update",c);d||(u[f]=p[f]),v?d||u.setAttribute(f,""):S(f,u,"remove",c)||u.removeAttribute(f)}}function S(u,p,f,c){return u==="value"&&c.ignoreActiveValue&&p===document.activeElement?!0:c.callbacks.beforeAttributeUpdated(u,p,f)===!1}function g(u,p){return!!p.ignoreActiveValue&&u===document.activeElement&&u!==document.body}return b}();function s(b,_,y,h){if(b.head.block){let S=_.querySelector("head"),g=y.querySelector("head");if(S&&g){let u=l(S,g,b);return Promise.all(u).then(()=>{let p=Object.assign(b,{head:{block:!1,ignore:!0}});return h(p)})}}return h(b)}function l(b,_,y){let h=[],S=[],g=[],u=[],p=new Map;for(let c of _.children)p.set(c.outerHTML,c);for(let c of b.children){let v=p.has(c.outerHTML),m=y.head.shouldReAppend(c),d=y.head.shouldPreserve(c);v||d?m?S.push(c):(p.delete(c.outerHTML),g.push(c)):y.head.style==="append"?m&&(S.push(c),u.push(c)):y.head.shouldRemove(c)!==!1&&S.push(c)}u.push(...p.values());let f=[];for(let c of u){let v=document.createRange().createContextualFragment(c.outerHTML).firstChild;if(y.callbacks.beforeNodeAdded(v)!==!1){if("href"in v&&v.href||"src"in v&&v.src){let m,d=new Promise(function(E){m=E});v.addEventListener("load",function(){m()}),f.push(d)}b.appendChild(v),y.callbacks.afterNodeAdded(v),h.push(v)}}for(let c of S)y.callbacks.beforeNodeRemoved(c)!==!1&&(b.removeChild(c),y.callbacks.afterNodeRemoved(c));return y.head.afterHeadMorphed(b,{added:h,kept:g,removed:S}),f}let T=function(){function b(p,f,c){let{persistentIds:v,idMap:m}=g(p,f),d=_(c),E=d.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(E))throw`Do not understand how to morph style ${E}`;return{target:p,newContent:f,config:d,morphStyle:E,ignoreActive:d.ignoreActive,ignoreActiveValue:d.ignoreActiveValue,restoreFocus:d.restoreFocus,idMap:m,persistentIds:v,pantry:y(),callbacks:d.callbacks,head:d.head}}function _(p){let f=Object.assign({},e);return Object.assign(f,p),f.callbacks=Object.assign({},e.callbacks,p.callbacks),f.head=Object.assign({},e.head,p.head),f}function y(){let p=document.createElement("div");return p.hidden=!0,document.body.insertAdjacentElement("afterend",p),p}function h(p){let f=Array.from(p.querySelectorAll("[id]"));return p.id&&f.push(p),f}function S(p,f,c,v){for(let m of v)if(f.has(m.id)){let d=m;for(;d;){let E=p.get(d);if(E==null&&(E=new Set,p.set(d,E)),E.add(m.id),d===c)break;d=d.parentElement}}}function g(p,f){let c=h(p),v=h(f),m=u(c,v),d=new Map;S(d,m,p,c);let E=f.__idiomorphRoot||f;return S(d,m,E,v),{persistentIds:m,idMap:d}}function u(p,f){let c=new Set,v=new Map;for(let{id:d,tagName:E}of p)v.has(d)?c.add(d):v.set(d,E);let m=new Set;for(let{id:d,tagName:E}of f)m.has(d)?c.add(d):v.get(d)===E&&m.add(d);for(let d of c)m.delete(d);return m}return b}(),{normalizeElement:A,normalizeParent:w}=function(){let b=new WeakSet;function _(g){return g instanceof Document?g.documentElement:g}function y(g){if(g==null)return document.createElement("div");if(typeof g=="string")return y(S(g));if(b.has(g))return g;if(g instanceof Node){if(g.parentNode)return h(g);{let u=document.createElement("div");return u.append(g),u}}else{let u=document.createElement("div");for(let p of[...g])u.append(p);return u}}function h(g){return{childNodes:[g],querySelectorAll:u=>{let p=g.querySelectorAll(u);return g.matches(u)?[g,...p]:p},insertBefore:(u,p)=>g.parentNode.insertBefore(u,p),moveBefore:(u,p)=>g.parentNode.moveBefore(u,p),get __idiomorphRoot(){return g}}}function S(g){let u=new DOMParser,p=g.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(p.match(/<\/html>/)||p.match(/<\/head>/)||p.match(/<\/body>/)){let f=u.parseFromString(g,"text/html");if(p.match(/<\/html>/))return b.add(f),f;{let c=f.firstChild;return c&&b.add(c),c}}else{let c=u.parseFromString("<body><template>"+g+"</template></body>","text/html").body.querySelector("template").content;return b.add(c),c}}return{normalizeElement:_,normalizeParent:y}}();return{morph:n,defaults:e}}();var Ft={type:2,name:F.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");K(F.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=ut,settleDuration:o=`${Te}`,useViewTransition:a=`${Ae}`})=>{let s=Number.parseInt(o),l=z(a);e.innerHTML=n.trim();let T=[...e.content.children];for(let A of T){if(!(A instanceof Element))throw W("NoFragmentsFound",t);let w=r||`#${A.getAttribute("id")}`,b=[...document.querySelectorAll(w)||[]];if(!b.length)throw W("NoTargetsFound",t,{selectorOrID:w});l&&X?ye.startViewTransition(()=>Ht(t,i,s,A,b)):Ht(t,i,s,A,b)}})}};function Ht(t,e,n,r,i){for(let o of i){o.classList.add(ne);let a=o.outerHTML,s=o;switch(e){case U.Morph:{let A=r.cloneNode(!0);pe(A,w=>{!w.id?.length&&Object.keys(w.dataset).length&&(w.id=_e(w));let b=t.removals.get(w.id);if(b){let _=new Map;for(let[y,h]of b){let S=Re(y,y);_.set(S,h),b.delete(y)}t.removals.set(w.id,_)}}),Ot.morph(s,A);break}case U.Inner:s.innerHTML=r.outerHTML;break;case U.Outer:s.replaceWith(r);break;case U.Prepend:s.prepend(r);break;case U.Append:s.append(r);break;case U.Before:s.before(r);break;case U.After:s.after(r);break;case U.UpsertAttributes:for(let A of r.getAttributeNames()){let w=r.getAttribute(A);s.setAttribute(A,w)}break;default:throw W("InvalidMergeMode",t,{mergeMode:e})}let l=s.classList;l?.add(ne),setTimeout(()=>{o.classList.remove(ne),l?.remove(ne)},n);let T=s.outerHTML;l&&a!==T&&(l.add(Xe),setTimeout(()=>{l.remove(Xe)},n))}}var qt={type:2,name:F.MergeSignals,onGlobalInit:async t=>{K(F.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${at}`})=>{let{signals:r}=t,i=z(n);r.merge(Ee(e),i)})}};var Wt={type:2,name:F.RemoveFragments,onGlobalInit:async t=>{K(F.RemoveFragments,({selector:e,settleDuration:n=`${Te}`,useViewTransition:r=`${Ae}`})=>{if(!e.length)throw W("NoSelectorProvided",t);let i=Number.parseInt(n),o=z(r),a=document.querySelectorAll(e),s=()=>{for(let l of a)l.classList.add(ne);setTimeout(()=>{for(let l of a)l.remove()},i)};o&&X?ye.startViewTransition(()=>s()):s()})}};var $t={type:2,name:F.RemoveSignals,onGlobalInit:async t=>{K(F.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw W("NoPathsProvided",t);t.signals.remove(...n)})}};var Gt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw C("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var Ut={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw C("CustomValidityInvalidElement",t);let i=n();return r(()=>{let o=i();if(typeof o!="string")throw C("CustomValidityInvalidExpression",t,{result:o});e.setCustomValidity(o)})}};var Bt="once",jt="half",Kt="full",Jt={type:1,name:"intersects",keyReq:2,mods:new Set([Bt,jt,Kt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(Kt)?i.threshold=1:n.has(jt)&&(i.threshold=.5);let o=r(),a=new IntersectionObserver(s=>{for(let l of s)l.isIntersecting&&(o(),n.has(Bt)&&(a.disconnect(),delete t.dataset[e]))},i);return a.observe(t),()=>a.disconnect()}};var zt="session",Yt={type:1,name:"persist",mods:new Set([zt]),onLoad:({key:t,effect:e,mods:n,signals:r,value:i})=>{t=k(t,n),t===""&&(t=H);let o=n.has(zt)?sessionStorage:localStorage,a=i.split(/\s+/).filter(T=>T!=="");a=a.map(T=>Y(T));let s=()=>{let T=o.getItem(t)||"{}",A=JSON.parse(T);r.merge(A)},l=()=>{let T;a.length?T=r.subset(...a):T=r.values(),o.setItem(t,JSON.stringify(T))};return s(),e(()=>{l()})}};var Xt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,o=new URL(r,i).toString();window.history.replaceState({},"",o)})}};var ke="smooth",Qe="instant",Ze="auto",Qt="hstart",Zt="hcenter",en="hend",tn="hnearest",nn="vstart",rn="vcenter",on="vend",sn="vnearest",jn="focus",Oe="center",an="start",ln="end",un="nearest",cn={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([ke,Qe,Ze,Qt,Zt,en,tn,nn,rn,on,sn,jn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:ke,block:Oe,inline:Oe};if(n.has(ke)&&(i.behavior=ke),n.has(Qe)&&(i.behavior=Qe),n.has(Ze)&&(i.behavior=Ze),n.has(Qt)&&(i.inline=an),n.has(Zt)&&(i.inline=Oe),n.has(en)&&(i.inline=ln),n.has(tn)&&(i.inline=un),n.has(nn)&&(i.block=an),n.has(rn)&&(i.block=Oe),n.has(on)&&(i.block=ln),n.has(sn)&&(i.block=un),!(e instanceof HTMLElement||e instanceof SVGElement))throw C("ScrollIntoViewInvalidElement",t);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r]}};var fn="none",dn="display",pn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===fn&&t.removeProperty(dn):t.setProperty(dn,fn)})}};var mn="view-transition",gn={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===mn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=mn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!X){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var hn={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let i=r();return e===""?n(async()=>{let o=i();for(let[a,s]of Object.entries(o))s===!1?t.removeAttribute(a):t.setAttribute(a,s)}):(e=de(e),n(async()=>{let o=!1;try{o=i()}catch{}let a;typeof o=="string"?a=o:a=JSON.stringify(o),!a||a==="false"||a==="null"||a==="undefined"?t.removeAttribute(e):t.setAttribute(e,a)}))}};var Kn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,vn=["change","input","keydown"],yn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:i,value:o,effect:a}=t,s=e,l=n?k(n,r):Y(o),T=e.tagName.toLowerCase(),A=T.includes("input"),w=T.includes("select"),b=e.getAttribute("type"),_=e.hasAttribute("value"),y="",h=A&&b==="checkbox";h&&(y=_?"":!1);let S=A&&b==="number";S&&(y=0);let g=A&&b==="radio";g&&(e.getAttribute("name")?.length||e.setAttribute("name",l));let u=A&&b==="file",{signal:p,inserted:f}=i.upsertIfMissing(l,y),c=-1;Array.isArray(p.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",l),c=[...document.querySelectorAll(`[name="${l}"]`)].findIndex(N=>N===t.el));let v=c>=0,m=()=>[...i.value(l)],d=()=>{let N=i.value(l);v&&!w&&(N=N[c]||y);let D=`${N}`;if(h||g)typeof N=="boolean"?s.checked=N:s.checked=D===s.value;else if(w){let I=e;if(I.multiple){if(!v)throw C("BindSelectMultiple",t);for(let O of I.options){if(O?.disabled)return;let V=S?Number(O.value):O.value;O.selected=N.includes(V)}}else I.value=D}else u||("value"in e?e.value=D:e.setAttribute("value",D))},E=async()=>{let N=i.value(l);if(v){let V=N;for(;c>=V.length;)V.push(y);N=V[c]||y}let D=(V,$)=>{let G=$;v&&!w&&(G=m(),G[c]=$),i.setValue(V,G)};if(u){let V=[...s?.files||[]],$=[],G=[],tt=[];await Promise.all(V.map(nt=>new Promise(Nn=>{let Q=new FileReader;Q.onload=()=>{if(typeof Q.result!="string")throw C("InvalidFileResultType",t,{resultType:typeof Q.result});let He=Q.result.match(Kn);if(!He?.groups)throw C("InvalidDataUri",t,{result:Q.result});$.push(He.groups.contents),G.push(He.groups.mime),tt.push(nt.name)},Q.onloadend=()=>Nn(void 0),Q.readAsDataURL(nt)}))),D(l,$),D(`${l}Mimes`,G),D(`${l}Names`,tt);return}let I=s.value||"",O;if(h){let V=s.checked||s.getAttribute("checked")==="true";_?O=V?I:"":O=V}else if(w){let $=[...e.selectedOptions];v?O=$.filter(G=>G.selected).map(G=>G.value):O=$[0]?.value||y}else typeof N=="boolean"?O=!!I:typeof N=="number"?O=Number(I):O=I||"";D(l,O)};f&&E();for(let N of vn)e.addEventListener(N,E);let x=N=>{N.persisted&&E()};window.addEventListener("pageshow",x);let R=a(()=>d());return()=>{R();for(let N of vn)e.removeEventListener(N,E);window.removeEventListener("pageshow",x)}}};var bn={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:i})=>{let o=t.classList,a=i();return r(()=>{if(e===""){let s=a();for(let[l,T]of Object.entries(s)){let A=l.split(/\s+/);T?o.add(...A):o.remove(...A)}}else e=k(e,n),a()?o.add(e):o.remove(e)})}};function be(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ce(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function Sn(t,e){return(...n)=>{setTimeout(()=>{t(...n)},e)}}function Tn(t,e,n=!1,r=!0){let i=-1,o=()=>i&&clearTimeout(i);return(...a)=>{o(),n&&!i&&t(...a),i=setTimeout(()=>{r&&t(...a),o()},e)}}function An(t,e,n=!0,r=!1){let i=!1;return(...o)=>{i||(n&&t(...o),i=!0,setTimeout(()=>{i=!1,r&&t(...o)},e))}}var Jn="evt",et="signalsChange",zn=et.length,En={type:1,name:"on",keyReq:1,valReq:1,argNames:[Jn],onLoad:({el:t,key:e,mods:n,signals:r,effect:i,genRX:o})=>{let a=o(),s=t;n.has("window")&&(s=window);let l=h=>{h&&((n.has("prevent")||e==="submit")&&h.preventDefault(),n.has("stop")&&h.stopPropagation()),a(h)},T=n.get("delay");if(T){let h=be(T);l=Sn(l,h)}let A=n.get("debounce");if(A){let h=be(A),S=ce(A,"leading",!1),g=!ce(A,"notrail",!1);l=Tn(l,h,S,g)}let w=n.get("throttle");if(w){let h=be(w),S=!ce(w,"noleading",!1),g=ce(w,"trail",!1);l=An(l,h,S,g)}if(n.has("viewtransition")&&X){let h=l;l=(...S)=>document.startViewTransition(()=>h(...S))}let b={capture:!0,passive:!1,once:!1};if(n.has("capture")||(b.capture=!1),n.has("passive")&&(b.passive=!0),n.has("once")&&(b.once=!0),e==="load")return setTimeout(l,0),()=>{};if(e==="interval"){let h=1e3,S=n.get("duration");S&&(h=be(S),ce(S,"leading",!1)&&l());let g=setInterval(l,h);return()=>{clearInterval(g)}}if(e==="raf"){let h,S=()=>{l(),h=requestAnimationFrame(S)};return h=requestAnimationFrame(S),()=>{h&&cancelAnimationFrame(h)}}if(e.startsWith(et)){if(e===et){let g=u=>l(u);return document.addEventListener(fe,g),()=>{document.removeEventListener(fe,g)}}let h=k(Z(e.slice(zn)),n),S=new Map;return r.walk((g,u)=>{g.startsWith(h)&&S.set(u,u.value)}),i(()=>{for(let[g,u]of S)u!==g.value&&(l(),S.set(g,g.value))})}if(n.has("outside")){s=document;let h=l;l=g=>{let u=g?.target;t.contains(u)||h(g)}}let y=k(e,n);return s.addEventListener(y,l,b),()=>{s.removeEventListener(y,l)}}};var _n={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?k(e,n):Y(i);r.setValue(o,t)}};var Rn={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,i=r();return e instanceof HTMLElement||C("TextInvalidElement",t),n(()=>{let o=i(t);e.textContent=`${o}`})}};var{round:Yn,max:Xn,min:Qn}=Math,xn={type:3,name:"fit",fn:(t,e,n,r,i,o,a=!1,s=!1)=>{let l=(e-n)/(r-n)*(o-i)+i;return s&&(l=Yn(l)),a&&(l=Xn(i,Qn(o,l))),l}};var wn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var Mn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};ze("ds");Ie(hn,yn,bn,En,_n,pn,Rn,Vt,Pt,Lt,Dt,It,Ct,Ft,qt,Wt,$t,kt,Gt,Ut,Jt,Yt,Xt,cn,gn,xn,wn,Mn);Ye();export{Ye as apply,Ie as load,ze as setAlias};
//# sourceMappingURL=datastar-aliased.js.map
