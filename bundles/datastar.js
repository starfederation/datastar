// Datastar v1.0.0-beta.9
var nt=/🖕JS_DS🚀/.source,be=nt.slice(0,5),Fe=nt.slice(4),H="datastar";var rt="Datastar-Request",Te=300,it=1e3,ot="type module",Ae=!1,st=!1,at=!0,U={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},lt=U.Morph,F={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var M=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(M||{});var fe=`${H}-signals`;var z=t=>t.trim()==="true",de=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),Z=t=>de(t).replace(/-./g,e=>e[1].toUpperCase()),qe=t=>de(t).replace(/-/g,"_"),Cn=t=>Z(t).replace(/^./,e=>e[0].toUpperCase()),Ee=t=>new Function(`return Object.assign({}, ${t})`)(),Y=t=>t.startsWith("$")?t.slice(1):t,Pn={kebab:de,snake:qe,pascal:Cn};function k(t,e){for(let n of e.get("case")||[]){let r=Pn[n];r&&(t=r(t))}return t}var In="computed",ut={type:1,name:In,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=k(t,e);let i=r();n.setComputed(t,i)}};var ct={type:1,name:"signals",onLoad:t=>{let{key:e,mods:n,signals:r,value:i,genRX:o}=t,l=n.has("ifmissing");if(e!==""){let s=k(e,n),c=i===""?i:o()();l?r.upsertIfMissing(s,c):r.setValue(s,c)}else{let s=Ee(t.value);t.value=JSON.stringify(s);let T=o()();r.merge(T,l)}}};var ft={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var re=class{#e=0;#t;constructor(e=H){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function _e(t){if(t.id)return t.id;let e=new re,n=t;for(;n;){if(e.with(n.tagName||""),n.id){e.with(n.id);break}let r=n?.parentNode;r&&e.with([...r.children].indexOf(n)),n=r}return e.string}function we(t,e){return new re().with(t).with(e).value}function pe(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)pe(r,e),r=r.nextElementSibling}var Ln="https://data-star.dev/errors";function We(t,e,n={}){let r=new Error;r.name=`${H} ${t} error`;let i=qe(e),o=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),l=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${Ln}/${t}/${i}?${o}
Context: ${l}`,r}function B(t,e,n={}){return We("internal",e,Object.assign({from:t},n))}function W(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]}};return We("init",t,Object.assign(r,n))}function C(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return We("runtime",t,Object.assign(r,n))}var ie="preact-signals",Vn=Symbol.for("preact-signals"),j=1,oe=2,ge=4,ae=8,Re=16,se=32;function Ge(){xe++}function Ue(){if(xe>1){xe--;return}let t,e=!1;for(;me!==void 0;){let n=me;for(me=void 0,$e++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~oe,!(n._flags&ae)&&pt(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if($e=0,xe--,e)throw t}var P;var me,xe=0,$e=0,Me=0;function dt(t){if(P===void 0)return;let e=t._node;if(e===void 0||e._target!==P)return e={_version:0,_source:t,_prevSource:P._sources,_nextSource:void 0,_target:P,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},P._sources!==void 0&&(P._sources._nextSource=e),P._sources=e,t._node=e,P._flags&se&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=P._sources,e._nextSource=void 0,P._sources._nextSource=e,P._sources=e),e}function L(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}L.prototype.brand=Vn;L.prototype._refresh=()=>!0;L.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};L.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};L.prototype.subscribe=function(t){return Ne(()=>{let e=this.value,n=P;P=void 0;try{t(e)}finally{P=n}})};L.prototype.valueOf=function(){return this.value};L.prototype.toString=function(){return`${this.value}`};L.prototype.toJSON=function(){return this.value};L.prototype.peek=function(){let t=P;P=void 0;try{return this.value}finally{P=t}};Object.defineProperty(L.prototype,"value",{get(){let t=dt(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if($e>100)throw B(ie,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Me++,Ge();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{Ue()}this?._onChange({old:e,revised:n})}}});function pt(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function mt(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function gt(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function ee(t){L.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Me-1,this._flags=ge}ee.prototype=new L;ee.prototype._refresh=function(){if(this._flags&=~oe,this._flags&j)return!1;if((this._flags&(ge|se))===se||(this._flags&=~ge,this._globalVersion===Me))return!0;if(this._globalVersion=Me,this._flags|=j,this._version>0&&!pt(this))return this._flags&=~j,!0;let t=P;try{mt(this),P=this;let e=this._fn();(this._flags&Re||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~Re,this._version++)}catch(e){this._value=e,this._flags|=Re,this._version++}return P=t,gt(this),this._flags&=~j,!0};ee.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=ge|se;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}L.prototype._subscribe.call(this,t)};ee.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(L.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~se;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};ee.prototype._notify=function(){if(!(this._flags&oe)){this._flags|=ge|oe;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(ee.prototype,"value",{get(){if(this._flags&j)throw B(ie,"SignalCycleDetected");let t=dt(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&Re)throw B(ie,"GetComputedError",{value:this._value});return this._value}});function ht(t){return new ee(t)}function vt(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){Ge();let n=P;P=void 0;try{e()}catch(r){throw t._flags&=~j,t._flags|=ae,Be(t),B(ie,"CleanupEffectError",{error:r})}finally{P=n,Ue()}}}function Be(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,vt(t)}function Dn(t){if(P!==this)throw B(ie,"EndEffectError");gt(this),P=t,this._flags&=~j,this._flags&ae&&Be(this),Ue()}function he(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=se}he.prototype._callback=function(){let t=this._start();try{if(this._flags&ae||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};he.prototype._start=function(){if(this._flags&j)throw B(ie,"SignalCycleDetected");this._flags|=j,this._flags&=~ae,vt(this),mt(this),Ge();let t=P;return P=this,Dn.bind(this,t)};he.prototype._notify=function(){this._flags&oe||(this._flags|=oe,this._nextBatchedEffect=me,me=this)};he.prototype._dispose=function(){this._flags|=ae,this._flags&j||Be(this)};function Ne(t){let e=new he(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var yt="namespacedSignals",le=t=>{document.dispatchEvent(new CustomEvent(fe,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function St(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof L?n[r]=i.value:n[r]=St(i)}return n}function bt(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw B(yt,"InvalidSignalKey",{key:i});let o=e[i];if(o instanceof Object&&!Array.isArray(o)){t[i]||(t[i]={});let l=bt(t[i],o,n);r.added.push(...l.added.map(s=>`${i}.${s}`)),r.removed.push(...l.removed.map(s=>`${i}.${s}`)),r.updated.push(...l.updated.map(s=>`${i}.${s}`))}else{if(Object.hasOwn(t,i)){if(n)continue;let c=t[i];if(c instanceof L){let T=c.value;c.value=o,T!==o&&r.updated.push(i);continue}}let s=new L(o);s._onChange=()=>{le({updated:[i]})},t[i]=s,r.added.push(i)}}return r}function Tt(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof L?e(n,r):Tt(r,(i,o)=>{e(`${n}.${i}`,o)})}}function kn(t,...e){let n={};for(let r of e){let i=r.split("."),o=t,l=n;for(let c=0;c<i.length-1;c++){let T=i[c];if(!o[T])return{};l[T]||(l[T]={}),o=o[T],l=l[T]}let s=i[i.length-1];l[s]=o[s]}return n}var Ce=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let l=0;l<n.length-1;l++){let s=n[l];if(!r[s])return null;r=r[s]}let i=n[n.length-1],o=r[i];if(!o)throw B(yt,"SignalNotFound",{path:e});return o}setSignal(e,n){let r=e.split("."),i=this.#e;for(let l=0;l<r.length-1;l++){let s=r[l];i[s]||(i[s]={}),i=i[s]}let o=r[r.length-1];i[o]=n}setComputed(e,n){let r=ht(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let{signal:r}=this.upsertIfMissing(e,n),i=r.value;r.value=n,i!==n&&le({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let c=0;c<r.length-1;c++){let T=r[c];i[T]||(i[T]={}),i=i[T]}let o=r[r.length-1],l=i[o];if(l instanceof L)return{signal:l,inserted:!1};let s=new L(n);return s._onChange=()=>{le({updated:[e]})},i[o]=s,le({added:[e]}),{signal:s,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let i=r.split("."),o=this.#e;for(let s=0;s<i.length-1;s++){let c=i[s];if(!o[c])return;o=o[c]}let l=i[i.length-1];delete o[l],n.push(r)}le({removed:n})}merge(e,n=!1){let r=bt(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&le(r)}subset(...e){return kn(this.values(),...e)}walk(e){Tt(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return St(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var At=new Ce,Ke=[],Pe={},On=[],te=new Map,je=null,Je="";function Et(t){Je=t}function Ie(...t){for(let e of t){let n={plugin:e,signals:At,effect:i=>Ne(i),actions:Pe,removals:te,applyToElement:Le},r;switch(e.type){case 2:{let i=e;On.push(i),r=i.onGlobalInit;break}case 3:{Pe[e.name]=e;break}case 1:{let i=e;Ke.push(i),r=i.onGlobalInit;break}default:throw W("InvalidPluginType",n)}r&&r(n)}Ke.sort((e,n)=>{let r=n.name.length-e.name.length;return r!==0?r:e.name.localeCompare(n.name)})}function ze(){Le(document.documentElement),Hn()}function Le(t){pe(t,e=>{let n=new Array,r=te.get(e.id)||new Map,i=new Map([...r]),o=new Map;for(let l of Object.keys(e.dataset)){if(!l.startsWith(Je))break;let s=e.dataset[l]||"",c=we(l,s);o.set(l,c),r.has(c)?i.delete(c):n.push(l)}for(let[l,s]of i)s();for(let l of n){let s=o.get(l);Fn(e,l,s)}})}function Hn(){je||(je=new MutationObserver(t=>{let e=new Set,n=new Set;for(let{target:r,type:i,addedNodes:o,removedNodes:l}of t)switch(i){case"childList":{for(let s of l)e.add(s);for(let s of o)n.add(s)}break;case"attributes":{n.add(r);break}}for(let r of e){let i=te.get(r.id);if(i){for(let[o,l]of i)l(),i.delete(o);i.size===0&&te.delete(r.id)}}for(let r of n)Le(r)}),je.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function Fn(t,e,n){let r=Z(e.slice(Je.length)),i=Ke.find(h=>r.startsWith(h.name));if(!i)return;t.id.length||(t.id=_e(t));let[o,...l]=r.slice(i.name.length).split(/\_\_+/),s=o.length>0;s&&(o=Z(o));let c=t.dataset[e]||"",T=c.length>0,A={signals:At,applyToElement:Le,effect:h=>Ne(h),actions:Pe,removals:te,genRX:()=>qn(A,...i.argNames||[]),plugin:i,el:t,rawKey:r,key:o,value:c,mods:new Map},x=i.keyReq||0;if(s){if(x===2)throw C(`${i.name}KeyNotAllowed`,A)}else if(x===1)throw C(`${i.name}KeyRequired`,A);let S=i.valReq||0;if(T){if(S===2)throw C(`${i.name}ValueNotAllowed`,A)}else if(S===1)throw C(`${i.name}ValueRequired`,A);if(x===3||S===3){if(s&&T)throw C(`${i.name}KeyAndValueProvided`,A);if(!s&&!T)throw C(`${i.name}KeyOrValueRequired`,A)}for(let h of l){let[b,...y]=h.split(".");A.mods.set(Z(b),new Set(y.map(a=>a.toLowerCase())))}let _=i.onLoad(A)??(()=>{}),v=te.get(t.id);v||(v=new Map,te.set(t.id,v)),v.set(n,_)}function qn(t,...e){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=t.value.trim().match(r);if(i){let _=i.length-1,v=i[_].trim();v.startsWith("return")||(i[_]=`return (${v});`),n=i.join(`;
`)}let o=new Map,l=new RegExp(`(?:${be})(.*?)(?:${Fe})`,"gm");for(let _ of n.matchAll(l)){let v=_[1],h=new re("dsEscaped").with(v).string;o.set(h,v),n=n.replace(be+v+Fe,h)}let s=/@(\w*)\(/gm,c=n.matchAll(s),T=new Set;for(let _ of c)T.add(_[1]);let A=new RegExp(`@(${Object.keys(Pe).join("|")})\\(`,"gm");n=n.replaceAll(A,"ctx.actions.$1.fn(ctx,");let x=t.signals.paths();if(x.length){let _=new RegExp(`\\$(${x.join("|")})(\\W|$)`,"gm");n=n.replaceAll(_,"ctx.signals.signal('$1').value$2")}for(let[_,v]of o)n=n.replace(_,v);let S=`return (()=> {
${n}
})()`;t.fnContent=S;try{let _=new Function("ctx",...e,S);return(...v)=>{try{return _(t,...v)}catch(h){throw C("ExecuteExpression",t,{error:h.message})}}}catch(_){throw C("GenerateExpression",t,{error:_.message})}}Ie(ft,ct,ut);async function Wn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function $n(t){let e,n,r,i=!1;return function(l){e===void 0?(e=l,n=0,r=-1):e=Un(e,l);let s=e.length,c=0;for(;n<s;){i&&(e[n]===10&&(c=++n),i=!1);let T=-1;for(;n<s&&T===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-c);break;case 13:i=!0;case 10:T=n;break}if(T===-1)break;t(e.subarray(c,T),r),c=n,r=-1}c===s?e=void 0:c!==0&&(e=e.subarray(c),n-=c)}}function Gn(t,e,n){let r=_t(),i=new TextDecoder;return function(l,s){if(l.length===0)n?.(r),r=_t();else if(s>0){let c=i.decode(l.subarray(0,s)),T=s+(l[s+1]===32?2:1),A=i.decode(l.subarray(T));switch(c){case"data":r.data=r.data?`${r.data}
${A}`:A;break;case"event":r.event=A;break;case"id":t(r.id=A);break;case"retry":{let x=Number.parseInt(A,10);Number.isNaN(x)||e(r.retry=x);break}}}}}function Un(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function _t(){return{data:"",event:"",id:"",retry:void 0}}var Bn="text/event-stream",wt="last-event-id";function Rt(t,e,{signal:n,headers:r,onopen:i,onmessage:o,onclose:l,onerror:s,openWhenHidden:c,fetch:T,retryInterval:A=1e3,retryScaler:x=2,retryMaxWaitMs:S=3e4,retryMaxCount:_=10,...v}){return new Promise((h,b)=>{let y=0,a={...r};a.accept||(a.accept=Bn);let p;function f(){p.abort(),document.hidden||E()}c||document.addEventListener("visibilitychange",f);let u=0;function g(){document.removeEventListener("visibilitychange",f),window.clearTimeout(u),p.abort()}n?.addEventListener("abort",()=>{g(),h()});let m=T??window.fetch,d=i??function(){};async function E(){p=new AbortController;try{let R=await m(e,{...v,headers:a,signal:p.signal});await d(R),await Wn(R.body,$n(Gn(w=>{w?a[wt]=w:delete a[wt]},w=>{A=w},o))),l?.(),g(),h()}catch(R){if(!p.signal.aborted)try{let w=s?.(R)??A;window.clearTimeout(u),u=window.setTimeout(E,w),A*=x,A=Math.min(A,S),y++,y>=_?(g(),b(C("SseMaxRetries",t,{retryMaxCount:_}))):console.error(`Datastar failed to reach ${v.method}: ${e.toString()} retry in ${w}ms`)}catch(w){g(),b(w)}}}E()})}var ue=`${H}-sse`,Ye=`${H}-settling`,ne=`${H}-swapping`,Ve="started",De="finished",xt="error",Mt="retrying";function K(t,e){document.addEventListener(ue,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function ve(t,e){document.dispatchEvent(new CustomEvent(ue,{detail:{type:t,argsRaw:e}}))}var Nt=t=>`${t}`.includes("text/event-stream"),J=async(t,e,n,r)=>{let{el:{id:i},el:o,signals:l}=t,{headers:s,contentType:c,includeLocal:T,selector:A,openWhenHidden:x,retryInterval:S,retryScaler:_,retryMaxWaitMs:v,retryMaxCount:h,abort:b}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:it,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),y=e.toLowerCase(),a=()=>{};try{if(ve(Ve,{elId:i}),!n?.length)throw C("SseNoUrlProvided",t,{action:y});let p={};p[rt]=!0,c==="json"&&(p["Content-Type"]="application/json");let f=Object.assign({},p,s),u={method:e,headers:f,openWhenHidden:x,retryInterval:S,retryScaler:_,retryMaxWaitMs:v,retryMaxCount:h,signal:b,onopen:async d=>{if(d.status>=400){let E=d.status.toString();ve(xt,{status:E})}},onmessage:d=>{if(!d.event.startsWith(H))return;let E=d.event,R={},w=d.data.split(`
`);for(let V of w){let I=V.indexOf(" "),O=V.slice(0,I),D=R[O];D||(D=[],R[O]=D);let $=V.slice(I+1);D.push($)}let N={};for(let[V,I]of Object.entries(R))N[V]=I.join(`
`);ve(E,N)},onerror:d=>{if(Nt(d))throw C("InvalidContentType",t,{url:n});d&&(console.error(d.message),ve(Mt,{message:d.message}))}},g=new URL(n,window.location.origin),m=new URLSearchParams(g.search);if(c==="json"){let d=l.JSON(!1,!T);e==="GET"?m.set(H,d):u.body=d}else if(c==="form"){let d=A?document.querySelector(A):o.closest("form");if(d===null)throw A?C("SseFormNotFound",t,{action:y,selector:A}):C("SseClosestFormNotFound",t,{action:y});if(o!==d){let R=w=>w.preventDefault();d.addEventListener("submit",R),a=()=>d.removeEventListener("submit",R)}if(!d.checkValidity()){d.reportValidity(),a();return}let E=new FormData(d);if(e==="GET"){let R=new URLSearchParams(E);for(let[w,N]of R)m.set(w,N)}else u.body=E}else throw C("SseInvalidContentType",t,{action:y,contentType:c});g.search=m.toString();try{await Rt(t,g.toString(),u)}catch(d){if(!Nt(d))throw C("SseFetchFailed",t,{method:e,url:n,error:d})}}finally{ve(De,{elId:i}),a()}};var Ct={type:3,name:"delete",fn:async(t,e,n)=>J(t,"DELETE",e,{...n})};var Pt={type:3,name:"get",fn:async(t,e,n)=>J(t,"GET",e,{...n})};var It={type:3,name:"patch",fn:async(t,e,n)=>J(t,"PATCH",e,{...n})};var Lt={type:3,name:"post",fn:async(t,e,n)=>J(t,"POST",e,{...n})};var Vt={type:3,name:"put",fn:async(t,e,n)=>J(t,"PUT",e,{...n})};var Dt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?k(e,n):Y(i),{signal:l}=r.upsertIfMissing(o,!1),s=c=>{let{type:T,argsRaw:{elId:A}}=c.detail;if(A===t.id)switch(T){case Ve:l.value=!0;break;case De:l.value=!1;break}};return document.addEventListener(ue,s),()=>{document.removeEventListener(ue,s)}}};var kt={type:2,name:F.ExecuteScript,onGlobalInit:async t=>{K(F.ExecuteScript,({autoRemove:e=`${at}`,attributes:n=ot,script:r})=>{let i=z(e);if(!r?.length)throw W("NoScriptProvided",t);let o=document.createElement("script");for(let l of n.split(`
`)){let s=l.indexOf(" "),c=s?l.slice(0,s):l,T=s?l.slice(s):"";o.setAttribute(c.trim(),T.trim())}o.text=r,document.head.appendChild(o),i&&o.remove()})}};var ye=document,X=!!ye.startViewTransition;var Ot=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:S=>S.getAttribute("im-preserve")==="true",shouldReAppend:S=>S.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!0};function n(S,_,v={}){S=A(S);let h=x(_),b=T(S,h,v),y=i(b,()=>s(b,S,h,a=>a.morphStyle==="innerHTML"?(o(a,S,h),Array.from(S.childNodes)):r(a,S,h)));return b.pantry.remove(),y}function r(S,_,v){let h=x(_);return o(S,h,v,_,_.nextSibling),Array.from(h.childNodes)}function i(S,_){if(!S.config.restoreFocus)return _();let v=document.activeElement;if(!(v instanceof HTMLInputElement||v instanceof HTMLTextAreaElement))return _();let{id:h,selectionStart:b,selectionEnd:y}=v,a=_();return h&&h!==document.activeElement?.id&&(v=S.target.querySelector(`[id="${h}"]`),v?.focus()),v&&!v.selectionEnd&&y&&v.setSelectionRange(b,y),a}let o=function(){function S(f,u,g,m=null,d=null){u instanceof HTMLTemplateElement&&g instanceof HTMLTemplateElement&&(u=u.content,g=g.content),m||=u.firstChild;for(let E of g.childNodes){if(m&&m!=d){let w=v(f,E,m,d);if(w){w!==m&&b(f,m,w),l(w,E,f),m=w.nextSibling;continue}}if(E instanceof Element&&f.persistentIds.has(E.id)){let w=y(u,E.id,m,f);l(w,E,f),m=w.nextSibling;continue}let R=_(u,E,m,f);R&&(m=R.nextSibling)}for(;m&&m!=d;){let E=m;m=m.nextSibling,h(f,E)}}function _(f,u,g,m){if(m.callbacks.beforeNodeAdded(u)===!1)return null;if(m.idMap.has(u)){let d=document.createElement(u.tagName);return f.insertBefore(d,g),l(d,u,m),m.callbacks.afterNodeAdded(d),d}else{let d=document.importNode(u,!0);return f.insertBefore(d,g),m.callbacks.afterNodeAdded(d),d}}let v=function(){function f(m,d,E,R){let w=null,N=d.nextSibling,V=0,I=E;for(;I&&I!=R;){if(g(I,d)){if(u(m,I,d))return I;w===null&&(m.idMap.has(I)||(w=I))}if(w===null&&N&&g(I,N)&&(V++,N=N.nextSibling,V>=2&&(w=void 0)),I.contains(document.activeElement))break;I=I.nextSibling}return w||null}function u(m,d,E){let R=m.idMap.get(d),w=m.idMap.get(E);if(!w||!R)return!1;for(let N of R)if(w.has(N))return!0;return!1}function g(m,d){let E=m,R=d;return E.nodeType===R.nodeType&&E.tagName===R.tagName&&(!E.id||E.id===R.id)}return f}();function h(f,u){if(f.idMap.has(u))p(f.pantry,u,null);else{if(f.callbacks.beforeNodeRemoved(u)===!1)return;u.parentNode?.removeChild(u),f.callbacks.afterNodeRemoved(u)}}function b(f,u,g){let m=u;for(;m&&m!==g;){let d=m;m=m.nextSibling,h(f,d)}return m}function y(f,u,g,m){let d=m.target.id===u&&m.target||m.target.querySelector(`[id="${u}"]`)||m.pantry.querySelector(`[id="${u}"]`);return a(d,m),p(f,d,g),d}function a(f,u){let g=f.id;for(;f=f.parentNode;){let m=u.idMap.get(f);m&&(m.delete(g),m.size||u.idMap.delete(f))}}function p(f,u,g){if(f.moveBefore)try{f.moveBefore(u,g)}catch{f.insertBefore(u,g)}else f.insertBefore(u,g)}return S}(),l=function(){function S(a,p,f){return f.ignoreActive&&a===document.activeElement?null:(f.callbacks.beforeNodeMorphed(a,p)===!1||(a instanceof HTMLHeadElement&&f.head.ignore||(a instanceof HTMLHeadElement&&f.head.style!=="morph"?c(a,p,f):(_(a,p,f),y(a,f)||o(f,a,p))),f.callbacks.afterNodeMorphed(a,p)),a)}function _(a,p,f){let u=p.nodeType;if(u===1){let g=a,m=p,d=g.attributes,E=m.attributes;for(let R of E)b(R.name,g,"update",f)||g.getAttribute(R.name)!==R.value&&g.setAttribute(R.name,R.value);for(let R=d.length-1;0<=R;R--){let w=d[R];if(w&&!m.hasAttribute(w.name)){if(b(w.name,g,"remove",f))continue;g.removeAttribute(w.name)}}y(g,f)||v(g,m,f)}(u===8||u===3)&&a.nodeValue!==p.nodeValue&&(a.nodeValue=p.nodeValue)}function v(a,p,f){if(a instanceof HTMLInputElement&&p instanceof HTMLInputElement&&p.type!=="file"){let u=p.value,g=a.value;h(a,p,"checked",f),h(a,p,"disabled",f),p.hasAttribute("value")?g!==u&&(b("value",a,"update",f)||(a.setAttribute("value",u),a.value=u)):b("value",a,"remove",f)||(a.value="",a.removeAttribute("value"))}else if(a instanceof HTMLOptionElement&&p instanceof HTMLOptionElement)h(a,p,"selected",f);else if(a instanceof HTMLTextAreaElement&&p instanceof HTMLTextAreaElement){let u=p.value,g=a.value;if(b("value",a,"update",f))return;u!==g&&(a.value=u),a.firstChild&&a.firstChild.nodeValue!==u&&(a.firstChild.nodeValue=u)}}function h(a,p,f,u){let g=p[f],m=a[f];if(g!==m){let d=b(f,a,"update",u);d||(a[f]=p[f]),g?d||a.setAttribute(f,""):b(f,a,"remove",u)||a.removeAttribute(f)}}function b(a,p,f,u){return a==="value"&&u.ignoreActiveValue&&p===document.activeElement?!0:u.callbacks.beforeAttributeUpdated(a,p,f)===!1}function y(a,p){return!!p.ignoreActiveValue&&a===document.activeElement&&a!==document.body}return S}();function s(S,_,v,h){if(S.head.block){let b=_.querySelector("head"),y=v.querySelector("head");if(b&&y){let a=c(b,y,S);return Promise.all(a).then(()=>{let p=Object.assign(S,{head:{block:!1,ignore:!0}});return h(p)})}}return h(S)}function c(S,_,v){let h=[],b=[],y=[],a=[],p=new Map;for(let u of _.children)p.set(u.outerHTML,u);for(let u of S.children){let g=p.has(u.outerHTML),m=v.head.shouldReAppend(u),d=v.head.shouldPreserve(u);g||d?m?b.push(u):(p.delete(u.outerHTML),y.push(u)):v.head.style==="append"?m&&(b.push(u),a.push(u)):v.head.shouldRemove(u)!==!1&&b.push(u)}a.push(...p.values());let f=[];for(let u of a){let g=document.createRange().createContextualFragment(u.outerHTML).firstChild;if(v.callbacks.beforeNodeAdded(g)!==!1){if("href"in g&&g.href||"src"in g&&g.src){let m,d=new Promise(function(E){m=E});g.addEventListener("load",function(){m()}),f.push(d)}S.appendChild(g),v.callbacks.afterNodeAdded(g),h.push(g)}}for(let u of b)v.callbacks.beforeNodeRemoved(u)!==!1&&(S.removeChild(u),v.callbacks.afterNodeRemoved(u));return v.head.afterHeadMorphed(S,{added:h,kept:y,removed:b}),f}let T=function(){function S(p,f,u){let{persistentIds:g,idMap:m}=y(p,f),d=_(u),E=d.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(E))throw`Do not understand how to morph style ${E}`;return{target:p,newContent:f,config:d,morphStyle:E,ignoreActive:d.ignoreActive,ignoreActiveValue:d.ignoreActiveValue,restoreFocus:d.restoreFocus,idMap:m,persistentIds:g,pantry:v(),callbacks:d.callbacks,head:d.head}}function _(p){let f=Object.assign({},e);return Object.assign(f,p),f.callbacks=Object.assign({},e.callbacks,p.callbacks),f.head=Object.assign({},e.head,p.head),f}function v(){let p=document.createElement("div");return p.hidden=!0,document.body.insertAdjacentElement("afterend",p),p}function h(p){let f=Array.from(p.querySelectorAll("[id]"));return p.id&&f.push(p),f}function b(p,f,u,g){for(let m of g)if(f.has(m.id)){let d=m;for(;d;){let E=p.get(d);if(E==null&&(E=new Set,p.set(d,E)),E.add(m.id),d===u)break;d=d.parentElement}}}function y(p,f){let u=h(p),g=h(f),m=a(u,g),d=new Map;b(d,m,p,u);let E=f.__idiomorphRoot||f;return b(d,m,E,g),{persistentIds:m,idMap:d}}function a(p,f){let u=new Set,g=new Map;for(let{id:d,tagName:E}of p)g.has(d)?u.add(d):g.set(d,E);let m=new Set;for(let{id:d,tagName:E}of f)m.has(d)?u.add(d):g.get(d)===E&&m.add(d);for(let d of u)m.delete(d);return m}return S}(),{normalizeElement:A,normalizeParent:x}=function(){let S=new WeakSet;function _(y){return y instanceof Document?y.documentElement:y}function v(y){if(y==null)return document.createElement("div");if(typeof y=="string")return v(b(y));if(S.has(y))return y;if(y instanceof Node){if(y.parentNode)return new h(y);{let a=document.createElement("div");return a.append(y),a}}else{let a=document.createElement("div");for(let p of[...y])a.append(p);return a}}class h{constructor(a){this.originalNode=a,this.realParentNode=a.parentNode,this.previousSibling=a.previousSibling,this.nextSibling=a.nextSibling}get childNodes(){let a=[],p=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;p&&p!=this.nextSibling;)a.push(p),p=p.nextSibling;return a}querySelectorAll(a){return this.childNodes.reduce((p,f)=>{if(f instanceof Element){f.matches(a)&&p.push(f);let u=f.querySelectorAll(a);for(let g=0;g<u.length;g++)p.push(u[g])}return p},[])}insertBefore(a,p){return this.realParentNode.insertBefore(a,p)}moveBefore(a,p){return this.realParentNode.moveBefore(a,p)}get __idiomorphRoot(){return this.originalNode}}function b(y){let a=new DOMParser,p=y.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(p.match(/<\/html>/)||p.match(/<\/head>/)||p.match(/<\/body>/)){let f=a.parseFromString(y,"text/html");if(p.match(/<\/html>/))return S.add(f),f;{let u=f.firstChild;return u&&S.add(u),u}}else{let u=a.parseFromString("<body><template>"+y+"</template></body>","text/html").body.querySelector("template").content;return S.add(u),u}}return{normalizeElement:_,normalizeParent:v}}();return{morph:n,defaults:e}}();var Ft={type:2,name:F.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");K(F.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=lt,settleDuration:o=`${Te}`,useViewTransition:l=`${Ae}`})=>{let s=Number.parseInt(o),c=z(l);e.innerHTML=n.trim();let T=[...e.content.children];for(let A of T){if(!(A instanceof Element))throw W("NoFragmentsFound",t);let x=r||`#${A.getAttribute("id")}`,S=[...document.querySelectorAll(x)||[]];if(!S.length)throw W("NoTargetsFound",t,{selectorOrID:x});c&&X?ye.startViewTransition(()=>Ht(t,i,s,A,S)):Ht(t,i,s,A,S)}})}};function Ht(t,e,n,r,i){for(let o of i){o.classList.add(ne);let l=o.outerHTML,s=o;switch(e){case U.Morph:{let A=r.cloneNode(!0);pe(A,x=>{!x.id?.length&&Object.keys(x.dataset).length&&(x.id=_e(x));let S=t.removals.get(x.id);if(S){let _=new Map;for(let[v,h]of S){let b=we(v,v);_.set(b,h),S.delete(v)}t.removals.set(x.id,_)}}),Ot.morph(s,A);break}case U.Inner:s.innerHTML=r.outerHTML;break;case U.Outer:s.replaceWith(r);break;case U.Prepend:s.prepend(r);break;case U.Append:s.append(r);break;case U.Before:s.before(r);break;case U.After:s.after(r);break;case U.UpsertAttributes:for(let A of r.getAttributeNames()){let x=r.getAttribute(A);s.setAttribute(A,x)}break;default:throw W("InvalidMergeMode",t,{mergeMode:e})}let c=s.classList;c?.add(ne),setTimeout(()=>{o.classList.remove(ne),c?.remove(ne)},n);let T=s.outerHTML;c&&l!==T&&(c.add(Ye),setTimeout(()=>{c.remove(Ye)},n))}}var qt={type:2,name:F.MergeSignals,onGlobalInit:async t=>{K(F.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${st}`})=>{let{signals:r}=t,i=z(n);r.merge(Ee(e),i)})}};var Wt={type:2,name:F.RemoveFragments,onGlobalInit:async t=>{K(F.RemoveFragments,({selector:e,settleDuration:n=`${Te}`,useViewTransition:r=`${Ae}`})=>{if(!e.length)throw W("NoSelectorProvided",t);let i=Number.parseInt(n),o=z(r),l=document.querySelectorAll(e),s=()=>{for(let c of l)c.classList.add(ne);setTimeout(()=>{for(let c of l)c.remove()},i)};o&&X?ye.startViewTransition(()=>s()):s()})}};var $t={type:2,name:F.RemoveSignals,onGlobalInit:async t=>{K(F.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw W("NoPathsProvided",t);t.signals.remove(...n)})}};var Gt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw C("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var Ut={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw C("CustomValidityInvalidElement",t);let i=n();return r(()=>{let o=i();if(typeof o!="string")throw C("CustomValidityInvalidExpression",t,{result:o});e.setCustomValidity(o)})}};var Bt="once",jt="half",Kt="full",Jt={type:1,name:"intersects",keyReq:2,mods:new Set([Bt,jt,Kt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(Kt)?i.threshold=1:n.has(jt)&&(i.threshold=.5);let o=r(),l=new IntersectionObserver(s=>{for(let c of s)c.isIntersecting&&(o(),n.has(Bt)&&(l.disconnect(),delete t.dataset[e]))},i);return l.observe(t),()=>l.disconnect()}};var zt="session",Yt={type:1,name:"persist",mods:new Set([zt]),onLoad:({key:t,effect:e,mods:n,signals:r,value:i})=>{t=k(t,n),t===""&&(t=H);let o=n.has(zt)?sessionStorage:localStorage,l=i.split(/\s+/).filter(T=>T!=="");l=l.map(T=>Y(T));let s=()=>{let T=o.getItem(t)||"{}",A=JSON.parse(T);r.merge(A)},c=()=>{let T;l.length?T=r.subset(...l):T=r.values(),o.setItem(t,JSON.stringify(T))};return s(),e(()=>{c()})}};var Xt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,o=new URL(r,i).toString();window.history.replaceState({},"",o)})}};var ke="smooth",Xe="instant",Qe="auto",Qt="hstart",Zt="hcenter",en="hend",tn="hnearest",nn="vstart",rn="vcenter",on="vend",sn="vnearest",jn="focus",Oe="center",an="start",ln="end",un="nearest",cn={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([ke,Xe,Qe,Qt,Zt,en,tn,nn,rn,on,sn,jn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:ke,block:Oe,inline:Oe};if(n.has(ke)&&(i.behavior=ke),n.has(Xe)&&(i.behavior=Xe),n.has(Qe)&&(i.behavior=Qe),n.has(Qt)&&(i.inline=an),n.has(Zt)&&(i.inline=Oe),n.has(en)&&(i.inline=ln),n.has(tn)&&(i.inline=un),n.has(nn)&&(i.block=an),n.has(rn)&&(i.block=Oe),n.has(on)&&(i.block=ln),n.has(sn)&&(i.block=un),!(e instanceof HTMLElement||e instanceof SVGElement))throw C("ScrollIntoViewInvalidElement",t);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r]}};var fn="none",dn="display",pn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===fn&&t.removeProperty(dn):t.setProperty(dn,fn)})}};var mn="view-transition",gn={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===mn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=mn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!X){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var hn={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let i=r();return e===""?n(async()=>{let o=i();for(let[l,s]of Object.entries(o))s===!1?t.removeAttribute(l):t.setAttribute(l,s)}):(e=de(e),n(async()=>{let o=!1;try{o=i()}catch{}let l;typeof o=="string"?l=o:l=JSON.stringify(o),!l||l==="false"||l==="null"||l==="undefined"?t.removeAttribute(e):t.setAttribute(e,l)}))}};var Kn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,vn=["change","input","keydown"],yn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:i,value:o,effect:l}=t,s=e,c=n?k(n,r):Y(o),T=e.tagName.toLowerCase(),A=T.includes("input"),x=T.includes("select"),S=e.getAttribute("type"),_=e.hasAttribute("value"),v="",h=A&&S==="checkbox";h&&(v=_?"":!1);let b=A&&S==="number";b&&(v=0);let y=A&&S==="radio";y&&(e.getAttribute("name")?.length||e.setAttribute("name",c));let a=A&&S==="file",{signal:p,inserted:f}=i.upsertIfMissing(c,v),u=-1;Array.isArray(p.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",c),u=[...document.querySelectorAll(`[name="${c}"]`)].findIndex(N=>N===t.el));let g=u>=0,m=()=>[...i.value(c)],d=()=>{let N=i.value(c);g&&!x&&(N=N[u]||v);let V=`${N}`;if(h||y)typeof N=="boolean"?s.checked=N:s.checked=V===s.value;else if(x){let I=e;if(I.multiple){if(!g)throw C("BindSelectMultiple",t);for(let O of I.options){if(O?.disabled)return;let D=b?Number(O.value):O.value;O.selected=N.includes(D)}}else I.value=V}else a||("value"in e?e.value=V:e.setAttribute("value",V))},E=async()=>{let N=i.value(c);if(g){let D=N;for(;u>=D.length;)D.push(v);N=D[u]||v}let V=(D,$)=>{let G=$;g&&!x&&(G=m(),G[u]=$),i.setValue(D,G)};if(a){let D=[...s?.files||[]],$=[],G=[],et=[];await Promise.all(D.map(tt=>new Promise(Nn=>{let Q=new FileReader;Q.onload=()=>{if(typeof Q.result!="string")throw C("InvalidFileResultType",t,{resultType:typeof Q.result});let He=Q.result.match(Kn);if(!He?.groups)throw C("InvalidDataUri",t,{result:Q.result});$.push(He.groups.contents),G.push(He.groups.mime),et.push(tt.name)},Q.onloadend=()=>Nn(void 0),Q.readAsDataURL(tt)}))),V(c,$),V(`${c}Mimes`,G),V(`${c}Names`,et);return}let I=s.value||"",O;if(h){let D=s.checked||s.getAttribute("checked")==="true";_?O=D?I:"":O=D}else if(x){let $=[...e.selectedOptions];g?O=$.filter(G=>G.selected).map(G=>G.value):O=$[0]?.value||v}else typeof N=="boolean"?O=!!I:typeof N=="number"?O=Number(I):O=I||"";V(c,O)};f&&E();for(let N of vn)e.addEventListener(N,E);let R=N=>{N.persisted&&E()};window.addEventListener("pageshow",R);let w=l(()=>d());return()=>{w();for(let N of vn)e.removeEventListener(N,E);window.removeEventListener("pageshow",R)}}};var Sn={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:i})=>{let o=t.classList,l=i();return r(()=>{if(e===""){let s=l();for(let[c,T]of Object.entries(s)){let A=c.split(/\s+/);T?o.add(...A):o.remove(...A)}}else e=k(e,n),l()?o.add(e):o.remove(e)})}};function Se(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ce(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function bn(t,e){return(...n)=>{setTimeout(()=>{t(...n)},e)}}function Tn(t,e,n=!1,r=!0){let i=-1,o=()=>i&&clearTimeout(i);return(...l)=>{o(),n&&!i&&t(...l),i=setTimeout(()=>{r&&t(...l),o()},e)}}function An(t,e,n=!0,r=!1){let i=!1;return(...o)=>{i||(n&&t(...o),i=!0,setTimeout(()=>{i=!1,r&&t(...o)},e))}}var Jn="evt",Ze="signalsChange",zn=Ze.length,En={type:1,name:"on",keyReq:1,valReq:1,argNames:[Jn],onLoad:({el:t,key:e,mods:n,signals:r,effect:i,genRX:o})=>{let l=o(),s=t;n.has("window")&&(s=window);let c=h=>{h&&((n.has("prevent")||e==="submit")&&h.preventDefault(),n.has("stop")&&h.stopPropagation()),l(h)},T=n.get("delay");if(T){let h=Se(T);c=bn(c,h)}let A=n.get("debounce");if(A){let h=Se(A),b=ce(A,"leading",!1),y=!ce(A,"notrail",!1);c=Tn(c,h,b,y)}let x=n.get("throttle");if(x){let h=Se(x),b=!ce(x,"noleading",!1),y=ce(x,"trail",!1);c=An(c,h,b,y)}if(n.has("viewtransition")&&X){let h=c;c=(...b)=>document.startViewTransition(()=>h(...b))}let S={capture:!0,passive:!1,once:!1};if(n.has("capture")||(S.capture=!1),n.has("passive")&&(S.passive=!0),n.has("once")&&(S.once=!0),e==="load")return setTimeout(c,0),()=>{};if(e==="interval"){let h=1e3,b=n.get("duration");b&&(h=Se(b),ce(b,"leading",!1)&&c());let y=setInterval(c,h);return()=>{clearInterval(y)}}if(e==="raf"){let h,b=()=>{c(),h=requestAnimationFrame(b)};return h=requestAnimationFrame(b),()=>{h&&cancelAnimationFrame(h)}}if(e.startsWith(Ze)){if(e===Ze){let y=a=>c(a);return document.addEventListener(fe,y),()=>{document.removeEventListener(fe,y)}}let h=k(Z(e.slice(zn)),n),b=new Map;return r.walk((y,a)=>{y.startsWith(h)&&b.set(a,a.value)}),i(()=>{for(let[y,a]of b)a!==y.value&&(c(),b.set(y,y.value))})}if(n.has("outside")){s=document;let h=c;c=y=>{let a=y?.target;t.contains(a)||h(y)}}let v=k(e,n);return s.addEventListener(v,c,S),()=>{s.removeEventListener(v,c)}}};var _n={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?k(e,n):Y(i);r.setValue(o,t)}};var wn={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,i=r();return e instanceof HTMLElement||C("TextInvalidElement",t),n(()=>{let o=i(t);e.textContent=`${o}`})}};var{round:Yn,max:Xn,min:Qn}=Math,Rn={type:3,name:"fit",fn:(t,e,n,r,i,o,l=!1,s=!1)=>{let c=(e-n)/(r-n)*(o-i)+i;return s&&(c=Yn(c)),l&&(c=Xn(i,Qn(o,c))),c}};var xn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var Mn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Ie(hn,yn,Sn,En,_n,pn,wn,Dt,Pt,Lt,Vt,It,Ct,Ft,qt,Wt,$t,kt,Gt,Ut,Jt,Yt,Xt,cn,gn,Rn,xn,Mn);ze();export{ze as apply,Ie as load,Et as setAlias};
//# sourceMappingURL=datastar.js.map
