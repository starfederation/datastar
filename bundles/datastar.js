// Datastar v1.0.0-beta.9
var nt=/🖕JS_DS🚀/.source,Se=nt.slice(0,5),Fe=nt.slice(4),H="datastar";var rt="Datastar-Request",Te=300,it=1e3,ot="type module",Ae=!1,st=!1,at=!0,U={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},lt=U.Morph,F={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var R=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(R||{});var fe=`${H}-signals`;var z=t=>t.trim()==="true",de=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),Z=t=>de(t).replace(/-./g,e=>e[1].toUpperCase()),qe=t=>de(t).replace(/-/g,"_"),Cn=t=>Z(t).replace(/^./,e=>e[0].toUpperCase()),Ee=t=>new Function(`return Object.assign({}, ${t})`)(),Y=t=>t.startsWith("$")?t.slice(1):t,Pn={kebab:de,snake:qe,pascal:Cn};function k(t,e){for(let n of e.get("case")||[]){let r=Pn[n];r&&(t=r(t))}return t}var In="computed",ut={type:1,name:In,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=k(t,e);let i=r();n.setComputed(t,i)}};var ct={type:1,name:"signals",onLoad:t=>{let{key:e,mods:n,signals:r,value:i,genRX:o}=t,l=n.has("ifmissing");if(e!==""){let s=k(e,n),c=i===""?i:o()();l?r.upsertIfMissing(s,c):r.setValue(s,c)}else{let s=Ee(t.value);t.value=JSON.stringify(s);let S=o()();r.merge(S,l)}}};var ft={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var re=class{#e=0;#t;constructor(e=H){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function _e(t){if(t.id)return t.id;let e=new re,n=t;for(;n;){if(e.with(n.tagName||""),n.id){e.with(n.id);break}let r=n?.parentNode;r&&e.with([...r.children].indexOf(n)),n=r}return e.string}function we(t,e){return new re().with(t).with(e).value}function pe(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)pe(r,e),r=r.nextElementSibling}var Ln="https://data-star.dev/errors";function We(t,e,n={}){let r=new Error;r.name=`${H} ${t} error`;let i=qe(e),o=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),l=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${Ln}/${t}/${i}?${o}
Context: ${l}`,r}function B(t,e,n={}){return We("internal",e,Object.assign({from:t},n))}function W(t,e,n={}){let r={plugin:{name:e.plugin.name,type:R[e.plugin.type]}};return We("init",t,Object.assign(r,n))}function C(t,e,n={}){let r={plugin:{name:e.plugin.name,type:R[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return We("runtime",t,Object.assign(r,n))}var ie="preact-signals",Vn=Symbol.for("preact-signals"),j=1,oe=2,ge=4,ae=8,xe=16,se=32;function Ge(){Re++}function Ue(){if(Re>1){Re--;return}let t,e=!1;for(;me!==void 0;){let n=me;for(me=void 0,$e++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~oe,!(n._flags&ae)&&pt(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if($e=0,Re--,e)throw t}var P;var me,Re=0,$e=0,Me=0;function dt(t){if(P===void 0)return;let e=t._node;if(e===void 0||e._target!==P)return e={_version:0,_source:t,_prevSource:P._sources,_nextSource:void 0,_target:P,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},P._sources!==void 0&&(P._sources._nextSource=e),P._sources=e,t._node=e,P._flags&se&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=P._sources,e._nextSource=void 0,P._sources._nextSource=e,P._sources=e),e}function L(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}L.prototype.brand=Vn;L.prototype._refresh=()=>!0;L.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};L.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};L.prototype.subscribe=function(t){return Ne(()=>{let e=this.value,n=P;P=void 0;try{t(e)}finally{P=n}})};L.prototype.valueOf=function(){return this.value};L.prototype.toString=function(){return`${this.value}`};L.prototype.toJSON=function(){return this.value};L.prototype.peek=function(){let t=P;P=void 0;try{return this.value}finally{P=t}};Object.defineProperty(L.prototype,"value",{get(){let t=dt(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if($e>100)throw B(ie,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Me++,Ge();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{Ue()}this?._onChange({old:e,revised:n})}}});function pt(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function mt(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function gt(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function ee(t){L.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Me-1,this._flags=ge}ee.prototype=new L;ee.prototype._refresh=function(){if(this._flags&=~oe,this._flags&j)return!1;if((this._flags&(ge|se))===se||(this._flags&=~ge,this._globalVersion===Me))return!0;if(this._globalVersion=Me,this._flags|=j,this._version>0&&!pt(this))return this._flags&=~j,!0;let t=P;try{mt(this),P=this;let e=this._fn();(this._flags&xe||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~xe,this._version++)}catch(e){this._value=e,this._flags|=xe,this._version++}return P=t,gt(this),this._flags&=~j,!0};ee.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=ge|se;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}L.prototype._subscribe.call(this,t)};ee.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(L.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~se;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};ee.prototype._notify=function(){if(!(this._flags&oe)){this._flags|=ge|oe;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(ee.prototype,"value",{get(){if(this._flags&j)throw B(ie,"SignalCycleDetected");let t=dt(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&xe)throw B(ie,"GetComputedError",{value:this._value});return this._value}});function ht(t){return new ee(t)}function vt(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){Ge();let n=P;P=void 0;try{e()}catch(r){throw t._flags&=~j,t._flags|=ae,Be(t),B(ie,"CleanupEffectError",{error:r})}finally{P=n,Ue()}}}function Be(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,vt(t)}function Dn(t){if(P!==this)throw B(ie,"EndEffectError");gt(this),P=t,this._flags&=~j,this._flags&ae&&Be(this),Ue()}function he(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=se}he.prototype._callback=function(){let t=this._start();try{if(this._flags&ae||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};he.prototype._start=function(){if(this._flags&j)throw B(ie,"SignalCycleDetected");this._flags|=j,this._flags&=~ae,vt(this),mt(this),Ge();let t=P;return P=this,Dn.bind(this,t)};he.prototype._notify=function(){this._flags&oe||(this._flags|=oe,this._nextBatchedEffect=me,me=this)};he.prototype._dispose=function(){this._flags|=ae,this._flags&j||Be(this)};function Ne(t){let e=new he(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var yt="namespacedSignals",le=t=>{document.dispatchEvent(new CustomEvent(fe,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function bt(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof L?n[r]=i.value:n[r]=bt(i)}return n}function St(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw B(yt,"InvalidSignalKey",{key:i});let o=e[i];if(o instanceof Object&&!Array.isArray(o)){t[i]||(t[i]={});let l=St(t[i],o,n);r.added.push(...l.added.map(s=>`${i}.${s}`)),r.removed.push(...l.removed.map(s=>`${i}.${s}`)),r.updated.push(...l.updated.map(s=>`${i}.${s}`))}else{if(Object.hasOwn(t,i)){if(n)continue;let c=t[i];if(c instanceof L){let S=c.value;c.value=o,S!==o&&r.updated.push(i);continue}}let s=new L(o);s._onChange=()=>{le({updated:[i]})},t[i]=s,r.added.push(i)}}return r}function Tt(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof L?e(n,r):Tt(r,(i,o)=>{e(`${n}.${i}`,o)})}}function kn(t,...e){let n={};for(let r of e){let i=r.split("."),o=t,l=n;for(let c=0;c<i.length-1;c++){let S=i[c];if(!o[S])return{};l[S]||(l[S]={}),o=o[S],l=l[S]}let s=i[i.length-1];l[s]=o[s]}return n}var Ce=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let l=0;l<n.length-1;l++){let s=n[l];if(!r[s])return null;r=r[s]}let i=n[n.length-1],o=r[i];if(!o)throw B(yt,"SignalNotFound",{path:e});return o}setSignal(e,n){let r=e.split("."),i=this.#e;for(let l=0;l<r.length-1;l++){let s=r[l];i[s]||(i[s]={}),i=i[s]}let o=r[r.length-1];i[o]=n}setComputed(e,n){let r=ht(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let{signal:r}=this.upsertIfMissing(e,n),i=r.value;r.value=n,i!==n&&le({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let c=0;c<r.length-1;c++){let S=r[c];i[S]||(i[S]={}),i=i[S]}let o=r[r.length-1],l=i[o];if(l instanceof L)return{signal:l,inserted:!1};let s=new L(n);return s._onChange=()=>{le({updated:[e]})},i[o]=s,le({added:[e]}),{signal:s,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let i=r.split("."),o=this.#e;for(let s=0;s<i.length-1;s++){let c=i[s];if(!o[c])return;o=o[c]}let l=i[i.length-1];delete o[l],n.push(r)}le({removed:n})}merge(e,n=!1){let r=St(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&le(r)}subset(...e){return kn(this.values(),...e)}walk(e){Tt(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return bt(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var At=new Ce,Ke=[],Pe={},On=[],te=new Map,je=null,Je="";function Et(t){Je=t}function Ie(...t){for(let e of t){let n={plugin:e,signals:At,effect:i=>Ne(i),actions:Pe,removals:te,applyToElement:Le},r;switch(e.type){case 2:{let i=e;On.push(i),r=i.onGlobalInit;break}case 3:{Pe[e.name]=e;break}case 1:{let i=e;Ke.push(i),r=i.onGlobalInit;break}default:throw W("InvalidPluginType",n)}r&&r(n)}Ke.sort((e,n)=>{let r=n.name.length-e.name.length;return r!==0?r:e.name.localeCompare(n.name)})}function ze(){queueMicrotask(()=>{Le(document.documentElement),Hn()})}function Le(t){pe(t,e=>{let n=new Array,r=te.get(e.id)||new Map,i=new Map([...r]),o=new Map;for(let l of Object.keys(e.dataset)){if(!l.startsWith(Je))break;let s=e.dataset[l]||"",c=we(l,s);o.set(l,c),r.has(c)?i.delete(c):n.push(l)}for(let[l,s]of i)s();for(let l of n){let s=o.get(l);Fn(e,l,s)}})}function Hn(){je||(je=new MutationObserver(t=>{let e=new Set,n=new Set;for(let{target:r,type:i,addedNodes:o,removedNodes:l}of t)switch(i){case"childList":{for(let s of l)e.add(s);for(let s of o)n.add(s)}break;case"attributes":{n.add(r);break}}for(let r of e){let i=te.get(r.id);if(i){for(let[o,l]of i)l(),i.delete(o);i.size===0&&te.delete(r.id)}}for(let r of n)Le(r)}),je.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function Fn(t,e,n){let r=Z(e.slice(Je.length)),i=Ke.find(g=>r.startsWith(g.name));if(!i)return;t.id.length||(t.id=_e(t));let[o,...l]=r.slice(i.name.length).split(/\_\_+/),s=o.length>0;s&&(o=Z(o));let c=t.dataset[e]||"",S=c.length>0,E={signals:At,applyToElement:Le,effect:g=>Ne(g),actions:Pe,removals:te,genRX:()=>qn(E,...i.argNames||[]),plugin:i,el:t,rawKey:r,key:o,value:c,mods:new Map},x=i.keyReq||0;if(s){if(x===2)throw C(`${i.name}KeyNotAllowed`,E)}else if(x===1)throw C(`${i.name}KeyRequired`,E);let b=i.valReq||0;if(S){if(b===2)throw C(`${i.name}ValueNotAllowed`,E)}else if(b===1)throw C(`${i.name}ValueRequired`,E);if(x===3||b===3){if(s&&S)throw C(`${i.name}KeyAndValueProvided`,E);if(!s&&!S)throw C(`${i.name}KeyOrValueRequired`,E)}for(let g of l){let[T,...v]=g.split(".");E.mods.set(Z(T),new Set(v.map(a=>a.toLowerCase())))}let _=i.onLoad(E)??(()=>{}),y=te.get(t.id);y||(y=new Map,te.set(t.id,y)),y.set(n,_)}function qn(t,...e){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=t.value.trim().match(r);if(i){let _=i.length-1,y=i[_].trim();y.startsWith("return")||(i[_]=`return (${y});`),n=i.join(`;
`)}let o=new Map,l=new RegExp(`(?:${Se})(.*?)(?:${Fe})`,"gm");for(let _ of n.matchAll(l)){let y=_[1],g=new re("dsEscaped").with(y).string;o.set(g,y),n=n.replace(Se+y+Fe,g)}let s=/@(\w*)\(/gm,c=n.matchAll(s),S=new Set;for(let _ of c)S.add(_[1]);let E=new RegExp(`@(${Object.keys(Pe).join("|")})\\(`,"gm");n=n.replaceAll(E,"ctx.actions.$1.fn(ctx,");let x=t.signals.paths();if(x.length){let _=new RegExp(`\\$(${x.join("|")})(\\W|$)`,"gm");n=n.replaceAll(_,"ctx.signals.signal('$1').value$2")}for(let[_,y]of o)n=n.replace(_,y);let b=`return (()=> {
${n}
})()`;t.fnContent=b;try{let _=new Function("ctx",...e,b);return(...y)=>{try{return _(t,...y)}catch(g){throw C("ExecuteExpression",t,{error:g.message})}}}catch(_){throw C("GenerateExpression",t,{error:_.message})}}Ie(ft,ct,ut);async function Wn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function $n(t){let e,n,r,i=!1;return function(l){e===void 0?(e=l,n=0,r=-1):e=Un(e,l);let s=e.length,c=0;for(;n<s;){i&&(e[n]===10&&(c=++n),i=!1);let S=-1;for(;n<s&&S===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-c);break;case 13:i=!0;case 10:S=n;break}if(S===-1)break;t(e.subarray(c,S),r),c=n,r=-1}c===s?e=void 0:c!==0&&(e=e.subarray(c),n-=c)}}function Gn(t,e,n){let r=_t(),i=new TextDecoder;return function(l,s){if(l.length===0)n?.(r),r=_t();else if(s>0){let c=i.decode(l.subarray(0,s)),S=s+(l[s+1]===32?2:1),E=i.decode(l.subarray(S));switch(c){case"data":r.data=r.data?`${r.data}
${E}`:E;break;case"event":r.event=E;break;case"id":t(r.id=E);break;case"retry":{let x=Number.parseInt(E,10);Number.isNaN(x)||e(r.retry=x);break}}}}}function Un(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function _t(){return{data:"",event:"",id:"",retry:void 0}}var Bn="text/event-stream",wt="last-event-id";function xt(t,{signal:e,headers:n,onopen:r,onmessage:i,onclose:o,onerror:l,openWhenHidden:s,fetch:c,retryInterval:S=1e3,retryScaler:E=2,retryMaxWaitMs:x=3e4,retryMaxCount:b=10,..._}){return new Promise((y,g)=>{let T=0,v={...n};v.accept||(v.accept=Bn);let a;function p(){a.abort(),document.hidden||d()}s||document.addEventListener("visibilitychange",p);let f=0;function u(){document.removeEventListener("visibilitychange",p),window.clearTimeout(f),a.abort()}e?.addEventListener("abort",()=>{u(),y()});let h=c??window.fetch,m=r??function(){};async function d(){a=new AbortController;try{let A=await h(t,{..._,headers:v,signal:a.signal});await m(A),await Wn(A.body,$n(Gn(w=>{w?v[wt]=w:delete v[wt]},w=>{S=w},i))),o?.(),u(),y()}catch(A){if(!a.signal.aborted)try{let w=l?.(A)??S;window.clearTimeout(f),f=window.setTimeout(d,w),S*=E,S=Math.min(S,x),T++,T>b?(u(),g("Max retries reached.")):console.error(`Datastar failed to reach ${t.toString()} retrying in ${w}ms.`)}catch(w){u(),g(w)}}}d()})}var ue=`${H}-sse`,Ye=`${H}-settling`,ne=`${H}-swapping`,Ve="started",De="finished",Rt="error",Mt="retrying";function K(t,e){document.addEventListener(ue,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function ve(t,e){document.dispatchEvent(new CustomEvent(ue,{detail:{type:t,argsRaw:e}}))}var Nt=t=>`${t}`.includes("text/event-stream"),J=async(t,e,n,r)=>{let{el:{id:i},el:o,signals:l}=t,{headers:s,contentType:c,includeLocal:S,selector:E,openWhenHidden:x,retryInterval:b,retryScaler:_,retryMaxWaitMs:y,retryMaxCount:g,abort:T}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:it,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),v=e.toLowerCase(),a=()=>{};try{if(ve(Ve,{elId:i}),!n?.length)throw C("SseNoUrlProvided",t,{action:v});let p={};p[rt]=!0,c==="json"&&(p["Content-Type"]="application/json");let f=Object.assign({},p,s),u={method:e,headers:f,openWhenHidden:x,retryInterval:b,retryScaler:_,retryMaxWaitMs:y,retryMaxCount:g,signal:T,onopen:async d=>{if(d.status>=400){let A=d.status.toString();ve(Rt,{status:A})}},onmessage:d=>{if(!d.event.startsWith(H))return;let A=d.event,w={},N=d.data.split(`
`);for(let V of N){let I=V.indexOf(" "),O=V.slice(0,I),D=w[O];D||(D=[],w[O]=D);let $=V.slice(I+1);D.push($)}let M={};for(let[V,I]of Object.entries(w))M[V]=I.join(`
`);ve(A,M)},onerror:d=>{if(Nt(d))throw C("InvalidContentType",t,{url:n});d&&(console.error(d.message),ve(Mt,{message:d.message}))}},h=new URL(n,window.location.origin),m=new URLSearchParams(h.search);if(c==="json"){let d=l.JSON(!1,!S);e==="GET"?m.set(H,d):u.body=d}else if(c==="form"){let d=E?document.querySelector(E):o.closest("form");if(d===null)throw E?C("SseFormNotFound",t,{action:v,selector:E}):C("SseClosestFormNotFound",t,{action:v});if(o!==d){let w=N=>N.preventDefault();d.addEventListener("submit",w),a=()=>d.removeEventListener("submit",w)}if(!d.checkValidity()){d.reportValidity(),a();return}let A=new FormData(d);if(e==="GET"){let w=new URLSearchParams(A);for(let[N,M]of w)m.set(N,M)}else u.body=A}else throw C("SseInvalidContentType",t,{action:v,contentType:c});h.search=m.toString();try{await xt(h.toString(),u)}catch(d){if(!Nt(d))throw C("SseFetchFailed",t,{method:e,url:n,error:d})}}finally{ve(De,{elId:i}),a()}};var Ct={type:3,name:"delete",fn:async(t,e,n)=>J(t,"DELETE",e,{...n})};var Pt={type:3,name:"get",fn:async(t,e,n)=>J(t,"GET",e,{...n})};var It={type:3,name:"patch",fn:async(t,e,n)=>J(t,"PATCH",e,{...n})};var Lt={type:3,name:"post",fn:async(t,e,n)=>J(t,"POST",e,{...n})};var Vt={type:3,name:"put",fn:async(t,e,n)=>J(t,"PUT",e,{...n})};var Dt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?k(e,n):Y(i),{signal:l}=r.upsertIfMissing(o,!1),s=c=>{let{type:S,argsRaw:{elId:E}}=c.detail;if(E===t.id)switch(S){case Ve:l.value=!0;break;case De:l.value=!1;break}};return document.addEventListener(ue,s),()=>{l.value=!1,document.removeEventListener(ue,s)}}};var kt={type:2,name:F.ExecuteScript,onGlobalInit:async t=>{K(F.ExecuteScript,({autoRemove:e=`${at}`,attributes:n=ot,script:r})=>{let i=z(e);if(!r?.length)throw W("NoScriptProvided",t);let o=document.createElement("script");for(let l of n.split(`
`)){let s=l.indexOf(" "),c=s?l.slice(0,s):l,S=s?l.slice(s):"";o.setAttribute(c.trim(),S.trim())}o.text=r,document.head.appendChild(o),i&&o.remove()})}};var ye=document,X=!!ye.startViewTransition;var Ot=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:b=>b.getAttribute("im-preserve")==="true",shouldReAppend:b=>b.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!0};function n(b,_,y={}){b=E(b);let g=x(_),T=S(b,g,y),v=i(T,()=>s(T,b,g,a=>a.morphStyle==="innerHTML"?(o(a,b,g),Array.from(b.childNodes)):r(a,b,g)));return T.pantry.remove(),v}function r(b,_,y){let g=x(_);return o(b,g,y,_,_.nextSibling),Array.from(g.childNodes)}function i(b,_){if(!b.config.restoreFocus)return _();let y=document.activeElement;if(!(y instanceof HTMLInputElement||y instanceof HTMLTextAreaElement))return _();let{id:g,selectionStart:T,selectionEnd:v}=y,a=_();return g&&g!==document.activeElement?.id&&(y=b.target.querySelector(`[id="${g}"]`),y?.focus()),y&&!y.selectionEnd&&v&&y.setSelectionRange(T,v),a}let o=function(){function b(f,u,h,m=null,d=null){u instanceof HTMLTemplateElement&&h instanceof HTMLTemplateElement&&(u=u.content,h=h.content),m||=u.firstChild;for(let A of h.childNodes){if(m&&m!=d){let N=y(f,A,m,d);if(N){N!==m&&T(f,m,N),l(N,A,f),m=N.nextSibling;continue}}if(A instanceof Element&&f.persistentIds.has(A.id)){let N=v(u,A.id,m,f);l(N,A,f),m=N.nextSibling;continue}let w=_(u,A,m,f);w&&(m=w.nextSibling)}for(;m&&m!=d;){let A=m;m=m.nextSibling,g(f,A)}}function _(f,u,h,m){if(m.callbacks.beforeNodeAdded(u)===!1)return null;if(m.idMap.has(u)){let d=document.createElement(u.tagName);return f.insertBefore(d,h),l(d,u,m),m.callbacks.afterNodeAdded(d),d}else{let d=document.importNode(u,!0);return f.insertBefore(d,h),m.callbacks.afterNodeAdded(d),d}}let y=function(){function f(m,d,A,w){let N=null,M=d.nextSibling,V=0,I=A;for(;I&&I!=w;){if(h(I,d)){if(u(m,I,d))return I;N===null&&(m.idMap.has(I)||(N=I))}if(N===null&&M&&h(I,M)&&(V++,M=M.nextSibling,V>=2&&(N=void 0)),I.contains(document.activeElement))break;I=I.nextSibling}return N||null}function u(m,d,A){let w=m.idMap.get(d),N=m.idMap.get(A);if(!N||!w)return!1;for(let M of w)if(N.has(M))return!0;return!1}function h(m,d){let A=m,w=d;return A.nodeType===w.nodeType&&A.tagName===w.tagName&&(!A.id||A.id===w.id)}return f}();function g(f,u){if(f.idMap.has(u))p(f.pantry,u,null);else{if(f.callbacks.beforeNodeRemoved(u)===!1)return;u.parentNode?.removeChild(u),f.callbacks.afterNodeRemoved(u)}}function T(f,u,h){let m=u;for(;m&&m!==h;){let d=m;m=m.nextSibling,g(f,d)}return m}function v(f,u,h,m){let d=m.target.id===u&&m.target||m.target.querySelector(`[id="${u}"]`)||m.pantry.querySelector(`[id="${u}"]`);return a(d,m),p(f,d,h),d}function a(f,u){let h=f.id;for(;f=f.parentNode;){let m=u.idMap.get(f);m&&(m.delete(h),m.size||u.idMap.delete(f))}}function p(f,u,h){if(f.moveBefore)try{f.moveBefore(u,h)}catch{f.insertBefore(u,h)}else f.insertBefore(u,h)}return b}(),l=function(){function b(a,p,f){return f.ignoreActive&&a===document.activeElement?null:(f.callbacks.beforeNodeMorphed(a,p)===!1||(a instanceof HTMLHeadElement&&f.head.ignore||(a instanceof HTMLHeadElement&&f.head.style!=="morph"?c(a,p,f):(_(a,p,f),v(a,f)||o(f,a,p))),f.callbacks.afterNodeMorphed(a,p)),a)}function _(a,p,f){let u=p.nodeType;if(u===1){let h=a,m=p,d=h.attributes,A=m.attributes;for(let w of A)T(w.name,h,"update",f)||h.getAttribute(w.name)!==w.value&&h.setAttribute(w.name,w.value);for(let w=d.length-1;0<=w;w--){let N=d[w];if(N&&!m.hasAttribute(N.name)){if(T(N.name,h,"remove",f))continue;h.removeAttribute(N.name)}}v(h,f)||y(h,m,f)}(u===8||u===3)&&a.nodeValue!==p.nodeValue&&(a.nodeValue=p.nodeValue)}function y(a,p,f){if(a instanceof HTMLInputElement&&p instanceof HTMLInputElement&&p.type!=="file"){let u=p.value,h=a.value;g(a,p,"checked",f),g(a,p,"disabled",f),p.hasAttribute("value")?h!==u&&(T("value",a,"update",f)||(a.setAttribute("value",u),a.value=u)):T("value",a,"remove",f)||(a.value="",a.removeAttribute("value"))}else if(a instanceof HTMLOptionElement&&p instanceof HTMLOptionElement)g(a,p,"selected",f);else if(a instanceof HTMLTextAreaElement&&p instanceof HTMLTextAreaElement){let u=p.value,h=a.value;if(T("value",a,"update",f))return;u!==h&&(a.value=u),a.firstChild&&a.firstChild.nodeValue!==u&&(a.firstChild.nodeValue=u)}}function g(a,p,f,u){let h=p[f],m=a[f];if(h!==m){let d=T(f,a,"update",u);d||(a[f]=p[f]),h?d||a.setAttribute(f,""):T(f,a,"remove",u)||a.removeAttribute(f)}}function T(a,p,f,u){return a==="value"&&u.ignoreActiveValue&&p===document.activeElement?!0:u.callbacks.beforeAttributeUpdated(a,p,f)===!1}function v(a,p){return!!p.ignoreActiveValue&&a===document.activeElement&&a!==document.body}return b}();function s(b,_,y,g){if(b.head.block){let T=_.querySelector("head"),v=y.querySelector("head");if(T&&v){let a=c(T,v,b);return Promise.all(a).then(()=>{let p=Object.assign(b,{head:{block:!1,ignore:!0}});return g(p)})}}return g(b)}function c(b,_,y){let g=[],T=[],v=[],a=[],p=new Map;for(let u of _.children)p.set(u.outerHTML,u);for(let u of b.children){let h=p.has(u.outerHTML),m=y.head.shouldReAppend(u),d=y.head.shouldPreserve(u);h||d?m?T.push(u):(p.delete(u.outerHTML),v.push(u)):y.head.style==="append"?m&&(T.push(u),a.push(u)):y.head.shouldRemove(u)!==!1&&T.push(u)}a.push(...p.values());let f=[];for(let u of a){let h=document.createRange().createContextualFragment(u.outerHTML).firstChild;if(y.callbacks.beforeNodeAdded(h)!==!1){if("href"in h&&h.href||"src"in h&&h.src){let m,d=new Promise(function(A){m=A});h.addEventListener("load",function(){m()}),f.push(d)}b.appendChild(h),y.callbacks.afterNodeAdded(h),g.push(h)}}for(let u of T)y.callbacks.beforeNodeRemoved(u)!==!1&&(b.removeChild(u),y.callbacks.afterNodeRemoved(u));return y.head.afterHeadMorphed(b,{added:g,kept:v,removed:T}),f}let S=function(){function b(p,f,u){let{persistentIds:h,idMap:m}=v(p,f),d=_(u),A=d.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(A))throw`Do not understand how to morph style ${A}`;return{target:p,newContent:f,config:d,morphStyle:A,ignoreActive:d.ignoreActive,ignoreActiveValue:d.ignoreActiveValue,restoreFocus:d.restoreFocus,idMap:m,persistentIds:h,pantry:y(),callbacks:d.callbacks,head:d.head}}function _(p){let f=Object.assign({},e);return Object.assign(f,p),f.callbacks=Object.assign({},e.callbacks,p.callbacks),f.head=Object.assign({},e.head,p.head),f}function y(){let p=document.createElement("div");return p.hidden=!0,document.body.insertAdjacentElement("afterend",p),p}function g(p){let f=Array.from(p.querySelectorAll("[id]"));return p.id&&f.push(p),f}function T(p,f,u,h){for(let m of h)if(f.has(m.id)){let d=m;for(;d;){let A=p.get(d);if(A==null&&(A=new Set,p.set(d,A)),A.add(m.id),d===u)break;d=d.parentElement}}}function v(p,f){let u=g(p),h=g(f),m=a(u,h),d=new Map;T(d,m,p,u);let A=f.__idiomorphRoot||f;return T(d,m,A,h),{persistentIds:m,idMap:d}}function a(p,f){let u=new Set,h=new Map;for(let{id:d,tagName:A}of p)h.has(d)?u.add(d):h.set(d,A);let m=new Set;for(let{id:d,tagName:A}of f)m.has(d)?u.add(d):h.get(d)===A&&m.add(d);for(let d of u)m.delete(d);return m}return b}(),{normalizeElement:E,normalizeParent:x}=function(){let b=new WeakSet;function _(v){return v instanceof Document?v.documentElement:v}function y(v){if(v==null)return document.createElement("div");if(typeof v=="string")return y(T(v));if(b.has(v))return v;if(v instanceof Node){if(v.parentNode)return new g(v);{let a=document.createElement("div");return a.append(v),a}}else{let a=document.createElement("div");for(let p of[...v])a.append(p);return a}}class g{constructor(a){this.originalNode=a,this.realParentNode=a.parentNode,this.previousSibling=a.previousSibling,this.nextSibling=a.nextSibling}get childNodes(){let a=[],p=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;p&&p!=this.nextSibling;)a.push(p),p=p.nextSibling;return a}querySelectorAll(a){return this.childNodes.reduce((p,f)=>{if(f instanceof Element){f.matches(a)&&p.push(f);let u=f.querySelectorAll(a);for(let h=0;h<u.length;h++)p.push(u[h])}return p},[])}insertBefore(a,p){return this.realParentNode.insertBefore(a,p)}moveBefore(a,p){return this.realParentNode.moveBefore(a,p)}get __idiomorphRoot(){return this.originalNode}}function T(v){let a=new DOMParser,p=v.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(p.match(/<\/html>/)||p.match(/<\/head>/)||p.match(/<\/body>/)){let f=a.parseFromString(v,"text/html");if(p.match(/<\/html>/))return b.add(f),f;{let u=f.firstChild;return u&&b.add(u),u}}else{let u=a.parseFromString("<body><template>"+v+"</template></body>","text/html").body.querySelector("template").content;return b.add(u),u}}return{normalizeElement:_,normalizeParent:y}}();return{morph:n,defaults:e}}();var Ft={type:2,name:F.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");K(F.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=lt,settleDuration:o=`${Te}`,useViewTransition:l=`${Ae}`})=>{let s=Number.parseInt(o),c=z(l);e.innerHTML=n.trim();let S=[...e.content.children];for(let E of S){if(!(E instanceof Element))throw W("NoFragmentsFound",t);let x=r||`#${E.getAttribute("id")}`,b=[...document.querySelectorAll(x)||[]];if(!b.length)throw W("NoTargetsFound",t,{selectorOrID:x});c&&X?ye.startViewTransition(()=>Ht(t,i,s,E,b)):Ht(t,i,s,E,b)}})}};function Ht(t,e,n,r,i){for(let o of i){o.classList.add(ne);let l=o.outerHTML,s=o;switch(e){case U.Morph:{let E=r.cloneNode(!0);pe(E,x=>{!x.id?.length&&Object.keys(x.dataset).length&&(x.id=_e(x));let b=t.removals.get(x.id);if(b){let _=new Map;for(let[y,g]of b){let T=we(y,y);_.set(T,g),b.delete(y)}t.removals.set(x.id,_)}}),Ot.morph(s,E);break}case U.Inner:s.innerHTML=r.outerHTML;break;case U.Outer:s.replaceWith(r);break;case U.Prepend:s.prepend(r);break;case U.Append:s.append(r);break;case U.Before:s.before(r);break;case U.After:s.after(r);break;case U.UpsertAttributes:for(let E of r.getAttributeNames()){let x=r.getAttribute(E);s.setAttribute(E,x)}break;default:throw W("InvalidMergeMode",t,{mergeMode:e})}let c=s.classList;c?.add(ne),setTimeout(()=>{o.classList.remove(ne),c?.remove(ne)},n);let S=s.outerHTML;c&&l!==S&&(c.add(Ye),setTimeout(()=>{c.remove(Ye)},n))}}var qt={type:2,name:F.MergeSignals,onGlobalInit:async t=>{K(F.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${st}`})=>{let{signals:r}=t,i=z(n);r.merge(Ee(e),i)})}};var Wt={type:2,name:F.RemoveFragments,onGlobalInit:async t=>{K(F.RemoveFragments,({selector:e,settleDuration:n=`${Te}`,useViewTransition:r=`${Ae}`})=>{if(!e.length)throw W("NoSelectorProvided",t);let i=Number.parseInt(n),o=z(r),l=document.querySelectorAll(e),s=()=>{for(let c of l)c.classList.add(ne);setTimeout(()=>{for(let c of l)c.remove()},i)};o&&X?ye.startViewTransition(()=>s()):s()})}};var $t={type:2,name:F.RemoveSignals,onGlobalInit:async t=>{K(F.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw W("NoPathsProvided",t);t.signals.remove(...n)})}};var Gt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw C("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var Ut={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw C("CustomValidityInvalidElement",t);let i=n();return r(()=>{let o=i();if(typeof o!="string")throw C("CustomValidityInvalidExpression",t,{result:o});e.setCustomValidity(o)})}};var Bt="once",jt="half",Kt="full",Jt={type:1,name:"intersects",keyReq:2,mods:new Set([Bt,jt,Kt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(Kt)?i.threshold=1:n.has(jt)&&(i.threshold=.5);let o=r(),l=new IntersectionObserver(s=>{for(let c of s)c.isIntersecting&&(o(),n.has(Bt)&&(l.disconnect(),delete t.dataset[e]))},i);return l.observe(t),()=>l.disconnect()}};var zt="session",Yt={type:1,name:"persist",mods:new Set([zt]),onLoad:({key:t,effect:e,mods:n,signals:r,value:i})=>{t=k(t,n),t===""&&(t=H);let o=n.has(zt)?sessionStorage:localStorage,l=i.split(/\s+/).filter(S=>S!=="");l=l.map(S=>Y(S));let s=()=>{let S=o.getItem(t)||"{}",E=JSON.parse(S);r.merge(E)},c=()=>{let S;l.length?S=r.subset(...l):S=r.values(),o.setItem(t,JSON.stringify(S))};return s(),e(()=>{c()})}};var Xt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,o=new URL(r,i).toString();window.history.replaceState({},"",o)})}};var ke="smooth",Xe="instant",Qe="auto",Qt="hstart",Zt="hcenter",en="hend",tn="hnearest",nn="vstart",rn="vcenter",on="vend",sn="vnearest",jn="focus",Oe="center",an="start",ln="end",un="nearest",cn={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([ke,Xe,Qe,Qt,Zt,en,tn,nn,rn,on,sn,jn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:ke,block:Oe,inline:Oe};if(n.has(ke)&&(i.behavior=ke),n.has(Xe)&&(i.behavior=Xe),n.has(Qe)&&(i.behavior=Qe),n.has(Qt)&&(i.inline=an),n.has(Zt)&&(i.inline=Oe),n.has(en)&&(i.inline=ln),n.has(tn)&&(i.inline=un),n.has(nn)&&(i.block=an),n.has(rn)&&(i.block=Oe),n.has(on)&&(i.block=ln),n.has(sn)&&(i.block=un),!(e instanceof HTMLElement||e instanceof SVGElement))throw C("ScrollIntoViewInvalidElement",t);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r]}};var fn="none",dn="display",pn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===fn&&t.removeProperty(dn):t.setProperty(dn,fn)})}};var mn="view-transition",gn={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===mn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=mn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!X){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var hn={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let i=r();return e===""?n(async()=>{let o=i();for(let[l,s]of Object.entries(o))s===!1?t.removeAttribute(l):t.setAttribute(l,s)}):(e=de(e),n(async()=>{let o=!1;try{o=i()}catch{}let l;typeof o=="string"?l=o:l=JSON.stringify(o),!l||l==="false"||l==="null"||l==="undefined"?t.removeAttribute(e):t.setAttribute(e,l)}))}};var Kn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,vn=["change","input","keydown"],yn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:i,value:o,effect:l}=t,s=e,c=n?k(n,r):Y(o),S=e.tagName.toLowerCase(),E=S.includes("input"),x=S.includes("select"),b=e.getAttribute("type"),_=e.hasAttribute("value"),y="",g=E&&b==="checkbox";g&&(y=_?"":!1);let T=E&&b==="number";T&&(y=0);let v=E&&b==="radio";v&&(e.getAttribute("name")?.length||e.setAttribute("name",c));let a=E&&b==="file",{signal:p,inserted:f}=i.upsertIfMissing(c,y),u=-1;Array.isArray(p.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",c),u=[...document.querySelectorAll(`[name="${c}"]`)].findIndex(M=>M===t.el));let h=u>=0,m=()=>[...i.value(c)],d=()=>{let M=i.value(c);h&&!x&&(M=M[u]||y);let V=`${M}`;if(g||v)typeof M=="boolean"?s.checked=M:s.checked=V===s.value;else if(x){let I=e;if(I.multiple){if(!h)throw C("BindSelectMultiple",t);for(let O of I.options){if(O?.disabled)return;let D=T?Number(O.value):O.value;O.selected=M.includes(D)}}else I.value=V}else a||("value"in e?e.value=V:e.setAttribute("value",V))},A=async()=>{let M=i.value(c);if(h){let D=M;for(;u>=D.length;)D.push(y);M=D[u]||y}let V=(D,$)=>{let G=$;h&&!x&&(G=m(),G[u]=$),i.setValue(D,G)};if(a){let D=[...s?.files||[]],$=[],G=[],et=[];await Promise.all(D.map(tt=>new Promise(Nn=>{let Q=new FileReader;Q.onload=()=>{if(typeof Q.result!="string")throw C("InvalidFileResultType",t,{resultType:typeof Q.result});let He=Q.result.match(Kn);if(!He?.groups)throw C("InvalidDataUri",t,{result:Q.result});$.push(He.groups.contents),G.push(He.groups.mime),et.push(tt.name)},Q.onloadend=()=>Nn(void 0),Q.readAsDataURL(tt)}))),V(c,$),V(`${c}Mimes`,G),V(`${c}Names`,et);return}let I=s.value||"",O;if(g){let D=s.checked||s.getAttribute("checked")==="true";_?O=D?I:"":O=D}else if(x){let $=[...e.selectedOptions];h?O=$.filter(G=>G.selected).map(G=>G.value):O=$[0]?.value||y}else typeof M=="boolean"?O=!!I:typeof M=="number"?O=Number(I):O=I||"";V(c,O)};f&&A();for(let M of vn)e.addEventListener(M,A);let w=M=>{M.persisted&&A()};window.addEventListener("pageshow",w);let N=l(()=>d());return()=>{N();for(let M of vn)e.removeEventListener(M,A);window.removeEventListener("pageshow",w)}}};var bn={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:i})=>{let o=t.classList,l=i();return r(()=>{if(e===""){let s=l();for(let[c,S]of Object.entries(s)){let E=c.split(/\s+/);S?o.add(...E):o.remove(...E)}}else e=k(e,n),l()?o.add(e):o.remove(e)})}};function be(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ce(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function Sn(t,e){return(...n)=>{setTimeout(()=>{t(...n)},e)}}function Tn(t,e,n=!1,r=!0){let i=-1,o=()=>i&&clearTimeout(i);return(...l)=>{o(),n&&!i&&t(...l),i=setTimeout(()=>{r&&t(...l),o()},e)}}function An(t,e,n=!0,r=!1){let i=!1;return(...o)=>{i||(n&&t(...o),i=!0,setTimeout(()=>{i=!1,r&&t(...o)},e))}}var Jn="evt",Ze="signalsChange",zn=Ze.length,En={type:1,name:"on",keyReq:1,valReq:1,argNames:[Jn],onLoad:({el:t,key:e,mods:n,signals:r,effect:i,genRX:o})=>{let l=o(),s=t;n.has("window")&&(s=window);let c=g=>{g&&((n.has("prevent")||e==="submit")&&g.preventDefault(),n.has("stop")&&g.stopPropagation()),l(g)},S=n.get("delay");if(S){let g=be(S);c=Sn(c,g)}let E=n.get("debounce");if(E){let g=be(E),T=ce(E,"leading",!1),v=!ce(E,"notrail",!1);c=Tn(c,g,T,v)}let x=n.get("throttle");if(x){let g=be(x),T=!ce(x,"noleading",!1),v=ce(x,"trail",!1);c=An(c,g,T,v)}if(n.has("viewtransition")&&X){let g=c;c=(...T)=>document.startViewTransition(()=>g(...T))}let b={capture:!0,passive:!1,once:!1};if(n.has("capture")||(b.capture=!1),n.has("passive")&&(b.passive=!0),n.has("once")&&(b.once=!0),e==="load")return setTimeout(c,0),()=>{};if(e==="interval"){let g=1e3,T=n.get("duration");T&&(g=be(T),ce(T,"leading",!1)&&c());let v=setInterval(c,g);return()=>{clearInterval(v)}}if(e==="raf"){let g,T=()=>{c(),g=requestAnimationFrame(T)};return g=requestAnimationFrame(T),()=>{g&&cancelAnimationFrame(g)}}if(e.startsWith(Ze)){if(e===Ze){let v=a=>c(a);return document.addEventListener(fe,v),()=>{document.removeEventListener(fe,v)}}let g=k(Z(e.slice(zn)),n),T=new Map;return r.walk((v,a)=>{v.startsWith(g)&&T.set(a,a.value)}),i(()=>{for(let[v,a]of T)a!==v.value&&(c(),T.set(v,v.value))})}if(n.has("outside")){s=document;let g=c;c=v=>{let a=v?.target;t.contains(a)||g(v)}}let y=k(e,n);return s.addEventListener(y,c,b),()=>{s.removeEventListener(y,c)}}};var _n={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?k(e,n):Y(i);r.setValue(o,t)}};var wn={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,i=r();return e instanceof HTMLElement||C("TextInvalidElement",t),n(()=>{let o=i(t);e.textContent=`${o}`})}};var{round:Yn,max:Xn,min:Qn}=Math,xn={type:3,name:"fit",fn:(t,e,n,r,i,o,l=!1,s=!1)=>{let c=(e-n)/(r-n)*(o-i)+i;return s&&(c=Yn(c)),l&&(c=Xn(i,Qn(o,c))),c}};var Rn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var Mn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Ie(hn,yn,bn,En,_n,pn,wn,Dt,Pt,Lt,Vt,It,Ct,Ft,qt,Wt,$t,kt,Gt,Ut,Jt,Yt,Xt,cn,gn,xn,Rn,Mn);ze();export{ze as apply,Ie as load,Et as setAlias};
//# sourceMappingURL=datastar.js.map
