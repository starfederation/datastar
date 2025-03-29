// Datastar v1.0.0-beta.10
var Qe=/🖕JS_DS🚀/.source,Te=Qe.slice(0,5),He=Qe.slice(4),q="datastar",et="Datastar-Request",tt=1e3,nt="type module",Ae=!1,rt=!1,it=!0,U={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},ot=U.Morph,F={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var w=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(w||{});var me=`${q}-signals`;var Y=t=>t.trim()==="true",z=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),ge=t=>z(t).replace(/-./g,e=>e[1].toUpperCase()),Fe=t=>z(t).replace(/-/g,"_"),Nn=t=>ge(t).replace(/^./,e=>e[0].toUpperCase()),Ee=t=>new Function(`return Object.assign({}, ${t})`)(),X=t=>t.startsWith("$")?t.slice(1):t,Pn={kebab:z,snake:Fe,pascal:Nn};function H(t,e){for(let n of e.get("case")||[]){let r=Pn[n];r&&(t=r(t))}return t}var Cn="computed",st={type:1,name:Cn,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=H(t,e);let i=r();n.setComputed(t,i)}};var at={type:1,name:"signals",onLoad:t=>{let{key:e,mods:n,signals:r,value:i,genRX:s}=t,o=n.has("ifmissing");if(e!==""){let a=H(e,n),p=i===""?i:s()();o?r.upsertIfMissing(a,p):r.setValue(a,p)}else{let a=Ee(t.value);t.value=JSON.stringify(a);let h=s()();r.merge(h,o)}}};var lt={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ae=class{#e=0;#t;constructor(e=q){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function _e(t){if(t.id)return t.id;let e=new ae,n=t;for(;n;){if(e.with(n.tagName||""),n.id){e.with(n.id);break}let r=n?.parentNode;r&&e.with([...r.children].indexOf(n)),n=r}return e.string}function Re(t,e){return new ae().with(t).with(e).value}function he(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)he(r,e),r=r.nextElementSibling}var In="https://data-star.dev/errors";function qe(t,e,n={}){let r=new Error;r.name=`${q} ${t} error`;let i=Fe(e),s=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),o=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${In}/${t}/${i}?${s}
Context: ${o}`,r}function B(t,e,n={}){return qe("internal",e,Object.assign({from:t},n))}function W(t,e,n={}){let r={plugin:{name:e.plugin.name,type:w[e.plugin.type]}};return qe("init",t,Object.assign(r,n))}function N(t,e,n={}){let r={plugin:{name:e.plugin.name,type:w[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return qe("runtime",t,Object.assign(r,n))}var le="preact-signals",kn=Symbol.for("preact-signals"),j=1,ue=2,ve=4,fe=8,we=16,ce=32;function $e(){xe++}function Ge(){if(xe>1){xe--;return}let t,e=!1;for(;ye!==void 0;){let n=ye;for(ye=void 0,We++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~ue,!(n._flags&fe)&&ct(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(We=0,xe--,e)throw t}var P;var ye,xe=0,We=0,Me=0;function ut(t){if(P===void 0)return;let e=t._node;if(e===void 0||e._target!==P)return e={_version:0,_source:t,_prevSource:P._sources,_nextSource:void 0,_target:P,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},P._sources!==void 0&&(P._sources._nextSource=e),P._sources=e,t._node=e,P._flags&ce&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=P._sources,e._nextSource=void 0,P._sources._nextSource=e,P._sources=e),e}function k(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}k.prototype.brand=kn;k.prototype._refresh=()=>!0;k.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};k.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};k.prototype.subscribe=function(t){return de(()=>{let e=this.value,n=P;P=void 0;try{t(e)}finally{P=n}})};k.prototype.valueOf=function(){return this.value};k.prototype.toString=function(){return`${this.value}`};k.prototype.toJSON=function(){return this.value};k.prototype.peek=function(){let t=P;P=void 0;try{return this.value}finally{P=t}};Object.defineProperty(k.prototype,"value",{get(){let t=ut(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(We>100)throw B(le,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Me++,$e();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{Ge()}this?._onChange({old:e,revised:n})}}});function ct(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function ft(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function dt(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function ne(t){k.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Me-1,this._flags=ve}ne.prototype=new k;ne.prototype._refresh=function(){if(this._flags&=~ue,this._flags&j)return!1;if((this._flags&(ve|ce))===ce||(this._flags&=~ve,this._globalVersion===Me))return!0;if(this._globalVersion=Me,this._flags|=j,this._version>0&&!ct(this))return this._flags&=~j,!0;let t=P;try{ft(this),P=this;let e=this._fn();(this._flags&we||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~we,this._version++)}catch(e){this._value=e,this._flags|=we,this._version++}return P=t,dt(this),this._flags&=~j,!0};ne.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=ve|ce;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}k.prototype._subscribe.call(this,t)};ne.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(k.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~ce;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};ne.prototype._notify=function(){if(!(this._flags&ue)){this._flags|=ve|ue;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(ne.prototype,"value",{get(){if(this._flags&j)throw B(le,"SignalCycleDetected");let t=ut(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&we)throw B(le,"GetComputedError",{value:this._value});return this._value}});function pt(t){return new ne(t)}function mt(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){$e();let n=P;P=void 0;try{e()}catch(r){throw t._flags&=~j,t._flags|=fe,Ue(t),B(le,"CleanupEffectError",{error:r})}finally{P=n,Ge()}}}function Ue(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,mt(t)}function Ln(t){if(P!==this)throw B(le,"EndEffectError");dt(this),P=t,this._flags&=~j,this._flags&fe&&Ue(this),Ge()}function be(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=ce}be.prototype._callback=function(){let t=this._start();try{if(this._flags&fe||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};be.prototype._start=function(){if(this._flags&j)throw B(le,"SignalCycleDetected");this._flags|=j,this._flags&=~fe,mt(this),ft(this),$e();let t=P;return P=this,Ln.bind(this,t)};be.prototype._notify=function(){this._flags&ue||(this._flags|=ue,this._nextBatchedEffect=ye,ye=this)};be.prototype._dispose=function(){this._flags|=fe,this._flags&j||Ue(this)};function de(t){let e=new be(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var gt="namespacedSignals",pe=t=>{document.dispatchEvent(new CustomEvent(me,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function ht(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof k?n[r]=i.value:n[r]=ht(i)}return n}function yt(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw B(gt,"InvalidSignalKey",{key:i});let s=e[i];if(s instanceof Object&&!Array.isArray(s)){t[i]||(t[i]={});let o=yt(t[i],s,n);r.added.push(...o.added.map(a=>`${i}.${a}`)),r.removed.push(...o.removed.map(a=>`${i}.${a}`)),r.updated.push(...o.updated.map(a=>`${i}.${a}`))}else{if(Object.hasOwn(t,i)){if(n)continue;let p=t[i];if(p instanceof k){let h=p.value;p.value=s,h!==s&&r.updated.push(i);continue}}let a=new k(s);a._onChange=()=>{pe({updated:[i]})},t[i]=a,r.added.push(i)}}return r}function vt(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof k?e(n,r):vt(r,(i,s)=>{e(`${n}.${i}`,s)})}}function Vn(t,...e){let n={};for(let r of e){let i=r.split("."),s=t,o=n;for(let p=0;p<i.length-1;p++){let h=i[p];if(!s[h])return{};o[h]||(o[h]={}),s=s[h],o=o[h]}let a=i[i.length-1];o[a]=s[a]}return n}var Ne=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let o=0;o<n.length-1;o++){let a=n[o];if(!r[a])return null;r=r[a]}let i=n[n.length-1],s=r[i];if(!s)throw B(gt,"SignalNotFound",{path:e});return s}setSignal(e,n){let r=e.split("."),i=this.#e;for(let o=0;o<r.length-1;o++){let a=r[o];i[a]||(i[a]={}),i=i[a]}let s=r[r.length-1];i[s]=n}setComputed(e,n){let r=pt(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let{signal:r}=this.upsertIfMissing(e,n),i=r.value;r.value=n,i!==n&&pe({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let p=0;p<r.length-1;p++){let h=r[p];i[h]||(i[h]={}),i=i[h]}let s=r[r.length-1],o=i[s];if(o instanceof k)return{signal:o,inserted:!1};let a=new k(n);return a._onChange=()=>{pe({updated:[e]})},i[s]=a,pe({added:[e]}),{signal:a,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let i=r.split("."),s=this.#e;for(let a=0;a<i.length-1;a++){let p=i[a];if(!s[p])return;s=s[p]}let o=i[i.length-1];delete s[o],n.push(r)}pe({removed:n})}merge(e,n=!1){let r=yt(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&pe(r)}subset(...e){return Vn(this.values(),...e)}walk(e){vt(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return ht(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var bt=new Ne,Pe={},je=[],re=new Map,Be=null,Ke="";function St(t){Ke=t}function Ce(...t){for(let e of t){let n={plugin:e,signals:bt,effect:i=>de(i),actions:Pe,removals:re,applyToElement:Ie},r;switch(e.type){case 3:{Pe[e.name]=e;break}case 1:{let i=e;je.push(i),r=i.onGlobalInit;break}case 2:{r=e.onGlobalInit;break}default:throw W("InvalidPluginType",n)}r&&r(n)}je.sort((e,n)=>{let r=n.name.length-e.name.length;return r!==0?r:e.name.localeCompare(n.name)})}function Je(){queueMicrotask(()=>{Ie(document.documentElement),Dn()})}function Ie(t){he(t,e=>{let n=new Array,r=re.get(e.id)||new Map,i=new Map([...r]),s=new Map;for(let o of Object.keys(e.dataset)){if(!o.startsWith(Ke))break;let a=e.dataset[o]||"",p=Re(o,a);s.set(o,p),r.has(p)?i.delete(p):n.push(o)}for(let[o,a]of i)a();for(let o of n){let a=s.get(o);On(e,o,a)}})}function Dn(){Be||(Be=new MutationObserver(t=>{let e=new Set,n=new Set;for(let{target:r,type:i,addedNodes:s,removedNodes:o}of t)switch(i){case"childList":{for(let a of o)e.add(a);for(let a of s)n.add(a)}break;case"attributes":{n.add(r);break}}for(let r of e){let i=re.get(r.id);if(i){for(let[s,o]of i)o(),i.delete(s);i.size===0&&re.delete(r.id)}}for(let r of n)Ie(r)}),Be.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function On(t,e,n){let r=ge(e.slice(Ke.length)),i=je.find(A=>new RegExp(`^${A.name}([A-Z]|_|$)`).test(r));if(!i)return;t.id.length||(t.id=_e(t));let[s,...o]=r.slice(i.name.length).split(/\_\_+/),a=s.length>0;a&&(s=ge(s));let p=t.dataset[e]||"",h=p.length>0,v={signals:bt,applyToElement:Ie,effect:A=>de(A),actions:Pe,removals:re,genRX:()=>Hn(v,...i.argNames||[]),plugin:i,el:t,rawKey:r,key:s,value:p,mods:new Map},C=i.keyReq||0;if(a){if(C===2)throw N(`${i.name}KeyNotAllowed`,v)}else if(C===1)throw N(`${i.name}KeyRequired`,v);let b=i.valReq||0;if(h){if(b===2)throw N(`${i.name}ValueNotAllowed`,v)}else if(b===1)throw N(`${i.name}ValueRequired`,v);if(C===3||b===3){if(a&&h)throw N(`${i.name}KeyAndValueProvided`,v);if(!a&&!h)throw N(`${i.name}KeyOrValueRequired`,v)}for(let A of o){let[_,...T]=A.split(".");v.mods.set(ge(_),new Set(T.map(c=>c.toLowerCase())))}let E=i.onLoad(v)??(()=>{}),y=re.get(t.id);y||(y=new Map,re.set(t.id,y)),y.set(n,E)}function Hn(t,...e){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=t.value.trim().match(r);if(i){let E=i.length-1,y=i[E].trim();y.startsWith("return")||(i[E]=`return (${y});`),n=i.join(`;
`)}let s=new Map,o=new RegExp(`(?:${Te})(.*?)(?:${He})`,"gm");for(let E of n.matchAll(o)){let y=E[1],A=new ae("dsEscaped").with(y).string;s.set(A,y),n=n.replace(Te+y+He,A)}let a=/@(\w*)\(/gm,p=n.matchAll(a),h=new Set;for(let E of p)h.add(E[1]);let v=new RegExp(`@(${Object.keys(Pe).join("|")})\\(`,"gm");n=n.replaceAll(v,"ctx.actions.$1.fn(ctx,");let C=t.signals.paths();if(C.length){let E=new RegExp(`\\$(${C.join("|")})(\\W|$)`,"gm");n=n.replaceAll(E,"ctx.signals.signal('$1').value$2")}for(let[E,y]of s)n=n.replace(E,y);let b=`return (() => {
${n}
})()`;t.fnContent=b;try{let E=new Function("ctx",...e,b);return(...y)=>{try{return E(t,...y)}catch(A){throw N("ExecuteExpression",t,{error:A.message})}}}catch(E){throw N("GenerateExpression",t,{error:E.message})}}Ce(lt,at,st);var ie=`${q}-sse`,ke="started",Le="finished",Tt="error",At="retrying",Et="retries-failed";function K(t,e){document.addEventListener(ie,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function Z(t,e,n){document.dispatchEvent(new CustomEvent(ie,{detail:{type:t,elId:e,argsRaw:n}}))}async function Fn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function qn(t){let e,n,r,i=!1;return function(o){e===void 0?(e=o,n=0,r=-1):e=$n(e,o);let a=e.length,p=0;for(;n<a;){i&&(e[n]===10&&(p=++n),i=!1);let h=-1;for(;n<a&&h===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-p);break;case 13:i=!0;case 10:h=n;break}if(h===-1)break;t(e.subarray(p,h),r),p=n,r=-1}p===a?e=void 0:p!==0&&(e=e.subarray(p),n-=p)}}function Wn(t,e,n){let r=_t(),i=new TextDecoder;return function(o,a){if(o.length===0)n?.(r),r=_t();else if(a>0){let p=i.decode(o.subarray(0,a)),h=a+(o[a+1]===32?2:1),v=i.decode(o.subarray(h));switch(p){case"data":r.data=r.data?`${r.data}
${v}`:v;break;case"event":r.event=v;break;case"id":t(r.id=v);break;case"retry":{let C=Number.parseInt(v,10);Number.isNaN(C)||e(r.retry=C);break}}}}}function $n(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function _t(){return{data:"",event:"",id:"",retry:void 0}}var Gn="text/event-stream",Rt="last-event-id";function wt(t,e,{signal:n,headers:r,onopen:i,onmessage:s,onclose:o,onerror:a,openWhenHidden:p,fetch:h,retryInterval:v=1e3,retryScaler:C=2,retryMaxWaitMs:b=3e4,retryMaxCount:E=10,...y}){return new Promise((A,_)=>{let T=0,c={...r};c.accept||(c.accept=Gn);let d;function u(){d.abort(),document.hidden||S()}p||document.addEventListener("visibilitychange",u);let l=0;function g(){document.removeEventListener("visibilitychange",u),window.clearTimeout(l),d.abort()}n?.addEventListener("abort",()=>{g(),A()});let m=h??window.fetch,f=i??function(){};async function S(){d=new AbortController;try{let x=await m(e,{...y,headers:c,signal:d.signal});await f(x),await Fn(x.body,qn(Wn(R=>{R?c[Rt]=R:delete c[Rt]},R=>{v=R},s))),o?.(),g(),A()}catch(x){if(!d.signal.aborted)try{let R=a?.(x)??v;window.clearTimeout(l),l=window.setTimeout(S,R),v*=C,v=Math.min(v,b),T++,T>E?(Z(t,Et,{}),g(),_("Max retries reached.")):console.error(`Datastar failed to reach ${e.toString()} retrying in ${R}ms.`)}catch(R){g(),_(R)}}}S()})}var xt=t=>`${t}`.includes("text/event-stream"),J=async(t,e,n,r)=>{let{el:i,signals:s}=t,o=i.id,{headers:a,contentType:p,includeLocal:h,selector:v,openWhenHidden:C,retryInterval:b,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:A,abort:_}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:tt,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),T=e.toLowerCase(),c=()=>{};try{if(Z(ke,o,{}),!n?.length)throw N("SseNoUrlProvided",t,{action:T});let d={};d[et]=!0,p==="json"&&(d["Content-Type"]="application/json");let u=Object.assign({},d,a),l={method:e,headers:u,openWhenHidden:C,retryInterval:b,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:A,signal:_,onopen:async f=>{if(f.status>=400){let S=f.status.toString();Z(Tt,o,{status:S})}},onmessage:f=>{if(!f.event.startsWith(q))return;let S=f.event,x={},R=f.data.split(`
`);for(let V of R){let I=V.indexOf(" "),O=V.slice(0,I),D=x[O];D||(D=[],x[O]=D);let $=V.slice(I+1);D.push($)}let M={};for(let[V,I]of Object.entries(x))M[V]=I.join(`
`);Z(S,o,M)},onerror:f=>{if(xt(f))throw N("InvalidContentType",t,{url:n});f&&(console.error(f.message),Z(At,o,{message:f.message}))}},g=new URL(n,window.location.origin),m=new URLSearchParams(g.search);if(p==="json"){let f=s.JSON(!1,!h);e==="GET"?m.set(q,f):l.body=f}else if(p==="form"){let f=v?document.querySelector(v):i.closest("form");if(f===null)throw v?N("SseFormNotFound",t,{action:T,selector:v}):N("SseClosestFormNotFound",t,{action:T});if(i!==f){let x=R=>R.preventDefault();f.addEventListener("submit",x),c=()=>f.removeEventListener("submit",x)}if(!f.checkValidity()){f.reportValidity(),c();return}let S=new FormData(f);if(e==="GET"){let x=new URLSearchParams(S);for(let[R,M]of x)m.set(R,M)}else l.body=S}else throw N("SseInvalidContentType",t,{action:T,contentType:p});g.search=m.toString();try{await wt(i,g.toString(),l)}catch(f){if(!xt(f))throw N("SseFetchFailed",t,{method:e,url:n,error:f})}}finally{Z(Le,o,{}),c()}};var Mt={type:3,name:"delete",fn:async(t,e,n)=>J(t,"DELETE",e,{...n})};var Nt={type:3,name:"get",fn:async(t,e,n)=>J(t,"GET",e,{...n})};var Pt={type:3,name:"patch",fn:async(t,e,n)=>J(t,"PATCH",e,{...n})};var Ct={type:3,name:"post",fn:async(t,e,n)=>J(t,"POST",e,{...n})};var It={type:3,name:"put",fn:async(t,e,n)=>J(t,"PUT",e,{...n})};var kt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let s=e?H(e,n):X(i),{signal:o}=r.upsertIfMissing(s,!1),a=p=>{let{type:h,elId:v}=p.detail;if(v===t.id)switch(h){case ke:o.value=!0;break;case Le:o.value=!1,document.removeEventListener(ie,a);break}};document.addEventListener(ie,a)}};var Lt={type:2,name:F.ExecuteScript,onGlobalInit:async t=>{K(F.ExecuteScript,({autoRemove:e=`${it}`,attributes:n=nt,script:r})=>{let i=Y(e);if(!r?.length)throw W("NoScriptProvided",t);let s=document.createElement("script");for(let o of n.split(`
`)){let a=o.indexOf(" "),p=a?o.slice(0,a):o,h=a?o.slice(a):"";s.setAttribute(p.trim(),h.trim())}s.text=r,document.head.appendChild(s),i&&s.remove()})}};var Se=document,Q=!!Se.startViewTransition;var Vt=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:b=>b.getAttribute("im-preserve")==="true",shouldReAppend:b=>b.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!0};function n(b,E,y={}){b=v(b);let A=C(E),_=h(b,A,y),T=i(_,()=>a(_,b,A,c=>c.morphStyle==="innerHTML"?(s(c,b,A),Array.from(b.childNodes)):r(c,b,A)));return _.pantry.remove(),T}function r(b,E,y){let A=C(E);return s(b,A,y,E,E.nextSibling),Array.from(A.childNodes)}function i(b,E){if(!b.config.restoreFocus)return E();let y=document.activeElement;if(!(y instanceof HTMLInputElement||y instanceof HTMLTextAreaElement))return E();let{id:A,selectionStart:_,selectionEnd:T}=y,c=E();return A&&A!==document.activeElement?.id&&(y=b.target.querySelector(`[id="${A}"]`),y?.focus()),y&&!y.selectionEnd&&T&&y.setSelectionRange(_,T),c}let s=function(){function b(u,l,g,m=null,f=null){l instanceof HTMLTemplateElement&&g instanceof HTMLTemplateElement&&(l=l.content,g=g.content),m||=l.firstChild;for(let S of g.childNodes){if(m&&m!=f){let R=y(u,S,m,f);if(R){R!==m&&_(u,m,R),o(R,S,u),m=R.nextSibling;continue}}if(S instanceof Element&&u.persistentIds.has(S.id)){let R=T(l,S.id,m,u);o(R,S,u),m=R.nextSibling;continue}let x=E(l,S,m,u);x&&(m=x.nextSibling)}for(;m&&m!=f;){let S=m;m=m.nextSibling,A(u,S)}}function E(u,l,g,m){if(m.callbacks.beforeNodeAdded(l)===!1)return null;if(m.idMap.has(l)){let f=document.createElement(l.tagName);return u.insertBefore(f,g),o(f,l,m),m.callbacks.afterNodeAdded(f),f}else{let f=document.importNode(l,!0);return u.insertBefore(f,g),m.callbacks.afterNodeAdded(f),f}}let y=function(){function u(m,f,S,x){let R=null,M=f.nextSibling,V=0,I=S;for(;I&&I!=x;){if(g(I,f)){if(l(m,I,f))return I;R===null&&(m.idMap.has(I)||(R=I))}if(R===null&&M&&g(I,M)&&(V++,M=M.nextSibling,V>=2&&(R=void 0)),I.contains(document.activeElement))break;I=I.nextSibling}return R||null}function l(m,f,S){let x=m.idMap.get(f),R=m.idMap.get(S);if(!R||!x)return!1;for(let M of x)if(R.has(M))return!0;return!1}function g(m,f){let S=m,x=f;return S.nodeType===x.nodeType&&S.tagName===x.tagName&&(!S.id||S.id===x.id)}return u}();function A(u,l){if(u.idMap.has(l))d(u.pantry,l,null);else{if(u.callbacks.beforeNodeRemoved(l)===!1)return;l.parentNode?.removeChild(l),u.callbacks.afterNodeRemoved(l)}}function _(u,l,g){let m=l;for(;m&&m!==g;){let f=m;m=m.nextSibling,A(u,f)}return m}function T(u,l,g,m){let f=m.target.id===l&&m.target||m.target.querySelector(`[id="${l}"]`)||m.pantry.querySelector(`[id="${l}"]`);return c(f,m),d(u,f,g),f}function c(u,l){let g=u.id;for(;u=u.parentNode;){let m=l.idMap.get(u);m&&(m.delete(g),m.size||l.idMap.delete(u))}}function d(u,l,g){if(u.moveBefore)try{u.moveBefore(l,g)}catch{u.insertBefore(l,g)}else u.insertBefore(l,g)}return b}(),o=function(){function b(c,d,u){return u.ignoreActive&&c===document.activeElement?null:(u.callbacks.beforeNodeMorphed(c,d)===!1||(c instanceof HTMLHeadElement&&u.head.ignore||(c instanceof HTMLHeadElement&&u.head.style!=="morph"?p(c,d,u):(E(c,d,u),T(c,u)||s(u,c,d))),u.callbacks.afterNodeMorphed(c,d)),c)}function E(c,d,u){let l=d.nodeType;if(l===1){let g=c,m=d,f=g.attributes,S=m.attributes;for(let x of S)_(x.name,g,"update",u)||g.getAttribute(x.name)!==x.value&&g.setAttribute(x.name,x.value);for(let x=f.length-1;0<=x;x--){let R=f[x];if(R&&!m.hasAttribute(R.name)){if(_(R.name,g,"remove",u))continue;g.removeAttribute(R.name)}}T(g,u)||y(g,m,u)}(l===8||l===3)&&c.nodeValue!==d.nodeValue&&(c.nodeValue=d.nodeValue)}function y(c,d,u){if(c instanceof HTMLInputElement&&d instanceof HTMLInputElement&&d.type!=="file"){let l=d.value,g=c.value;A(c,d,"checked",u),A(c,d,"disabled",u),d.hasAttribute("value")?g!==l&&(_("value",c,"update",u)||(c.setAttribute("value",l),c.value=l)):_("value",c,"remove",u)||(c.value="",c.removeAttribute("value"))}else if(c instanceof HTMLOptionElement&&d instanceof HTMLOptionElement)A(c,d,"selected",u);else if(c instanceof HTMLTextAreaElement&&d instanceof HTMLTextAreaElement){let l=d.value,g=c.value;if(_("value",c,"update",u))return;l!==g&&(c.value=l),c.firstChild&&c.firstChild.nodeValue!==l&&(c.firstChild.nodeValue=l)}}function A(c,d,u,l){let g=d[u],m=c[u];if(g!==m){let f=_(u,c,"update",l);f||(c[u]=d[u]),g?f||c.setAttribute(u,""):_(u,c,"remove",l)||c.removeAttribute(u)}}function _(c,d,u,l){return c==="value"&&l.ignoreActiveValue&&d===document.activeElement?!0:l.callbacks.beforeAttributeUpdated(c,d,u)===!1}function T(c,d){return!!d.ignoreActiveValue&&c===document.activeElement&&c!==document.body}return b}();function a(b,E,y,A){if(b.head.block){let _=E.querySelector("head"),T=y.querySelector("head");if(_&&T){let c=p(_,T,b);return Promise.all(c).then(()=>{let d=Object.assign(b,{head:{block:!1,ignore:!0}});return A(d)})}}return A(b)}function p(b,E,y){let A=[],_=[],T=[],c=[],d=new Map;for(let l of E.children)d.set(l.outerHTML,l);for(let l of b.children){let g=d.has(l.outerHTML),m=y.head.shouldReAppend(l),f=y.head.shouldPreserve(l);g||f?m?_.push(l):(d.delete(l.outerHTML),T.push(l)):y.head.style==="append"?m&&(_.push(l),c.push(l)):y.head.shouldRemove(l)!==!1&&_.push(l)}c.push(...d.values());let u=[];for(let l of c){let g=document.createRange().createContextualFragment(l.outerHTML).firstChild;if(y.callbacks.beforeNodeAdded(g)!==!1){if("href"in g&&g.href||"src"in g&&g.src){let m,f=new Promise(function(S){m=S});g.addEventListener("load",function(){m()}),u.push(f)}b.appendChild(g),y.callbacks.afterNodeAdded(g),A.push(g)}}for(let l of _)y.callbacks.beforeNodeRemoved(l)!==!1&&(b.removeChild(l),y.callbacks.afterNodeRemoved(l));return y.head.afterHeadMorphed(b,{added:A,kept:T,removed:_}),u}let h=function(){function b(d,u,l){let{persistentIds:g,idMap:m}=T(d,u),f=E(l),S=f.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(S))throw`Do not understand how to morph style ${S}`;return{target:d,newContent:u,config:f,morphStyle:S,ignoreActive:f.ignoreActive,ignoreActiveValue:f.ignoreActiveValue,restoreFocus:f.restoreFocus,idMap:m,persistentIds:g,pantry:y(),callbacks:f.callbacks,head:f.head}}function E(d){let u=Object.assign({},e);return Object.assign(u,d),u.callbacks=Object.assign({},e.callbacks,d.callbacks),u.head=Object.assign({},e.head,d.head),u}function y(){let d=document.createElement("div");return d.hidden=!0,document.body.insertAdjacentElement("afterend",d),d}function A(d){let u=Array.from(d.querySelectorAll("[id]"));return d.id&&u.push(d),u}function _(d,u,l,g){for(let m of g)if(u.has(m.id)){let f=m;for(;f;){let S=d.get(f);if(S==null&&(S=new Set,d.set(f,S)),S.add(m.id),f===l)break;f=f.parentElement}}}function T(d,u){let l=A(d),g=A(u),m=c(l,g),f=new Map;_(f,m,d,l);let S=u.__idiomorphRoot||u;return _(f,m,S,g),{persistentIds:m,idMap:f}}function c(d,u){let l=new Set,g=new Map;for(let{id:f,tagName:S}of d)g.has(f)?l.add(f):g.set(f,S);let m=new Set;for(let{id:f,tagName:S}of u)m.has(f)?l.add(f):g.get(f)===S&&m.add(f);for(let f of l)m.delete(f);return m}return b}(),{normalizeElement:v,normalizeParent:C}=function(){let b=new WeakSet;function E(T){return T instanceof Document?T.documentElement:T}function y(T){if(T==null)return document.createElement("div");if(typeof T=="string")return y(_(T));if(b.has(T))return T;if(T instanceof Node){if(T.parentNode)return new A(T);{let c=document.createElement("div");return c.append(T),c}}else{let c=document.createElement("div");for(let d of[...T])c.append(d);return c}}class A{constructor(c){this.originalNode=c,this.realParentNode=c.parentNode,this.previousSibling=c.previousSibling,this.nextSibling=c.nextSibling}get childNodes(){let c=[],d=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;d&&d!=this.nextSibling;)c.push(d),d=d.nextSibling;return c}querySelectorAll(c){return this.childNodes.reduce((d,u)=>{if(u instanceof Element){u.matches(c)&&d.push(u);let l=u.querySelectorAll(c);for(let g=0;g<l.length;g++)d.push(l[g])}return d},[])}insertBefore(c,d){return this.realParentNode.insertBefore(c,d)}moveBefore(c,d){return this.realParentNode.moveBefore(c,d)}get __idiomorphRoot(){return this.originalNode}}function _(T){let c=new DOMParser,d=T.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(d.match(/<\/html>/)||d.match(/<\/head>/)||d.match(/<\/body>/)){let u=c.parseFromString(T,"text/html");if(d.match(/<\/html>/))return b.add(u),u;{let l=u.firstChild;return l&&b.add(l),l}}else{let l=c.parseFromString("<body><template>"+T+"</template></body>","text/html").body.querySelector("template").content;return b.add(l),l}}return{normalizeElement:E,normalizeParent:y}}();return{morph:n,defaults:e}}();var Ot={type:2,name:F.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");K(F.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=ot,useViewTransition:s=`${Ae}`})=>{let o=Y(s);e.innerHTML=n.trim();let a=[...e.content.children];for(let p of a){if(!(p instanceof Element))throw W("NoFragmentsFound",t);let h=r||`#${p.getAttribute("id")}`,v=[...document.querySelectorAll(h)||[]];if(!v.length)throw W("NoTargetsFound",t,{selectorOrID:h});o&&Q?Se.startViewTransition(()=>Dt(t,i,p,v)):Dt(t,i,p,v)}})}};function Dt(t,e,n,r){for(let i of r){i.dataset.fragmentMergeTarget="true";let s=n.cloneNode(!0);switch(e){case U.Morph:{he(s,o=>{!o.id?.length&&Object.keys(o.dataset).length&&(o.id=_e(o));let a=t.removals.get(o.id);if(a){let p=new Map;for(let[h,v]of a){let C=Re(h,h);p.set(C,v),a.delete(h)}t.removals.set(o.id,p)}}),Vt.morph(i,s);break}case U.Inner:i.innerHTML=s.outerHTML;break;case U.Outer:i.replaceWith(s);break;case U.Prepend:i.prepend(s);break;case U.Append:i.append(s);break;case U.Before:i.before(s);break;case U.After:i.after(s);break;case U.UpsertAttributes:for(let o of s.getAttributeNames()){let a=s.getAttribute(o);i.setAttribute(o,a)}break;default:throw W("InvalidMergeMode",t,{mergeMode:e})}}}var Ht={type:2,name:F.MergeSignals,onGlobalInit:async t=>{K(F.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${rt}`})=>{let{signals:r}=t,i=Y(n);r.merge(Ee(e),i)})}};var Ft={type:2,name:F.RemoveFragments,onGlobalInit:async t=>{K(F.RemoveFragments,({selector:e,useViewTransition:n=`${Ae}`})=>{if(!e.length)throw W("NoSelectorProvided",t);let r=Y(n),i=document.querySelectorAll(e),s=()=>{for(let o of i)o.remove()};r&&Q?Se.startViewTransition(()=>s()):s()})}};var qt={type:2,name:F.RemoveSignals,onGlobalInit:async t=>{K(F.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw W("NoPathsProvided",t);t.signals.remove(...n)})}};var Wt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw N("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var $t={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw N("CustomValidityInvalidElement",t);let i=n();return r(()=>{let s=i();if(typeof s!="string")throw N("CustomValidityInvalidExpression",t,{result:s});e.setCustomValidity(s)})}};function oe(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function se(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function Un(t,e,n=!1,r=!0){let i=-1,s=()=>i&&clearTimeout(i);return(...o)=>{s(),n&&!i&&t(...o),i=setTimeout(()=>{r&&t(...o),s()},e)}}function Bn(t,e,n=!0,r=!1){let i=!1;return(...s)=>{i||(n&&t(...s),i=!0,setTimeout(()=>{i=!1,r&&t(...s)},e))}}function ee(t,e){let n=e.get("debounce");if(n){let i=oe(n),s=se(n,"leading",!1),o=!se(n,"notrail",!1);t=Un(t,i,s,o)}let r=e.get("throttle");if(r){let i=oe(r),s=!se(r,"noleading",!1),o=se(r,"trail",!1);t=Bn(t,i,s,o)}return t}var Gt="once",Ut="half",Bt="full",jt={type:1,name:"onIntersect",keyReq:2,mods:new Set([Gt,Ut,Bt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i=ee(r(),n),s={threshold:0};n.has(Bt)?s.threshold=1:n.has(Ut)&&(s.threshold=.5);let o=new IntersectionObserver(a=>{for(let p of a)p.isIntersecting&&(i(),n.has(Gt)&&(o.disconnect(),delete t.dataset[e]))},s);return o.observe(t),()=>o.disconnect()}};var Kt={type:1,name:"onInterval",keyReq:2,valReq:1,onLoad:({mods:t,genRX:e})=>{let n=e(),r=1e3,i=t.get("duration");i&&(r=oe(i),se(i,"leading",!1)&&n());let s=setInterval(n,r);return()=>{clearInterval(s)}}};var Jt={type:1,name:"onLoad",keyReq:2,valReq:1,onLoad:({mods:t,genRX:e})=>{let n=e(),r=0,i=t.get("delay");return i&&(r=oe(i)),setTimeout(n,r),()=>{}}};var zt={type:1,name:"onRaf",keyReq:2,valReq:1,onLoad:({mods:t,genRX:e})=>{let n=ee(e(),t),r,i=()=>{n(),r=requestAnimationFrame(i)};return r=requestAnimationFrame(i),()=>{r&&cancelAnimationFrame(r)}}};var Yt={type:1,name:"onSignalChange",valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{let i=ee(r(),e);if(t===""){let a=p=>i(p);return document.addEventListener(me,a),()=>{document.removeEventListener(me,a)}}let s=H(t,e),o=new Map;return n.walk((a,p)=>{a.startsWith(s)&&o.set(p,p.value)}),de(()=>{for(let[a,p]of o)p!==a.value&&(i(),o.set(a,a.value))})}};var Xt="session",Zt={type:1,name:"persist",keyReq:2,mods:new Set([Xt]),onLoad:({effect:t,mods:e,signals:n,value:r})=>{let i=q,s=e.has(Xt)?sessionStorage:localStorage,o=r.split(/\s+/).filter(h=>h!=="");o=o.map(h=>X(h));let a=()=>{let h=s.getItem(i)||"{}",v=JSON.parse(h);n.merge(v)},p=()=>{let h;o.length?h=n.subset(...o):h=n.values(),s.setItem(i,JSON.stringify(h))};return a(),t(()=>{p()})}};var Qt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,s=new URL(r,i).toString();window.history.replaceState({},"",s)})}};var Ve="smooth",ze="instant",Ye="auto",en="hstart",tn="hcenter",nn="hend",rn="hnearest",on="vstart",sn="vcenter",an="vend",ln="vnearest",jn="focus",De="center",un="start",cn="end",fn="nearest",dn={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ve,ze,Ye,en,tn,nn,rn,on,sn,an,ln,jn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:Ve,block:De,inline:De};if(n.has(Ve)&&(i.behavior=Ve),n.has(ze)&&(i.behavior=ze),n.has(Ye)&&(i.behavior=Ye),n.has(en)&&(i.inline=un),n.has(tn)&&(i.inline=De),n.has(nn)&&(i.inline=cn),n.has(rn)&&(i.inline=fn),n.has(on)&&(i.block=un),n.has(sn)&&(i.block=De),n.has(an)&&(i.block=cn),n.has(ln)&&(i.block=fn),!(e instanceof HTMLElement||e instanceof SVGElement))throw N("ScrollIntoViewInvalidElement",t);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r]}};var pn="view-transition",mn={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===pn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=pn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!Q){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let s=e.style;s.viewTransitionName=i})}};var gn={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let i=r();return e===""?n(async()=>{let s=i();for(let[o,a]of Object.entries(s))a===!1?t.removeAttribute(o):t.setAttribute(o,a)}):(e=z(e),n(async()=>{let s=!1;try{s=i()}catch{}let o;typeof s=="string"?o=s:o=JSON.stringify(s),!o||o==="false"||o==="null"||o==="undefined"?t.removeAttribute(e):t.setAttribute(e,o)}))}};var Kn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,hn=["change","input","keydown"],yn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:i,value:s,effect:o}=t,a=e,p=n?H(n,r):X(s),h=e.tagName.toLowerCase(),v=h.includes("input"),C=h.includes("select"),b=e.getAttribute("type"),E=e.hasAttribute("value"),y="",A=v&&b==="checkbox";A&&(y=E?"":!1);let _=v&&b==="number";_&&(y=0);let T=v&&b==="radio";T&&(e.getAttribute("name")?.length||e.setAttribute("name",p));let c=v&&b==="file",{signal:d,inserted:u}=i.upsertIfMissing(p,y),l=-1;Array.isArray(d.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",p),l=[...document.querySelectorAll(`[name="${p}"]`)].findIndex(M=>M===t.el));let g=l>=0,m=()=>[...i.value(p)],f=()=>{let M=i.value(p);g&&!C&&(M=M[l]||y);let V=`${M}`;if(A||T)typeof M=="boolean"?a.checked=M:a.checked=V===a.value;else if(C){let I=e;if(I.multiple){if(!g)throw N("BindSelectMultiple",t);for(let O of I.options){if(O?.disabled)return;let D=_?Number(O.value):O.value;O.selected=M.includes(D)}}else I.value=V}else c||("value"in e?e.value=V:e.setAttribute("value",V))},S=async()=>{let M=i.value(p);if(g){let D=M;for(;l>=D.length;)D.push(y);M=D[l]||y}let V=(D,$)=>{let G=$;g&&!C&&(G=m(),G[l]=$),i.setValue(D,G)};if(c){let D=[...a?.files||[]],$=[],G=[],Xe=[];await Promise.all(D.map(Ze=>new Promise(Mn=>{let te=new FileReader;te.onload=()=>{if(typeof te.result!="string")throw N("InvalidFileResultType",t,{resultType:typeof te.result});let Oe=te.result.match(Kn);if(!Oe?.groups)throw N("InvalidDataUri",t,{result:te.result});$.push(Oe.groups.contents),G.push(Oe.groups.mime),Xe.push(Ze.name)},te.onloadend=()=>Mn(void 0),te.readAsDataURL(Ze)}))),V(p,$),V(`${p}Mimes`,G),V(`${p}Names`,Xe);return}let I=a.value||"",O;if(A){let D=a.checked||a.getAttribute("checked")==="true";E?O=D?I:"":O=D}else if(C){let $=[...e.selectedOptions];g?O=$.filter(G=>G.selected).map(G=>G.value):O=$[0]?.value||y}else typeof M=="boolean"?O=!!I:typeof M=="number"?O=Number(I):O=I||"";V(p,O)};u&&S();for(let M of hn)e.addEventListener(M,S);let x=M=>{M.persisted&&S()};window.addEventListener("pageshow",x);let R=o(()=>f());return()=>{R();for(let M of hn)e.removeEventListener(M,S);window.removeEventListener("pageshow",x)}}};var vn={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:i})=>{let s=t.classList,o=i();return r(()=>{if(e===""){let a=o();for(let[p,h]of Object.entries(a)){let v=p.split(/\s+/);h?s.add(...v):s.remove(...v)}}else{let a=z(e);a=H(a,n),o()?s.add(a):s.remove(a)}})}};var bn={type:1,name:"on",keyReq:1,valReq:1,argNames:["evt"],onLoad:({el:t,key:e,mods:n,genRX:r})=>{let i=r(),s=t;n.has("window")&&(s=window);let o=v=>{v&&((n.has("prevent")||e==="submit")&&v.preventDefault(),n.has("stop")&&v.stopPropagation()),i(v)};if(o=ee(o,n),n.has("viewtransition")&&Q){let v=o;o=(...C)=>document.startViewTransition(()=>v(...C))}let a={capture:!1,passive:!1,once:!1};if(n.has("capture")&&(a.capture=!0),n.has("passive")&&(a.passive=!0),n.has("once")&&(a.once=!0),n.has("outside")){s=document;let v=o;o=b=>{let E=b?.target;t.contains(E)||v(b)}}let h=z(e);return h=H(h,n),h===ie&&(s=document),s.addEventListener(h,o,a),()=>{s.removeEventListener(h,o)}}};var Sn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let s=e?H(e,n):X(i);r.setValue(s,t)}};var Tn="none",An="display",En={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===Tn&&t.removeProperty(An):t.setProperty(An,Tn)})}};var _n={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,i=r();return e instanceof HTMLElement||N("TextInvalidElement",t),n(()=>{let s=i(t);e.textContent=`${s}`})}};var{round:Jn,max:zn,min:Yn}=Math,Rn={type:3,name:"fit",fn:(t,e,n,r,i,s,o=!1,a=!1)=>{let p=(e-n)/(r-n)*(s-i)+i;return a&&(p=Jn(p)),o&&(p=zn(i,Yn(s,p))),p}};var wn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var xn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Ce(gn,yn,vn,bn,Sn,En,_n,kt,Nt,Ct,It,Pt,Mt,Ot,Ht,Ft,qt,Lt,Wt,$t,jt,Kt,Jt,zt,Yt,Zt,Qt,dn,mn,Rn,wn,xn);Je();export{Je as apply,Ce as load,St as setAlias};
//# sourceMappingURL=datastar.js.map
