// Datastar v1.0.0-beta.10
var Qe=/🖕JS_DS🚀/.source,Te=Qe.slice(0,5),He=Qe.slice(4),q="datastar",et="Datastar-Request",tt=1e3,nt="type module",Ae=!1,rt=!1,it=!0,U={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},ot=U.Morph,F={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var x=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(x||{});var de=`${q}-signals`;var Y=t=>t.trim()==="true",z=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),pe=t=>z(t).replace(/-./g,e=>e[1].toUpperCase()),Fe=t=>z(t).replace(/-/g,"_"),Mn=t=>pe(t).replace(/^./,e=>e[0].toUpperCase()),Ee=t=>new Function(`return Object.assign({}, ${t})`)(),X=t=>t.startsWith("$")?t.slice(1):t,Nn={kebab:z,snake:Fe,pascal:Mn};function H(t,e){for(let n of e.get("case")||[]){let r=Nn[n];r&&(t=r(t))}return t}var Pn="computed",st={type:1,name:Pn,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=H(t,e);let i=r();n.setComputed(t,i)}};var at={type:1,name:"signals",onLoad:t=>{let{key:e,mods:n,signals:r,value:i,genRX:o}=t,s=n.has("ifmissing");if(e!==""){let a=H(e,n),d=i===""?i:o()();s?r.upsertIfMissing(a,d):r.setValue(a,d)}else{let a=Ee(t.value);t.value=JSON.stringify(a);let h=o()();r.merge(h,s)}}};var lt={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ie=class{#e=0;#t;constructor(e=q){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function _e(t){if(t.id)return t.id;let e=new ie,n=t;for(;n;){if(e.with(n.tagName||""),n.id){e.with(n.id);break}let r=n?.parentNode;r&&e.with([...r.children].indexOf(n)),n=r}return e.string}function Re(t,e){return new ie().with(t).with(e).value}function me(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)me(r,e),r=r.nextElementSibling}var Cn="https://data-star.dev/errors";function qe(t,e,n={}){let r=new Error;r.name=`${q} ${t} error`;let i=Fe(e),o=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),s=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${Cn}/${t}/${i}?${o}
Context: ${s}`,r}function B(t,e,n={}){return qe("internal",e,Object.assign({from:t},n))}function W(t,e,n={}){let r={plugin:{name:e.plugin.name,type:x[e.plugin.type]}};return qe("init",t,Object.assign(r,n))}function N(t,e,n={}){let r={plugin:{name:e.plugin.name,type:x[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return qe("runtime",t,Object.assign(r,n))}var oe="preact-signals",kn=Symbol.for("preact-signals"),j=1,se=2,he=4,le=8,xe=16,ae=32;function $e(){we++}function Ge(){if(we>1){we--;return}let t,e=!1;for(;ge!==void 0;){let n=ge;for(ge=void 0,We++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~se,!(n._flags&le)&&ct(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(We=0,we--,e)throw t}var P;var ge,we=0,We=0,Me=0;function ut(t){if(P===void 0)return;let e=t._node;if(e===void 0||e._target!==P)return e={_version:0,_source:t,_prevSource:P._sources,_nextSource:void 0,_target:P,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},P._sources!==void 0&&(P._sources._nextSource=e),P._sources=e,t._node=e,P._flags&ae&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=P._sources,e._nextSource=void 0,P._sources._nextSource=e,P._sources=e),e}function I(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}I.prototype.brand=kn;I.prototype._refresh=()=>!0;I.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};I.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};I.prototype.subscribe=function(t){return ue(()=>{let e=this.value,n=P;P=void 0;try{t(e)}finally{P=n}})};I.prototype.valueOf=function(){return this.value};I.prototype.toString=function(){return`${this.value}`};I.prototype.toJSON=function(){return this.value};I.prototype.peek=function(){let t=P;P=void 0;try{return this.value}finally{P=t}};Object.defineProperty(I.prototype,"value",{get(){let t=ut(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(We>100)throw B(oe,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Me++,$e();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{Ge()}this?._onChange({old:e,revised:n})}}});function ct(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function ft(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function dt(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function te(t){I.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Me-1,this._flags=he}te.prototype=new I;te.prototype._refresh=function(){if(this._flags&=~se,this._flags&j)return!1;if((this._flags&(he|ae))===ae||(this._flags&=~he,this._globalVersion===Me))return!0;if(this._globalVersion=Me,this._flags|=j,this._version>0&&!ct(this))return this._flags&=~j,!0;let t=P;try{ft(this),P=this;let e=this._fn();(this._flags&xe||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~xe,this._version++)}catch(e){this._value=e,this._flags|=xe,this._version++}return P=t,dt(this),this._flags&=~j,!0};te.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=he|ae;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}I.prototype._subscribe.call(this,t)};te.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(I.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~ae;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};te.prototype._notify=function(){if(!(this._flags&se)){this._flags|=he|se;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(te.prototype,"value",{get(){if(this._flags&j)throw B(oe,"SignalCycleDetected");let t=ut(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&xe)throw B(oe,"GetComputedError",{value:this._value});return this._value}});function pt(t){return new te(t)}function mt(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){$e();let n=P;P=void 0;try{e()}catch(r){throw t._flags&=~j,t._flags|=le,Ue(t),B(oe,"CleanupEffectError",{error:r})}finally{P=n,Ge()}}}function Ue(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,mt(t)}function In(t){if(P!==this)throw B(oe,"EndEffectError");dt(this),P=t,this._flags&=~j,this._flags&le&&Ue(this),Ge()}function ye(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=ae}ye.prototype._callback=function(){let t=this._start();try{if(this._flags&le||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};ye.prototype._start=function(){if(this._flags&j)throw B(oe,"SignalCycleDetected");this._flags|=j,this._flags&=~le,mt(this),ft(this),$e();let t=P;return P=this,In.bind(this,t)};ye.prototype._notify=function(){this._flags&se||(this._flags|=se,this._nextBatchedEffect=ge,ge=this)};ye.prototype._dispose=function(){this._flags|=le,this._flags&j||Ue(this)};function ue(t){let e=new ye(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var gt="namespacedSignals",ce=t=>{document.dispatchEvent(new CustomEvent(de,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function ht(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof I?n[r]=i.value:n[r]=ht(i)}return n}function yt(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw B(gt,"InvalidSignalKey",{key:i});let o=e[i];if(o instanceof Object&&!Array.isArray(o)){t[i]||(t[i]={});let s=yt(t[i],o,n);r.added.push(...s.added.map(a=>`${i}.${a}`)),r.removed.push(...s.removed.map(a=>`${i}.${a}`)),r.updated.push(...s.updated.map(a=>`${i}.${a}`))}else{if(Object.hasOwn(t,i)){if(n)continue;let d=t[i];if(d instanceof I){let h=d.value;d.value=o,h!==o&&r.updated.push(i);continue}}let a=new I(o);a._onChange=()=>{ce({updated:[i]})},t[i]=a,r.added.push(i)}}return r}function vt(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof I?e(n,r):vt(r,(i,o)=>{e(`${n}.${i}`,o)})}}function Ln(t,...e){let n={};for(let r of e){let i=r.split("."),o=t,s=n;for(let d=0;d<i.length-1;d++){let h=i[d];if(!o[h])return{};s[h]||(s[h]={}),o=o[h],s=s[h]}let a=i[i.length-1];s[a]=o[a]}return n}var Ne=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let s=0;s<n.length-1;s++){let a=n[s];if(!r[a])return null;r=r[a]}let i=n[n.length-1],o=r[i];if(!o)throw B(gt,"SignalNotFound",{path:e});return o}setSignal(e,n){let r=e.split("."),i=this.#e;for(let s=0;s<r.length-1;s++){let a=r[s];i[a]||(i[a]={}),i=i[a]}let o=r[r.length-1];i[o]=n}setComputed(e,n){let r=pt(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let{signal:r}=this.upsertIfMissing(e,n),i=r.value;r.value=n,i!==n&&ce({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let d=0;d<r.length-1;d++){let h=r[d];i[h]||(i[h]={}),i=i[h]}let o=r[r.length-1],s=i[o];if(s instanceof I)return{signal:s,inserted:!1};let a=new I(n);return a._onChange=()=>{ce({updated:[e]})},i[o]=a,ce({added:[e]}),{signal:a,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let i=r.split("."),o=this.#e;for(let a=0;a<i.length-1;a++){let d=i[a];if(!o[d])return;o=o[d]}let s=i[i.length-1];delete o[s],n.push(r)}ce({removed:n})}merge(e,n=!1){let r=yt(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&ce(r)}subset(...e){return Ln(this.values(),...e)}walk(e){vt(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return ht(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var bt=new Ne,Pe={},je=[],ne=new Map,Be=null,Ke="";function St(t){Ke=t}function Ce(...t){for(let e of t){let n={plugin:e,signals:bt,effect:i=>ue(i),actions:Pe,removals:ne,applyToElement:ke},r;switch(e.type){case 3:{Pe[e.name]=e;break}case 1:{let i=e;je.push(i),r=i.onGlobalInit;break}case 2:{r=e.onGlobalInit;break}default:throw W("InvalidPluginType",n)}r&&r(n)}je.sort((e,n)=>{let r=n.name.length-e.name.length;return r!==0?r:e.name.localeCompare(n.name)})}function Je(){queueMicrotask(()=>{ke(document.documentElement),Vn()})}function ke(t){me(t,e=>{let n=new Array,r=ne.get(e.id)||new Map,i=new Map([...r]),o=new Map;for(let s of Object.keys(e.dataset)){if(!s.startsWith(Ke))break;let a=e.dataset[s]||"",d=Re(s,a);o.set(s,d),r.has(d)?i.delete(d):n.push(s)}for(let[s,a]of i)a();for(let s of n){let a=o.get(s);On(e,s,a)}})}function Vn(){Be||(Be=new MutationObserver(t=>{let e=new Set,n=new Set;for(let{target:r,type:i,addedNodes:o,removedNodes:s}of t)switch(i){case"childList":{for(let a of s)e.add(a);for(let a of o)n.add(a)}break;case"attributes":{n.add(r);break}}for(let r of e){let i=ne.get(r.id);if(i){for(let[o,s]of i)s(),i.delete(o);i.size===0&&ne.delete(r.id)}}for(let r of n)ke(r)}),Be.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function On(t,e,n){let r=pe(e.slice(Ke.length)),i=je.find(A=>new RegExp(`^${A.name}([A-Z]|_|$)`).test(r));if(!i)return;t.id.length||(t.id=_e(t));let[o,...s]=r.slice(i.name.length).split(/\_\_+/),a=o.length>0;a&&(o=pe(o));let d=t.dataset[e]||"",h=d.length>0,S={signals:bt,applyToElement:ke,effect:A=>ue(A),actions:Pe,removals:ne,genRX:()=>Dn(S,...i.argNames||[]),plugin:i,el:t,rawKey:r,key:o,value:d,mods:new Map},C=i.keyReq||0;if(a){if(C===2)throw N(`${i.name}KeyNotAllowed`,S)}else if(C===1)throw N(`${i.name}KeyRequired`,S);let v=i.valReq||0;if(h){if(v===2)throw N(`${i.name}ValueNotAllowed`,S)}else if(v===1)throw N(`${i.name}ValueRequired`,S);if(C===3||v===3){if(a&&h)throw N(`${i.name}KeyAndValueProvided`,S);if(!a&&!h)throw N(`${i.name}KeyOrValueRequired`,S)}for(let A of s){let[R,...T]=A.split(".");S.mods.set(pe(R),new Set(T.map(c=>c.toLowerCase())))}let E=i.onLoad(S)??(()=>{}),y=ne.get(t.id);y||(y=new Map,ne.set(t.id,y)),y.set(n,E)}function Dn(t,...e){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=t.value.trim().match(r);if(i){let E=i.length-1,y=i[E].trim();y.startsWith("return")||(i[E]=`return (${y});`),n=i.join(`;
`)}let o=new Map,s=new RegExp(`(?:${Te})(.*?)(?:${He})`,"gm");for(let E of n.matchAll(s)){let y=E[1],A=new ie("dsEscaped").with(y).string;o.set(A,y),n=n.replace(Te+y+He,A)}let a=/@(\w*)\(/gm,d=n.matchAll(a),h=new Set;for(let E of d)h.add(E[1]);let S=new RegExp(`@(${Object.keys(Pe).join("|")})\\(`,"gm");n=n.replaceAll(S,"ctx.actions.$1.fn(ctx,");let C=t.signals.paths();if(C.length){let E=new RegExp(`\\$(${C.join("|")})(\\W|$)`,"gm");n=n.replaceAll(E,"ctx.signals.signal('$1').value$2")}for(let[E,y]of o)n=n.replace(E,y);let v=`return (() => {
${n}
})()`;t.fnContent=v;try{let E=new Function("ctx",...e,v);return(...y)=>{try{return E(t,...y)}catch(A){throw N("ExecuteExpression",t,{error:A.message})}}}catch(E){throw N("GenerateExpression",t,{error:E.message})}}Ce(lt,at,st);async function Hn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function Fn(t){let e,n,r,i=!1;return function(s){e===void 0?(e=s,n=0,r=-1):e=Wn(e,s);let a=e.length,d=0;for(;n<a;){i&&(e[n]===10&&(d=++n),i=!1);let h=-1;for(;n<a&&h===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-d);break;case 13:i=!0;case 10:h=n;break}if(h===-1)break;t(e.subarray(d,h),r),d=n,r=-1}d===a?e=void 0:d!==0&&(e=e.subarray(d),n-=d)}}function qn(t,e,n){let r=Tt(),i=new TextDecoder;return function(s,a){if(s.length===0)n?.(r),r=Tt();else if(a>0){let d=i.decode(s.subarray(0,a)),h=a+(s[a+1]===32?2:1),S=i.decode(s.subarray(h));switch(d){case"data":r.data=r.data?`${r.data}
${S}`:S;break;case"event":r.event=S;break;case"id":t(r.id=S);break;case"retry":{let C=Number.parseInt(S,10);Number.isNaN(C)||e(r.retry=C);break}}}}}function Wn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function Tt(){return{data:"",event:"",id:"",retry:void 0}}var $n="text/event-stream",At="last-event-id";function Et(t,{signal:e,headers:n,onopen:r,onmessage:i,onclose:o,onerror:s,openWhenHidden:a,fetch:d,retryInterval:h=1e3,retryScaler:S=2,retryMaxWaitMs:C=3e4,retryMaxCount:v=10,...E}){return new Promise((y,A)=>{let R=0,T={...n};T.accept||(T.accept=$n);let c;function p(){c.abort(),document.hidden||f()}a||document.addEventListener("visibilitychange",p);let u=0;function l(){document.removeEventListener("visibilitychange",p),window.clearTimeout(u),c.abort()}e?.addEventListener("abort",()=>{l(),y()});let g=d??window.fetch,m=r??function(){};async function f(){c=new AbortController;try{let b=await g(t,{...E,headers:T,signal:c.signal});await m(b),await Hn(b.body,Fn(qn(_=>{_?T[At]=_:delete T[At]},_=>{h=_},i))),o?.(),l(),y()}catch(b){if(!c.signal.aborted)try{let _=s?.(b)??h;window.clearTimeout(u),u=window.setTimeout(f,_),h*=S,h=Math.min(h,C),R++,R>v?(l(),A("Max retries reached.")):console.error(`Datastar failed to reach ${t.toString()} retrying in ${_}ms.`)}catch(_){l(),A(_)}}}f()})}var fe=`${q}-sse`,Ie="started",Le="finished",_t="error",Rt="retrying";function K(t,e){document.addEventListener(fe,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function ve(t,e,n){t.dispatchEvent(new CustomEvent(fe,{detail:{type:e,argsRaw:n},bubbles:!0}))}var xt=t=>`${t}`.includes("text/event-stream"),J=async(t,e,n,r)=>{let{el:{id:i},el:o,signals:s}=t,{headers:a,contentType:d,includeLocal:h,selector:S,openWhenHidden:C,retryInterval:v,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:A,abort:R}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:tt,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),T=e.toLowerCase(),c=()=>{};try{if(ve(o,Ie,{elId:i}),!n?.length)throw N("SseNoUrlProvided",t,{action:T});let p={};p[et]=!0,d==="json"&&(p["Content-Type"]="application/json");let u=Object.assign({},p,a),l={method:e,headers:u,openWhenHidden:C,retryInterval:v,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:A,signal:R,onopen:async f=>{if(f.status>=400){let b=f.status.toString();ve(o,_t,{status:b})}},onmessage:f=>{if(!f.event.startsWith(q))return;let b=f.event,_={},M=f.data.split(`
`);for(let V of M){let k=V.indexOf(" "),D=V.slice(0,k),O=_[D];O||(O=[],_[D]=O);let $=V.slice(k+1);O.push($)}let w={};for(let[V,k]of Object.entries(_))w[V]=k.join(`
`);ve(o,b,w)},onerror:f=>{if(xt(f))throw N("InvalidContentType",t,{url:n});f&&(console.error(f.message),ve(o,Rt,{message:f.message}))}},g=new URL(n,window.location.origin),m=new URLSearchParams(g.search);if(d==="json"){let f=s.JSON(!1,!h);e==="GET"?m.set(q,f):l.body=f}else if(d==="form"){let f=S?document.querySelector(S):o.closest("form");if(f===null)throw S?N("SseFormNotFound",t,{action:T,selector:S}):N("SseClosestFormNotFound",t,{action:T});if(o!==f){let _=M=>M.preventDefault();f.addEventListener("submit",_),c=()=>f.removeEventListener("submit",_)}if(!f.checkValidity()){f.reportValidity(),c();return}let b=new FormData(f);if(e==="GET"){let _=new URLSearchParams(b);for(let[M,w]of _)m.set(M,w)}else l.body=b}else throw N("SseInvalidContentType",t,{action:T,contentType:d});g.search=m.toString();try{await Et(g.toString(),l)}catch(f){if(!xt(f))throw N("SseFetchFailed",t,{method:e,url:n,error:f})}}finally{ve(o,Le,{elId:i}),c()}};var wt={type:3,name:"delete",fn:async(t,e,n)=>J(t,"DELETE",e,{...n})};var Mt={type:3,name:"get",fn:async(t,e,n)=>J(t,"GET",e,{...n})};var Nt={type:3,name:"patch",fn:async(t,e,n)=>J(t,"PATCH",e,{...n})};var Pt={type:3,name:"post",fn:async(t,e,n)=>J(t,"POST",e,{...n})};var Ct={type:3,name:"put",fn:async(t,e,n)=>J(t,"PUT",e,{...n})};var kt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?H(e,n):X(i),{signal:s}=r.upsertIfMissing(o,!1),a=d=>{let{type:h,argsRaw:{elId:S}}=d.detail;if(S===t.id)switch(h){case Ie:s.value=!0;break;case Le:s.value=!1;break}};return t.addEventListener(fe,a),()=>{setTimeout(()=>t.removeEventListener(fe,a))}}};var It={type:2,name:F.ExecuteScript,onGlobalInit:async t=>{K(F.ExecuteScript,({autoRemove:e=`${it}`,attributes:n=nt,script:r})=>{let i=Y(e);if(!r?.length)throw W("NoScriptProvided",t);let o=document.createElement("script");for(let s of n.split(`
`)){let a=s.indexOf(" "),d=a?s.slice(0,a):s,h=a?s.slice(a):"";o.setAttribute(d.trim(),h.trim())}o.text=r,document.head.appendChild(o),i&&o.remove()})}};var be=document,Z=!!be.startViewTransition;var Lt=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:v=>v.getAttribute("im-preserve")==="true",shouldReAppend:v=>v.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!0};function n(v,E,y={}){v=S(v);let A=C(E),R=h(v,A,y),T=i(R,()=>a(R,v,A,c=>c.morphStyle==="innerHTML"?(o(c,v,A),Array.from(v.childNodes)):r(c,v,A)));return R.pantry.remove(),T}function r(v,E,y){let A=C(E);return o(v,A,y,E,E.nextSibling),Array.from(A.childNodes)}function i(v,E){if(!v.config.restoreFocus)return E();let y=document.activeElement;if(!(y instanceof HTMLInputElement||y instanceof HTMLTextAreaElement))return E();let{id:A,selectionStart:R,selectionEnd:T}=y,c=E();return A&&A!==document.activeElement?.id&&(y=v.target.querySelector(`[id="${A}"]`),y?.focus()),y&&!y.selectionEnd&&T&&y.setSelectionRange(R,T),c}let o=function(){function v(u,l,g,m=null,f=null){l instanceof HTMLTemplateElement&&g instanceof HTMLTemplateElement&&(l=l.content,g=g.content),m||=l.firstChild;for(let b of g.childNodes){if(m&&m!=f){let M=y(u,b,m,f);if(M){M!==m&&R(u,m,M),s(M,b,u),m=M.nextSibling;continue}}if(b instanceof Element&&u.persistentIds.has(b.id)){let M=T(l,b.id,m,u);s(M,b,u),m=M.nextSibling;continue}let _=E(l,b,m,u);_&&(m=_.nextSibling)}for(;m&&m!=f;){let b=m;m=m.nextSibling,A(u,b)}}function E(u,l,g,m){if(m.callbacks.beforeNodeAdded(l)===!1)return null;if(m.idMap.has(l)){let f=document.createElement(l.tagName);return u.insertBefore(f,g),s(f,l,m),m.callbacks.afterNodeAdded(f),f}else{let f=document.importNode(l,!0);return u.insertBefore(f,g),m.callbacks.afterNodeAdded(f),f}}let y=function(){function u(m,f,b,_){let M=null,w=f.nextSibling,V=0,k=b;for(;k&&k!=_;){if(g(k,f)){if(l(m,k,f))return k;M===null&&(m.idMap.has(k)||(M=k))}if(M===null&&w&&g(k,w)&&(V++,w=w.nextSibling,V>=2&&(M=void 0)),k.contains(document.activeElement))break;k=k.nextSibling}return M||null}function l(m,f,b){let _=m.idMap.get(f),M=m.idMap.get(b);if(!M||!_)return!1;for(let w of _)if(M.has(w))return!0;return!1}function g(m,f){let b=m,_=f;return b.nodeType===_.nodeType&&b.tagName===_.tagName&&(!b.id||b.id===_.id)}return u}();function A(u,l){if(u.idMap.has(l))p(u.pantry,l,null);else{if(u.callbacks.beforeNodeRemoved(l)===!1)return;l.parentNode?.removeChild(l),u.callbacks.afterNodeRemoved(l)}}function R(u,l,g){let m=l;for(;m&&m!==g;){let f=m;m=m.nextSibling,A(u,f)}return m}function T(u,l,g,m){let f=m.target.id===l&&m.target||m.target.querySelector(`[id="${l}"]`)||m.pantry.querySelector(`[id="${l}"]`);return c(f,m),p(u,f,g),f}function c(u,l){let g=u.id;for(;u=u.parentNode;){let m=l.idMap.get(u);m&&(m.delete(g),m.size||l.idMap.delete(u))}}function p(u,l,g){if(u.moveBefore)try{u.moveBefore(l,g)}catch{u.insertBefore(l,g)}else u.insertBefore(l,g)}return v}(),s=function(){function v(c,p,u){return u.ignoreActive&&c===document.activeElement?null:(u.callbacks.beforeNodeMorphed(c,p)===!1||(c instanceof HTMLHeadElement&&u.head.ignore||(c instanceof HTMLHeadElement&&u.head.style!=="morph"?d(c,p,u):(E(c,p,u),T(c,u)||o(u,c,p))),u.callbacks.afterNodeMorphed(c,p)),c)}function E(c,p,u){let l=p.nodeType;if(l===1){let g=c,m=p,f=g.attributes,b=m.attributes;for(let _ of b)R(_.name,g,"update",u)||g.getAttribute(_.name)!==_.value&&g.setAttribute(_.name,_.value);for(let _=f.length-1;0<=_;_--){let M=f[_];if(M&&!m.hasAttribute(M.name)){if(R(M.name,g,"remove",u))continue;g.removeAttribute(M.name)}}T(g,u)||y(g,m,u)}(l===8||l===3)&&c.nodeValue!==p.nodeValue&&(c.nodeValue=p.nodeValue)}function y(c,p,u){if(c instanceof HTMLInputElement&&p instanceof HTMLInputElement&&p.type!=="file"){let l=p.value,g=c.value;A(c,p,"checked",u),A(c,p,"disabled",u),p.hasAttribute("value")?g!==l&&(R("value",c,"update",u)||(c.setAttribute("value",l),c.value=l)):R("value",c,"remove",u)||(c.value="",c.removeAttribute("value"))}else if(c instanceof HTMLOptionElement&&p instanceof HTMLOptionElement)A(c,p,"selected",u);else if(c instanceof HTMLTextAreaElement&&p instanceof HTMLTextAreaElement){let l=p.value,g=c.value;if(R("value",c,"update",u))return;l!==g&&(c.value=l),c.firstChild&&c.firstChild.nodeValue!==l&&(c.firstChild.nodeValue=l)}}function A(c,p,u,l){let g=p[u],m=c[u];if(g!==m){let f=R(u,c,"update",l);f||(c[u]=p[u]),g?f||c.setAttribute(u,""):R(u,c,"remove",l)||c.removeAttribute(u)}}function R(c,p,u,l){return c==="value"&&l.ignoreActiveValue&&p===document.activeElement?!0:l.callbacks.beforeAttributeUpdated(c,p,u)===!1}function T(c,p){return!!p.ignoreActiveValue&&c===document.activeElement&&c!==document.body}return v}();function a(v,E,y,A){if(v.head.block){let R=E.querySelector("head"),T=y.querySelector("head");if(R&&T){let c=d(R,T,v);return Promise.all(c).then(()=>{let p=Object.assign(v,{head:{block:!1,ignore:!0}});return A(p)})}}return A(v)}function d(v,E,y){let A=[],R=[],T=[],c=[],p=new Map;for(let l of E.children)p.set(l.outerHTML,l);for(let l of v.children){let g=p.has(l.outerHTML),m=y.head.shouldReAppend(l),f=y.head.shouldPreserve(l);g||f?m?R.push(l):(p.delete(l.outerHTML),T.push(l)):y.head.style==="append"?m&&(R.push(l),c.push(l)):y.head.shouldRemove(l)!==!1&&R.push(l)}c.push(...p.values());let u=[];for(let l of c){let g=document.createRange().createContextualFragment(l.outerHTML).firstChild;if(y.callbacks.beforeNodeAdded(g)!==!1){if("href"in g&&g.href||"src"in g&&g.src){let m,f=new Promise(function(b){m=b});g.addEventListener("load",function(){m()}),u.push(f)}v.appendChild(g),y.callbacks.afterNodeAdded(g),A.push(g)}}for(let l of R)y.callbacks.beforeNodeRemoved(l)!==!1&&(v.removeChild(l),y.callbacks.afterNodeRemoved(l));return y.head.afterHeadMorphed(v,{added:A,kept:T,removed:R}),u}let h=function(){function v(p,u,l){let{persistentIds:g,idMap:m}=T(p,u),f=E(l),b=f.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(b))throw`Do not understand how to morph style ${b}`;return{target:p,newContent:u,config:f,morphStyle:b,ignoreActive:f.ignoreActive,ignoreActiveValue:f.ignoreActiveValue,restoreFocus:f.restoreFocus,idMap:m,persistentIds:g,pantry:y(),callbacks:f.callbacks,head:f.head}}function E(p){let u=Object.assign({},e);return Object.assign(u,p),u.callbacks=Object.assign({},e.callbacks,p.callbacks),u.head=Object.assign({},e.head,p.head),u}function y(){let p=document.createElement("div");return p.hidden=!0,document.body.insertAdjacentElement("afterend",p),p}function A(p){let u=Array.from(p.querySelectorAll("[id]"));return p.id&&u.push(p),u}function R(p,u,l,g){for(let m of g)if(u.has(m.id)){let f=m;for(;f;){let b=p.get(f);if(b==null&&(b=new Set,p.set(f,b)),b.add(m.id),f===l)break;f=f.parentElement}}}function T(p,u){let l=A(p),g=A(u),m=c(l,g),f=new Map;R(f,m,p,l);let b=u.__idiomorphRoot||u;return R(f,m,b,g),{persistentIds:m,idMap:f}}function c(p,u){let l=new Set,g=new Map;for(let{id:f,tagName:b}of p)g.has(f)?l.add(f):g.set(f,b);let m=new Set;for(let{id:f,tagName:b}of u)m.has(f)?l.add(f):g.get(f)===b&&m.add(f);for(let f of l)m.delete(f);return m}return v}(),{normalizeElement:S,normalizeParent:C}=function(){let v=new WeakSet;function E(T){return T instanceof Document?T.documentElement:T}function y(T){if(T==null)return document.createElement("div");if(typeof T=="string")return y(R(T));if(v.has(T))return T;if(T instanceof Node){if(T.parentNode)return new A(T);{let c=document.createElement("div");return c.append(T),c}}else{let c=document.createElement("div");for(let p of[...T])c.append(p);return c}}class A{constructor(c){this.originalNode=c,this.realParentNode=c.parentNode,this.previousSibling=c.previousSibling,this.nextSibling=c.nextSibling}get childNodes(){let c=[],p=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;p&&p!=this.nextSibling;)c.push(p),p=p.nextSibling;return c}querySelectorAll(c){return this.childNodes.reduce((p,u)=>{if(u instanceof Element){u.matches(c)&&p.push(u);let l=u.querySelectorAll(c);for(let g=0;g<l.length;g++)p.push(l[g])}return p},[])}insertBefore(c,p){return this.realParentNode.insertBefore(c,p)}moveBefore(c,p){return this.realParentNode.moveBefore(c,p)}get __idiomorphRoot(){return this.originalNode}}function R(T){let c=new DOMParser,p=T.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(p.match(/<\/html>/)||p.match(/<\/head>/)||p.match(/<\/body>/)){let u=c.parseFromString(T,"text/html");if(p.match(/<\/html>/))return v.add(u),u;{let l=u.firstChild;return l&&v.add(l),l}}else{let l=c.parseFromString("<body><template>"+T+"</template></body>","text/html").body.querySelector("template").content;return v.add(l),l}}return{normalizeElement:E,normalizeParent:y}}();return{morph:n,defaults:e}}();var Ot={type:2,name:F.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");K(F.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=ot,useViewTransition:o=`${Ae}`})=>{let s=Y(o);e.innerHTML=n.trim();let a=[...e.content.children];for(let d of a){if(!(d instanceof Element))throw W("NoFragmentsFound",t);let h=r||`#${d.getAttribute("id")}`,S=[...document.querySelectorAll(h)||[]];if(!S.length)throw W("NoTargetsFound",t,{selectorOrID:h});s&&Z?be.startViewTransition(()=>Vt(t,i,d,S)):Vt(t,i,d,S)}})}};function Vt(t,e,n,r){for(let i of r){i.dataset.fragmentMergeTarget="true";let o=n.cloneNode(!0);switch(e){case U.Morph:{me(o,s=>{!s.id?.length&&Object.keys(s.dataset).length&&(s.id=_e(s));let a=t.removals.get(s.id);if(a){let d=new Map;for(let[h,S]of a){let C=Re(h,h);d.set(C,S),a.delete(h)}t.removals.set(s.id,d)}}),Lt.morph(i,o);break}case U.Inner:i.innerHTML=o.outerHTML;break;case U.Outer:i.replaceWith(o);break;case U.Prepend:i.prepend(o);break;case U.Append:i.append(o);break;case U.Before:i.before(o);break;case U.After:i.after(o);break;case U.UpsertAttributes:for(let s of o.getAttributeNames()){let a=o.getAttribute(s);i.setAttribute(s,a)}break;default:throw W("InvalidMergeMode",t,{mergeMode:e})}}}var Dt={type:2,name:F.MergeSignals,onGlobalInit:async t=>{K(F.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${rt}`})=>{let{signals:r}=t,i=Y(n);r.merge(Ee(e),i)})}};var Ht={type:2,name:F.RemoveFragments,onGlobalInit:async t=>{K(F.RemoveFragments,({selector:e,useViewTransition:n=`${Ae}`})=>{if(!e.length)throw W("NoSelectorProvided",t);let r=Y(n),i=document.querySelectorAll(e),o=()=>{for(let s of i)s.remove()};r&&Z?be.startViewTransition(()=>o()):o()})}};var Ft={type:2,name:F.RemoveSignals,onGlobalInit:async t=>{K(F.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw W("NoPathsProvided",t);t.signals.remove(...n)})}};var qt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw N("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var Wt={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw N("CustomValidityInvalidElement",t);let i=n();return r(()=>{let o=i();if(typeof o!="string")throw N("CustomValidityInvalidExpression",t,{result:o});e.setCustomValidity(o)})}};var $t="once",Gt="half",Ut="full",Bt={type:1,name:"onIntersect",keyReq:2,mods:new Set([$t,Gt,Ut]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i=r(),o={threshold:0};n.has(Ut)?o.threshold=1:n.has(Gt)&&(o.threshold=.5);let s=new IntersectionObserver(a=>{for(let d of a)d.isIntersecting&&(i(),n.has($t)&&(s.disconnect(),delete t.dataset[e]))},o);return s.observe(t),()=>s.disconnect()}};function Se(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function re(t,e,n=!1){return t?t.has(e.toLowerCase()):n}var jt={type:1,name:"onInterval",keyReq:2,valReq:1,onLoad:({mods:t,genRX:e})=>{let n=e(),r=()=>n(),i=1e3,o=t.get("duration");o&&(i=Se(o),re(o,"leading",!1)&&r());let s=setInterval(r,i);return()=>{clearInterval(s)}}};function Gn(t,e,n=!1,r=!0){let i=-1,o=()=>i&&clearTimeout(i);return(...s)=>{o(),n&&!i&&t(...s),i=setTimeout(()=>{r&&t(...s),o()},e)}}function Un(t,e,n=!0,r=!1){let i=!1;return(...o)=>{i||(n&&t(...o),i=!0,setTimeout(()=>{i=!1,r&&t(...o)},e))}}function Q(t,e){let n=e.get("debounce");if(n){let i=Se(n),o=re(n,"leading",!1),s=!re(n,"notrail",!1);t=Gn(t,i,o,s)}let r=e.get("throttle");if(r){let i=Se(r),o=!re(r,"noleading",!1),s=re(r,"trail",!1);t=Un(t,i,o,s)}return t}var Kt={type:1,name:"onLoad",keyReq:2,valReq:1,onLoad:({mods:t,genRX:e})=>{let n=e(),r=Q(n,t);return setTimeout(r),()=>{}}};var Jt={type:1,name:"onRaf",keyReq:2,valReq:1,onLoad:({mods:t,genRX:e})=>{let n=e(),r=Q(()=>n(),t),i,o=()=>{r(),i=requestAnimationFrame(o)};return i=requestAnimationFrame(o),()=>{i&&cancelAnimationFrame(i)}}};var zt={type:1,name:"onSignalChange",valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{let i=r(),o=Q(d=>i(d),e);if(t===""){let d=h=>o(h);return document.addEventListener(de,d),()=>{document.removeEventListener(de,d)}}let s=H(t,e),a=new Map;return n.walk((d,h)=>{d.startsWith(s)&&a.set(h,h.value)}),ue(()=>{for(let[d,h]of a)h!==d.value&&(o(),a.set(d,d.value))})}};var Yt="session",Xt={type:1,name:"persist",keyReq:2,mods:new Set([Yt]),onLoad:({effect:t,mods:e,signals:n,value:r})=>{let i=q,o=e.has(Yt)?sessionStorage:localStorage,s=r.split(/\s+/).filter(h=>h!=="");s=s.map(h=>X(h));let a=()=>{let h=o.getItem(i)||"{}",S=JSON.parse(h);n.merge(S)},d=()=>{let h;s.length?h=n.subset(...s):h=n.values(),o.setItem(i,JSON.stringify(h))};return a(),t(()=>{d()})}};var Zt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,o=new URL(r,i).toString();window.history.replaceState({},"",o)})}};var Ve="smooth",ze="instant",Ye="auto",Qt="hstart",en="hcenter",tn="hend",nn="hnearest",rn="vstart",on="vcenter",sn="vend",an="vnearest",Bn="focus",Oe="center",ln="start",un="end",cn="nearest",fn={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ve,ze,Ye,Qt,en,tn,nn,rn,on,sn,an,Bn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:Ve,block:Oe,inline:Oe};if(n.has(Ve)&&(i.behavior=Ve),n.has(ze)&&(i.behavior=ze),n.has(Ye)&&(i.behavior=Ye),n.has(Qt)&&(i.inline=ln),n.has(en)&&(i.inline=Oe),n.has(tn)&&(i.inline=un),n.has(nn)&&(i.inline=cn),n.has(rn)&&(i.block=ln),n.has(on)&&(i.block=Oe),n.has(sn)&&(i.block=un),n.has(an)&&(i.block=cn),!(e instanceof HTMLElement||e instanceof SVGElement))throw N("ScrollIntoViewInvalidElement",t);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r]}};var dn="view-transition",pn={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===dn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=dn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!Z){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var mn={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let i=r();return e===""?n(async()=>{let o=i();for(let[s,a]of Object.entries(o))a===!1?t.removeAttribute(s):t.setAttribute(s,a)}):(e=z(e),n(async()=>{let o=!1;try{o=i()}catch{}let s;typeof o=="string"?s=o:s=JSON.stringify(o),!s||s==="false"||s==="null"||s==="undefined"?t.removeAttribute(e):t.setAttribute(e,s)}))}};var jn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,gn=["change","input","keydown"],hn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:i,value:o,effect:s}=t,a=e,d=n?H(n,r):X(o),h=e.tagName.toLowerCase(),S=h.includes("input"),C=h.includes("select"),v=e.getAttribute("type"),E=e.hasAttribute("value"),y="",A=S&&v==="checkbox";A&&(y=E?"":!1);let R=S&&v==="number";R&&(y=0);let T=S&&v==="radio";T&&(e.getAttribute("name")?.length||e.setAttribute("name",d));let c=S&&v==="file",{signal:p,inserted:u}=i.upsertIfMissing(d,y),l=-1;Array.isArray(p.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",d),l=[...document.querySelectorAll(`[name="${d}"]`)].findIndex(w=>w===t.el));let g=l>=0,m=()=>[...i.value(d)],f=()=>{let w=i.value(d);g&&!C&&(w=w[l]||y);let V=`${w}`;if(A||T)typeof w=="boolean"?a.checked=w:a.checked=V===a.value;else if(C){let k=e;if(k.multiple){if(!g)throw N("BindSelectMultiple",t);for(let D of k.options){if(D?.disabled)return;let O=R?Number(D.value):D.value;D.selected=w.includes(O)}}else k.value=V}else c||("value"in e?e.value=V:e.setAttribute("value",V))},b=async()=>{let w=i.value(d);if(g){let O=w;for(;l>=O.length;)O.push(y);w=O[l]||y}let V=(O,$)=>{let G=$;g&&!C&&(G=m(),G[l]=$),i.setValue(O,G)};if(c){let O=[...a?.files||[]],$=[],G=[],Xe=[];await Promise.all(O.map(Ze=>new Promise(wn=>{let ee=new FileReader;ee.onload=()=>{if(typeof ee.result!="string")throw N("InvalidFileResultType",t,{resultType:typeof ee.result});let De=ee.result.match(jn);if(!De?.groups)throw N("InvalidDataUri",t,{result:ee.result});$.push(De.groups.contents),G.push(De.groups.mime),Xe.push(Ze.name)},ee.onloadend=()=>wn(void 0),ee.readAsDataURL(Ze)}))),V(d,$),V(`${d}Mimes`,G),V(`${d}Names`,Xe);return}let k=a.value||"",D;if(A){let O=a.checked||a.getAttribute("checked")==="true";E?D=O?k:"":D=O}else if(C){let $=[...e.selectedOptions];g?D=$.filter(G=>G.selected).map(G=>G.value):D=$[0]?.value||y}else typeof w=="boolean"?D=!!k:typeof w=="number"?D=Number(k):D=k||"";V(d,D)};u&&b();for(let w of gn)e.addEventListener(w,b);let _=w=>{w.persisted&&b()};window.addEventListener("pageshow",_);let M=s(()=>f());return()=>{M();for(let w of gn)e.removeEventListener(w,b);window.removeEventListener("pageshow",_)}}};var yn={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:i})=>{let o=t.classList,s=i();return r(()=>{if(e===""){let a=s();for(let[d,h]of Object.entries(a)){let S=d.split(/\s+/);h?o.add(...S):o.remove(...S)}}else{let a=z(e);a=H(a,n),s()?o.add(a):o.remove(a)}})}};var vn={type:1,name:"on",keyReq:1,valReq:1,argNames:["evt"],onLoad:({el:t,key:e,mods:n,genRX:r})=>{let i=r(),o=t;n.has("window")&&(o=window);let s=S=>{S&&((n.has("prevent")||e==="submit")&&S.preventDefault(),n.has("stop")&&S.stopPropagation()),i(S)};if(s=Q(s,n),n.has("viewtransition")&&Z){let S=s;s=(...C)=>document.startViewTransition(()=>S(...C))}let a={capture:!0,passive:!1,once:!1};if(n.has("capture")||(a.capture=!1),n.has("passive")&&(a.passive=!0),n.has("once")&&(a.once=!0),n.has("outside")){o=document;let S=s;s=v=>{let E=v?.target;t.contains(E)||S(v)}}let h=z(e);return h=H(h,n),o.addEventListener(h,s,a),()=>{o.removeEventListener(h,s)}}};var bn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let o=e?H(e,n):X(i);r.setValue(o,t)}};var Sn="none",Tn="display",An={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===Sn&&t.removeProperty(Tn):t.setProperty(Tn,Sn)})}};var En={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,i=r();return e instanceof HTMLElement||N("TextInvalidElement",t),n(()=>{let o=i(t);e.textContent=`${o}`})}};var{round:Kn,max:Jn,min:zn}=Math,_n={type:3,name:"fit",fn:(t,e,n,r,i,o,s=!1,a=!1)=>{let d=(e-n)/(r-n)*(o-i)+i;return a&&(d=Kn(d)),s&&(d=Jn(i,zn(o,d))),d}};var Rn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var xn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Ce(mn,hn,yn,vn,bn,An,En,kt,Mt,Pt,Ct,Nt,wt,Ot,Dt,Ht,Ft,It,qt,Wt,Bt,jt,Kt,Jt,zt,Xt,Zt,fn,pn,_n,Rn,xn);Je();export{Je as apply,Ce as load,St as setAlias};
//# sourceMappingURL=datastar.js.map
