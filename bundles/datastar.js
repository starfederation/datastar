// Datastar v1.0.0-beta.7
var Ke=/🖕JS_DS🚀/.source,ve=Ke.slice(0,5),Ie=Ke.slice(4),D="datastar";var Je="Datastar-Request",ye=300,ze=1e3,Ye="type module",Se=!1,Xe=!1,Qe=!0,W={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ze=W.Morph,k={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var w=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(w||{});var le=`${D}-signals`;var K=t=>t.trim()==="true",ue=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),Y=t=>ue(t).replace(/-./g,e=>e[1].toUpperCase()),Le=t=>ue(t).replace(/-/g,"_"),An=t=>Y(t).replace(/^./,e=>e[0].toUpperCase()),be=t=>new Function(`return Object.assign({}, ${t})`)(),J=t=>t.startsWith("$")?t.slice(1):t,En={kebab:ue,snake:Le,pascal:An};function V(t,e){for(let n of e.get("case")||[]){let r=En[n];r&&(t=r(t))}return t}var _n="computed",et={type:1,name:_n,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=V(t,e);let i=r();n.setComputed(t,i)}};var tt={type:1,name:"signals",removeOnLoad:()=>!0,onLoad:t=>{let{key:e,mods:n,signals:r,value:i,genRX:s}=t,u=n.has("ifmissing");if(e!==""){let o=V(e,n),h=i===""?i:s()();u?r.upsertIfMissing(o,h):r.setValue(o,h)}else{let o=be(t.value);t.value=JSON.stringify(o);let b=s()();r.merge(b,u)}}};var nt={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var Z=class{#e=0;#t;constructor(e=D){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function rt(t){if(t.id)return t.id;let e=new Z,n=t;for(;n;){if(e.with(n.tagName||""),n.id){e.with(n.id);break}let r=n?.parentNode;r&&e.with([...r.children].indexOf(n)),n=r}return e.string}function it(t,e){return new Z().with(t).with(e).value}function Ve(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)Ve(r,e),r=r.nextElementSibling}var xn="https://data-star.dev/errors";function De(t,e,n={}){let r=new Error;r.name=`${D} ${t} error`;let i=Le(e),s=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),u=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${xn}/${t}/${i}?${s}
Context: ${u}`,r}function $(t,e,n={}){return De("internal",e,Object.assign({from:t},n))}function H(t,e,n={}){let r={plugin:{name:e.plugin.name,type:w[e.plugin.type]}};return De("init",t,Object.assign(r,n))}function M(t,e,n={}){let r={plugin:{name:e.plugin.name,type:w[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return De("runtime",t,Object.assign(r,n))}var ee="preact-signals",Rn=Symbol.for("preact-signals"),G=1,te=2,fe=4,re=8,Te=16,ne=32;function Oe(){Ae++}function He(){if(Ae>1){Ae--;return}let t,e=!1;for(;ce!==void 0;){let n=ce;for(ce=void 0,ke++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~te,!(n._flags&re)&&st(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(ke=0,Ae--,e)throw t}var C;var ce,Ae=0,ke=0,Ee=0;function ot(t){if(C===void 0)return;let e=t._node;if(e===void 0||e._target!==C)return e={_version:0,_source:t,_prevSource:C._sources,_nextSource:void 0,_target:C,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},C._sources!==void 0&&(C._sources._nextSource=e),C._sources=e,t._node=e,C._flags&ne&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=C._sources,e._nextSource=void 0,C._sources._nextSource=e,C._sources=e),e}function P(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}P.prototype.brand=Rn;P.prototype._refresh=()=>!0;P.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};P.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};P.prototype.subscribe=function(t){return _e(()=>{let e=this.value,n=C;C=void 0;try{t(e)}finally{C=n}})};P.prototype.valueOf=function(){return this.value};P.prototype.toString=function(){return`${this.value}`};P.prototype.toJSON=function(){return this.value};P.prototype.peek=function(){let t=C;C=void 0;try{return this.value}finally{C=t}};Object.defineProperty(P.prototype,"value",{get(){let t=ot(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(ke>100)throw $(ee,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Ee++,Oe();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{He()}this?._onChange({old:e,revised:n})}}});function st(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function at(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function lt(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function X(t){P.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Ee-1,this._flags=fe}X.prototype=new P;X.prototype._refresh=function(){if(this._flags&=~te,this._flags&G)return!1;if((this._flags&(fe|ne))===ne||(this._flags&=~fe,this._globalVersion===Ee))return!0;if(this._globalVersion=Ee,this._flags|=G,this._version>0&&!st(this))return this._flags&=~G,!0;let t=C;try{at(this),C=this;let e=this._fn();(this._flags&Te||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~Te,this._version++)}catch(e){this._value=e,this._flags|=Te,this._version++}return C=t,lt(this),this._flags&=~G,!0};X.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=fe|ne;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}P.prototype._subscribe.call(this,t)};X.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(P.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~ne;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};X.prototype._notify=function(){if(!(this._flags&te)){this._flags|=fe|te;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(X.prototype,"value",{get(){if(this._flags&G)throw $(ee,"SignalCycleDetected");let t=ot(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&Te)throw $(ee,"GetComputedError",{value:this._value});return this._value}});function ut(t){return new X(t)}function ct(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){Oe();let n=C;C=void 0;try{e()}catch(r){throw t._flags&=~G,t._flags|=re,Fe(t),$(ee,"CleanupEffectError",{error:r})}finally{C=n,He()}}}function Fe(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,ct(t)}function wn(t){if(C!==this)throw $(ee,"EndEffectError");lt(this),C=t,this._flags&=~G,this._flags&re&&Fe(this),He()}function de(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=ne}de.prototype._callback=function(){let t=this._start();try{if(this._flags&re||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};de.prototype._start=function(){if(this._flags&G)throw $(ee,"SignalCycleDetected");this._flags|=G,this._flags&=~re,ct(this),at(this),Oe();let t=C;return C=this,wn.bind(this,t)};de.prototype._notify=function(){this._flags&te||(this._flags|=te,this._nextBatchedEffect=ce,ce=this)};de.prototype._dispose=function(){this._flags|=re,this._flags&G||Fe(this)};function _e(t){let e=new de(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var ft="namespacedSignals",ie=t=>{document.dispatchEvent(new CustomEvent(le,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function dt(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof P?n[r]=i.value:n[r]=dt(i)}return n}function pt(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw $(ft,"InvalidSignalKey",{key:i});let s=e[i];if(s instanceof Object&&!Array.isArray(s)){t[i]||(t[i]={});let u=pt(t[i],s,n);r.added.push(...u.added.map(o=>`${i}.${o}`)),r.removed.push(...u.removed.map(o=>`${i}.${o}`)),r.updated.push(...u.updated.map(o=>`${i}.${o}`))}else{if(Object.hasOwn(t,i)){if(n)continue;let h=t[i];if(h instanceof P){let b=h.value;h.value=s,b!==s&&r.updated.push(i);continue}}let o=new P(s);o._onChange=()=>{ie({updated:[i]})},t[i]=o,r.added.push(i)}}return r}function mt(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof P?e(n,r):mt(r,(i,s)=>{e(`${n}.${i}`,s)})}}function Mn(t,...e){let n={};for(let r of e){let i=r.split("."),s=t,u=n;for(let h=0;h<i.length-1;h++){let b=i[h];if(!s[b])return{};u[b]||(u[b]={}),s=s[b],u=u[b]}let o=i[i.length-1];u[o]=s[o]}return n}var xe=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let u=0;u<n.length-1;u++){let o=n[u];if(!r[o])return null;r=r[o]}let i=n[n.length-1],s=r[i];if(!s)throw $(ft,"SignalNotFound",{path:e});return s}setSignal(e,n){let r=e.split("."),i=this.#e;for(let u=0;u<r.length-1;u++){let o=r[u];i[o]||(i[o]={}),i=i[o]}let s=r[r.length-1];i[s]=n}setComputed(e,n){let r=ut(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let{signal:r}=this.upsertIfMissing(e,n),i=r.value;r.value=n,i!==n&&ie({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let h=0;h<r.length-1;h++){let b=r[h];i[b]||(i[b]={}),i=i[b]}let s=r[r.length-1],u=i[s];if(u instanceof P)return{signal:u,inserted:!1};let o=new P(n);return o._onChange=()=>{ie({updated:[e]})},i[s]=o,ie({added:[e]}),{signal:o,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let i=r.split("."),s=this.#e;for(let o=0;o<i.length-1;o++){let h=i[o];if(!s[h])return;s=s[h]}let u=i[i.length-1];delete s[u],n.push(r)}ie({removed:n})}merge(e,n=!1){let r=pt(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&ie(r)}subset(...e){return Mn(this.values(),...e)}walk(e){mt(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return dt(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var gt=new xe,We=[],Re={},Nn=[],$e="";function ht(t){$e=t}var qe=null,pe=new Map;function we(...t){for(let e of t){let n={signals:gt,effect:i=>_e(i),actions:Re,plugin:e,apply:oe},r;switch(e.type){case 2:{let i=e;Nn.push(i),r=i.onGlobalInit;break}case 3:{Re[e.name]=e;break}case 1:{let i=e;We.push(i),r=i.onGlobalInit;break}default:throw H("InvalidPluginType",n)}r&&r(n)}We.sort((e,n)=>{let r=n.name.length-e.name.length;return r!==0?r:e.name.localeCompare(n.name)})}function oe(t=document.documentElement){Ve(t,e=>{let n=new Array,r=pe.get(e)||new Map,i=new Map([...r]),s=new Map;for(let u of Object.keys(e.dataset)){if(!u.startsWith($e))break;let o=e.dataset[u]||"",h=it(u,o);s.set(u,h),r.has(h)?i.delete(h):n.push(u)}for(let[u,o]of i)o();for(let u of n){let o=s.get(u);Pn(e,u,o)}}),Cn()}function Cn(){qe||(qe=new MutationObserver(t=>{let e=new Set,n=new Set;for(let{target:r,type:i,addedNodes:s,removedNodes:u}of t)switch(i){case"childList":{for(let o of u)e.add(o);for(let o of s)n.add(o)}break;case"attributes":{n.add(r);break}}for(let r of e){let i=pe.get(r);if(i){for(let[s,u]of i)u(),i.delete(s);i.size===0&&pe.delete(r)}}for(let r of n)oe(r)}),qe.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function Pn(t,e,n){let r=Y(e.slice($e.length)),i=We.find(y=>r.startsWith(y.name));if(!i)return;t.id.length||(t.id=rt(t));let[s,...u]=r.slice(i.name.length).split(/\_\_+/),o=s.length>0;o&&(s=Y(s));let h=t.dataset[e]||"",b=h.length>0,v={signals:gt,apply:oe,effect:y=>_e(y),actions:Re,genRX:()=>In(v,...i.argNames||[]),plugin:i,el:t,rawKey:r,key:s,value:h,mods:new Map},N=i.keyReq||0;if(o){if(N===2)throw M(`${i.name}KeyNotAllowed`,v)}else if(N===1)throw M(`${i.name}KeyRequired`,v);let T=i.valReq||0;if(b){if(T===2)throw M(`${i.name}ValueNotAllowed`,v)}else if(T===1)throw M(`${i.name}ValueRequired`,v);if(N===3||T===3){if(o&&b)throw M(`${i.name}KeyAndValueProvided`,v);if(!o&&!b)throw M(`${i.name}KeyOrValueRequired`,v)}for(let y of u){let[x,..._]=y.split(".");v.mods.set(Y(x),new Set(_.map(g=>g.toLowerCase())))}let E=i.onLoad(v);if(E){let y=pe.get(t);y||(y=new Map,pe.set(t,y)),y.set(n,E)}}function In(t,...e){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=t.value.trim().match(r);if(i){let E=i.length-1,y=i[E].trim();y.startsWith("return")||(i[E]=`return (${y});`),n=i.join(`;
`)}let s=new Map,u=new RegExp(`(?:${ve})(.*?)(?:${Ie})`,"gm");for(let E of n.matchAll(u)){let y=E[1],x=new Z("dsEscaped").with(y).string;s.set(x,y),n=n.replace(ve+y+Ie,x)}let o=/@(\w*)\(/gm,h=n.matchAll(o),b=new Set;for(let E of h)b.add(E[1]);let v=new RegExp(`@(${Object.keys(Re).join("|")})\\(`,"gm");n=n.replaceAll(v,"ctx.actions.$1.fn(ctx,");let N=t.signals.paths();if(N.length){let E=new RegExp(`\\$(${N.join("|")})(\\W|$)`,"gm");n=n.replaceAll(E,"ctx.signals.signal('$1').value$2")}for(let[E,y]of s)n=n.replace(E,y);let T=`return (()=> {
${n}
})()`;t.fnContent=T;try{let E=new Function("ctx",...e,T);return(...y)=>{try{return E(t,...y)}catch(x){throw M("ExecuteExpression",t,{error:x.message})}}}catch(E){throw M("GenerateExpression",t,{error:E.message})}}we(nt,tt,et);async function Ln(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function Vn(t){let e,n,r,i=!1;return function(u){e===void 0?(e=u,n=0,r=-1):e=kn(e,u);let o=e.length,h=0;for(;n<o;){i&&(e[n]===10&&(h=++n),i=!1);let b=-1;for(;n<o&&b===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-h);break;case 13:i=!0;case 10:b=n;break}if(b===-1)break;t(e.subarray(h,b),r),h=n,r=-1}h===o?e=void 0:h!==0&&(e=e.subarray(h),n-=h)}}function Dn(t,e,n){let r=vt(),i=new TextDecoder;return function(u,o){if(u.length===0)n?.(r),r=vt();else if(o>0){let h=i.decode(u.subarray(0,o)),b=o+(u[o+1]===32?2:1),v=i.decode(u.subarray(b));switch(h){case"data":r.data=r.data?`${r.data}
${v}`:v;break;case"event":r.event=v;break;case"id":t(r.id=v);break;case"retry":{let N=Number.parseInt(v,10);Number.isNaN(N)||e(r.retry=N);break}}}}}function kn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function vt(){return{data:"",event:"",id:"",retry:void 0}}var On="text/event-stream",yt="last-event-id";function St(t,e,{signal:n,headers:r,onopen:i,onmessage:s,onclose:u,onerror:o,openWhenHidden:h,fetch:b,retryInterval:v=1e3,retryScaler:N=2,retryMaxWaitMs:T=3e4,retryMaxCount:E=10,...y}){return new Promise((x,_)=>{let g=0,a={...r};a.accept||(a.accept=On);let c;function l(){c.abort(),document.hidden||S()}h||document.addEventListener("visibilitychange",l);let d=0;function m(){document.removeEventListener("visibilitychange",l),window.clearTimeout(d),c.abort()}n?.addEventListener("abort",()=>{m(),x()});let p=b??window.fetch,f=i??function(){};async function S(){c=new AbortController;try{let A=await p(e,{...y,headers:a,signal:c.signal});await f(A),await Ln(A.body,Vn(Dn(R=>{R?a[yt]=R:delete a[yt]},R=>{v=R},s))),u?.(),m(),x()}catch(A){if(!c.signal.aborted)try{let R=o?.(A)??v;window.clearTimeout(d),d=window.setTimeout(S,R),v*=N,v=Math.min(v,T),g++,g>=E?(m(),_(M("SseMaxRetries",t,{retryMaxCount:E}))):console.error(`Datastar failed to reach ${y.method}: ${e.toString()} retry in ${R}ms`)}catch(R){m(),_(R)}}}S()})}var se=`${D}-sse`,Ge=`${D}-settling`,Q=`${D}-swapping`,Me="started",Ne="finished",bt="error",Tt="retrying";function U(t,e){document.addEventListener(se,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function me(t,e){document.dispatchEvent(new CustomEvent(se,{detail:{type:t,argsRaw:e}}))}var At=t=>`${t}`.includes("text/event-stream"),B=async(t,e,n,r)=>{let{el:{id:i},el:s,signals:u}=t,{headers:o,contentType:h,includeLocal:b,selector:v,openWhenHidden:N,retryInterval:T,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:x,abort:_}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:ze,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),g=e.toLowerCase(),a=()=>{};try{if(me(Me,{elId:i}),!n?.length)throw M("SseNoUrlProvided",t,{action:g});let c={};c[Je]=!0,h==="json"&&(c["Content-Type"]="application/json");let l=Object.assign({},c,o),d={method:e,headers:l,openWhenHidden:N,retryInterval:T,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:x,signal:_,onopen:async f=>{if(f.status>=400){let S=f.status.toString();me(bt,{status:S})}},onmessage:f=>{if(!f.event.startsWith(D))return;let S=f.event,A={},R=f.data.split(`
`);for(let F of R){let L=F.indexOf(" "),q=F.slice(0,L),j=A[q];j||(j=[],A[q]=j);let Tn=F.slice(L+1);j.push(Tn)}let I={};for(let[F,L]of Object.entries(A))I[F]=L.join(`
`);me(S,I)},onerror:f=>{if(At(f))throw M("InvalidContentType",t,{url:n});f&&(console.error(f.message),me(Tt,{message:f.message}))}},m=new URL(n,window.location.origin),p=new URLSearchParams(m.search);if(h==="json"){let f=u.JSON(!1,!b);e==="GET"?p.set(D,f):d.body=f}else if(h==="form"){let f=v?document.querySelector(v):s.closest("form");if(f===null)throw v?M("SseFormNotFound",t,{action:g,selector:v}):M("SseClosestFormNotFound",t,{action:g});if(s!==f){let A=R=>R.preventDefault();f.addEventListener("submit",A),a=()=>f.removeEventListener("submit",A)}if(!f.checkValidity()){f.reportValidity(),a();return}let S=new FormData(f);if(e==="GET"){let A=new URLSearchParams(S);for(let[R,I]of A)p.set(R,I)}else d.body=S}else throw M("SseInvalidContentType",t,{action:g,contentType:h});m.search=p.toString();try{await St(t,m.toString(),d)}catch(f){if(!At(f))throw M("SseFetchFailed",t,{method:e,url:n,error:f})}}finally{me(Ne,{elId:i}),a()}};var Et={type:3,name:"delete",fn:async(t,e,n)=>B(t,"DELETE",e,{...n})};var _t={type:3,name:"get",fn:async(t,e,n)=>B(t,"GET",e,{...n})};var xt={type:3,name:"patch",fn:async(t,e,n)=>B(t,"PATCH",e,{...n})};var Rt={type:3,name:"post",fn:async(t,e,n)=>B(t,"POST",e,{...n})};var wt={type:3,name:"put",fn:async(t,e,n)=>B(t,"PUT",e,{...n})};var Mt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let s=e?V(e,n):J(i),{signal:u}=r.upsertIfMissing(s,!1),o=h=>{let{type:b,argsRaw:{elId:v}}=h.detail;if(v===t.id)switch(b){case Me:u.value=!0;break;case Ne:u.value=!1;break}};return document.addEventListener(se,o),()=>{document.removeEventListener(se,o)}}};var Nt={type:2,name:k.ExecuteScript,onGlobalInit:async t=>{U(k.ExecuteScript,({autoRemove:e=`${Qe}`,attributes:n=Ye,script:r})=>{let i=K(e);if(!r?.length)throw H("NoScriptProvided",t);let s=document.createElement("script");for(let u of n.split(`
`)){let o=u.indexOf(" "),h=o?u.slice(0,o):u,b=o?u.slice(o):"";s.setAttribute(h.trim(),b.trim())}s.text=r,document.head.appendChild(s),i&&s.remove()})}};var ge=document,z=!!ge.startViewTransition;var Ct=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:T=>T.getAttribute("im-preserve")==="true",shouldReAppend:T=>T.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!0};function n(T,E,y={}){T=v(T);let x=N(E),_=b(T,x,y),g=i(_,()=>o(_,T,x,a=>a.morphStyle==="innerHTML"?(s(a,T,x),Array.from(T.childNodes)):r(a,T,x)));return _.pantry.remove(),g}function r(T,E,y){let x=N(E),_=Array.from(x.childNodes),g=_.indexOf(E),a=_.length-(g+1);return s(T,x,y,E,E.nextSibling),_=Array.from(x.childNodes),_.slice(g,_.length-a)}function i(T,E){if(!T.config.restoreFocus)return E();let y=document.activeElement;if(!(y instanceof HTMLInputElement||y instanceof HTMLTextAreaElement))return E();let{id:x,selectionStart:_,selectionEnd:g}=y,a=E();return x&&x!==document.activeElement?.id&&(y=T.target.querySelector(`#${x}`),y?.focus()),y&&!y.selectionEnd&&g&&y.setSelectionRange(_,g),a}let s=function(){function T(l,d,m,p=null,f=null){d instanceof HTMLTemplateElement&&m instanceof HTMLTemplateElement&&(d=d.content,m=m.content),p||=d.firstChild;for(let S of m.childNodes){if(p&&p!=f){let R=y(l,S,p,f);if(R){R!==p&&_(l,p,R),u(R,S,l),p=R.nextSibling;continue}}if(S instanceof Element&&l.persistentIds.has(S.id)){let R=g(d,S.id,p,l);u(R,S,l),p=R.nextSibling;continue}let A=E(d,S,p,l);A&&(p=A.nextSibling)}for(;p&&p!=f;){let S=p;p=p.nextSibling,x(l,S)}}function E(l,d,m,p){if(p.callbacks.beforeNodeAdded(d)===!1)return null;if(p.idMap.has(d)){let f=document.createElement(d.tagName);return l.insertBefore(f,m),u(f,d,p),p.callbacks.afterNodeAdded(f),f}else{let f=document.importNode(d,!0);return l.insertBefore(f,m),p.callbacks.afterNodeAdded(f),f}}let y=function(){function l(p,f,S,A){let R=null,I=f.nextSibling,F=0,L=S;for(;L&&L!=A;){if(m(L,f)){if(d(p,L,f))return L;R===null&&(p.idMap.has(L)||(R=L))}if(R===null&&I&&m(L,I)&&(F++,I=I.nextSibling,F>=2&&(R=void 0)),L.contains(document.activeElement))break;L=L.nextSibling}return R||null}function d(p,f,S){let A=p.idMap.get(f),R=p.idMap.get(S);if(!R||!A)return!1;for(let I of A)if(R.has(I))return!0;return!1}function m(p,f){let S=p,A=f;return S.nodeType===A.nodeType&&S.tagName===A.tagName&&(!S.id||S.id===A.id)}return l}();function x(l,d){if(l.idMap.has(d))c(l.pantry,d,null);else{if(l.callbacks.beforeNodeRemoved(d)===!1)return;d.parentNode?.removeChild(d),l.callbacks.afterNodeRemoved(d)}}function _(l,d,m){let p=d;for(;p&&p!==m;){let f=p;p=p.nextSibling,x(l,f)}return p}function g(l,d,m,p){let f=p.target.querySelector(`#${d}`)||p.pantry.querySelector(`#${d}`);return a(f,p),c(l,f,m),f}function a(l,d){let m=l.id;for(;l=l.parentNode;){let p=d.idMap.get(l);p&&(p.delete(m),p.size||d.idMap.delete(l))}}function c(l,d,m){if(l.moveBefore)try{l.moveBefore(d,m)}catch{l.insertBefore(d,m)}else l.insertBefore(d,m)}return T}(),u=function(){function T(a,c,l){return l.ignoreActive&&a===document.activeElement?null:(l.callbacks.beforeNodeMorphed(a,c)===!1||(a instanceof HTMLHeadElement&&l.head.ignore||(a instanceof HTMLHeadElement&&l.head.style!=="morph"?h(a,c,l):(E(a,c,l),g(a,l)||s(l,a,c))),l.callbacks.afterNodeMorphed(a,c)),a)}function E(a,c,l){let d=c.nodeType;if(d===1){let m=a,p=c,f=m.attributes,S=p.attributes;for(let A of S)_(A.name,m,"update",l)||m.getAttribute(A.name)!==A.value&&m.setAttribute(A.name,A.value);for(let A=f.length-1;0<=A;A--){let R=f[A];if(R&&!p.hasAttribute(R.name)){if(_(R.name,m,"remove",l))continue;m.removeAttribute(R.name)}}g(m,l)||y(m,p,l)}(d===8||d===3)&&a.nodeValue!==c.nodeValue&&(a.nodeValue=c.nodeValue)}function y(a,c,l){if(a instanceof HTMLInputElement&&c instanceof HTMLInputElement&&c.type!=="file"){let d=c.value,m=a.value;x(a,c,"checked",l),x(a,c,"disabled",l),c.hasAttribute("value")?m!==d&&(_("value",a,"update",l)||(a.setAttribute("value",d),a.value=d)):_("value",a,"remove",l)||(a.value="",a.removeAttribute("value"))}else if(a instanceof HTMLOptionElement&&c instanceof HTMLOptionElement)x(a,c,"selected",l);else if(a instanceof HTMLTextAreaElement&&c instanceof HTMLTextAreaElement){let d=c.value,m=a.value;if(_("value",a,"update",l))return;d!==m&&(a.value=d),a.firstChild&&a.firstChild.nodeValue!==d&&(a.firstChild.nodeValue=d)}}function x(a,c,l,d){let m=c[l],p=a[l];if(m!==p){let f=_(l,a,"update",d);f||(a[l]=c[l]),m?f||a.setAttribute(l,""):_(l,a,"remove",d)||a.removeAttribute(l)}}function _(a,c,l,d){return a==="value"&&d.ignoreActiveValue&&c===document.activeElement?!0:d.callbacks.beforeAttributeUpdated(a,c,l)===!1}function g(a,c){return!!c.ignoreActiveValue&&a===document.activeElement&&a!==document.body}return T}();function o(T,E,y,x){if(T.head.block){let _=E.querySelector("head"),g=y.querySelector("head");if(_&&g){let a=h(_,g,T);return Promise.all(a).then(()=>{let c=Object.assign(T,{head:{block:!1,ignore:!0}});return x(c)})}}return x(T)}function h(T,E,y){let x=[],_=[],g=[],a=[],c=new Map;for(let d of E.children)c.set(d.outerHTML,d);for(let d of T.children){let m=c.has(d.outerHTML),p=y.head.shouldReAppend(d),f=y.head.shouldPreserve(d);m||f?p?_.push(d):(c.delete(d.outerHTML),g.push(d)):y.head.style==="append"?p&&(_.push(d),a.push(d)):y.head.shouldRemove(d)!==!1&&_.push(d)}a.push(...c.values());let l=[];for(let d of a){let m=document.createRange().createContextualFragment(d.outerHTML).firstChild;if(y.callbacks.beforeNodeAdded(m)!==!1){if("href"in m&&m.href||"src"in m&&m.src){let p,f=new Promise(function(S){p=S});m.addEventListener("load",function(){p()}),l.push(f)}T.appendChild(m),y.callbacks.afterNodeAdded(m),x.push(m)}}for(let d of _)y.callbacks.beforeNodeRemoved(d)!==!1&&(T.removeChild(d),y.callbacks.afterNodeRemoved(d));return y.head.afterHeadMorphed(T,{added:x,kept:g,removed:_}),l}let b=function(){function T(c,l,d){let{persistentIds:m,idMap:p}=g(c,l),f=E(d),S=f.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(S))throw`Do not understand how to morph style ${S}`;return{target:c,newContent:l,config:f,morphStyle:S,ignoreActive:f.ignoreActive,ignoreActiveValue:f.ignoreActiveValue,restoreFocus:f.restoreFocus,idMap:p,persistentIds:m,pantry:y(),callbacks:f.callbacks,head:f.head}}function E(c){let l=Object.assign({},e);return Object.assign(l,c),l.callbacks=Object.assign({},e.callbacks,c.callbacks),l.head=Object.assign({},e.head,c.head),l}function y(){let c=document.createElement("div");return c.hidden=!0,document.body.insertAdjacentElement("afterend",c),c}function x(c){let l=Array.from(c.querySelectorAll("[id]"));return c.id&&l.push(c),l}function _(c,l,d,m){for(let p of m)if(l.has(p.id)){let f=p;for(;f;){let S=c.get(f);if(S==null&&(S=new Set,c.set(f,S)),S.add(p.id),f===d)break;f=f.parentElement}}}function g(c,l){let d=x(c),m=x(l),p=a(d,m),f=new Map;_(f,p,c,d);let S=l.__idiomorphRoot||l;return _(f,p,S,m),{persistentIds:p,idMap:f}}function a(c,l){let d=new Set,m=new Map;for(let{id:f,tagName:S}of c)m.has(f)?d.add(f):m.set(f,S);let p=new Set;for(let{id:f,tagName:S}of l)p.has(f)?d.add(f):m.get(f)===S&&p.add(f);for(let f of d)p.delete(f);return p}return T}(),{normalizeElement:v,normalizeParent:N}=function(){let T=new WeakSet;function E(g){return g instanceof Document?g.documentElement:g}function y(g){if(g==null)return document.createElement("div");if(typeof g=="string")return y(_(g));if(T.has(g))return g;if(g instanceof Node){if(g.parentNode)return x(g);{let a=document.createElement("div");return a.append(g),a}}else{let a=document.createElement("div");for(let c of[...g])a.append(c);return a}}function x(g){return{childNodes:[g],querySelectorAll:a=>{let c=g.querySelectorAll(a);return g.matches(a)?[g,...c]:c},insertBefore:(a,c)=>g.parentNode.insertBefore(a,c),moveBefore:(a,c)=>g.parentNode.moveBefore(a,c),get __idiomorphRoot(){return g}}}function _(g){let a=new DOMParser,c=g.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(c.match(/<\/html>/)||c.match(/<\/head>/)||c.match(/<\/body>/)){let l=a.parseFromString(g,"text/html");if(c.match(/<\/html>/))return T.add(l),l;{let d=l.firstChild;return d&&T.add(d),d}}else{let d=a.parseFromString("<body><template>"+g+"</template></body>","text/html").body.querySelector("template").content;return T.add(d),d}}return{normalizeElement:E,normalizeParent:y}}();return{morph:n,defaults:e}}();var It={type:2,name:k.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");U(k.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=Ze,settleDuration:s=`${ye}`,useViewTransition:u=`${Se}`})=>{let o=Number.parseInt(s),h=K(u);e.innerHTML=n.trim();let b=[...e.content.children];for(let v of b){if(!(v instanceof Element))throw H("NoFragmentsFound",t);let N=r||`#${v.getAttribute("id")}`,T=[...document.querySelectorAll(N)||[]];if(!T.length)throw H("NoTargetsFound",t,{selectorOrID:N});h&&z?ge.startViewTransition(()=>Pt(t,i,o,v,T)):Pt(t,i,o,v,T)}})}};function Pt(t,e,n,r,i){for(let s of i){s.classList.add(Q);let u=s.outerHTML,o=s;switch(e){case W.Morph:{Ct.morph(o,r.cloneNode(!0));break}case W.Inner:o.innerHTML=r.outerHTML;break;case W.Outer:o.replaceWith(r);break;case W.Prepend:o.prepend(r);break;case W.Append:o.append(r);break;case W.Before:o.before(r);break;case W.After:o.after(r);break;case W.UpsertAttributes:for(let v of r.getAttributeNames()){let N=r.getAttribute(v);o.setAttribute(v,N)}break;default:throw H("InvalidMergeMode",t,{mergeMode:e})}let h=o.classList;h?.add(Q),setTimeout(()=>{s.classList.remove(Q),h?.remove(Q)},n);let b=o.outerHTML;h&&u!==b&&(h.add(Ge),setTimeout(()=>{h.remove(Ge)},n))}}var Lt={type:2,name:k.MergeSignals,onGlobalInit:async t=>{U(k.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${Xe}`})=>{let{signals:r}=t,i=K(n);r.merge(be(e),i)})}};var Vt={type:2,name:k.RemoveFragments,onGlobalInit:async t=>{U(k.RemoveFragments,({selector:e,settleDuration:n=`${ye}`,useViewTransition:r=`${Se}`})=>{if(!e.length)throw H("NoSelectorProvided",t);let i=Number.parseInt(n),s=K(r),u=document.querySelectorAll(e),o=()=>{for(let h of u)h.classList.add(Q);setTimeout(()=>{for(let h of u)h.remove()},i)};s&&z?ge.startViewTransition(()=>o()):o()})}};var Dt={type:2,name:k.RemoveSignals,onGlobalInit:async t=>{U(k.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw H("NoPathsProvided",t);t.signals.remove(...n)})}};var kt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw M("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var Ot={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw M("CustomValidityInvalidElement",t);let i=n();return r(()=>{let s=i();if(typeof s!="string")throw M("CustomValidityInvalidExpression",t,{result:s});e.setCustomValidity(s)})}};var Ht="once",Ft="half",qt="full",Wt={type:1,name:"intersects",keyReq:2,mods:new Set([Ht,Ft,qt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(qt)?i.threshold=1:n.has(Ft)&&(i.threshold=.5);let s=r(),u=new IntersectionObserver(o=>{for(let h of o)h.isIntersecting&&(s(),n.has(Ht)&&(u.disconnect(),delete t.dataset[e]))},i);return u.observe(t),()=>u.disconnect()}};var $t="session",Gt={type:1,name:"persist",mods:new Set([$t]),onLoad:({key:t,effect:e,mods:n,signals:r,value:i})=>{t=V(t,n),t===""&&(t=D);let s=n.has($t)?sessionStorage:localStorage,u=i.split(/\s+/).filter(b=>b!=="");u=u.map(b=>J(b));let o=()=>{let b=s.getItem(t)||"{}",v=JSON.parse(b);r.merge(v)},h=()=>{let b;u.length?b=r.subset(...u):b=r.values(),s.setItem(t,JSON.stringify(b))};return o(),e(()=>{h()})}};var Ut={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,s=new URL(r,i).toString();window.history.replaceState({},"",s)})}};var Ce="smooth",Ue="instant",Be="auto",Bt="hstart",jt="hcenter",Kt="hend",Jt="hnearest",zt="vstart",Yt="vcenter",Xt="vend",Qt="vnearest",Hn="focus",Pe="center",Zt="start",en="end",tn="nearest",nn={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ce,Ue,Be,Bt,jt,Kt,Jt,zt,Yt,Xt,Qt,Hn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:Ce,block:Pe,inline:Pe};if(n.has(Ce)&&(i.behavior=Ce),n.has(Ue)&&(i.behavior=Ue),n.has(Be)&&(i.behavior=Be),n.has(Bt)&&(i.inline=Zt),n.has(jt)&&(i.inline=Pe),n.has(Kt)&&(i.inline=en),n.has(Jt)&&(i.inline=tn),n.has(zt)&&(i.block=Zt),n.has(Yt)&&(i.block=Pe),n.has(Xt)&&(i.block=en),n.has(Qt)&&(i.block=tn),!(e instanceof HTMLElement||e instanceof SVGElement))throw M("ScrollIntoViewInvalidElement",t);return e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r],()=>{}}};var rn="none",on="display",sn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===rn&&t.removeProperty(on):t.setProperty(on,rn)})}};var an="view-transition",ln={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===an&&(t=!0);if(!t){let e=document.createElement("meta");e.name=an,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!z){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let s=e.style;s.viewTransitionName=i})}};var un={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let i=r();return e===""?n(async()=>{let s=i();for(let[u,o]of Object.entries(s))o===!1?t.removeAttribute(u):t.setAttribute(u,o)}):(e=ue(e),n(async()=>{let s=!1;try{s=i()}catch{}let u;typeof s=="string"?u=s:u=JSON.stringify(s),!u||u==="false"||u==="null"||u==="undefined"?t.removeAttribute(e):t.setAttribute(e,u)}))}};var Fn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,cn=["change","input","keydown"],fn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:i,value:s,effect:u}=t,o=n?V(n,r):J(s),h=e.tagName.toLowerCase(),b="",v=h.includes("input"),N=e.getAttribute("type"),T=h.includes("checkbox")||v&&N==="checkbox";T&&(b=!1),v&&N==="number"&&(b=0);let y=h.includes("select"),x=h.includes("radio")||v&&N==="radio",_=v&&N==="file";x&&(e.getAttribute("name")?.length||e.setAttribute("name",o));let g=()=>{let m="value"in e,p=i.value(o),f=`${p}`;if(T||x){let S=e;Array.isArray(p)?S.checked=p.includes(S.value):typeof p=="boolean"?S.checked=!!p:S.checked=f===S.value}else if(y){let S=e;if(S.multiple)for(let A of S.options){if(A?.disabled)return;Array.isArray(p)||typeof p=="string"?A.selected=p.includes(A.value):typeof p=="number"?A.selected=p===Number(A.value):A.selected=p}else S.value=f}else _||(m?e.value=f:e.setAttribute("value",f))},a=async()=>{if(_){let S=[...e?.files||[]],A=[],R=[],I=[];await Promise.all(S.map(F=>new Promise(L=>{let q=new FileReader;q.onload=()=>{if(typeof q.result!="string")throw M("InvalidFileResultType",t,{resultType:typeof q.result});let j=q.result.match(Fn);if(!j?.groups)throw M("InvalidDataUri",t,{result:q.result});A.push(j.groups.contents),R.push(j.groups.mime),I.push(F.name)},q.onloadend=()=>L(void 0),q.readAsDataURL(F)}))),i.setValue(o,A),i.setValue(`${o}Mimes`,R),i.setValue(`${o}Names`,I);return}let m=i.value(o),p=e||e,f=p.value||p.getAttribute("value")||"";if(T){let S=p.checked||p.getAttribute("checked")==="true";if(Array.isArray(m)){let A=new Set(m);S?A.add(p.value):A.delete(p.value),i.setValue(o,[...A])}else if(p.getAttribute("value")){let R=S?f:!1;i.setValue(o,R)}else i.setValue(o,S);return}if(typeof m=="number")i.setValue(o,Number(f));else if(typeof m=="string")i.setValue(o,f||"");else if(typeof m=="boolean")i.setValue(o,!!f);else if(Array.isArray(m))if(y){let R=[...e.selectedOptions].filter(I=>I.selected).map(I=>I.value);i.setValue(o,R)}else i.setValue(o,JSON.stringify(f.split(",")));else if(!(typeof m>"u"))throw M("BindUnsupportedSignalType",t,{signalType:typeof m})},{inserted:c}=i.upsertIfMissing(o,b);c&&a();for(let m of cn)e.addEventListener(m,a);let l=u(()=>g()),d=m=>{m.persisted&&a()};return window.addEventListener("pageshow",d),()=>{l();for(let m of cn)e.removeEventListener(m,a);window.removeEventListener("pageshow",d)}}};var dn={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:i})=>{let s=t.classList,u=i();return r(()=>{if(e===""){let o=u();for(let[h,b]of Object.entries(o)){let v=h.split(/\s+/);b?s.add(...v):s.remove(...v)}}else e=V(e,n),u()?s.add(e):s.remove(e)})}};function he(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ae(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function pn(t,e,n=!1,r=!0){let i=-1,s=()=>i&&clearTimeout(i);return(...u)=>{s(),n&&!i&&t(...u),i=setTimeout(()=>{r&&t(...u),s()},e)}}function mn(t,e,n=!0,r=!1){let i=!1;return(...s)=>{i||(n&&t(...s),i=!0,setTimeout(()=>{i=!1,r&&t(...s)},e))}}var qn="evt",je="signalsChange",Wn=je.length,gn={type:1,name:"on",keyReq:1,valReq:1,argNames:[qn],onLoad:({el:t,key:e,mods:n,rawKey:r,signals:i,value:s,effect:u,genRX:o})=>{let h=o(),b=t;n.has("window")&&(b=window);let v=g=>{g&&((n.has("prevent")||e==="submit")&&g.preventDefault(),n.has("stop")&&g.stopPropagation()),h(g)},N=n.get("delay");if(N){let g=he(N);setTimeout(()=>{v()},g)}let T=n.get("debounce");if(T){let g=he(T),a=ae(T,"leading",!1),c=!ae(T,"notrail",!1);v=pn(v,g,a,c)}let E=n.get("throttle");if(E){let g=he(E),a=!ae(E,"noleading",!1),c=ae(E,"trail",!1);v=mn(v,g,a,c)}if(n.has("viewtransition")&&z){let g=v;v=(...a)=>document.startViewTransition(()=>g(...a))}let y={capture:!0,passive:!1,once:!1};if(n.has("capture")||(y.capture=!1),n.has("passive")&&(y.passive=!0),n.has("once")&&(y.once=!0),e==="load")return setTimeout(()=>v(),0),()=>{};if(e==="interval"){let g=1e3,a=n.get("duration");a&&(g=he(a),ae(a,"leading",!1)&&(t.dataset[r.replace(".leading","")]=s,delete t.dataset[r],v()));let c=setInterval(v,g);return()=>{clearInterval(c)}}if(e==="raf"){let g,a=()=>{v(),g=requestAnimationFrame(a)};return g=requestAnimationFrame(a),()=>{g&&cancelAnimationFrame(g)}}if(e.startsWith(je)){if(e===je){v();let c=l=>v(l);return document.addEventListener(le,c),()=>{document.removeEventListener(le,c)}}let g=V(Y(e.slice(Wn)),n),a=new Map;return i.walk((c,l)=>{c.startsWith(g)&&a.set(l,l.value)}),u(()=>{for(let[c,l]of a)l!==c.value&&(v(),a.set(c,c.value))})}if(n.has("outside")){b=document;let g=v;v=c=>{let l=c?.target;t.contains(l)||g(c)}}let _=V(e,n);return b.addEventListener(_,v,y),()=>{b.removeEventListener(_,v)}}};var hn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:i})=>{let s=e?V(e,n):J(i);r.setValue(s,t)}};var vn={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,i=r();return e instanceof HTMLElement||M("TextInvalidElement",t),n(()=>{let s=i(t);e.textContent=`${s}`})}};var{round:$n,max:Gn,min:Un}=Math,yn={type:3,name:"fit",fn:(t,e,n,r,i,s,u=!1,o=!1)=>{let h=(e-n)/(r-n)*(s-i)+i;return o&&(h=$n(h)),u&&(h=Gn(i,Un(s,h))),h}};var Sn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var bn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};we(un,fn,dn,gn,hn,sn,vn,Mt,_t,Rt,wt,xt,Et,It,Lt,Vt,Dt,Nt,kt,Ot,Wt,Gt,Ut,nn,ln,yn,Sn,bn);oe();export{oe as apply,we as load,ht as setAlias};
//# sourceMappingURL=datastar.js.map
