// Datastar v1.0.0-beta.1
var Be=/🖕JS_DS🚀/.source,de=Be.slice(0,5),Ce=Be.slice(4),L="datastar";var Ge="Datastar-Request",Ke="1.0.0-beta.1",pe=300;var Je="type module",me=!1,ze=!1,Xe=!0,O={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ye=O.Morph,I={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var p=(i=>(i[i.Macro=0]="Macro",i[i.Attribute=1]="Attribute",i[i.Watcher=2]="Watcher",i[i.Action=3]="Action",i))(p||{});var wn="computed",Ze={type:1,name:wn,keyReq:1,valReq:1,onLoad:({key:t,signals:e,genRX:n})=>{let r=n();e.setComputed(t,r)}};var $=t=>t.trim()==="true",U=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),Qe=t=>t.replace(/(?:^\w|[A-Z]|\b\w)/g,(e,n)=>n===0?e.toLowerCase():e.toUpperCase()).replace(/\s+/g,""),ge=t=>new Function(`return Object.assign({}, ${t})`)(),j=t=>t.startsWith("$")?t.slice(1):t;var et={type:1,name:"signals",valReq:1,removeOnLoad:!0,onLoad:t=>{let{key:e,genRX:n,signals:r,mods:i}=t,o=i.has("ifmissing");if(e!==""&&!o)r.setValue(e,n()());else{let s=ge(t.value);t.value=JSON.stringify(s);let l=n()();r.merge(l,o)}}};var tt={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var re=class{#e=0;#t;constructor(e=L){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}reset(){return this.#e=0,this}get value(){return this.#t+Math.abs(this.#e).toString(36)}};function nt(t){if(t.id)return t.id;let e=new re,n=t;for(;n.parentNode;){if(n.id){e.with(n.id);break}if(n===n.ownerDocument.documentElement)e.with(n.tagName);else{for(let r=1,i=t;i.previousElementSibling;i=i.previousElementSibling,r++)e.with(r);n=n.parentNode}n=n.parentNode}return e.value}function rt(t,e){let n=new MutationObserver(r=>{for(let i of r)for(let o of i.removedNodes)if(o===t){n.disconnect(),e();return}});n.observe(t.parentNode,{childList:!0})}var xn=`${window.location.origin}/errors`;function Ie(t,e,n={}){let r=new Error;e=e[0].toUpperCase()+e.slice(1),r.name=`${L} ${t} error ${e}`;let i=U(e).replaceAll("-","_"),o=new URLSearchParams({metadata:JSON.stringify(n)}).toString();return r.message=`for more info see ${xn}/${t}/${i}?${o}`,r}function P(t,e,n={}){return Ie("internal",e,Object.assign({from:t},n))}function V(t,e,n={}){let r={plugin:{name:e.plugin.name,type:p[e.plugin.type]}};return Ie("init",t,Object.assign(r,{metadata:n}))}function y(t,e,n={}){let r={plugin:{name:e.plugin.name,type:p[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},raw:{key:e.rawKey,value:e.rawValue},expression:{key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return Ie("runtime",t,Object.assign(r,n))}var G="preact-signals",Pn=Symbol.for("preact-signals"),F=1,X=2,oe=4,Z=8,he=16,Y=32;function De(){ye++}function Le(){if(ye>1){ye--;return}let t,e=!1;for(;ie!==void 0;){let n=ie;for(ie=void 0,ke++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~X,!(n._flags&Z)&&ot(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(ke=0,ye--,e)throw P(G,"BatchError, error",{error:t})}var E;var ie,ye=0,ke=0,ve=0;function it(t){if(E===void 0)return;let e=t._node;if(e===void 0||e._target!==E)return e={_version:0,_source:t,_prevSource:E._sources,_nextSource:void 0,_target:E,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},E._sources!==void 0&&(E._sources._nextSource=e),E._sources=e,t._node=e,E._flags&Y&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=E._sources,e._nextSource=void 0,E._sources._nextSource=e,E._sources=e),e}function w(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}w.prototype.brand=Pn;w.prototype._refresh=()=>!0;w.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};w.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};w.prototype.subscribe=function(t){return be(()=>{let e=this.value,n=E;E=void 0;try{t(e)}finally{E=n}})};w.prototype.valueOf=function(){return this.value};w.prototype.toString=function(){return`${this.value}`};w.prototype.toJSON=function(){return this.value};w.prototype.peek=function(){let t=E;E=void 0;try{return this.value}finally{E=t}};Object.defineProperty(w.prototype,"value",{get(){let t=it(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(ke>100)throw P(G,"SignalCycleDetected");this._value=t,this._version++,ve++,De();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{Le()}}}});function ot(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function st(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function at(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function K(t){w.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=ve-1,this._flags=oe}K.prototype=new w;K.prototype._refresh=function(){if(this._flags&=~X,this._flags&F)return!1;if((this._flags&(oe|Y))===Y||(this._flags&=~oe,this._globalVersion===ve))return!0;if(this._globalVersion=ve,this._flags|=F,this._version>0&&!ot(this))return this._flags&=~F,!0;let t=E;try{st(this),E=this;let e=this._fn();(this._flags&he||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~he,this._version++)}catch(e){this._value=e,this._flags|=he,this._version++}return E=t,at(this),this._flags&=~F,!0};K.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=oe|Y;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}w.prototype._subscribe.call(this,t)};K.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(w.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~Y;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};K.prototype._notify=function(){if(!(this._flags&X)){this._flags|=oe|X;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(K.prototype,"value",{get(){if(this._flags&F)throw P(G,"SignalCycleDetected");let t=it(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&he)throw P(G,"GetComputedError",{value:this._value});return this._value}});function lt(t){return new K(t)}function ut(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){De();let n=E;E=void 0;try{e()}catch(r){throw t._flags&=~F,t._flags|=Z,Oe(t),P(G,"CleanupEffectError",{error:r})}finally{E=n,Le()}}}function Oe(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,ut(t)}function Mn(t){if(E!==this)throw P(G,"EndEffectError");at(this),E=t,this._flags&=~F,this._flags&Z&&Oe(this),Le()}function se(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=Y}se.prototype._callback=function(){let t=this._start();try{if(this._flags&Z||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};se.prototype._start=function(){if(this._flags&F)throw P(G,"SignalCycleDetected");this._flags|=F,this._flags&=~Z,ut(this),st(this),De();let t=E;return E=this,Mn.bind(this,t)};se.prototype._notify=function(){this._flags&X||(this._flags|=X,this._nextBatchedEffect=ie,ie=this)};se.prototype._dispose=function(){this._flags|=Z,this._flags&F||Oe(this)};function be(t){let e=new se(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var ct="namespacedSignals";function ft(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof w?n[r]=i.value:n[r]=ft(i)}return n}function dt(t,e,n=!1){for(let r in e)if(Object.hasOwn(e,r)){if(r.match(/\_\_+/))throw P(ct,"InvalidSignalKey",{key:r});let i=e[r];if(i instanceof Object&&!Array.isArray(i))t[r]||(t[r]={}),dt(t[r],i,n);else{if(Object.hasOwn(t,r)){if(n)continue;let s=t[r];if(s instanceof w){s.value=i;continue}}t[r]=new w(i)}}}function pt(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof w?e(n,r):pt(r,(i,o)=>{e(`${n}.${i}`,o)})}}function Nn(t,...e){let n={};for(let r of e){let i=r.split("."),o=t,s=n;for(let l=0;l<i.length-1;l++){let c=i[l];if(!o[c])return{};s[c]||(s[c]={}),o=o[c],s=s[c]}let a=i[i.length-1];s[a]=o[a]}return n}var Se=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let s=0;s<n.length-1;s++){let a=n[s];if(!r[a])return null;r=r[a]}let i=n[n.length-1],o=r[i];if(!o)throw P(ct,"SignalNotFound",{path:e});return o}setSignal(e,n){let r=e.split("."),i=this.#e;for(let s=0;s<r.length-1;s++){let a=r[s];i[a]||(i[a]={}),i=i[a]}let o=r[r.length-1];i[o]=n}setComputed(e,n){let r=lt(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let r=this.upsertIfMissing(e,n);r.value=n}upsertIfMissing(e,n){let r=e.split("."),i=this.#e;for(let l=0;l<r.length-1;l++){let c=r[l];i[c]||(i[c]={}),i=i[c]}let o=r[r.length-1],s=i[o];if(s instanceof w)return s;let a=new w(n);return i[o]=a,a}remove(...e){for(let n of e){let r=n.split("."),i=this.#e;for(let s=0;s<r.length-1;s++){let a=r[s];if(!i[a])return;i=i[a]}let o=r[r.length-1];delete i[o]}}merge(e,n=!1){dt(this.#e,e,n)}subset(...e){return Nn(this.values(),...e)}walk(e){pt(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return ft(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var Ee=class{#e=new Se;#t=[];#o=[];#r={};#a=[];#n=new Map;get signals(){return this.#e}get version(){return Ke}load(...e){for(let n of e){let r=this,i={get signals(){return r.#e},effect:s=>be(s),actions:this.#r,apply:this.apply.bind(this),cleanup:this.#i.bind(this),plugin:n},o;switch(n.type){case 0:{this.#o.push(n);break}case 2:{let s=n;this.#a.push(s),o=s.onGlobalInit;break}case 3:{this.#r[n.name]=n;break}case 1:{let s=n;this.#t.push(s),o=s.onGlobalInit;break}default:throw V("InvalidPluginType",i)}o&&o(i)}this.apply(document.body)}apply(e){let n=new Set;this.#t.forEach((r,i)=>{this.#s(e,o=>{if(!("starIgnore"in o.dataset)){i||this.#i(o);for(let s in o.dataset){if(!s.startsWith(r.name))continue;let a=s.slice(r.name.length),[l,...c]=a.split(/\_\_+/),u=l.length>0;u&&(l.startsWith("-")?l=l.slice(1):l=l[0].toLowerCase()+l.slice(1));let f=`${o.dataset[s]}`||"",b=f.length>0,d=this,m={get signals(){return d.#e},effect:v=>be(v),apply:d.apply.bind(d),cleanup:d.#i.bind(d),actions:d.#r,genRX:()=>this.#l(m,...r.argNames||[]),plugin:r,el:o,rawKey:s,rawValue:f,key:l,value:f,mods:new Map},h=r.keyReq||0;if(u){if(h===2)throw y(`${r.name}KeyNotAllowed`,m)}else if(h===1)throw y(`${r.name}KeyRequired`,m);let A=r.valReq||0;if(b){if(A===2)throw y(`${r.name}ValueNotAllowed`,m)}else if(A===1)throw y(`${r.name}ValueRequired`,m);if(h===3||A===3){if(u&&b)throw y(`${r.name}KeyAndValueProvided`,m);if(!u&&!b)throw y(`${r.name}KeyOrValueRequired`,m)}o.id.length||(o.id=nt(o));for(let v of c){let[T,...S]=v.split(".");m.mods.set(Qe(T),new Set(S.map(R=>R.toLowerCase())))}n.clear();let x=[...r.macros?.pre||[],...this.#o,...r.macros?.post||[]];for(let v of x)n.has(v)||(n.add(v),m.value=v.fn(m,m.value));let k=r.onLoad(m);k&&(this.#n.has(o)||this.#n.set(o,{id:o.id,set:new Set}),this.#n.get(o)?.set.add(k)),r?.removeOnLoad&&delete o.dataset[s]}}})})}#l(e,...n){let r=e.value.split(/;|\n/).map(h=>h.trim()).filter(h=>h!==""),i=r.length-1;r[i].startsWith("return")||(r[i]=`return (${r[i]});`);let s=r.join(`;
`).trim(),a=new Map,l=new RegExp(`(?:${de})(.*?)(?:${Ce})`,"gm");for(let h of s.matchAll(l)){let A=h[1],x=new re("dsEscaped").with(A).value;a.set(x,A),s=s.replace(de+A+Ce,x)}let c=/@(\w*)\(/gm,u=s.matchAll(c),f=new Set;for(let h of u)f.add(h[1]);let b=new RegExp(`@(${Object.keys(this.#r).join("|")})\\(`,"gm");s=s.replaceAll(b,"ctx.actions.$1.fn(ctx,");let d=e.signals.paths();if(d.length){let h=new RegExp(`\\$(${d.join("|")})(\\W|$)`,"gm");s=s.replaceAll(h,"ctx.signals.signal('$1').value$2")}for(let[h,A]of a)s=s.replace(h,A);let m=`return (()=> {
${s}
})()`;e.fnContent=m;try{let h=new Function("ctx",...n,m);return(...A)=>{try{return h(e,...A)}catch(x){throw y("ExecuteExpression",e,{error:x.message})}}}catch(h){throw y("GenerateExpression",e,{error:h.message})}}#s(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;n(e);let r=e.firstElementChild;for(;r;)this.#s(r,n),r=r.nextElementSibling}#i(e){let n=this.#n.get(e);if(n){for(let r of n.set)r();this.#n.delete(e)}}};var mt=new Ee;mt.load(tt,et,Ze);var Ve=mt;async function Cn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function In(t){let e,n,r,i=!1;return function(s){e===void 0?(e=s,n=0,r=-1):e=Dn(e,s);let a=e.length,l=0;for(;n<a;){i&&(e[n]===10&&(l=++n),i=!1);let c=-1;for(;n<a&&c===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-l);break;case 13:i=!0;case 10:c=n;break}if(c===-1)break;t(e.subarray(l,c),r),l=n,r=-1}l===a?e=void 0:l!==0&&(e=e.subarray(l),n-=l)}}function kn(t,e,n){let r=gt(),i=new TextDecoder;return function(s,a){if(s.length===0)n?.(r),r=gt();else if(a>0){let l=i.decode(s.subarray(0,a)),c=a+(s[a+1]===32?2:1),u=i.decode(s.subarray(c));switch(l){case"data":r.data=r.data?`${r.data}
${u}`:u;break;case"event":r.event=u;break;case"id":t(r.id=u);break;case"retry":{let f=Number.parseInt(u,10);Number.isNaN(f)||e(r.retry=f);break}}}}}function Dn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function gt(){return{data:"",event:"",id:"",retry:void 0}}var Ln="text/event-stream",ht="last-event-id";function yt(t,e,{signal:n,headers:r,onopen:i,onmessage:o,onclose:s,onerror:a,openWhenHidden:l,fetch:c,retryInterval:u=1e3,retryScaler:f=2,retryMaxWaitMs:b=3e4,retryMaxCount:d=10,...m}){return new Promise((h,A)=>{let x=0,k={...r};k.accept||(k.accept=Ln);let v;function T(){v.abort(),document.hidden||D()}l||document.addEventListener("visibilitychange",T);let S=0;function R(){document.removeEventListener("visibilitychange",T),window.clearTimeout(S),v.abort()}n?.addEventListener("abort",()=>{R(),h()});let M=c??window.fetch,g=i??function(){};async function D(){v=new AbortController;try{let N=await M(e,{...m,headers:k,signal:v.signal});await g(N),await Cn(N.body,In(kn(_=>{_?k[ht]=_:delete k[ht]},_=>{u=_},o))),s?.(),R(),h()}catch(N){if(!v.signal.aborted)try{let _=a?.(N)??u;window.clearTimeout(S),S=window.setTimeout(D,_),u*=f,u=Math.min(u,b),x++,x>=d?(R(),A(y("SseMaxRetries",t,{retryMaxCount:d}))):console.error(`Datastar failed to reach ${m.method}: ${e.toString()} retry in ${_}ms`)}catch(_){R(),A(_)}}}D()})}var Q=`${L}-sse`,Fe=`${L}-settling`,J=`${L}-swapping`,Te="started",Ae="finished",vt="error";function H(t,e){document.addEventListener(Q,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function _e(t,e){document.dispatchEvent(new CustomEvent(Q,{detail:{type:t,argsRaw:e}}))}var bt=t=>`${t}`.includes("text/event-stream"),q=async(t,e,n,r)=>{let{el:{id:i},el:o,signals:s}=t,{headers:a,contentType:l,includeLocal:c,selector:u,openWhenHidden:f,retryInterval:b,retryScaler:d,retryMaxWaitMs:m,retryMaxCount:h,abort:A}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:1e3,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),x=e.toLowerCase(),k=()=>{};try{if(_e(Te,{elId:i}),!n?.length)throw y("SseNoUrlProvided",t,{action:x});let v={};v[Ge]=!0,l==="json"&&(v["Content-Type"]="application/json");let T=Object.assign({},v,a),S={method:e,headers:T,openWhenHidden:f,retryInterval:b,retryScaler:d,retryMaxWaitMs:m,retryMaxCount:h,signal:A,onopen:async g=>{if(g.status>=400){let D=g.status.toString();_e(vt,{status:D})}},onmessage:g=>{if(!g.event.startsWith(L))return;let D=g.event,N={},_=g.data.split(`
`);for(let ne of _){let ce=ne.indexOf(" "),je=ne.slice(0,ce),fe=N[je];fe||(fe=[],N[je]=fe);let Rn=ne.slice(ce+1).trim();fe.push(Rn)}let W={};for(let[ne,ce]of Object.entries(N))W[ne]=ce.join(`
`);_e(D,W)},onerror:g=>{if(bt(g))throw y("InvalidContentType",t,{url:n,error:g});g&&console.error(g.message)}},R=new URL(n,window.location.origin),M=new URLSearchParams(R.search);if(l==="json"){let g=s.JSON(!1,!c);e==="GET"?M.set(L,g):S.body=g}else if(l==="form"){let g=u?document.querySelector(u):o.closest("form");if(g===null)throw u?y("SseFormNotFound",t,{action:x,selector:u}):y("SseClosestFormNotFound",t,{action:x});if(o!==g){let N=_=>_.preventDefault();g.addEventListener("submit",N),k=()=>g.removeEventListener("submit",N)}if(!g.checkValidity()){g.reportValidity(),k();return}let D=new FormData(g);if(e==="GET"){let N=new URLSearchParams(D);for(let[_,W]of N)M.set(_,W)}else S.body=D}else throw y("SseInvalidContentType",t,{action:x,contentType:l});R.search=M.toString();try{await yt(t,R.toString(),S)}catch(g){if(!bt(g))throw y("SseFetchFailed",t,{method:e,url:n,error:g})}}finally{_e(Ae,{elId:i}),k()}};var St={type:3,name:"delete",fn:async(t,e,n)=>q(t,"DELETE",e,{...n})};var Et={type:3,name:"get",fn:async(t,e,n)=>q(t,"GET",e,{...n})};var Tt={type:3,name:"patch",fn:async(t,e,n)=>q(t,"PATCH",e,{...n})};var At={type:3,name:"post",fn:async(t,e,n)=>q(t,"POST",e,{...n})};var _t={type:3,name:"put",fn:async(t,e,n)=>q(t,"PUT",e,{...n})};var Rt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({value:t,signals:e,el:n,key:r})=>{let i=r||j(t),o=e.upsertIfMissing(i,!1),s=a=>{let{type:l,argsRaw:{elId:c}}=a.detail;if(c===n.id)switch(l){case Te:o.value=!0;break;case Ae:o.value=!1;break}};return document.addEventListener(Q,s),()=>{document.removeEventListener(Q,s)}}};var wt={type:2,name:I.ExecuteScript,onGlobalInit:async t=>{H(I.ExecuteScript,({autoRemove:e=`${Xe}`,attributes:n=Je,script:r})=>{let i=$(e);if(!r?.length)throw V("NoScriptProvided",t);let o=document.createElement("script");for(let s of n.split(`
`)){let a=s.indexOf(" "),l=a?s.slice(0,a):s,c=a?s.slice(a):"";o.setAttribute(l.trim(),c.trim())}o.text=r,document.head.appendChild(o),i&&o.remove()})}};var ae=document,ee=!!ae.startViewTransition;var te="idiomorph",we=new WeakSet;function Nt(t,e,n={}){t instanceof Document&&(t=t.documentElement);let r;typeof e=="string"?r=qn(e):r=e;let i=Wn(r),o=Vn(t,i,n);return Ct(t,i,o)}function Ct(t,e,n){if(n.head.block){let r=t.querySelector("head"),i=e.querySelector("head");if(r&&i){let o=kt(i,r,n);Promise.all(o).then(()=>{Ct(t,e,Object.assign(n,{head:{block:!1,ignore:!0}}))});return}}if(n.morphStyle==="innerHTML")return It(e,t,n),t.children;if(n.morphStyle==="outerHTML"||n.morphStyle==null){let r=Un(e,t,n);if(!r)throw P(te,"NoBestMatchFound",{old:t,new:e});let i=r?.previousSibling,o=r?.nextSibling,s=xe(t,r,n);return r?$n(i,s,o):[]}throw P(te,"InvalidMorphStyle",{style:n.morphStyle})}function xe(t,e,n){if(!(n.ignoreActive&&t===document.activeElement))if(e==null){if(n.callbacks.beforeNodeRemoved(t)===!1)return;t.remove(),n.callbacks.afterNodeRemoved(t);return}else{if(Pe(t,e))return n.callbacks.beforeNodeMorphed(t,e)===!1?void 0:(t instanceof HTMLHeadElement&&n.head.ignore||(e instanceof HTMLHeadElement&&t instanceof HTMLHeadElement&&n.head.style!==O.Morph?kt(e,t,n):(On(e,t),It(e,t,n))),n.callbacks.afterNodeMorphed(t,e),t);if(n.callbacks.beforeNodeRemoved(t)===!1||n.callbacks.beforeNodeAdded(e)===!1)return;if(!t.parentElement)throw P(te,"NoParentElementFound",{oldNode:t});return t.parentElement.replaceChild(e,t),n.callbacks.afterNodeAdded(e),n.callbacks.afterNodeRemoved(t),e}}function It(t,e,n){let r=t.firstChild,i=e.firstChild,o;for(;r;){if(o=r,r=o.nextSibling,i==null){if(n.callbacks.beforeNodeAdded(o)===!1)return;e.appendChild(o),n.callbacks.afterNodeAdded(o),z(n,o);continue}if(Dt(o,i,n)){xe(i,o,n),i=i.nextSibling,z(n,o);continue}let s=Fn(t,e,o,i,n);if(s){i=xt(i,s,n),xe(s,o,n),z(n,o);continue}let a=Hn(t,o,i,n);if(a){i=xt(i,a,n),xe(a,o,n),z(n,o);continue}if(n.callbacks.beforeNodeAdded(o)===!1)return;e.insertBefore(o,i),n.callbacks.afterNodeAdded(o),z(n,o)}for(;i!==null;){let s=i;i=i.nextSibling,Lt(s,n)}}function On(t,e){let n=t.nodeType;if(n===1){for(let r of t.attributes)e.getAttribute(r.name)!==r.value&&e.setAttribute(r.name,r.value);for(let r of e.attributes)t.hasAttribute(r.name)||e.removeAttribute(r.name)}if((n===Node.COMMENT_NODE||n===Node.TEXT_NODE)&&e.nodeValue!==t.nodeValue&&(e.nodeValue=t.nodeValue),t instanceof HTMLInputElement&&e instanceof HTMLInputElement&&t.type!=="file")e.value=t.value||"",Re(t,e,"value"),Re(t,e,"checked"),Re(t,e,"disabled");else if(t instanceof HTMLOptionElement)Re(t,e,"selected");else if(t instanceof HTMLTextAreaElement&&e instanceof HTMLTextAreaElement){let r=t.value,i=e.value;r!==i&&(e.value=r),e.firstChild&&e.firstChild.nodeValue!==r&&(e.firstChild.nodeValue=r)}}function Re(t,e,n){let r=t.getAttribute(n),i=e.getAttribute(n);r!==i&&(r?e.setAttribute(n,r):e.removeAttribute(n))}function kt(t,e,n){let r=[],i=[],o=[],s=[],a=n.head.style,l=new Map;for(let u of t.children)l.set(u.outerHTML,u);for(let u of e.children){let f=l.has(u.outerHTML),b=n.head.shouldReAppend(u),d=n.head.shouldPreserve(u);f||d?b?i.push(u):(l.delete(u.outerHTML),o.push(u)):a===O.Append?b&&(i.push(u),s.push(u)):n.head.shouldRemove(u)!==!1&&i.push(u)}s.push(...l.values());let c=[];for(let u of s){let f=document.createRange().createContextualFragment(u.outerHTML).firstChild;if(!f)throw P(te,"NewElementCouldNotBeCreated",{newNode:u});if(n.callbacks.beforeNodeAdded(f)){if(f.hasAttribute("href")||f.hasAttribute("src")){let b,d=new Promise(m=>{b=m});f.addEventListener("load",()=>{b(void 0)}),c.push(d)}e.appendChild(f),n.callbacks.afterNodeAdded(f),r.push(f)}}for(let u of i)n.callbacks.beforeNodeRemoved(u)!==!1&&(e.removeChild(u),n.callbacks.afterNodeRemoved(u));return n.head.afterHeadMorphed(e,{added:r,kept:o,removed:i}),c}function B(){}function Vn(t,e,n){return{target:t,newContent:e,config:n,morphStyle:n.morphStyle,ignoreActive:n.ignoreActive,idMap:Kn(t,e),deadIds:new Set,callbacks:Object.assign({beforeNodeAdded:B,afterNodeAdded:B,beforeNodeMorphed:B,afterNodeMorphed:B,beforeNodeRemoved:B,afterNodeRemoved:B},n.callbacks),head:Object.assign({style:"merge",shouldPreserve:r=>r.getAttribute("im-preserve")==="true",shouldReAppend:r=>r.getAttribute("im-re-append")==="true",shouldRemove:B,afterHeadMorphed:B},n.head)}}function Dt(t,e,n){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName?t?.id?.length&&t.id===e.id?!0:le(n,t,e)>0:!1}function Pe(t,e){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName}function xt(t,e,n){for(;t!==e;){let r=t;if(t=t?.nextSibling,!r)throw P(te,"NoTemporaryNodeFound",{startInclusive:t,endExclusive:e});Lt(r,n)}return z(n,e),e.nextSibling}function Fn(t,e,n,r,i){let o=le(i,n,e),s=null;if(o>0){s=r;let a=0;for(;s!=null;){if(Dt(n,s,i))return s;if(a+=le(i,s,t),a>o)return null;s=s.nextSibling}}return s}function Hn(t,e,n,r){let i=n,o=e.nextSibling,s=0;for(;i&&o;){if(le(r,i,t)>0)return null;if(Pe(e,i))return i;if(Pe(o,i)&&(s++,o=o.nextSibling,s>=2))return null;i=i.nextSibling}return i}var Pt=new DOMParser;function qn(t){let e=t.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(e.match(/<\/html>/)||e.match(/<\/head>/)||e.match(/<\/body>/)){let i=Pt.parseFromString(t,"text/html");if(e.match(/<\/html>/))return we.add(i),i;let o=i.firstChild;return o?(we.add(o),o):null}let r=Pt.parseFromString(`<body><template>${t}</template></body>`,"text/html").body.querySelector("template")?.content;if(!r)throw P(te,"NoContentFound",{newContent:t});return we.add(r),r}function Wn(t){if(t==null)return document.createElement("div");if(we.has(t))return t;if(t instanceof Node){let n=document.createElement("div");return n.append(t),n}let e=document.createElement("div");for(let n of[...t])e.append(n);return e}function $n(t,e,n){let r=[],i=[];for(;t;)r.push(t),t=t.previousSibling;for(;r.length>0;){let o=r.pop();i.push(o),e?.parentElement?.insertBefore(o,e)}for(i.push(e);n;)r.push(n),i.push(n),n=n.nextSibling;for(;r.length;)e?.parentElement?.insertBefore(r.pop(),e.nextSibling);return i}function Un(t,e,n){let r=t.firstChild,i=r,o=0;for(;r;){let s=jn(r,e,n);s>o&&(i=r,o=s),r=r.nextSibling}return i}function jn(t,e,n){return Pe(t,e)?.5+le(n,t,e):0}function Lt(t,e){z(e,t),e.callbacks.beforeNodeRemoved(t)!==!1&&(t.remove(),e.callbacks.afterNodeRemoved(t))}function Bn(t,e){return!t.deadIds.has(e)}function Gn(t,e,n){return t.idMap.get(n)?.has(e)||!1}function z(t,e){let n=t.idMap.get(e);if(n)for(let r of n)t.deadIds.add(r)}function le(t,e,n){let r=t.idMap.get(e);if(!r)return 0;let i=0;for(let o of r)Bn(t,o)&&Gn(t,o,n)&&++i;return i}function Mt(t,e){let n=t.parentElement,r=t.querySelectorAll("[id]");for(let i of r){let o=i;for(;o!==n&&o;){let s=e.get(o);s==null&&(s=new Set,e.set(o,s)),s.add(i.id),o=o.parentElement}}}function Kn(t,e){let n=new Map;return Mt(t,n),Mt(e,n),n}var Vt={type:2,name:I.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");H(I.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=Ye,settleDuration:o=`${pe}`,useViewTransition:s=`${me}`})=>{let a=Number.parseInt(o),l=$(s);e.innerHTML=n.trim();let c=[...e.content.children];for(let u of c){if(!(u instanceof Element))throw V("NoFragmentsFound",t);let f=r||`#${u.getAttribute("id")}`,b=[...document.querySelectorAll(f)||[]];if(!b.length)throw V("NoTargetsFound",t,{selectorOrID:f});ee&&l?ae.startViewTransition(()=>Ot(t,i,a,u,b)):Ot(t,i,a,u,b)}})}};function Ot(t,e,n,r,i){for(let o of i){o.classList.add(J);let s=o.outerHTML,a=o;switch(e){case O.Morph:{let u=Nt(a,r,{callbacks:{beforeNodeRemoved:(f,b)=>(t.cleanup(f),!0)}});if(!u?.length)throw V("MorphFailed",t);a=u[0];break}case O.Inner:a.innerHTML=r.innerHTML;break;case O.Outer:a.replaceWith(r);break;case O.Prepend:a.prepend(r);break;case O.Append:a.append(r);break;case O.Before:a.before(r);break;case O.After:a.after(r);break;case O.UpsertAttributes:for(let u of r.getAttributeNames()){let f=r.getAttribute(u);a.setAttribute(u,f)}break;default:throw V("InvalidMergeMode",t,{mergeMode:e})}t.cleanup(a);let l=a.classList;l.add(J),t.apply(document.body),setTimeout(()=>{o.classList.remove(J),l.remove(J)},n);let c=a.outerHTML;s!==c&&(l.add(Fe),setTimeout(()=>{l.remove(Fe)},n))}}var Ft={type:2,name:I.MergeSignals,onGlobalInit:async t=>{H(I.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${ze}`})=>{let{signals:r}=t,i=$(n);r.merge(ge(e),i),t.apply(document.body)})}};var Ht={type:2,name:I.RemoveFragments,onGlobalInit:async t=>{H(I.RemoveFragments,({selector:e,settleDuration:n=`${pe}`,useViewTransition:r=`${me}`})=>{if(!e.length)throw V("NoSelectorProvided",t);let i=Number.parseInt(n),o=$(r),s=document.querySelectorAll(e),a=()=>{for(let l of s)l.classList.add(J);setTimeout(()=>{for(let l of s)l.remove()},i)};ee&&o?ae.startViewTransition(()=>a()):a()})}};var qt={type:2,name:I.RemoveSignals,onGlobalInit:async t=>{H(I.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw V("NoPathsProvided",t);t.signals.remove(...n),t.apply(document.body)})}};var Wt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw y("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var $t={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement))throw y("CustomValidityInvalidElement",t);let i=n();return r(()=>{let o=i();if(typeof o!="string")throw y("CustomValidityInvalidExpression",t,{result:o});e.setCustomValidity(o)})}};var Ut="once",jt="half",Bt="full",Gt={type:1,name:"intersects",keyReq:2,mods:new Set([Ut,jt,Bt]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(Bt)?i.threshold=1:n.has(jt)&&(i.threshold=.5);let o=r(),s=new IntersectionObserver(a=>{for(let l of a)l.isIntersecting&&(o(),n.has(Ut)&&(s.disconnect(),delete t.dataset[e]))},i);return s.observe(t),()=>s.disconnect()}};var Kt="session",Jt={type:1,name:"persist",mods:new Set([Kt]),onLoad:({key:t,value:e,signals:n,effect:r,mods:i})=>{t===""&&(t=L);let o=i.has(Kt)?sessionStorage:localStorage,s=e.split(/\s+/).filter(c=>c!=="");s=s.map(c=>j(c));let a=()=>{let c=o.getItem(t)||"{}",u=JSON.parse(c);n.merge(u)},l=()=>{let c;s.length?c=n.subset(...s):c=n.values(),o.setItem(t,JSON.stringify(c))};return a(),r(()=>{l()})}};var zt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,o=new URL(r,i).toString();window.history.replaceState({},"",o)})}};var Me="smooth",He="instant",qe="auto",Xt="hstart",Yt="hcenter",Zt="hend",Qt="hnearest",en="vstart",tn="vcenter",nn="vend",rn="vnearest",Jn="focus",Ne="center",on="start",sn="end",an="nearest",ln={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Me,He,qe,Xt,Yt,Zt,Qt,en,tn,nn,rn,Jn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let i={behavior:Me,block:Ne,inline:Ne};if(n.has(Me)&&(i.behavior=Me),n.has(He)&&(i.behavior=He),n.has(qe)&&(i.behavior=qe),n.has(Xt)&&(i.inline=on),n.has(Yt)&&(i.inline=Ne),n.has(Zt)&&(i.inline=sn),n.has(Qt)&&(i.inline=an),n.has(en)&&(i.block=on),n.has(tn)&&(i.block=Ne),n.has(nn)&&(i.block=sn),n.has(rn)&&(i.block=an),!(e instanceof HTMLElement||e instanceof SVGElement))throw y("NotHtmlSvgElement",t);return e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(i),n.has("focus")&&e.focus(),delete e.dataset[r],()=>{}}};var un="none",cn="display",fn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===un&&t.removeProperty(cn):t.setProperty(cn,un)})}};var We="view-transition",dn={type:1,name:We,keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===We&&(t=!0);if(!t){let e=document.createElement("meta");e.name=We,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!ee){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var pn={type:1,name:"attr",valReq:1,onLoad:({el:t,genRX:e,key:n,effect:r})=>{let i=e();return n===""?r(async()=>{let o=i();for(let[s,a]of Object.entries(o))t.setAttribute(s,a)}):(n=U(n),r(async()=>{let o=!1;try{o=i()}catch{}let s;typeof o=="string"?s=o:s=JSON.stringify(o),!s||s==="false"||s==="null"||s==="undefined"?t.removeAttribute(n):t.setAttribute(n,s)}))}};var zn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,mn=["change","input","keydown"],gn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,value:n,key:r,signals:i,effect:o}=t,s=r||j(n),a=()=>{},l=()=>{};if(typeof s!="string")throw y("InvalidExpression",t);let c=e.tagName.toLowerCase(),u="",f=c.includes("input"),b=e.getAttribute("type"),d=c.includes("checkbox")||f&&b==="checkbox";d&&(u=!1),f&&b==="number"&&(u=0);let h=c.includes("select"),A=c.includes("radio")||f&&b==="radio",x=f&&b==="file";A&&(e.getAttribute("name")?.length||e.setAttribute("name",s)),i.upsertIfMissing(s,u),a=()=>{let v="value"in e,T=i.value(s),S=`${T}`;if(d||A){let R=e;d?R.checked=!!T||T==="true":A&&(R.checked=S===R.value)}else if(!x)if(h){let R=e;if(R.multiple)for(let M of R.options){if(M?.disabled)return;Array.isArray(T)||typeof T=="string"?M.selected=T.includes(M.value):typeof T=="number"?M.selected=T===Number(M.value):M.selected=T}else R.value=S}else v?e.value=S:e.setAttribute("value",S)},l=async()=>{if(x){let S=[...e?.files||[]],R=[],M=[],g=[];await Promise.all(S.map(D=>new Promise(N=>{let _=new FileReader;_.onload=()=>{if(typeof _.result!="string")throw y("InvalidFileResultType",t,{type:typeof _.result});let W=_.result.match(zn);if(!W?.groups)throw y("InvalidDataUri",t,{result:_.result});R.push(W.groups.contents),M.push(W.groups.mime),g.push(D.name)},_.onloadend=()=>N(void 0),_.readAsDataURL(D)}))),i.setValue(s,R),i.setValue(`${s}Mimes`,M),i.setValue(`${s}Names`,g);return}let v=i.value(s),T=e||e;if(typeof v=="number"){let S=Number(T.value||T.getAttribute("value"));i.setValue(s,S)}else if(typeof v=="string"){let S=T.value||T.getAttribute("value")||"";i.setValue(s,S)}else if(typeof v=="boolean")if(d){let S=T.checked||T.getAttribute("checked")==="true";i.setValue(s,S)}else{let S=!!(T.value||T.getAttribute("value"));i.setValue(s,S)}else if(!(typeof v>"u"))if(Array.isArray(v))if(h){let M=[...e.selectedOptions].filter(g=>g.selected).map(g=>g.value);i.setValue(s,M)}else{let S=JSON.stringify(T.value.split(","));i.setValue(s,S)}else throw y("UnsupportedSignalType",t,{current:typeof v})};for(let v of mn)e.addEventListener(v,l);let k=o(()=>a());return()=>{k();for(let v of mn)e.removeEventListener(v,l)}}};var hn={type:1,name:"class",valReq:1,onLoad:({key:t,el:e,genRX:n,effect:r})=>{let i=e.classList,o=n();return r(()=>{if(t===""){let s=o();for(let[a,l]of Object.entries(s)){let c=a.split(/\s+/);l?i.add(...c):i.remove(...c)}}else{let s=o(),a=U(t);s?i.add(a):i.remove(a)}})}};function $e(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ue(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function yn(t,e,n=!1,r=!0){let i=-1,o=()=>i&&clearTimeout(i);return function(...a){o(),n&&!i&&t(...a),i=setTimeout(()=>{r&&t(...a),o()},e)}}function vn(t,e,n=!0,r=!1){let i=!1;return function(...s){i||(n&&t(...s),i=!0,setTimeout(()=>{i=!1,r&&t(...s)},e))}}var Ue=new Map,Xn="evt",bn={type:1,name:"on",keyReq:1,valReq:1,argNames:[Xn],onLoad:({el:t,key:e,genRX:n,mods:r,signals:i,effect:o})=>{let s=n(),a=t;r.has("window")&&(a=window);let l=d=>{d&&((r.has("prevent")||e==="submit")&&d.preventDefault(),r.has("stop")&&d.stopPropagation()),s(d)},c=r.get("debounce");if(c){let d=$e(c),m=ue(c,"leading",!1),h=!ue(c,"notrail",!1);l=yn(l,d,m,h)}let u=r.get("throttle");if(u){let d=$e(u),m=!ue(u,"noleading",!1),h=ue(u,"trail",!1);l=vn(l,d,m,h)}let f={capture:!0,passive:!1,once:!1};r.has("capture")||(f.capture=!1),r.has("passive")&&(f.passive=!0),r.has("once")&&(f.once=!0);let b=U(e).toLowerCase();switch(b){case"load":return l(),delete t.dataset.onLoad,()=>{};case"raf":{let d,m=()=>{l(),d=requestAnimationFrame(m)};return d=requestAnimationFrame(m),()=>{d&&cancelAnimationFrame(d)}}case"signals-change":return rt(t,()=>{Ue.delete(t.id)}),o(()=>{let d=r.has("remote"),m=i.JSON(!1,d);(Ue.get(t.id)||"")!==m&&(Ue.set(t.id,m),l())});default:{if(r.has("outside")){a=document;let m=l;l=A=>{let x=A?.target;t.contains(x)||m(A)}}return a.addEventListener(b,l,f),()=>{a.removeEventListener(b,l)}}}}};var Sn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,value:n,signals:r})=>{let i=e||j(n);return r.setValue(i,t),()=>r.setValue(i,null)}};var En={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t,i=n();return e instanceof HTMLElement||y("NotHtmlElement",t),r(()=>{let o=i(t);e.textContent=`${o}`})}};var{round:Yn,max:Zn,min:Qn}=Math,Tn={type:3,name:"fit",fn:(t,e,n,r,i,o,s=!1,a=!1)=>{let l=(e-n)/(r-n)*(o-i)+i;return a&&(l=Yn(l)),s&&(l=Zn(i,Qn(o,l))),l}};var An={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var _n={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Ve.load(gn,Rt,Sn,pn,hn,bn,fn,En,Et,At,_t,Tt,St,Vt,Ft,Ht,qt,wt,Wt,$t,Gt,Jt,zt,ln,dn,Tn,An,_n);var vs=Ve;export{vs as Datastar};
//# sourceMappingURL=datastar.js.map
