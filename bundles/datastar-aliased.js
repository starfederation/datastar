// Datastar v1.0.0-beta.7
var Ye=/🖕JS_DS🚀/.source,se=Ye.slice(0,5),ke=Ye.slice(4),D="datastar";var Xe="Datastar-Request",Se=300,Qe=1e3,Ze="type module",be=!1,et=!1,tt=!0,W={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},nt=W.Morph,k={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var M=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(M||{});var ae=`${D}-signals`;var j=t=>t.trim()==="true",le=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),B=t=>le(t).replace(/-./g,e=>e[1].toUpperCase()),Oe=t=>le(t).replace(/-/g,"_"),_n=t=>B(t).replace(/^./,e=>e[0].toUpperCase()),Te=t=>new Function(`return Object.assign({}, ${t})`)(),K=t=>t.startsWith("$")?t.slice(1):t,xn={kebab:le,snake:Oe,pascal:_n};function V(t,e){for(let n of e.get("case")||[]){let r=xn[n];r&&(t=r(t))}return t}var Rn="computed",rt={type:1,name:Rn,keyReq:1,valReq:1,onLoad:({key:t,mods:e,signals:n,genRX:r})=>{t=V(t,e);let o=r();n.setComputed(t,o)}};var ot={type:1,name:"signals",removeOnLoad:()=>!0,onLoad:t=>{let{key:e,mods:n,signals:r,value:o,genRX:l}=t,c=n.has("ifmissing");if(e!==""){let s=V(e,n),g=o===""?o:l()();c?r.upsertIfMissing(s,g):r.setValue(s,g)}else{let s=Te(t.value);t.value=JSON.stringify(s);let S=l()();r.merge(S,c)}}};var it={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ue=class{#e=0;#t;constructor(e=D){this.#t=e}with(e){if(typeof e=="string")for(let n of e.split(""))this.with(n.charCodeAt(0));else this.#e=(this.#e<<5)-this.#e+e;return this}get value(){return this.#t+Math.abs(this.#e).toString(36)}};function Ae(t){if(t.id)return t.id;let e=new ue,n=t;for(;n;){if(n.id){e.with(n.id);break}let r=n?.parentNode;r?e.with([...r.children].indexOf(n)):e.with(n.tagName),n=r}return e.value}function ce(t,e){if(!t||!(t instanceof HTMLElement||t instanceof SVGElement))return null;let n=t.dataset;if("starIgnore"in n)return null;"starIgnore__self"in n||e(t);let r=t.firstElementChild;for(;r;)ce(r,e),r=r.nextElementSibling}function Ee(t,e){return(...n)=>{setTimeout(()=>{t(...n)},e)}}function st(t,e,n=!1,r=!0){let o=-1,l=()=>o&&clearTimeout(o);return(...c)=>{l(),n&&!o&&t(...c),o=setTimeout(()=>{r&&t(...c),l()},e)}}function at(t,e,n=!0,r=!1){let o=!1;return(...l)=>{o||(n&&t(...l),o=!0,setTimeout(()=>{o=!1,r&&t(...l)},e))}}var wn="https://data-star.dev/errors";function Fe(t,e,n={}){let r=new Error;r.name=`${D} ${t} error`;let o=Oe(e),l=new URLSearchParams({metadata:JSON.stringify(n)}).toString(),c=JSON.stringify(n,null,2);return r.message=`${e}
More info: ${wn}/${t}/${o}?${l}
Context: ${c}`,r}function q(t,e,n={}){return Fe("internal",e,Object.assign({from:t},n))}function H(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]}};return Fe("init",t,Object.assign(r,n))}function N(t,e,n={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return Fe("runtime",t,Object.assign(r,n))}var Z="preact-signals",Mn=Symbol.for("preact-signals"),$=1,ee=2,de=4,ne=8,_e=16,te=32;function We(){xe++}function qe(){if(xe>1){xe--;return}let t,e=!1;for(;fe!==void 0;){let n=fe;for(fe=void 0,He++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~ee,!(n._flags&ne)&&ut(n))try{n._callback()}catch(o){e||(t=o,e=!0)}n=r}}if(He=0,xe--,e)throw t}var P;var fe,xe=0,He=0,Re=0;function lt(t){if(P===void 0)return;let e=t._node;if(e===void 0||e._target!==P)return e={_version:0,_source:t,_prevSource:P._sources,_nextSource:void 0,_target:P,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},P._sources!==void 0&&(P._sources._nextSource=e),P._sources=e,t._node=e,P._flags&te&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=P._sources,e._nextSource=void 0,P._sources._nextSource=e,P._sources=e),e}function I(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}I.prototype.brand=Mn;I.prototype._refresh=()=>!0;I.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};I.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};I.prototype.subscribe=function(t){return we(()=>{let e=this.value,n=P;P=void 0;try{t(e)}finally{P=n}})};I.prototype.valueOf=function(){return this.value};I.prototype.toString=function(){return`${this.value}`};I.prototype.toJSON=function(){return this.value};I.prototype.peek=function(){let t=P;P=void 0;try{return this.value}finally{P=t}};Object.defineProperty(I.prototype,"value",{get(){let t=lt(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(He>100)throw q(Z,"SignalCycleDetected");let e=this._value,n=t;this._value=t,this._version++,Re++,We();try{for(let r=this._targets;r!==void 0;r=r._nextTarget)r._target._notify()}finally{qe()}this?._onChange({old:e,revised:n})}}});function ut(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function ct(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ft(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function Y(t){I.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=Re-1,this._flags=de}Y.prototype=new I;Y.prototype._refresh=function(){if(this._flags&=~ee,this._flags&$)return!1;if((this._flags&(de|te))===te||(this._flags&=~de,this._globalVersion===Re))return!0;if(this._globalVersion=Re,this._flags|=$,this._version>0&&!ut(this))return this._flags&=~$,!0;let t=P;try{ct(this),P=this;let e=this._fn();(this._flags&_e||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~_e,this._version++)}catch(e){this._value=e,this._flags|=_e,this._version++}return P=t,ft(this),this._flags&=~$,!0};Y.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=de|te;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}I.prototype._subscribe.call(this,t)};Y.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(I.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~te;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};Y.prototype._notify=function(){if(!(this._flags&ee)){this._flags|=de|ee;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(Y.prototype,"value",{get(){if(this._flags&$)throw q(Z,"SignalCycleDetected");let t=lt(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&_e)throw q(Z,"GetComputedError",{value:this._value});return this._value}});function dt(t){return new Y(t)}function pt(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){We();let n=P;P=void 0;try{e()}catch(r){throw t._flags&=~$,t._flags|=ne,$e(t),q(Z,"CleanupEffectError",{error:r})}finally{P=n,qe()}}}function $e(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,pt(t)}function Nn(t){if(P!==this)throw q(Z,"EndEffectError");ft(this),P=t,this._flags&=~$,this._flags&ne&&$e(this),qe()}function pe(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=te}pe.prototype._callback=function(){let t=this._start();try{if(this._flags&ne||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};pe.prototype._start=function(){if(this._flags&$)throw q(Z,"SignalCycleDetected");this._flags|=$,this._flags&=~ne,pt(this),ct(this),We();let t=P;return P=this,Nn.bind(this,t)};pe.prototype._notify=function(){this._flags&ee||(this._flags|=ee,this._nextBatchedEffect=fe,fe=this)};pe.prototype._dispose=function(){this._flags|=ne,this._flags&$||$e(this)};function we(t){let e=new pe(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var mt="namespacedSignals",re=t=>{document.dispatchEvent(new CustomEvent(ae,{detail:Object.assign({added:[],removed:[],updated:[]},t)}))};function gt(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){if(e&&r.startsWith("_"))continue;let o=t[r];o instanceof I?n[r]=o.value:n[r]=gt(o)}return n}function ht(t,e,n=!1){let r={added:[],removed:[],updated:[]};for(let o in e)if(Object.hasOwn(e,o)){if(o.match(/\_\_+/))throw q(mt,"InvalidSignalKey",{key:o});let l=e[o];if(l instanceof Object&&!Array.isArray(l)){t[o]||(t[o]={});let c=ht(t[o],l,n);r.added.push(...c.added.map(s=>`${o}.${s}`)),r.removed.push(...c.removed.map(s=>`${o}.${s}`)),r.updated.push(...c.updated.map(s=>`${o}.${s}`))}else{if(Object.hasOwn(t,o)){if(n)continue;let g=t[o];if(g instanceof I){let S=g.value;g.value=l,S!==l&&r.updated.push(o);continue}}let s=new I(l);s._onChange=()=>{re({updated:[o]})},t[o]=s,r.added.push(o)}}return r}function vt(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof I?e(n,r):vt(r,(o,l)=>{e(`${n}.${o}`,l)})}}function Pn(t,...e){let n={};for(let r of e){let o=r.split("."),l=t,c=n;for(let g=0;g<o.length-1;g++){let S=o[g];if(!l[S])return{};c[S]||(c[S]={}),l=l[S],c=c[S]}let s=o[o.length-1];c[s]=l[s]}return n}var Me=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let c=0;c<n.length-1;c++){let s=n[c];if(!r[s])return null;r=r[s]}let o=n[n.length-1],l=r[o];if(!l)throw q(mt,"SignalNotFound",{path:e});return l}setSignal(e,n){let r=e.split("."),o=this.#e;for(let c=0;c<r.length-1;c++){let s=r[c];o[s]||(o[s]={}),o=o[s]}let l=r[r.length-1];o[l]=n}setComputed(e,n){let r=dt(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let r=this.upsertIfMissing(e,n),o=r.value;r.value=n,o!==n&&re({updated:[e]})}upsertIfMissing(e,n){let r=e.split("."),o=this.#e;for(let g=0;g<r.length-1;g++){let S=r[g];o[S]||(o[S]={}),o=o[S]}let l=r[r.length-1],c=o[l];if(c instanceof I)return c;let s=new I(n);return s._onChange=()=>{re({updated:[e]})},o[l]=s,re({added:[e]}),s}remove(...e){if(!e.length){this.#e={};return}let n=Array();for(let r of e){let o=r.split("."),l=this.#e;for(let s=0;s<o.length-1;s++){let g=o[s];if(!l[g])return;l=l[g]}let c=o[o.length-1];delete l[c],n.push(r)}re({removed:n})}merge(e,n=!1){let r=ht(this.#e,e,n);(r.added.length||r.removed.length||r.updated.length)&&re(r)}subset(...e){return Pn(this.values(),...e)}walk(e){vt(this.#e,e)}paths(){let e=new Array;return this.walk(n=>e.push(n)),e}values(e=!1){return gt(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var yt=(t,e)=>`${t}${se}${e}`,St=new Me,Ue=[],Ne={},Cn=[],Pe="";function Be(t){Pe=t}var Ge=null,J=new Map;function me(...t){for(let e of t){let n={get signals(){return St},effect:o=>we(o),actions:Ne,plugin:e,apply:Ce},r;switch(e.type){case 2:{let o=e;Cn.push(o),r=o.onGlobalInit;break}case 3:{Ne[e.name]=e;break}case 1:{let o=e;Ue.push(o),r=o.onGlobalInit;break}default:throw H("InvalidPluginType",n)}r&&r(n)}Ue.sort((e,n)=>{let r=n.name.length-e.name.length;return r!==0?r:e.name.localeCompare(n.name)}),Ln()}var Ln=Ee(()=>{Ce(document.body),In()},1);function Ce(t){ce(t,e=>{let n=J.get(e);if(n){for(let[,r]of n)r();J.delete(e)}for(let r of Object.keys(e.dataset))bt(e,r)})}function In(){Ge||(Ge=new MutationObserver(t=>{for(let{target:e,type:n,attributeName:r,oldValue:o,addedNodes:l,removedNodes:c}of t)switch(n){case"childList":{for(let s of c){let g=s,S=J.get(g);if(S){for(let[h,w]of S)w();J.delete(g)}}for(let s of l)Ce(s)}break;case"attributes":{{let s="data-",g=s+(Pe?`${Pe}-`:"");if(!r?.startsWith(g))break;let S=e,h=B(r.slice(s.length));if(o!==null&&S.dataset[h]!==o){let w=J.get(S);if(w){let b=yt(h,o),T=w.get(b);T&&(T(),w.delete(b))}}h in S.dataset&&bt(S,h)}break}}}),Ge.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function bt(t,e){let n=e.slice(Pe.length),r=Ue.find(E=>n.startsWith(E.name));if(!r)return;let o=J.get(t);if(o)for(let[E,v]of o)E.startsWith(e)&&(v(),o.delete(E));t.id.length||(t.id=Ae(t));let[l,...c]=n.slice(r.name.length).split(/\_\_+/),s=l.length>0;s&&(l=B(l));let g=t.dataset[e]||"",S=g.length>0,h={get signals(){return St},apply:Ce,effect:E=>we(E),actions:Ne,genRX:()=>Vn(h,...r.argNames||[]),plugin:r,el:t,rawKey:n,key:l,value:g,mods:new Map},w=r.keyReq||0;if(s){if(w===2)throw N(`${r.name}KeyNotAllowed`,h)}else if(w===1)throw N(`${r.name}KeyRequired`,h);let b=r.valReq||0;if(S){if(b===2)throw N(`${r.name}ValueNotAllowed`,h)}else if(b===1)throw N(`${r.name}ValueRequired`,h);if(w===3||b===3){if(s&&S)throw N(`${r.name}KeyAndValueProvided`,h);if(!s&&!S)throw N(`${r.name}KeyOrValueRequired`,h)}for(let E of c){let[v,...y]=E.split(".");h.mods.set(B(v),new Set(y.map(i=>i.toLowerCase())))}let T=r.onLoad(h);if(T){let E=J.get(t);E||(E=new Map,J.set(t,E)),E.set(yt(e,g),T)}let A=r.removeOnLoad;A&&A(n)===!0&&delete t.dataset[e]}function Vn(t,...e){let n="",r=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,o=t.value.trim().match(r);if(o){let T=o.length-1,A=o[T].trim();A.startsWith("return")||(o[T]=`return (${A});`),n=o.join(`;
`)}let l=new Map,c=new RegExp(`(?:${se})(.*?)(?:${ke})`,"gm");for(let T of n.matchAll(c)){let A=T[1],E=new ue("dsEscaped").with(A).value;l.set(E,A),n=n.replace(se+A+ke,E)}let s=/@(\w*)\(/gm,g=n.matchAll(s),S=new Set;for(let T of g)S.add(T[1]);let h=new RegExp(`@(${Object.keys(Ne).join("|")})\\(`,"gm");n=n.replaceAll(h,"ctx.actions.$1.fn(ctx,");let w=t.signals.paths();if(w.length){let T=new RegExp(`\\$(${w.join("|")})(\\W|$)`,"gm");n=n.replaceAll(T,"ctx.signals.signal('$1').value$2")}for(let[T,A]of l)n=n.replace(T,A);let b=`return (()=> {
${n}
})()`;t.fnContent=b;try{let T=new Function("ctx",...e,b);return(...A)=>{try{return T(t,...A)}catch(E){throw N("ExecuteExpression",t,{error:E.message})}}}catch(T){throw N("GenerateExpression",t,{error:T.message})}}me(it,ot,rt);async function Dn(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function kn(t){let e,n,r,o=!1;return function(c){e===void 0?(e=c,n=0,r=-1):e=Fn(e,c);let s=e.length,g=0;for(;n<s;){o&&(e[n]===10&&(g=++n),o=!1);let S=-1;for(;n<s&&S===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-g);break;case 13:o=!0;case 10:S=n;break}if(S===-1)break;t(e.subarray(g,S),r),g=n,r=-1}g===s?e=void 0:g!==0&&(e=e.subarray(g),n-=g)}}function On(t,e,n){let r=Tt(),o=new TextDecoder;return function(c,s){if(c.length===0)n?.(r),r=Tt();else if(s>0){let g=o.decode(c.subarray(0,s)),S=s+(c[s+1]===32?2:1),h=o.decode(c.subarray(S));switch(g){case"data":r.data=r.data?`${r.data}
${h}`:h;break;case"event":r.event=h;break;case"id":t(r.id=h);break;case"retry":{let w=Number.parseInt(h,10);Number.isNaN(w)||e(r.retry=w);break}}}}}function Fn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function Tt(){return{data:"",event:"",id:"",retry:void 0}}var Hn="text/event-stream",At="last-event-id";function Et(t,e,{signal:n,headers:r,onopen:o,onmessage:l,onclose:c,onerror:s,openWhenHidden:g,fetch:S,retryInterval:h=1e3,retryScaler:w=2,retryMaxWaitMs:b=3e4,retryMaxCount:T=10,...A}){return new Promise((E,v)=>{let y=0,i={...r};i.accept||(i.accept=Hn);let d;function u(){d.abort(),document.hidden||_()}g||document.addEventListener("visibilitychange",u);let a=0;function m(){document.removeEventListener("visibilitychange",u),window.clearTimeout(a),d.abort()}n?.addEventListener("abort",()=>{m(),E()});let p=S??window.fetch,f=o??function(){};async function _(){d=new AbortController;try{let R=await p(e,{...A,headers:i,signal:d.signal});await f(R),await Dn(R.body,kn(On(x=>{x?i[At]=x:delete i[At]},x=>{h=x},l))),c?.(),m(),E()}catch(R){if(!d.signal.aborted)try{let x=s?.(R)??h;window.clearTimeout(a),a=window.setTimeout(_,x),h*=w,h=Math.min(h,b),y++,y>=T?(m(),v(N("SseMaxRetries",t,{retryMaxCount:T}))):console.error(`Datastar failed to reach ${A.method}: ${e.toString()} retry in ${x}ms`)}catch(x){m(),v(x)}}}_()})}var oe=`${D}-sse`,je=`${D}-settling`,X=`${D}-swapping`,Le="started",Ie="finished",_t="error",xt="retrying";function G(t,e){document.addEventListener(oe,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function ge(t,e){document.dispatchEvent(new CustomEvent(oe,{detail:{type:t,argsRaw:e}}))}var Rt=t=>`${t}`.includes("text/event-stream"),U=async(t,e,n,r)=>{let{el:{id:o},el:l,signals:c}=t,{headers:s,contentType:g,includeLocal:S,selector:h,openWhenHidden:w,retryInterval:b,retryScaler:T,retryMaxWaitMs:A,retryMaxCount:E,abort:v}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:Qe,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),y=e.toLowerCase(),i=()=>{};try{if(ge(Le,{elId:o}),!n?.length)throw N("SseNoUrlProvided",t,{action:y});let d={};d[Xe]=!0,g==="json"&&(d["Content-Type"]="application/json");let u=Object.assign({},d,s),a={method:e,headers:u,openWhenHidden:w,retryInterval:b,retryScaler:T,retryMaxWaitMs:A,retryMaxCount:E,signal:v,onopen:async f=>{if(f.status>=400){let _=f.status.toString();ge(_t,{status:_})}},onmessage:f=>{if(!f.event.startsWith(D))return;let _=f.event,R={},x=f.data.split(`
`);for(let F of x){let L=F.indexOf(" "),Q=F.slice(0,L),ye=R[Q];ye||(ye=[],R[Q]=ye);let En=F.slice(L+1);ye.push(En)}let C={};for(let[F,L]of Object.entries(R))C[F]=L.join(`
`);ge(_,C)},onerror:f=>{if(Rt(f))throw N("InvalidContentType",t,{url:n});f&&(console.error(f.message),ge(xt,{message:f.message}))}},m=new URL(n,window.location.origin),p=new URLSearchParams(m.search);if(g==="json"){let f=c.JSON(!1,!S);e==="GET"?p.set(D,f):a.body=f}else if(g==="form"){let f=h?document.querySelector(h):l.closest("form");if(f===null)throw h?N("SseFormNotFound",t,{action:y,selector:h}):N("SseClosestFormNotFound",t,{action:y});if(l!==f){let R=x=>x.preventDefault();f.addEventListener("submit",R),i=()=>f.removeEventListener("submit",R)}if(!f.checkValidity()){f.reportValidity(),i();return}let _=new FormData(f);if(e==="GET"){let R=new URLSearchParams(_);for(let[x,C]of R)p.set(x,C)}else a.body=_}else throw N("SseInvalidContentType",t,{action:y,contentType:g});m.search=p.toString();try{await Et(t,m.toString(),a)}catch(f){if(!Rt(f))throw N("SseFetchFailed",t,{method:e,url:n,error:f})}}finally{ge(Ie,{elId:o}),i()}};var wt={type:3,name:"delete",fn:async(t,e,n)=>U(t,"DELETE",e,{...n})};var Mt={type:3,name:"get",fn:async(t,e,n)=>U(t,"GET",e,{...n})};var Nt={type:3,name:"patch",fn:async(t,e,n)=>U(t,"PATCH",e,{...n})};var Pt={type:3,name:"post",fn:async(t,e,n)=>U(t,"POST",e,{...n})};var Ct={type:3,name:"put",fn:async(t,e,n)=>U(t,"PUT",e,{...n})};var Lt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:o})=>{let l=e?V(e,n):K(o),c=r.upsertIfMissing(l,!1),s=g=>{let{type:S,argsRaw:{elId:h}}=g.detail;if(h===t.id)switch(S){case Le:c.value=!0;break;case Ie:c.value=!1;break}};return document.addEventListener(oe,s),()=>{document.removeEventListener(oe,s)}}};var It={type:2,name:k.ExecuteScript,onGlobalInit:async t=>{G(k.ExecuteScript,({autoRemove:e=`${tt}`,attributes:n=Ze,script:r})=>{let o=j(e);if(!r?.length)throw H("NoScriptProvided",t);let l=document.createElement("script");for(let c of n.split(`
`)){let s=c.indexOf(" "),g=s?c.slice(0,s):c,S=s?c.slice(s):"";l.setAttribute(g.trim(),S.trim())}l.text=r,document.head.appendChild(l),o&&l.remove()})}};var he=document,z=!!he.startViewTransition;var Vt=function(){"use strict";let t=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:t,afterNodeAdded:t,beforeNodeMorphed:t,afterNodeMorphed:t,beforeNodeRemoved:t,afterNodeRemoved:t,beforeAttributeUpdated:t},head:{style:"merge",shouldPreserve:b=>b.getAttribute("im-preserve")==="true",shouldReAppend:b=>b.getAttribute("im-re-append")==="true",shouldRemove:t,afterHeadMorphed:t},restoreFocus:!0};function n(b,T,A={}){b=h(b);let E=w(T),v=S(b,E,A),y=o(v,()=>s(v,b,E,i=>i.morphStyle==="innerHTML"?(l(i,b,E),Array.from(b.childNodes)):r(i,b,E)));return v.pantry.remove(),y}function r(b,T,A){let E=w(T),v=Array.from(E.childNodes),y=v.indexOf(T),i=v.length-(y+1);return l(b,E,A,T,T.nextSibling),v=Array.from(E.childNodes),v.slice(y,v.length-i)}function o(b,T){if(!b.config.restoreFocus)return T();let A=document.activeElement;if(!(A instanceof HTMLInputElement||A instanceof HTMLTextAreaElement))return T();let{id:E,selectionStart:v,selectionEnd:y}=A,i=T();return E&&E!==document.activeElement?.id&&(A=b.target.querySelector(`#${E}`),A?.focus()),A&&v&&y&&A.setSelectionRange(v,y),i}let l=function(){function b(u,a,m,p=null,f=null){a instanceof HTMLTemplateElement&&m instanceof HTMLTemplateElement&&(a=a.content,m=m.content),p||=a.firstChild;for(let _ of m.childNodes){if(p&&p!=f){let x=A(u,_,p,f);if(x){x!==p&&v(u,p,x),c(x,_,u),p=x.nextSibling;continue}}if(_ instanceof Element&&u.persistentIds.has(_.id)){let x=y(a,_.id,p,u);c(x,_,u),p=x.nextSibling;continue}let R=T(a,_,p,u);R&&(p=R.nextSibling)}for(;p&&p!=f;){let _=p;p=p.nextSibling,E(u,_)}}function T(u,a,m,p){if(p.callbacks.beforeNodeAdded(a)===!1)return null;if(p.idMap.has(a)&&a instanceof Element){let f=document.createElement(a.tagName);return u.insertBefore(f,m),c(f,a,p),p.callbacks.afterNodeAdded(f),f}else{let f=document.importNode(a,!0);return u.insertBefore(f,m),p.callbacks.afterNodeAdded(f),f}}let A=function(){function u(p,f,_,R){let x=null,C=f.nextSibling,F=0,L=_;for(;L&&L!=R;){if(m(L,f)){if(a(p,L,f))return L;x===null&&(p.idMap.has(L)||(x=L))}x===null&&C&&m(L,C)&&(F++,C=C.nextSibling,F>=2&&(x=void 0)),L=L.nextSibling}return x||null}function a(p,f,_){let R=p.idMap.get(f),x=p.idMap.get(_);if(!x||!R)return!1;for(let C of R)if(x.has(C))return!0;return!1}function m(p,f){let _=p,R=f;return _.nodeType===R.nodeType&&_.tagName===R.tagName&&(!_.id||_.id===R.id)}return u}();function E(u,a){if(u.idMap.has(a)&&a instanceof Element)d(u.pantry,a,null);else{if(u.callbacks.beforeNodeRemoved(a)===!1)return;a.parentNode?.removeChild(a),u.callbacks.afterNodeRemoved(a)}}function v(u,a,m){let p=a;for(;p&&p!==m;){let f=p;p=p.nextSibling,E(u,f)}return p}function y(u,a,m,p){let f=p.target.querySelector(`#${a}`)||p.pantry.querySelector(`#${a}`);return i(f,p),d(u,f,m),f}function i(u,a){let m=u.id;for(;u=u.parentNode;){let p=a.idMap.get(u);p&&(p.delete(m),p.size||a.idMap.delete(u))}}function d(u,a,m){if(u.moveBefore)try{u.moveBefore(a,m)}catch{u.insertBefore(a,m)}else u.insertBefore(a,m)}return b}(),c=function(){function b(i,d,u){return u.ignoreActive&&i===document.activeElement?null:(u.callbacks.beforeNodeMorphed(i,d)===!1||(i instanceof HTMLHeadElement&&u.head.ignore||(i instanceof HTMLHeadElement&&u.head.style!=="morph"?g(i,d,u):(T(i,d,u),y(i,u)||l(u,i,d))),u.callbacks.afterNodeMorphed(i,d)),i)}function T(i,d,u){let a=d.nodeType;if(a===1){let m=i,p=d,f=m.attributes,_=p.attributes;for(let R of _)v(R.name,m,"update",u)||m.getAttribute(R.name)!==R.value&&m.setAttribute(R.name,R.value);for(let R=f.length-1;0<=R;R--){let x=f[R];if(x&&!p.hasAttribute(x.name)){if(v(x.name,m,"remove",u))continue;m.removeAttribute(x.name)}}y(m,u)||A(m,p,u)}(a===8||a===3)&&i.nodeValue!==d.nodeValue&&(i.nodeValue=d.nodeValue)}function A(i,d,u){if(i instanceof HTMLInputElement&&d instanceof HTMLInputElement&&d.type!=="file"){let a=d.value,m=i.value;E(i,d,"checked",u),E(i,d,"disabled",u),d.hasAttribute("value")?m!==a&&(v("value",i,"update",u)||(i.setAttribute("value",a),i.value=a)):v("value",i,"remove",u)||(i.value="",i.removeAttribute("value"))}else if(i instanceof HTMLOptionElement&&d instanceof HTMLOptionElement)E(i,d,"selected",u);else if(i instanceof HTMLTextAreaElement&&d instanceof HTMLTextAreaElement){let a=d.value,m=i.value;if(v("value",i,"update",u))return;a!==m&&(i.value=a),i.firstChild&&i.firstChild.nodeValue!==a&&(i.firstChild.nodeValue=a)}}function E(i,d,u,a){let m=d[u],p=i[u];if(m!==p){let f=v(u,i,"update",a);f||(i[u]=d[u]),m?f||i.setAttribute(u,""):v(u,i,"remove",a)||i.removeAttribute(u)}}function v(i,d,u,a){return i==="value"&&a.ignoreActiveValue&&d===document.activeElement?!0:a.callbacks.beforeAttributeUpdated(i,d,u)===!1}function y(i,d){return!!d.ignoreActiveValue&&i===document.activeElement&&i!==document.body}return b}();function s(b,T,A,E){if(b.head.block){let v=T.querySelector("head"),y=A.querySelector("head");if(v&&y){let i=g(v,y,b);return Promise.all(i).then(()=>{let d=Object.assign(b,{head:{block:!1,ignore:!0}});return E(d)})}}return E(b)}function g(b,T,A){let E=[],v=[],y=[],i=[],d=new Map;for(let a of T.children)d.set(a.outerHTML,a);for(let a of b.children){let m=d.has(a.outerHTML),p=A.head.shouldReAppend(a),f=A.head.shouldPreserve(a);m||f?p?v.push(a):(d.delete(a.outerHTML),y.push(a)):A.head.style==="append"?p&&(v.push(a),i.push(a)):A.head.shouldRemove(a)!==!1&&v.push(a)}i.push(...d.values());let u=[];for(let a of i){let m=document.createRange().createContextualFragment(a.outerHTML).firstChild;if(A.callbacks.beforeNodeAdded(m)!==!1){if("href"in m&&m.href||"src"in m&&m.src){let p,f=new Promise(function(_){p=_});m.addEventListener("load",function(){p()}),u.push(f)}b.appendChild(m),A.callbacks.afterNodeAdded(m),E.push(m)}}for(let a of v)A.callbacks.beforeNodeRemoved(a)!==!1&&(b.removeChild(a),A.callbacks.afterNodeRemoved(a));return A.head.afterHeadMorphed(b,{added:E,kept:y,removed:v}),u}let S=function(){function b(i,d,u){let a=T(u),{persistentIds:m,idMap:p}=y(i,d),f=a.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(f))throw`Do not understand how to morph style ${f}`;return{target:i,newContent:d,config:a,morphStyle:f,ignoreActive:a.ignoreActive,ignoreActiveValue:a.ignoreActiveValue,restoreFocus:a.restoreFocus,idMap:p,persistentIds:m,pantry:A(),callbacks:a.callbacks,head:a.head}}function T(i){let d=Object.assign({},e);return Object.assign(d,i),d.callbacks=Object.assign({},e.callbacks,i.callbacks),d.head=Object.assign({},e.head,i.head),d}function A(){let i=document.createElement("div");return i.hidden=!0,document.body.insertAdjacentElement("afterend",i),i}function E(i){let d=Array.from(i.querySelectorAll("[id]"));return i.id&&d.push(i),d}function v(i,d,u,a){for(let m of d)if(u.has(m.id)){let p=m;for(;p;){let f=a.get(p);if(f==null&&(f=new Set,a.set(p,f)),f.add(m.id),p===i)break;p=p.parentElement}}}function y(i,d){let u=new Map,a=new Set,m=E(i);for(let x of m){let C=x.id;u.has(C)?a.add(C):u.set(C,x.tagName)}let p=new Set,f=E(d);for(let x of f){let C=x.id,F=u.get(C);(p.has(C)||F&&F!==x.tagName)&&(a.add(C),p.delete(C)),F===x.tagName&&!a.has(C)&&p.add(C)}let _=new Map;v(i,m,p,_);let R=d;return d.__idiomorphDummyParent&&(R=d.childNodes[0]),v(R,f,p,_),{persistentIds:p,idMap:_}}return b}(),{normalizeElement:h,normalizeParent:w}=function(){let b=new WeakSet;function T(v){return v instanceof Document?v.documentElement:v}function A(v){if(v==null)return document.createElement("div");if(typeof v=="string")return A(E(v));if(b.has(v))return v;if(v instanceof Node){if(v.parentNode)return{childNodes:[v],querySelectorAll:y=>{let i=v.querySelectorAll(y);return v.matches(y)?[v,...i]:i},insertBefore:(y,i)=>v.parentNode.insertBefore(y,i),moveBefore:(y,i)=>v.parentNode.moveBefore(y,i),__idiomorphDummyParent:!0};{let y=document.createElement("div");return y.append(v),y}}else{let y=document.createElement("div");for(let i of[...v])y.append(i);return y}}function E(v){let y=new DOMParser,i=v.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(i.match(/<\/html>/)||i.match(/<\/head>/)||i.match(/<\/body>/)){let d=y.parseFromString(v,"text/html");if(i.match(/<\/html>/))return b.add(d),d;{let u=d.firstChild;return u&&b.add(u),u}}else{let u=y.parseFromString("<body><template>"+v+"</template></body>","text/html").body.querySelector("template").content;return b.add(u),u}}return{normalizeElement:T,normalizeParent:A}}();return{morph:n,defaults:e}}();var kt={type:2,name:k.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");G(k.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:o=nt,settleDuration:l=`${Se}`,useViewTransition:c=`${be}`})=>{let s=Number.parseInt(l),g=j(c);e.innerHTML=n.trim();let S=[...e.content.children];for(let h of S){if(!(h instanceof Element))throw H("NoFragmentsFound",t);let w=r||`#${h.getAttribute("id")}`,b=[...document.querySelectorAll(w)||[]];if(!b.length)throw H("NoTargetsFound",t,{selectorOrID:w});g&&z?he.startViewTransition(()=>Dt(t,o,s,h,b)):Dt(t,o,s,h,b)}})}};function Dt(t,e,n,r,o){for(let l of o){l.classList.add(X);let c=l.outerHTML,s=l;switch(e){case W.Morph:{let h=new Map,w=r.cloneNode(!0);ce(w,T=>{!T.id?.length&&Object.keys(T.dataset).length&&(T.id=Ae(T))});let b=Vt.morph(s,w,{restoreFocus:!0,callbacks:{beforeAttributeUpdated:(T,A,E)=>{if(E==="update"&&T.startsWith("data-")){let v=h.get(A);v||(v=[],h.set(A,v));let y=T.slice(5);v.push(B(y))}return!0}}});if(b?.length){s=b[0];for(let[T,A]of h.entries())for(let E of A)t.apply(T,E)}break}case W.Inner:s.innerHTML=r.outerHTML;break;case W.Outer:s.replaceWith(r);break;case W.Prepend:s.prepend(r);break;case W.Append:s.append(r);break;case W.Before:s.before(r);break;case W.After:s.after(r);break;case W.UpsertAttributes:for(let h of r.getAttributeNames()){let w=r.getAttribute(h);s.setAttribute(h,w)}break;default:throw H("InvalidMergeMode",t,{mergeMode:e})}let g=s.classList;g?.add(X),setTimeout(()=>{l.classList.remove(X),g?.remove(X)},n);let S=s.outerHTML;g&&c!==S&&(g.add(je),setTimeout(()=>{g.remove(je)},n))}}var Ot={type:2,name:k.MergeSignals,onGlobalInit:async t=>{G(k.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${et}`})=>{let{signals:r}=t,o=j(n);r.merge(Te(e),o)})}};var Ft={type:2,name:k.RemoveFragments,onGlobalInit:async t=>{G(k.RemoveFragments,({selector:e,settleDuration:n=`${Se}`,useViewTransition:r=`${be}`})=>{if(!e.length)throw H("NoSelectorProvided",t);let o=Number.parseInt(n),l=j(r),c=document.querySelectorAll(e),s=()=>{for(let g of c)g.classList.add(X);setTimeout(()=>{for(let g of c)g.remove()},o)};l&&z?he.startViewTransition(()=>s()):s()})}};var Ht={type:2,name:k.RemoveSignals,onGlobalInit:async t=>{G(k.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw H("NoPathsProvided",t);t.signals.remove(...n)})}};var Wt={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw N("ClipboardNotAvailable",t);navigator.clipboard.writeText(e)}};var qt={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw N("CustomValidityInvalidElement",t);let o=n();return r(()=>{let l=o();if(typeof l!="string")throw N("CustomValidityInvalidExpression",t,{result:l});e.setCustomValidity(l)})}};var $t="once",Gt="half",Ut="full",Bt={type:1,name:"intersects",keyReq:2,mods:new Set([$t,Gt,Ut]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let o={threshold:0};n.has(Ut)?o.threshold=1:n.has(Gt)&&(o.threshold=.5);let l=r(),c=new IntersectionObserver(s=>{for(let g of s)g.isIntersecting&&(l(),n.has($t)&&(c.disconnect(),delete t.dataset[e]))},o);return c.observe(t),()=>c.disconnect()}};var jt="session",Kt={type:1,name:"persist",mods:new Set([jt]),onLoad:({key:t,effect:e,mods:n,signals:r,value:o})=>{t=V(t,n),t===""&&(t=D);let l=n.has(jt)?sessionStorage:localStorage,c=o.split(/\s+/).filter(S=>S!=="");c=c.map(S=>K(S));let s=()=>{let S=l.getItem(t)||"{}",h=JSON.parse(S);r.merge(h)},g=()=>{let S;c.length?S=r.subset(...c):S=r.values(),l.setItem(t,JSON.stringify(S))};return s(),e(()=>{g()})}};var Jt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),o=window.location.href,l=new URL(r,o).toString();window.history.replaceState({},"",l)})}};var Ve="smooth",Ke="instant",Je="auto",zt="hstart",Yt="hcenter",Xt="hend",Qt="hnearest",Zt="vstart",en="vcenter",tn="vend",nn="vnearest",Wn="focus",De="center",rn="start",on="end",sn="nearest",an={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ve,Ke,Je,zt,Yt,Xt,Qt,Zt,en,tn,nn,Wn]),onLoad:t=>{let{el:e,mods:n,rawKey:r}=t;e.tabIndex||e.setAttribute("tabindex","0");let o={behavior:Ve,block:De,inline:De};if(n.has(Ve)&&(o.behavior=Ve),n.has(Ke)&&(o.behavior=Ke),n.has(Je)&&(o.behavior=Je),n.has(zt)&&(o.inline=rn),n.has(Yt)&&(o.inline=De),n.has(Xt)&&(o.inline=on),n.has(Qt)&&(o.inline=sn),n.has(Zt)&&(o.block=rn),n.has(en)&&(o.block=De),n.has(tn)&&(o.block=on),n.has(nn)&&(o.block=sn),!(e instanceof HTMLElement||e instanceof SVGElement))throw N("ScrollIntoViewInvalidElement",t);return e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(o),n.has("focus")&&e.focus(),delete e.dataset[r],()=>{}}};var ln="none",un="display",cn={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===ln&&t.removeProperty(un):t.setProperty(un,ln)})}};var fn="view-transition",dn={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===fn&&(t=!0);if(!t){let e=document.createElement("meta");e.name=fn,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!z){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let o=r();if(!o?.length)return;let l=e.style;l.viewTransitionName=o})}};var pn={type:1,name:"attr",valReq:1,onLoad:({el:t,key:e,effect:n,genRX:r})=>{let o=r();return e===""?n(async()=>{let l=o();for(let[c,s]of Object.entries(l))t.setAttribute(c,s)}):(e=le(e),n(async()=>{let l=!1;try{l=o()}catch{}let c;typeof l=="string"?c=l:c=JSON.stringify(l),!c||c==="false"||c==="null"||c==="undefined"?t.removeAttribute(e):t.setAttribute(e,c)}))}};var qn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,mn=["change","input","keydown"],gn={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,key:n,mods:r,signals:o,value:l,effect:c}=t,s=n?V(n,r):K(l),g=()=>{},S=()=>{},h=e.tagName.toLowerCase(),w="",b=h.includes("input"),T=e.getAttribute("type"),A=h.includes("checkbox")||b&&T==="checkbox";A&&(w=!1),b&&T==="number"&&(w=0);let v=h.includes("select"),y=h.includes("radio")||b&&T==="radio",i=b&&T==="file";y&&(e.getAttribute("name")?.length||e.setAttribute("name",s)),o.upsertIfMissing(s,w),g=()=>{let a="value"in e,m=o.value(s),p=`${m}`;if(A||y){let f=e;Array.isArray(m)?f.checked=m.includes(f.value):typeof m=="string"?f.checked=p===f.value:f.checked=!!m||m==="true"}else if(v){let f=e;if(f.multiple)for(let _ of f.options){if(_?.disabled)return;Array.isArray(m)||typeof m=="string"?_.selected=m.includes(_.value):typeof m=="number"?_.selected=m===Number(_.value):_.selected=m}else f.value=p}else i||(a?e.value=p:e.setAttribute("value",p))},S=async()=>{if(i){let f=[...e?.files||[]],_=[],R=[],x=[];await Promise.all(f.map(C=>new Promise(F=>{let L=new FileReader;L.onload=()=>{if(typeof L.result!="string")throw N("InvalidFileResultType",t,{resultType:typeof L.result});let Q=L.result.match(qn);if(!Q?.groups)throw N("InvalidDataUri",t,{result:L.result});_.push(Q.groups.contents),R.push(Q.groups.mime),x.push(C.name)},L.onloadend=()=>F(void 0),L.readAsDataURL(C)}))),o.setValue(s,_),o.setValue(`${s}Mimes`,R),o.setValue(`${s}Names`,x);return}let a=o.value(s),m=e||e;if(A){let f=m.checked||m.getAttribute("checked")==="true";if(Array.isArray(a)){let _=new Set(a);f?_.add(m.value):_.delete(m.value),o.setValue(s,[..._])}else if(typeof a=="string"){let _=f?m.value:"";o.setValue(s,_)}else o.setValue(s,f);return}let p=m.value||m.getAttribute("value")||"";if(typeof a=="number")o.setValue(s,Number(p));else if(typeof a=="string")o.setValue(s,p||"");else if(typeof a=="boolean")o.setValue(s,!!p);else if(Array.isArray(a))if(v){let R=[...e.selectedOptions].filter(x=>x.selected).map(x=>x.value);o.setValue(s,R)}else o.setValue(s,JSON.stringify(p.split(",")));else if(!(typeof a>"u"))throw N("BindUnsupportedSignalType",t,{signalType:typeof a})};for(let a of mn)e.addEventListener(a,S);let d=c(()=>g()),u=a=>{a.persisted&&S()};return window.addEventListener("pageshow",u),()=>{d();for(let a of mn)e.removeEventListener(a,S);window.removeEventListener("pageshow",u)}}};var hn={type:1,name:"class",valReq:1,onLoad:({el:t,key:e,mods:n,effect:r,genRX:o})=>{let l=t.classList,c=o();return r(()=>{if(e===""){let s=c();for(let[g,S]of Object.entries(s)){let h=g.split(/\s+/);S?l.add(...h):l.remove(...h)}}else e=V(e,n),c()?l.add(e):l.remove(e)})}};function ve(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ie(t,e,n=!1){return t?t.has(e.toLowerCase()):n}var $n="evt",ze="signalsChange",Gn=ze.length,vn={type:1,name:"on",keyReq:1,valReq:1,argNames:[$n],removeOnLoad:t=>t.startsWith("onLoad"),onLoad:({el:t,key:e,mods:n,rawKey:r,signals:o,value:l,effect:c,genRX:s})=>{let g=s(),S=t;n.has("window")&&(S=window);let h=y=>{y&&((n.has("prevent")||e==="submit")&&y.preventDefault(),n.has("stop")&&y.stopPropagation()),g(y)},w=n.get("delay");if(w){let y=ve(w);h=Ee(h,y)}let b=n.get("debounce");if(b){let y=ve(b),i=ie(b,"leading",!1),d=!ie(b,"notrail",!1);h=st(h,y,i,d)}let T=n.get("throttle");if(T){let y=ve(T),i=!ie(T,"noleading",!1),d=ie(T,"trail",!1);h=at(h,y,i,d)}if(n.has("viewtransition")&&z){let y=h;h=(...i)=>document.startViewTransition(()=>y(...i))}let A={capture:!0,passive:!1,once:!1};if(n.has("capture")||(A.capture=!1),n.has("passive")&&(A.passive=!0),n.has("once")&&(A.once=!0),e==="load")return setTimeout(()=>h(),0),()=>{};if(e==="interval"){let y=1e3,i=n.get("duration");i&&(y=ve(i),ie(i,"leading",!1)&&(t.dataset[r.replace(".leading","")]=l,delete t.dataset[r],h()));let d=setInterval(h,y);return()=>{clearInterval(d)}}if(e==="raf"){let y,i=()=>{h(),y=requestAnimationFrame(i)};return y=requestAnimationFrame(i),()=>{y&&cancelAnimationFrame(y)}}if(e.startsWith(ze)){if(e===ze){h();let d=u=>h(u);return document.addEventListener(ae,d),()=>{document.removeEventListener(ae,d)}}let y=V(B(e.slice(Gn)),n),i=new Map;return o.walk((d,u)=>{d.startsWith(y)&&i.set(u,u.value)}),c(()=>{for(let[d,u]of i)u!==d.value&&(h(),i.set(d,d.value))})}if(n.has("outside")){S=document;let y=h;h=d=>{let u=d?.target;t.contains(u)||y(d)}}let v=V(e,n);return S.addEventListener(v,h,A),()=>{S.removeEventListener(v,h)}}};var yn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,mods:n,signals:r,value:o})=>{let l=e?V(e,n):K(o);r.setValue(l,t)}};var Sn={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,effect:n,genRX:r}=t,o=r();return e instanceof HTMLElement||N("TextInvalidElement",t),n(()=>{let l=o(t);e.textContent=`${l}`})}};var{round:Un,max:Bn,min:jn}=Math,bn={type:3,name:"fit",fn:(t,e,n,r,o,l,c=!1,s=!1)=>{let g=(e-n)/(r-n)*(l-o)+o;return s&&(g=Un(g)),c&&(g=Bn(o,jn(l,g))),g}};var Tn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,o)=>{r.startsWith(e)&&(o.value=n)})}};var An={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Be("ds");me(pn,gn,hn,vn,yn,cn,Sn,Lt,Mt,Pt,Ct,Nt,wt,kt,Ot,Ft,Ht,It,Wt,qt,Bt,Kt,Jt,an,dn,bn,Tn,An);
//# sourceMappingURL=datastar-aliased.js.map
