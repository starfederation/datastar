var rn="computed",Ce={type:1,name:rn,keyReq:1,valReq:1,onLoad:({key:t,signals:e,genRX:n})=>{let r=n();e.setComputed(t,r)}};var F=t=>t.trim()==="true",B=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),ke=t=>t.replace(/(?:^\w|[A-Z]|\b\w)/g,function(e,n){return n==0?e.toLowerCase():e.toUpperCase()}).replace(/\s+/g,""),re=t=>new Function(`return Object.assign({}, ${t})`)();var De={type:1,name:"signals",valReq:1,removeOnLoad:!0,onLoad:t=>{let{key:e,genRX:n,signals:r}=t;if(e!="")r.setValue(e,n()());else{let i=re(t.value);t.value=JSON.stringify(i),r.merge(n()())}}};var Ve={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var Oe={name:"signalValue",type:0,fn:t=>{let e=/(?<path>[\w0-9.]*)((\.value))/gm;return t.replaceAll(e,"ctx.signals.signal('$1').value")}};var C="datastar";var Fe="Datastar-Request",He="0.21.3";var qe="type module";var k={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},We=k.Morph,I={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};function Ue(t){if(t.id)return t.id;let e=0,n=i=>(e=(e<<5)-e+i,e&e),r=i=>i.split("").forEach(s=>n(s.charCodeAt(0)));for(;t.parentNode;){if(t.id){r(`${t.id}`);break}else if(t===t.ownerDocument.documentElement)r(t.tagName);else{for(let i=1,s=t;s.previousElementSibling;s=s.previousElementSibling,i++)n(i);t=t.parentNode}t=t.parentNode}return C+e}function $e(t,e){let n=new MutationObserver(r=>{for(let i of r)for(let s of i.removedNodes)if(s===t){n.disconnect(),e();return}});n.observe(t.parentNode,{childList:!0})}var sn="https://data-star.dev/errors";var c=(t,e)=>{let n=new Error;t=t.charAt(0).toUpperCase()+t.slice(1),n.name=`error ${t}`;let r=`${sn}/${t}?${new URLSearchParams(e)}`;return n.message=`for more info see ${r}`,n};var on=Symbol.for("preact-signals"),D=1,G=2,Z=4,K=8,ie=16,j=32;function Ee(){se++}function Se(){if(se>1){se--;return}let t,e=!1;for(;Y!==void 0;){let n=Y;for(Y=void 0,ye++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~G,!(n._flags&K)&&Ge(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(ye=0,se--,e)throw c("BatchError, error",{error:t})}var E;var Y,se=0,ye=0,oe=0;function Be(t){if(E===void 0)return;let e=t._node;if(e===void 0||e._target!==E)return e={_version:0,_source:t,_prevSource:E._sources,_nextSource:void 0,_target:E,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},E._sources!==void 0&&(E._sources._nextSource=e),E._sources=e,t._node=e,E._flags&j&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=E._sources,e._nextSource=void 0,E._sources._nextSource=e,E._sources=e),e}function R(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}R.prototype.brand=on;R.prototype._refresh=function(){return!0};R.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};R.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};R.prototype.subscribe=function(t){return ae(()=>{let e=this.value,n=E;E=void 0;try{t(e)}finally{E=n}})};R.prototype.valueOf=function(){return this.value};R.prototype.toString=function(){return this.value+""};R.prototype.toJSON=function(){return this.value};R.prototype.peek=function(){let t=E;E=void 0;try{return this.value}finally{E=t}};Object.defineProperty(R.prototype,"value",{get(){let t=Be(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(ye>100)throw c("SignalCycleDetected");this._value=t,this._version++,oe++,Ee();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{Se()}}}});function Ge(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function je(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function Ke(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function q(t){R.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=oe-1,this._flags=Z}q.prototype=new R;q.prototype._refresh=function(){if(this._flags&=~G,this._flags&D)return!1;if((this._flags&(Z|j))===j||(this._flags&=~Z,this._globalVersion===oe))return!0;if(this._globalVersion=oe,this._flags|=D,this._version>0&&!Ge(this))return this._flags&=~D,!0;let t=E;try{je(this),E=this;let e=this._fn();(this._flags&ie||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~ie,this._version++)}catch(e){this._value=e,this._flags|=ie,this._version++}return E=t,Ke(this),this._flags&=~D,!0};q.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=Z|j;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}R.prototype._subscribe.call(this,t)};q.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(R.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~j;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};q.prototype._notify=function(){if(!(this._flags&G)){this._flags|=Z|G;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(q.prototype,"value",{get(){if(this._flags&D)throw c("SignalCycleDetected");let t=Be(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&ie)throw c("GetComputedError",{value:this._value});return this._value}});function Je(t){return new q(t)}function ze(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){Ee();let n=E;E=void 0;try{e()}catch(r){throw t._flags&=~D,t._flags|=K,Te(t),c("CleanupEffectError",{error:r})}finally{E=n,Se()}}}function Te(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,ze(t)}function an(t){if(E!==this)throw c("EndEffectError");Ke(this),E=t,this._flags&=~D,this._flags&K&&Te(this),Se()}function Q(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=j}Q.prototype._callback=function(){let t=this._start();try{if(this._flags&K||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};Q.prototype._start=function(){if(this._flags&D)throw c("SignalCycleDetected");this._flags|=D,this._flags&=~K,ze(this),je(this),Ee();let t=E;return E=this,an.bind(this,t)};Q.prototype._notify=function(){this._flags&G||(this._flags|=G,this._nextBatchedEffect=Y,Y=this)};Q.prototype._dispose=function(){this._flags|=K,this._flags&D||Te(this)};function ae(t){let e=new Q(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}function Xe(t,e=!1){let n={};for(let r in t)if(t.hasOwnProperty(r)){if(e&&r.startsWith("_"))continue;let i=t[r];i instanceof R?n[r]=i.value:n[r]=Xe(i)}return n}function Ye(t,e,n=!1){for(let r in e)if(e.hasOwnProperty(r)){if(r.match(/\_\_+/))throw c("InvalidSignalKey",{key:r});let i=e[r];if(i instanceof Object&&!Array.isArray(i))t[r]||(t[r]={}),Ye(t[r],i,n);else{if(n&&t[r])continue;t[r]=new R(i)}}}function Ze(t,e){for(let n in t)if(t.hasOwnProperty(n)){let r=t[n];r instanceof R?e(n,r):Ze(r,(i,s)=>{e(`${n}.${i}`,s)})}}function ln(t,...e){let n={};for(let r of e){let i=r.split("."),s=t,o=n;for(let l=0;l<i.length-1;l++){let f=i[l];if(!s[f])return{};o[f]||(o[f]={}),s=s[f],o=o[f]}let a=i[i.length-1];o[a]=s[a]}return n}var le=class{#e={};constructor(){}exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let o=0;o<n.length-1;o++){let a=n[o];if(!r[a])return null;r=r[a]}let i=n[n.length-1],s=r[i];if(!s)throw c("SignalNotFound",{path:e});return s}setSignal(e,n){let r=e.split("."),i=this.#e;for(let o=0;o<r.length-1;o++){let a=r[o];i[a]||(i[a]={}),i=i[a]}let s=r[r.length-1];i[s]=n}setComputed(e,n){let r=Je(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let r=this.upsert(e,n);r.value=n}upsert(e,n){let r=e.split("."),i=this.#e;for(let l=0;l<r.length-1;l++){let f=r[l];i[f]||(i[f]={}),i=i[f]}let s=r[r.length-1],o=i[s];if(o)return(o.value===null||o.value===void 0)&&(o.value=n),o;let a=new R(n);return i[s]=a,a}remove(...e){for(let n of e){let r=n.split("."),i=this.#e;for(let o=0;o<r.length-1;o++){let a=r[o];if(!i[a])return;i=i[a]}let s=r[r.length-1];delete i[s]}}merge(e,n=!1){Ye(this.#e,e,n)}subset(...e){return ln(this.values(),...e)}walk(e){Ze(this.#e,e)}values(e=!1){return Xe(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var ue=class{#e=new le;#i=[];#s=[];#n={};#a=[];#t=new Map;get signals(){return this.#e}get version(){return He}load(...e){e.forEach(n=>{let r;switch(n.type){case 0:this.#s.push(n);break;case 2:let i=n;this.#a.push(i),r=i.onGlobalInit;break;case 3:this.#n[n.name]=n;break;case 1:let s=n;this.#i.push(s),r=s.onGlobalInit;break;default:throw c("InvalidPluginType",{name:n.name,type:n.type})}if(r){let i=this;r({get signals(){return i.#e},effect:s=>ae(s),actions:this.#n,apply:this.apply.bind(this),cleanup:this.#r.bind(this)})}}),this.apply(document.body)}apply(e){let n=new Set;this.#i.forEach((r,i)=>{this.#o(e,s=>{if(!("starIgnore"in s.dataset)){i||this.#r(s);for(let o in s.dataset){if(!o.startsWith(r.name))continue;let a=o.slice(r.name.length),[l,...f]=a.split(/\_\_+/),u=l.length>0;u&&(l=l[0].toLowerCase()+l.slice(1));let m=`${s.dataset[o]}`||"",g=m,v=g.length>0,h=r.keyReq||0;if(u){if(h===2)throw c(r.name+"KeyNotAllowed",{key:l})}else if(h===1)throw c(r.name+"KeyRequired");let T=r.valReq||0;if(v){if(T===2)throw c(r.name+"ValueNotAllowed",{value:g})}else if(T===1)throw c(r.name+"ValueRequired");if(h===3||T===3){if(u&&v)throw c(r.name+"KeyAndValueProvided");if(!u&&!v)throw c(r.name+"KeyOrValueRequired")}s.id.length||(s.id=Ue(s)),n.clear();let _=new Map;f.forEach(p=>{let[A,...w]=p.split(".");_.set(ke(A),new Set(w.map(M=>M.toLowerCase())))});let S=[...r.macros?.pre||[],...this.#s,...r.macros?.post||[]];for(let p of S)n.has(p)||(n.add(p),g=p.fn(g));let x=this,b;b={get signals(){return x.#e},effect:p=>ae(p),apply:x.apply.bind(x),cleanup:x.#r.bind(x),actions:x.#n,genRX:()=>this.#l(b,...r.argNames||[]),el:s,rawKey:o,rawValue:m,key:l,value:g,mods:_};let d=r.onLoad(b);d&&(this.#t.has(s)||this.#t.set(s,{id:s.id,set:new Set}),this.#t.get(s).set.add(d)),r?.removeOnLoad&&delete s.dataset[o]}}})})}#l(e,...n){let r=e.value.split(/;|\n/).map(h=>h.trim()).filter(h=>h!=""),i=r.length-1;r[i].startsWith("return")||(r[i]=`return (${r[i]});`);let o=r.join(`
`),a=/(\w*)\(/gm,l=o.matchAll(a),f=new Set;for(let h of l)f.add(h[1]);let u=Object.keys(this.#n).filter(h=>f.has(h)),g=`${u.map(h=>`const ${h} = ctx.actions.${h}.fn;`).join(`
`)}return (()=> {${o}})()`,v=g.trim();u.forEach(h=>{v=v.replaceAll(h+"(",h+"(ctx,")});try{let h=n||[],T=new Function("ctx",...h,v);return(..._)=>T(e,..._)}catch(h){throw c("GeneratingExpressionFailed",{error:h,fnContent:g})}}#o(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;for(n(e),e=e.firstElementChild;e;)this.#o(e,n),e=e.nextElementSibling}#r(e){let n=this.#t.get(e);if(n){for(let r of n.set)r();this.#t.delete(e)}}};var Qe=new ue;Qe.load(Ve,Oe,De,Ce);var Ae=Qe;async function un(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function cn(t){let e,n,r,i=!1;return function(o){e===void 0?(e=o,n=0,r=-1):e=dn(e,o);let a=e.length,l=0;for(;n<a;){i&&(e[n]===10&&(l=++n),i=!1);let f=-1;for(;n<a&&f===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-l);break;case 13:i=!0;case 10:f=n;break}if(f===-1)break;t(e.subarray(l,f),r),l=n,r=-1}l===a?e=void 0:l!==0&&(e=e.subarray(l),n-=l)}}function fn(t,e,n){let r=et(),i=new TextDecoder;return function(o,a){if(o.length===0)n?.(r),r=et();else if(a>0){let l=i.decode(o.subarray(0,a)),f=a+(o[a+1]===32?2:1),u=i.decode(o.subarray(f));switch(l){case"data":r.data=r.data?r.data+`
`+u:u;break;case"event":r.event=u;break;case"id":t(r.id=u);break;case"retry":let m=parseInt(u,10);isNaN(m)||e(r.retry=m);break}}}}function dn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function et(){return{data:"",event:"",id:"",retry:void 0}}var mn="text/event-stream",pn=1e3,tt="last-event-id";function nt(t,{signal:e,headers:n,onopen:r,onmessage:i,onclose:s,onerror:o,openWhenHidden:a,fetch:l,retryScaler:f=2,retryMaxWaitMs:u=3e4,retryMaxCount:m=10,...g}){return new Promise((v,h)=>{let T=0,_={...n};_.accept||(_.accept=mn);let S;function x(){S.abort(),document.hidden||M()}a||document.addEventListener("visibilitychange",x);let b=pn,d=0;function p(){document.removeEventListener("visibilitychange",x),window.clearTimeout(d),S.abort()}e?.addEventListener("abort",()=>{p(),v()});let A=l??window.fetch,w=r??function(){};async function M(){S=new AbortController;try{let L=await A(t,{...g,headers:_,signal:S.signal});await w(L),await un(L.body,cn(fn(N=>{N?_[tt]=N:delete _[tt]},N=>{b=N},i))),s?.(),p(),v()}catch(L){if(!S.signal.aborted)try{let N=o?.(L)??b;window.clearTimeout(d),d=window.setTimeout(M,N),b*=f,b=Math.min(b,u),T++,T>=m?(p(),h(c("SSE_MAX_RETRIES",{retryInterval:b,retryMaxCount:m,...g}))):console.error(`Datastar failed to reach ${g.method}:${t.toString()} retry in ${N}ms`)}catch(N){p(),h(N)}}}M()})}var J=`${C}-sse`,_e=`${C}-settling`,W=`${C}-swapping`,ce="started",fe="finished";function V(t,e){document.addEventListener(J,n=>{if(n.detail.type!=t)return;let{argsRaw:r}=n.detail;e(r)})}function we(t,e){document.dispatchEvent(new CustomEvent(J,{detail:{type:t,argsRaw:e}}))}var rt=t=>`${t}`.includes("text/event-stream"),it={type:3,name:"sse",fn:async(t,e,n)=>{let{el:{id:r},el:i,signals:s}=t,{method:o,headers:a,form:l,includeLocal:f,openWhenHidden:u,retryScaler:m,retryMaxWaitMs:g,retryMaxCount:v,abort:h}=Object.assign({method:"GET",headers:{},form:!1,includeLocal:!1,openWhenHidden:!1,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},n),T=o.toUpperCase();try{if(we(ce,{elId:r}),!e?.length)throw c("NoUrlProvided");let _=Object.assign({"Content-Type":l?T==="GET"?"application/x-www-form-urlencoded":"multipart/form-data":"application/json",[Fe]:!0},a),S={method:T,headers:_,openWhenHidden:u,retryScaler:m,retryMaxWaitMs:g,retryMaxCount:v,signal:h,onmessage:d=>{if(!d.event.startsWith(C))return;let p=d.event,A={},w=d.data.split(`
`);for(let L of w){let N=L.indexOf(" "),X=L.slice(0,N),$=A[X];$||($=[],A[X]=$);let O=L.slice(N+1).trim();$.push(O)}let M={};for(let[L,N]of Object.entries(A))M[L]=N.join(`
`);we(p,M)},onerror:d=>{if(rt(d))throw c("InvalidContentType",{url:e,error:d});d&&console.error(d.message)}},x=new URL(e,window.location.origin),b=new URLSearchParams(x.search);if(l){let d=i.closest("form");if(d===null)throw c("ClosestFormNotFound");d.addEventListener("submit",A=>A.preventDefault());let p=new FormData(d);T==="GET"?new URLSearchParams(p).forEach((w,M)=>{b.set(M,w)}):S.body=p}else{let d=s.JSON(!1,!f);T==="GET"?b.set(C,d):S.body=d}x.search=b.toString();try{await nt(x.toString(),S)}catch(d){if(!rt(d))throw c("SseFetchFailed",{method:T,url:e,error:d})}}finally{we(fe,{elId:r})}}};var gn=`${C}-indicator`,Qr=`${gn}-loading`,st={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({value:t,signals:e,el:n,key:r})=>{let i=r||t,s=e.upsert(i,!1),o=a=>{let{type:l,argsRaw:{elId:f}}=a.detail;if(f===n.id)switch(l){case ce:s.value=!0;break;case fe:s.value=!1;break}};return document.addEventListener(J,o),()=>{document.removeEventListener(J,o)}}};var ot={type:2,name:I.ExecuteScript,onGlobalInit:async()=>{V(I.ExecuteScript,({autoRemove:t=`${!0}`,attributes:e=qe,script:n})=>{let r=F(t);if(!n?.length)throw c("NoScriptProvided");let i=document.createElement("script");e.split(`
`).forEach(s=>{let o=s.indexOf(" "),a=o?s.slice(0,o):s,l=o?s.slice(o):"";i.setAttribute(a.trim(),l.trim())}),i.text=n,document.head.appendChild(i),r&&i.remove()})}};var ee=document,z=!!ee.startViewTransition;var me=new WeakSet;function ct(t,e,n={}){t instanceof Document&&(t=t.documentElement);let r;typeof e=="string"?r=Sn(e):r=e;let i=Tn(r),s=bn(t,i,n);return ft(t,i,s)}function ft(t,e,n){if(n.head.block){let r=t.querySelector("head"),i=e.querySelector("head");if(r&&i){let s=mt(i,r,n);Promise.all(s).then(()=>{ft(t,e,Object.assign(n,{head:{block:!1,ignore:!0}}))});return}}if(n.morphStyle==="innerHTML")return dt(e,t,n),t.children;if(n.morphStyle==="outerHTML"||n.morphStyle==null){let r=_n(e,t,n);if(!r)throw c("NoBestMatchFound",{old:t,new:e});let i=r?.previousSibling,s=r?.nextSibling,o=pe(t,r,n);return r?An(i,o,s):[]}else throw c("InvalidMorphStyle",{style:n.morphStyle})}function pe(t,e,n){if(!(n.ignoreActive&&t===document.activeElement))if(e==null){if(n.callbacks.beforeNodeRemoved(t)===!1)return;t.remove(),n.callbacks.afterNodeRemoved(t);return}else{if(ge(t,e))return n.callbacks.beforeNodeMorphed(t,e)===!1?void 0:(t instanceof HTMLHeadElement&&n.head.ignore||(e instanceof HTMLHeadElement&&t instanceof HTMLHeadElement&&n.head.style!==k.Morph?mt(e,t,n):(vn(e,t),dt(e,t,n))),n.callbacks.afterNodeMorphed(t,e),t);if(n.callbacks.beforeNodeRemoved(t)===!1||n.callbacks.beforeNodeAdded(e)===!1)return;if(!t.parentElement)throw c("NoParentElementFound",{oldNode:t});return t.parentElement.replaceChild(e,t),n.callbacks.afterNodeAdded(e),n.callbacks.afterNodeRemoved(t),e}}function dt(t,e,n){let r=t.firstChild,i=e.firstChild,s;for(;r;){if(s=r,r=s.nextSibling,i==null){if(n.callbacks.beforeNodeAdded(s)===!1)return;e.appendChild(s),n.callbacks.afterNodeAdded(s),U(n,s);continue}if(pt(s,i,n)){pe(i,s,n),i=i.nextSibling,U(n,s);continue}let o=yn(t,e,s,i,n);if(o){i=at(i,o,n),pe(o,s,n),U(n,s);continue}let a=En(t,s,i,n);if(a){i=at(i,a,n),pe(a,s,n),U(n,s);continue}if(n.callbacks.beforeNodeAdded(s)===!1)return;e.insertBefore(s,i),n.callbacks.afterNodeAdded(s),U(n,s)}for(;i!==null;){let o=i;i=i.nextSibling,gt(o,n)}}function vn(t,e){let n=t.nodeType;if(n===1){for(let r of t.attributes)e.getAttribute(r.name)!==r.value&&e.setAttribute(r.name,r.value);for(let r of e.attributes)t.hasAttribute(r.name)||e.removeAttribute(r.name)}if((n===Node.COMMENT_NODE||n===Node.TEXT_NODE)&&e.nodeValue!==t.nodeValue&&(e.nodeValue=t.nodeValue),t instanceof HTMLInputElement&&e instanceof HTMLInputElement&&t.type!=="file")e.value=t.value||"",de(t,e,"value"),de(t,e,"checked"),de(t,e,"disabled");else if(t instanceof HTMLOptionElement)de(t,e,"selected");else if(t instanceof HTMLTextAreaElement&&e instanceof HTMLTextAreaElement){let r=t.value,i=e.value;r!==i&&(e.value=r),e.firstChild&&e.firstChild.nodeValue!==r&&(e.firstChild.nodeValue=r)}}function de(t,e,n){let r=t.getAttribute(n),i=e.getAttribute(n);r!==i&&(r?e.setAttribute(n,r):e.removeAttribute(n))}function mt(t,e,n){let r=[],i=[],s=[],o=[],a=n.head.style,l=new Map;for(let u of t.children)l.set(u.outerHTML,u);for(let u of e.children){let m=l.has(u.outerHTML),g=n.head.shouldReAppend(u),v=n.head.shouldPreserve(u);m||v?g?i.push(u):(l.delete(u.outerHTML),s.push(u)):a===k.Append?g&&(i.push(u),o.push(u)):n.head.shouldRemove(u)!==!1&&i.push(u)}o.push(...l.values());let f=[];for(let u of o){let m=document.createRange().createContextualFragment(u.outerHTML).firstChild;if(!m)throw c("NewElementCouldNotBeCreated",{newNode:u});if(n.callbacks.beforeNodeAdded(m)){if(m.hasAttribute("href")||m.hasAttribute("src")){let g,v=new Promise(h=>{g=h});m.addEventListener("load",function(){g(void 0)}),f.push(v)}e.appendChild(m),n.callbacks.afterNodeAdded(m),r.push(m)}}for(let u of i)n.callbacks.beforeNodeRemoved(u)!==!1&&(e.removeChild(u),n.callbacks.afterNodeRemoved(u));return n.head.afterHeadMorphed(e,{added:r,kept:s,removed:i}),f}function H(){}function bn(t,e,n){return{target:t,newContent:e,config:n,morphStyle:n.morphStyle,ignoreActive:n.ignoreActive,idMap:Mn(t,e),deadIds:new Set,callbacks:Object.assign({beforeNodeAdded:H,afterNodeAdded:H,beforeNodeMorphed:H,afterNodeMorphed:H,beforeNodeRemoved:H,afterNodeRemoved:H},n.callbacks),head:Object.assign({style:"merge",shouldPreserve:r=>r.getAttribute("im-preserve")==="true",shouldReAppend:r=>r.getAttribute("im-re-append")==="true",shouldRemove:H,afterHeadMorphed:H},n.head)}}function pt(t,e,n){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName?t?.id?.length&&t.id===e.id?!0:te(n,t,e)>0:!1}function ge(t,e){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName}function at(t,e,n){for(;t!==e;){let r=t;if(t=t?.nextSibling,!r)throw c("NoTemporaryNodeFound",{startInclusive:t,endExclusive:e});gt(r,n)}return U(n,e),e.nextSibling}function yn(t,e,n,r,i){let s=te(i,n,e),o=null;if(s>0){o=r;let a=0;for(;o!=null;){if(pt(n,o,i))return o;if(a+=te(i,o,t),a>s)return null;o=o.nextSibling}}return o}function En(t,e,n,r){let i=n,s=e.nextSibling,o=0;for(;i&&s;){if(te(r,i,t)>0)return null;if(ge(e,i))return i;if(ge(s,i)&&(o++,s=s.nextSibling,o>=2))return null;i=i.nextSibling}return i}var lt=new DOMParser;function Sn(t){let e=t.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(e.match(/<\/html>/)||e.match(/<\/head>/)||e.match(/<\/body>/)){let n=lt.parseFromString(t,"text/html");if(e.match(/<\/html>/))return me.add(n),n;{let r=n.firstChild;return r?(me.add(r),r):null}}else{let r=lt.parseFromString(`<body><template>${t}</template></body>`,"text/html").body.querySelector("template")?.content;if(!r)throw c("NoContentFound",{newContent:t});return me.add(r),r}}function Tn(t){if(t==null)return document.createElement("div");if(me.has(t))return t;if(t instanceof Node){let e=document.createElement("div");return e.append(t),e}else{let e=document.createElement("div");for(let n of[...t])e.append(n);return e}}function An(t,e,n){let r=[],i=[];for(;t;)r.push(t),t=t.previousSibling;for(;r.length>0;){let s=r.pop();i.push(s),e?.parentElement?.insertBefore(s,e)}for(i.push(e);n;)r.push(n),i.push(n),n=n.nextSibling;for(;r.length;)e?.parentElement?.insertBefore(r.pop(),e.nextSibling);return i}function _n(t,e,n){let r=t.firstChild,i=r,s=0;for(;r;){let o=wn(r,e,n);o>s&&(i=r,s=o),r=r.nextSibling}return i}function wn(t,e,n){return ge(t,e)?.5+te(n,t,e):0}function gt(t,e){U(e,t),e.callbacks.beforeNodeRemoved(t)!==!1&&(t.remove(),e.callbacks.afterNodeRemoved(t))}function Rn(t,e){return!t.deadIds.has(e)}function xn(t,e,n){return t.idMap.get(n)?.has(e)||!1}function U(t,e){let n=t.idMap.get(e);if(n)for(let r of n)t.deadIds.add(r)}function te(t,e,n){let r=t.idMap.get(e);if(!r)return 0;let i=0;for(let s of r)Rn(t,s)&&xn(t,s,n)&&++i;return i}function ut(t,e){let n=t.parentElement,r=t.querySelectorAll("[id]");for(let i of r){let s=i;for(;s!==n&&s;){let o=e.get(s);o==null&&(o=new Set,e.set(s,o)),o.add(i.id),s=s.parentElement}}}function Mn(t,e){let n=new Map;return ut(t,n),ut(e,n),n}var vt={type:2,name:I.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");V(I.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=We,settleDuration:s=`${300}`,useViewTransition:o=`${!1}`})=>{let a=parseInt(s),l=F(o);e.innerHTML=n.trim(),[...e.content.children].forEach(u=>{if(!(u instanceof Element))throw c("NoFragmentsFound");let m=r||`#${u.getAttribute("id")}`,g=[...document.querySelectorAll(m)||[]];if(!g.length)throw c("NoTargetsFound",{selectorOrID:m});z&&l?ee.startViewTransition(()=>ht(t,i,a,u,g)):ht(t,i,a,u,g)})})}};function ht(t,e,n,r,i){for(let s of i){s.classList.add(W);let o=s.outerHTML,a=s;switch(e){case k.Morph:let u=ct(a,r,{callbacks:{beforeNodeRemoved:(m,g)=>(t.cleanup(m),!0)}});if(!u?.length)throw c("MorphFailed");a=u[0];break;case k.Inner:a.innerHTML=r.innerHTML;break;case k.Outer:a.replaceWith(r);break;case k.Prepend:a.prepend(r);break;case k.Append:a.append(r);break;case k.Before:a.before(r);break;case k.After:a.after(r);break;case k.UpsertAttributes:r.getAttributeNames().forEach(m=>{let g=r.getAttribute(m);a.setAttribute(m,g)});break;default:throw c("InvalidMergeMode",{mergeMode:e})}t.cleanup(a);let l=a.classList;l.add(W),t.apply(document.body),setTimeout(()=>{s.classList.remove(W),l.remove(W)},n);let f=a.outerHTML;o!==f&&(l.add(_e),setTimeout(()=>{l.remove(_e)},n))}}var bt={type:2,name:I.MergeSignals,onGlobalInit:async t=>{V(I.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${!1}`})=>{let{signals:r}=t,i=F(n);r.merge(re(e),i),t.apply(document.body)})}};var yt={type:2,name:I.RemoveFragments,onGlobalInit:async()=>{V(I.RemoveFragments,({selector:t,settleDuration:e=`${300}`,useViewTransition:n=`${!1}`})=>{if(!t.length)throw c("NoSelectorProvided");let r=parseInt(e),i=F(n),s=document.querySelectorAll(t),o=()=>{for(let a of s)a.classList.add(W);setTimeout(()=>{for(let a of s)a.remove()},r)};z&&i?ee.startViewTransition(()=>o()):o()})}};var Et={type:2,name:I.RemoveSignals,onGlobalInit:async t=>{V(I.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw c("NoPathsProvided");t.signals.remove(...n),t.apply(document.body)})}};var St={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw c("ClipboardNotAvailable");navigator.clipboard.writeText(e)}};var Tt="once",At="half",_t="full",wt={type:1,name:"intersects",keyReq:2,mods:new Set([Tt,At,_t]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(_t)?i.threshold=1:n.has(At)&&(i.threshold=.5);let s=r(),o=new IntersectionObserver(a=>{a.forEach(l=>{l.isIntersecting&&(s(),n.has(Tt)&&(o.disconnect(),delete t.dataset[e]))})},i);return o.observe(t),()=>o.disconnect()}};var Rt="session",xt={type:1,name:"persist",mods:new Set([Rt]),onLoad:({key:t,value:e,signals:n,effect:r,mods:i})=>{t===""&&(t=C);let s=i.has(Rt)?sessionStorage:localStorage,o=e.split(/\s+/).filter(f=>f!==""),a=()=>{let f=s.getItem(t)||"{}",u=JSON.parse(f);n.merge(u)},l=()=>{let f;o.length?f=n.subset(...o):f=n.values(),s.setItem(t,JSON.stringify(f))};return a(),r(()=>{l()})}};var Mt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,s=new URL(r,i).toString();window.history.replaceState({},"",s)})}};var he="smooth",Me="instant",Ne="auto",Nt="hstart",Pt="hcenter",It="hend",Lt="hnearest",Ct="vstart",kt="vcenter",Dt="vend",Vt="vnearest",Pn="focus",ve="center",Ot="start",Ft="end",Ht="nearest",qt={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([he,Me,Ne,Nt,Pt,It,Lt,Ct,kt,Dt,Vt,Pn]),onLoad:({el:t,mods:e,rawKey:n})=>{t.tabIndex||t.setAttribute("tabindex","0");let r={behavior:he,block:ve,inline:ve};if(e.has(he)&&(r.behavior=he),e.has(Me)&&(r.behavior=Me),e.has(Ne)&&(r.behavior=Ne),e.has(Nt)&&(r.inline=Ot),e.has(Pt)&&(r.inline=ve),e.has(It)&&(r.inline=Ft),e.has(Lt)&&(r.inline=Ht),e.has(Ct)&&(r.block=Ot),e.has(kt)&&(r.block=ve),e.has(Dt)&&(r.block=Ft),e.has(Vt)&&(r.block=Ht),!(t instanceof HTMLElement||t instanceof SVGElement))throw c("NotHtmlSvgElement, el");return t.tabIndex||t.setAttribute("tabindex","0"),t.scrollIntoView(r),e.has("focus")&&t.focus(),delete t.dataset[n],()=>{}}};var Wt="none",Ut="display",$t={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===Wt&&t.removeProperty(Ut):t.setProperty(Ut,Wt)})}};var Pe="view-transition",Bt={type:1,name:Pe,keyReq:2,valReq:1,onGlobalInit(){let t=!1;if(document.head.childNodes.forEach(e=>{e instanceof HTMLMetaElement&&e.name===Pe&&(t=!0)}),!t){let e=document.createElement("meta");e.name=Pe,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!z){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let s=e.style;s.viewTransitionName=i})}};var Gt={type:1,name:"attributes",valReq:1,onLoad:({el:t,genRX:e,key:n,effect:r})=>{let i=e();return n===""?r(async()=>{let s=i();Object.entries(s).forEach(([o,a])=>{t.setAttribute(o,a)})}):(n=B(n),r(async()=>{let s=!1;try{s=i()}catch{}let o;typeof s=="string"?o=s:o=JSON.stringify(s),!o||o==="false"||o==="null"||o==="undefined"?t.removeAttribute(n):t.setAttribute(n,o)}))}};var In=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,jt=["change","input","keydown"],Kt={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,value:n,key:r,signals:i,effect:s}=t,o=r||n,a=()=>{},l=()=>{};if(typeof o!="string")throw c("InvalidExpression");let f=e.tagName.toLowerCase(),u="",m=f.includes("input"),g=e.getAttribute("type"),v=f.includes("checkbox")||m&&g==="checkbox";v&&(u=!1),m&&g==="number"&&(u=0);let T=f.includes("select"),_=f.includes("radio")||m&&g==="radio",S=m&&g==="file";_&&(e.getAttribute("name")?.length||e.setAttribute("name",o)),i.upsert(o,u),a=()=>{let b="value"in e,d=i.value(o),p=`${d}`;if(v||_){let A=e;v?A.checked=!!d||d==="true":_&&(A.checked=p===A.value)}else if(!S)if(T){let A=e;A.multiple?Array.from(A.options).forEach(w=>{w?.disabled||(Array.isArray(d)||typeof d=="string"?w.selected=d.includes(w.value):typeof d=="number"?w.selected=d===Number(w.value):w.selected=d)}):A.value=p}else b?e.value=p:e.setAttribute("value",p)},l=async()=>{if(S){let p=[...e?.files||[]],A=[],w=[],M=[];await Promise.all(p.map(X=>new Promise($=>{let O=new FileReader;O.onload=()=>{if(typeof O.result!="string")throw c("InvalidFileResultType",{type:typeof O.result});let be=O.result.match(In);if(!be?.groups)throw c("InvalidDataUri",{result:O.result});A.push(be.groups.contents),w.push(be.groups.mime),M.push(X.name)},O.onloadend=()=>$(void 0),O.readAsDataURL(X)}))),i.setValue(o,A);let L=`${o}Mimes`,N=`${o}Names`;L in i&&i.upsert(L,w),N in i&&i.upsert(N,M);return}let b=i.value(o),d=e||e;if(typeof b=="number"){let p=Number(d.value||d.getAttribute("value"));i.setValue(o,p)}else if(typeof b=="string"){let p=d.value||d.getAttribute("value")||"";i.setValue(o,p)}else if(typeof b=="boolean")if(v){let p=d.checked||d.getAttribute("checked")==="true";i.setValue(o,p)}else{let p=!!(d.value||d.getAttribute("value"));i.setValue(o,p)}else if(!(typeof b>"u"))if(Array.isArray(b))if(T){let w=[...e.selectedOptions].filter(M=>M.selected).map(M=>M.value);i.setValue(o,w)}else{let p=JSON.stringify(d.value.split(","));i.setValue(o,p)}else throw c("UnsupportedSignalType",{current:typeof b})},jt.forEach(b=>e.addEventListener(b,l));let x=s(()=>a());return()=>{x(),jt.forEach(b=>{e.removeEventListener(b,l)})}}};var Jt={type:1,name:"class",valReq:1,onLoad:({key:t,el:e,genRX:n,effect:r})=>{let i=e.classList,s=n();return r(()=>{if(t===""){let o=s();for(let[a,l]of Object.entries(o)){let f=a.split(/\s+/);l?i.add(...f):i.remove(...f)}}else{let o=s(),a=B(t);o?i.add(a):i.remove(a)}})}};function Ie(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return parseFloat(e)}catch{}}return 0}function ne(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function zt(t,e,n=!1,r=!0){let i=-1,s=()=>i&&clearTimeout(i);return function(...a){s(),n&&!i&&t(...a),i=setTimeout(()=>{r&&t(...a),s()},e)}}function Xt(t,e,n=!0,r=!1){let i=!1;return function(...o){i||(n&&t(...o),i=!0,setTimeout(()=>{i=!1,r&&t(...o)},e))}}var Le=new Map,Ln="evt",Yt={type:1,name:"on",keyReq:1,valReq:1,argNames:[Ln],macros:{pre:[{type:0,name:"evtEsc",fn:t=>t.replaceAll(/evt.([\w\.]+)value/gm,"EVT_$1_VALUE")}],post:[{type:0,name:"evtUnesc",fn:t=>t.replaceAll(/EVT_([\w\.]+)_VALUE/gm,"evt.$1value")}]},onLoad:({el:t,key:e,genRX:n,mods:r,signals:i,effect:s})=>{let o=n(),a=t;r.has("window")&&(a=window);let l=v=>{v&&(r.has("prevent")&&v.preventDefault(),r.has("stop")&&v.stopPropagation()),o(v)},f=r.get("debounce");if(f){let v=Ie(f),h=ne(f,"leading",!1),T=!ne(f,"notrail",!1);l=zt(l,v,h,T)}let u=r.get("throttle");if(u){let v=Ie(u),h=!ne(u,"noleading",!1),T=ne(u,"trail",!1);l=Xt(l,v,h,T)}let m={capture:!0,passive:!1,once:!1};r.has("capture")||(m.capture=!1),r.has("passive")&&(m.passive=!0),r.has("once")&&(m.once=!0);let g=B(e).toLowerCase();switch(g){case"load":return l(),delete t.dataset.onLoad,()=>{};case"raf":let v,h=()=>{l(),v=requestAnimationFrame(h)};return v=requestAnimationFrame(h),()=>{v&&cancelAnimationFrame(v)};case"signals-change":return $e(t,()=>{Le.delete(t.id)}),s(()=>{let _=r.has("remote"),S=i.JSON(!1,_);(Le.get(t.id)||"")!==S&&(Le.set(t.id,S),l())});default:if(r.has("outside")){a=document;let _=l,S=!1;l=b=>{let d=b?.target;if(!d)return;let p=t.id===d.id;p&&S&&(S=!1),!p&&!S&&(_(b),S=!0)}}return a.addEventListener(g,l,m),()=>{a.removeEventListener(g,l)}}}};var Zt={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,value:n,signals:r})=>{let i=e||n;return r.upsert(i,t),()=>r.setValue(i,null)}};var Qt={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t,i=n();return e instanceof HTMLElement||c("NotHtmlElement"),r(()=>{let s=i(t);e.textContent=`${s}`})}};var{round:Cn,max:kn,min:Dn}=Math,en={type:3,name:"fit",fn:(t,e,n,r,i,s,o=!1,a=!1)=>{let l=(e-n)/(r-n)*(s-i)+i;return a&&(l=Cn(l)),o&&(l=kn(i,Dn(s,l))),l}};var tn={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var nn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Ae.load(Kt,st,Zt,Gt,Jt,Yt,$t,Qt,it,vt,bt,yt,Et,ot,St,wt,xt,Mt,qt,Bt,en,tn,nn);var _o=Ae;export{_o as Datastar};
//# sourceMappingURL=datastar.js.map
