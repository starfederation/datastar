var rn="computed",Le={type:1,name:rn,keyReq:1,valReq:1,onLoad:({key:t,signals:e,genRX:n})=>{let r=n();e.setComputed(t,r)}};var O=t=>t.trim()==="true",U=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),ke=t=>t.replace(/(?:^\w|[A-Z]|\b\w)/g,(e,n)=>n===0?e.toLowerCase():e.toUpperCase()).replace(/\s+/g,""),ne=t=>new Function(`return Object.assign({}, ${t})`)();var Ce={type:1,name:"signals",valReq:1,removeOnLoad:!0,onLoad:t=>{let{key:e,genRX:n,signals:r}=t;if(e!=="")r.setValue(e,n()());else{let i=ne(t.value);t.value=JSON.stringify(i),r.merge(n()())}}};var De={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var Ve={name:"signalValue",type:0,fn:t=>{let e=/(?<path>[\w0-9.]*)((\.value))/gm;return t.replaceAll(e,"ctx.signals.signal('$1').value")}};var L="datastar";var Oe="Datastar-Request",Fe="0.21.3";var He="type module";var k={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},qe=k.Morph,I={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};function We(t){if(t.id)return t.id;let e=0,n=o=>(e=(e<<5)-e+o,e&e),r=o=>{for(let s of o.split(""))n(s.charCodeAt(0))},i=t;for(;i.parentNode;){if(i.id){r(`${i.id}`);break}if(i===i.ownerDocument.documentElement)r(i.tagName);else{for(let o=1,s=t;s.previousElementSibling;s=s.previousElementSibling,o++)n(o);i=i.parentNode}i=i.parentNode}return L+e}function $e(t,e){let n=new MutationObserver(r=>{for(let i of r)for(let o of i.removedNodes)if(o===t){n.disconnect(),e();return}});n.observe(t.parentNode,{childList:!0})}var on="https://data-star.dev/errors";var f=(t,e)=>{let n=new Error;t=t.charAt(0).toUpperCase()+t.slice(1),n.name=`error ${t}`;let r=`${on}/${t}?${new URLSearchParams(e)}`;return n.message=`for more info see ${r}`,n};var sn=Symbol.for("preact-signals"),D=1,B=2,X=4,j=8,re=16,G=32;function be(){ie++}function Ee(){if(ie>1){ie--;return}let t,e=!1;for(;z!==void 0;){let n=z;for(z=void 0,ve++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~B,!(n._flags&j)&&Be(n))try{n._callback()}catch(i){e||(t=i,e=!0)}n=r}}if(ve=0,ie--,e)throw f("BatchError, error",{error:t})}var S;var z,ie=0,ve=0,oe=0;function Ue(t){if(S===void 0)return;let e=t._node;if(e===void 0||e._target!==S)return e={_version:0,_source:t,_prevSource:S._sources,_nextSource:void 0,_target:S,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},S._sources!==void 0&&(S._sources._nextSource=e),S._sources=e,t._node=e,S._flags&G&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=S._sources,e._nextSource=void 0,S._sources._nextSource=e,S._sources=e),e}function w(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}w.prototype.brand=sn;w.prototype._refresh=()=>!0;w.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};w.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};w.prototype.subscribe=function(t){return se(()=>{let e=this.value,n=S;S=void 0;try{t(e)}finally{S=n}})};w.prototype.valueOf=function(){return this.value};w.prototype.toString=function(){return`${this.value}`};w.prototype.toJSON=function(){return this.value};w.prototype.peek=function(){let t=S;S=void 0;try{return this.value}finally{S=t}};Object.defineProperty(w.prototype,"value",{get(){let t=Ue(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(ve>100)throw f("SignalCycleDetected");this._value=t,this._version++,oe++,be();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{Ee()}}}});function Be(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function Ge(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function je(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function q(t){w.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=oe-1,this._flags=X}q.prototype=new w;q.prototype._refresh=function(){if(this._flags&=~B,this._flags&D)return!1;if((this._flags&(X|G))===G||(this._flags&=~X,this._globalVersion===oe))return!0;if(this._globalVersion=oe,this._flags|=D,this._version>0&&!Be(this))return this._flags&=~D,!0;let t=S;try{Ge(this),S=this;let e=this._fn();(this._flags&re||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~re,this._version++)}catch(e){this._value=e,this._flags|=re,this._version++}return S=t,je(this),this._flags&=~D,!0};q.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=X|G;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}w.prototype._subscribe.call(this,t)};q.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(w.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~G;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};q.prototype._notify=function(){if(!(this._flags&B)){this._flags|=X|B;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(q.prototype,"value",{get(){if(this._flags&D)throw f("SignalCycleDetected");let t=Ue(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&re)throw f("GetComputedError",{value:this._value});return this._value}});function Ke(t){return new q(t)}function Je(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){be();let n=S;S=void 0;try{e()}catch(r){throw t._flags&=~D,t._flags|=j,Se(t),f("CleanupEffectError",{error:r})}finally{S=n,Ee()}}}function Se(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,Je(t)}function an(t){if(S!==this)throw f("EndEffectError");je(this),S=t,this._flags&=~D,this._flags&j&&Se(this),Ee()}function Y(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=G}Y.prototype._callback=function(){let t=this._start();try{if(this._flags&j||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};Y.prototype._start=function(){if(this._flags&D)throw f("SignalCycleDetected");this._flags|=D,this._flags&=~j,Je(this),Ge(this),be();let t=S;return S=this,an.bind(this,t)};Y.prototype._notify=function(){this._flags&B||(this._flags|=B,this._nextBatchedEffect=z,z=this)};Y.prototype._dispose=function(){this._flags|=j,this._flags&D||Se(this)};function se(t){let e=new Y(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}function ze(t,e=!1){let n={};for(let r in t)if(Object.hasOwn(t,r)){let i=t[r];if(i instanceof w){if(e&&r.startsWith("_"))continue;n[r]=i.value}else n[r]=ze(i)}return n}function Xe(t,e,n=!1){for(let r in e)if(Object.hasOwn(e,r)){if(r.match(/\_\_+/))throw f("InvalidSignalKey",{key:r});let i=e[r];if(i instanceof Object&&!Array.isArray(i))t[r]||(t[r]={}),Xe(t[r],i,n);else{if(n&&t[r])continue;t[r]=new w(i)}}}function Ye(t,e){for(let n in t)if(Object.hasOwn(t,n)){let r=t[n];r instanceof w?e(n,r):Ye(r,(i,o)=>{e(`${n}.${i}`,o)})}}function ln(t,...e){let n={};for(let r of e){let i=r.split("."),o=t,s=n;for(let l=0;l<i.length-1;l++){let c=i[l];if(!o[c])return{};s[c]||(s[c]={}),o=o[c],s=s[c]}let a=i[i.length-1];s[a]=o[a]}return n}var ae=class{#e={};exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),r=this.#e;for(let s=0;s<n.length-1;s++){let a=n[s];if(!r[a])return null;r=r[a]}let i=n[n.length-1],o=r[i];if(!o)throw f("SignalNotFound",{path:e});return o}setSignal(e,n){let r=e.split("."),i=this.#e;for(let s=0;s<r.length-1;s++){let a=r[s];i[a]||(i[a]={}),i=i[a]}let o=r[r.length-1];i[o]=n}setComputed(e,n){let r=Ke(()=>n());this.setSignal(e,r)}value(e){return this.signal(e)?.value}setValue(e,n){let r=this.upsert(e,n);r.value=n}upsert(e,n){let r=e.split("."),i=this.#e;for(let l=0;l<r.length-1;l++){let c=r[l];i[c]||(i[c]={}),i=i[c]}let o=r[r.length-1],s=i[o];if(s)return(s.value===null||s.value===void 0)&&(s.value=n),s;let a=new w(n);return i[o]=a,a}remove(...e){for(let n of e){let r=n.split("."),i=this.#e;for(let s=0;s<r.length-1;s++){let a=r[s];if(!i[a])return;i=i[a]}let o=r[r.length-1];delete i[o]}}merge(e,n=!1){Xe(this.#e,e,n)}subset(...e){return ln(this.values(),...e)}walk(e){Ye(this.#e,e)}values(e=!1){return ze(this.#e,e)}JSON(e=!0,n=!1){let r=this.values(n);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var le=class{#e=new ae;#i=[];#o=[];#n={};#a=[];#t=new Map;get signals(){return this.#e}get version(){return Fe}load(...e){for(let n of e){let r;switch(n.type){case 0:{this.#o.push(n);break}case 2:{let i=n;this.#a.push(i),r=i.onGlobalInit;break}case 3:{this.#n[n.name]=n;break}case 1:{let i=n;this.#i.push(i),r=i.onGlobalInit;break}default:throw f("InvalidPluginType",{name:n.name,type:n.type})}if(r){let i=this;r({get signals(){return i.#e},effect:o=>se(o),actions:this.#n,apply:this.apply.bind(this),cleanup:this.#r.bind(this)})}}this.apply(document.body)}apply(e){let n=new Set;this.#i.forEach((r,i)=>{this.#s(e,o=>{if(!("starIgnore"in o.dataset)){i||this.#r(o);for(let s in o.dataset){if(!s.startsWith(r.name))continue;let a=s.slice(r.name.length),[l,...c]=a.split(/\_\_+/),u=l.length>0;u&&(l=l[0].toLowerCase()+l.slice(1));let m=`${o.dataset[s]}`||"",g=m,p=g.length>0,d=r.keyReq||0;if(u){if(d===2)throw f(`${r.name}KeyNotAllowed`,{key:l})}else if(d===1)throw f(`${r.name}KeyRequired`);let T=r.valReq||0;if(p){if(T===2)throw f(`${r.name}ValueNotAllowed`,{value:g})}else if(T===1)throw f(`${r.name}ValueRequired`);if(d===3||T===3){if(u&&p)throw f(`${r.name}KeyAndValueProvided`);if(!u&&!p)throw f(`${r.name}KeyOrValueRequired`)}o.id.length||(o.id=We(o)),n.clear();let _=new Map;for(let h of c){let[R,...A]=h.split(".");_.set(ke(R),new Set(A.map(M=>M.toLowerCase())))}let x=[...r.macros?.pre||[],...this.#o,...r.macros?.post||[]];for(let h of x)n.has(h)||(n.add(h),g=h.fn(g));let v=this,b={get signals(){return v.#e},effect:h=>se(h),apply:v.apply.bind(v),cleanup:v.#r.bind(v),actions:v.#n,genRX:()=>this.#l(b,...r.argNames||[]),el:o,rawKey:s,rawValue:m,key:l,value:g,mods:_},y=r.onLoad(b);y&&(this.#t.has(o)||this.#t.set(o,{id:o.id,set:new Set}),this.#t.get(o)?.set.add(y)),r?.removeOnLoad&&delete o.dataset[s]}}})})}#l(e,...n){let r=e.value.split(/;|\n/).map(d=>d.trim()).filter(d=>d!==""),i=r.length-1;r[i].startsWith("return")||(r[i]=`return (${r[i]});`);let s=r.join(`
`),a=/(\w*)\(/gm,l=s.matchAll(a),c=new Set;for(let d of l)c.add(d[1]);let u=Object.keys(this.#n).filter(d=>c.has(d)),g=`${u.map(d=>`const ${d} = ctx.actions.${d}.fn;`).join(`
`)}return (()=> {${s}})()`,p=g.trim();for(let d of u)p=p.replaceAll(`${d}(`,`${d}(ctx,`);try{let d=n||[],T=new Function("ctx",...d,p);return(..._)=>T(e,..._)}catch(d){throw f("GeneratingExpressionFailed",{error:d,fnContent:g})}}#s(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;n(e);let r=e.firstElementChild;for(;r;)this.#s(r,n),r=r.nextElementSibling}#r(e){let n=this.#t.get(e);if(n){for(let r of n.set)r();this.#t.delete(e)}}};var Ze=new le;Ze.load(De,Ve,Ce,Le);var Te=Ze;async function un(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function cn(t){let e,n,r,i=!1;return function(s){e===void 0?(e=s,n=0,r=-1):e=dn(e,s);let a=e.length,l=0;for(;n<a;){i&&(e[n]===10&&(l=++n),i=!1);let c=-1;for(;n<a&&c===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-l);break;case 13:i=!0;case 10:c=n;break}if(c===-1)break;t(e.subarray(l,c),r),l=n,r=-1}l===a?e=void 0:l!==0&&(e=e.subarray(l),n-=l)}}function fn(t,e,n){let r=Qe(),i=new TextDecoder;return function(s,a){if(s.length===0)n?.(r),r=Qe();else if(a>0){let l=i.decode(s.subarray(0,a)),c=a+(s[a+1]===32?2:1),u=i.decode(s.subarray(c));switch(l){case"data":r.data=r.data?`${r.data}
${u}`:u;break;case"event":r.event=u;break;case"id":t(r.id=u);break;case"retry":{let m=Number.parseInt(u,10);Number.isNaN(m)||e(r.retry=m);break}}}}}function dn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function Qe(){return{data:"",event:"",id:"",retry:void 0}}var pn="text/event-stream",mn=1e3,et="last-event-id";function tt(t,{signal:e,headers:n,onopen:r,onmessage:i,onclose:o,onerror:s,openWhenHidden:a,fetch:l,retryScaler:c=2,retryMaxWaitMs:u=3e4,retryMaxCount:m=10,...g}){return new Promise((p,d)=>{let T=0,_={...n};_.accept||(_.accept=pn);let x;function v(){x.abort(),document.hidden||M()}a||document.addEventListener("visibilitychange",v);let b=mn,y=0;function h(){document.removeEventListener("visibilitychange",v),window.clearTimeout(y),x.abort()}e?.addEventListener("abort",()=>{h(),p()});let R=l??window.fetch,A=r??function(){};async function M(){x=new AbortController;try{let C=await R(t,{...g,headers:_,signal:x.signal});await A(C),await un(C.body,cn(fn(N=>{N?_[et]=N:delete _[et]},N=>{b=N},i))),o?.(),h(),p()}catch(C){if(!x.signal.aborted)try{let N=s?.(C)??b;window.clearTimeout(y),y=window.setTimeout(M,N),b*=c,b=Math.min(b,u),T++,T>=m?(h(),d(f("SSE_MAX_RETRIES",{retryInterval:b,retryMaxCount:m,...g}))):console.error(`Datastar failed to reach ${g.method}:${t.toString()} retry in ${N}ms`)}catch(N){h(),d(N)}}}M()})}var K=`${L}-sse`,Ae=`${L}-settling`,W=`${L}-swapping`,ue="started",ce="finished";function V(t,e){document.addEventListener(K,n=>{if(n.detail.type!==t)return;let{argsRaw:r}=n.detail;e(r)})}function _e(t,e){document.dispatchEvent(new CustomEvent(K,{detail:{type:t,argsRaw:e}}))}var nt=t=>`${t}`.includes("text/event-stream"),rt={type:3,name:"sse",fn:async(t,e,n)=>{let{el:{id:r},signals:i}=t,{method:o,headers:s,includeLocal:a,openWhenHidden:l,retryScaler:c,retryMaxWaitMs:u,retryMaxCount:m,abort:g}=Object.assign({method:"GET",headers:{},includeLocal:!1,openWhenHidden:!1,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},n),p=o.toUpperCase();try{if(_e(ue,{elId:r}),!e?.length)throw f("NoUrlProvided");let d=Object.assign({"Content-Type":"application/json",[Oe]:!0},s),T={method:p,headers:d,openWhenHidden:l,retryScaler:c,retryMaxWaitMs:u,retryMaxCount:m,signal:g,onmessage:v=>{if(!v.event.startsWith(L))return;let b=v.event,y={},h=v.data.split(`
`);for(let A of h){let M=A.indexOf(" "),C=A.slice(0,M),N=y[C];N||(N=[],y[C]=N);let te=A.slice(M+1).trim();N.push(te)}let R={};for(let[A,M]of Object.entries(y))R[A]=M.join(`
`);_e(b,R)},onerror:v=>{if(nt(v))throw f("InvalidContentType",{url:e,error:v});v&&console.error(v.message)}},_=new URL(e,window.location.origin),x=i.JSON(!1,!a);if(p==="GET"){let v=new URLSearchParams(_.search);v.set(L,x),_.search=v.toString()}else T.body=x;try{await tt(_.toString(),T)}catch(v){if(!nt(v))throw f("SseFetchFailed",{method:p,url:e,error:v})}}finally{_e(ce,{elId:r})}}};var gn=`${L}-indicator`,Nr=`${gn}-loading`,it={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({value:t,signals:e,el:n,key:r})=>{let i=r||t,o=e.upsert(i,!1),s=a=>{let{type:l,argsRaw:{elId:c}}=a.detail;if(c===n.id)switch(l){case ue:o.value=!0;break;case ce:o.value=!1;break}};return document.addEventListener(K,s),()=>{document.removeEventListener(K,s)}}};var ot={type:2,name:I.ExecuteScript,onGlobalInit:async()=>{V(I.ExecuteScript,({autoRemove:t=`${!0}`,attributes:e=He,script:n})=>{let r=O(t);if(!n?.length)throw f("NoScriptProvided");let i=document.createElement("script");for(let o of e.split(`
`)){let s=o.indexOf(" "),a=s?o.slice(0,s):o,l=s?o.slice(s):"";i.setAttribute(a.trim(),l.trim())}i.text=n,document.head.appendChild(i),r&&i.remove()})}};var Z=document,J=!!Z.startViewTransition;var de=new WeakSet;function ut(t,e,n={}){t instanceof Document&&(t=t.documentElement);let r;typeof e=="string"?r=Sn(e):r=e;let i=Tn(r),o=vn(t,i,n);return ct(t,i,o)}function ct(t,e,n){if(n.head.block){let r=t.querySelector("head"),i=e.querySelector("head");if(r&&i){let o=dt(i,r,n);Promise.all(o).then(()=>{ct(t,e,Object.assign(n,{head:{block:!1,ignore:!0}}))});return}}if(n.morphStyle==="innerHTML")return ft(e,t,n),t.children;if(n.morphStyle==="outerHTML"||n.morphStyle==null){let r=_n(e,t,n);if(!r)throw f("NoBestMatchFound",{old:t,new:e});let i=r?.previousSibling,o=r?.nextSibling,s=pe(t,r,n);return r?An(i,s,o):[]}throw f("InvalidMorphStyle",{style:n.morphStyle})}function pe(t,e,n){if(!(n.ignoreActive&&t===document.activeElement))if(e==null){if(n.callbacks.beforeNodeRemoved(t)===!1)return;t.remove(),n.callbacks.afterNodeRemoved(t);return}else{if(me(t,e))return n.callbacks.beforeNodeMorphed(t,e)===!1?void 0:(t instanceof HTMLHeadElement&&n.head.ignore||(e instanceof HTMLHeadElement&&t instanceof HTMLHeadElement&&n.head.style!==k.Morph?dt(e,t,n):(yn(e,t),ft(e,t,n))),n.callbacks.afterNodeMorphed(t,e),t);if(n.callbacks.beforeNodeRemoved(t)===!1||n.callbacks.beforeNodeAdded(e)===!1)return;if(!t.parentElement)throw f("NoParentElementFound",{oldNode:t});return t.parentElement.replaceChild(e,t),n.callbacks.afterNodeAdded(e),n.callbacks.afterNodeRemoved(t),e}}function ft(t,e,n){let r=t.firstChild,i=e.firstChild,o;for(;r;){if(o=r,r=o.nextSibling,i==null){if(n.callbacks.beforeNodeAdded(o)===!1)return;e.appendChild(o),n.callbacks.afterNodeAdded(o),$(n,o);continue}if(pt(o,i,n)){pe(i,o,n),i=i.nextSibling,$(n,o);continue}let s=bn(t,e,o,i,n);if(s){i=st(i,s,n),pe(s,o,n),$(n,o);continue}let a=En(t,o,i,n);if(a){i=st(i,a,n),pe(a,o,n),$(n,o);continue}if(n.callbacks.beforeNodeAdded(o)===!1)return;e.insertBefore(o,i),n.callbacks.afterNodeAdded(o),$(n,o)}for(;i!==null;){let s=i;i=i.nextSibling,mt(s,n)}}function yn(t,e){let n=t.nodeType;if(n===1){for(let r of t.attributes)e.getAttribute(r.name)!==r.value&&e.setAttribute(r.name,r.value);for(let r of e.attributes)t.hasAttribute(r.name)||e.removeAttribute(r.name)}if((n===Node.COMMENT_NODE||n===Node.TEXT_NODE)&&e.nodeValue!==t.nodeValue&&(e.nodeValue=t.nodeValue),t instanceof HTMLInputElement&&e instanceof HTMLInputElement&&t.type!=="file")e.value=t.value||"",fe(t,e,"value"),fe(t,e,"checked"),fe(t,e,"disabled");else if(t instanceof HTMLOptionElement)fe(t,e,"selected");else if(t instanceof HTMLTextAreaElement&&e instanceof HTMLTextAreaElement){let r=t.value,i=e.value;r!==i&&(e.value=r),e.firstChild&&e.firstChild.nodeValue!==r&&(e.firstChild.nodeValue=r)}}function fe(t,e,n){let r=t.getAttribute(n),i=e.getAttribute(n);r!==i&&(r?e.setAttribute(n,r):e.removeAttribute(n))}function dt(t,e,n){let r=[],i=[],o=[],s=[],a=n.head.style,l=new Map;for(let u of t.children)l.set(u.outerHTML,u);for(let u of e.children){let m=l.has(u.outerHTML),g=n.head.shouldReAppend(u),p=n.head.shouldPreserve(u);m||p?g?i.push(u):(l.delete(u.outerHTML),o.push(u)):a===k.Append?g&&(i.push(u),s.push(u)):n.head.shouldRemove(u)!==!1&&i.push(u)}s.push(...l.values());let c=[];for(let u of s){let m=document.createRange().createContextualFragment(u.outerHTML).firstChild;if(!m)throw f("NewElementCouldNotBeCreated",{newNode:u});if(n.callbacks.beforeNodeAdded(m)){if(m.hasAttribute("href")||m.hasAttribute("src")){let g,p=new Promise(d=>{g=d});m.addEventListener("load",()=>{g(void 0)}),c.push(p)}e.appendChild(m),n.callbacks.afterNodeAdded(m),r.push(m)}}for(let u of i)n.callbacks.beforeNodeRemoved(u)!==!1&&(e.removeChild(u),n.callbacks.afterNodeRemoved(u));return n.head.afterHeadMorphed(e,{added:r,kept:o,removed:i}),c}function F(){}function vn(t,e,n){return{target:t,newContent:e,config:n,morphStyle:n.morphStyle,ignoreActive:n.ignoreActive,idMap:Mn(t,e),deadIds:new Set,callbacks:Object.assign({beforeNodeAdded:F,afterNodeAdded:F,beforeNodeMorphed:F,afterNodeMorphed:F,beforeNodeRemoved:F,afterNodeRemoved:F},n.callbacks),head:Object.assign({style:"merge",shouldPreserve:r=>r.getAttribute("im-preserve")==="true",shouldReAppend:r=>r.getAttribute("im-re-append")==="true",shouldRemove:F,afterHeadMorphed:F},n.head)}}function pt(t,e,n){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName?t?.id?.length&&t.id===e.id?!0:Q(n,t,e)>0:!1}function me(t,e){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName}function st(t,e,n){for(;t!==e;){let r=t;if(t=t?.nextSibling,!r)throw f("NoTemporaryNodeFound",{startInclusive:t,endExclusive:e});mt(r,n)}return $(n,e),e.nextSibling}function bn(t,e,n,r,i){let o=Q(i,n,e),s=null;if(o>0){s=r;let a=0;for(;s!=null;){if(pt(n,s,i))return s;if(a+=Q(i,s,t),a>o)return null;s=s.nextSibling}}return s}function En(t,e,n,r){let i=n,o=e.nextSibling,s=0;for(;i&&o;){if(Q(r,i,t)>0)return null;if(me(e,i))return i;if(me(o,i)&&(s++,o=o.nextSibling,s>=2))return null;i=i.nextSibling}return i}var at=new DOMParser;function Sn(t){let e=t.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(e.match(/<\/html>/)||e.match(/<\/head>/)||e.match(/<\/body>/)){let i=at.parseFromString(t,"text/html");if(e.match(/<\/html>/))return de.add(i),i;let o=i.firstChild;return o?(de.add(o),o):null}let r=at.parseFromString(`<body><template>${t}</template></body>`,"text/html").body.querySelector("template")?.content;if(!r)throw f("NoContentFound",{newContent:t});return de.add(r),r}function Tn(t){if(t==null)return document.createElement("div");if(de.has(t))return t;if(t instanceof Node){let n=document.createElement("div");return n.append(t),n}let e=document.createElement("div");for(let n of[...t])e.append(n);return e}function An(t,e,n){let r=[],i=[];for(;t;)r.push(t),t=t.previousSibling;for(;r.length>0;){let o=r.pop();i.push(o),e?.parentElement?.insertBefore(o,e)}for(i.push(e);n;)r.push(n),i.push(n),n=n.nextSibling;for(;r.length;)e?.parentElement?.insertBefore(r.pop(),e.nextSibling);return i}function _n(t,e,n){let r=t.firstChild,i=r,o=0;for(;r;){let s=wn(r,e,n);s>o&&(i=r,o=s),r=r.nextSibling}return i}function wn(t,e,n){return me(t,e)?.5+Q(n,t,e):0}function mt(t,e){$(e,t),e.callbacks.beforeNodeRemoved(t)!==!1&&(t.remove(),e.callbacks.afterNodeRemoved(t))}function Rn(t,e){return!t.deadIds.has(e)}function xn(t,e,n){return t.idMap.get(n)?.has(e)||!1}function $(t,e){let n=t.idMap.get(e);if(n)for(let r of n)t.deadIds.add(r)}function Q(t,e,n){let r=t.idMap.get(e);if(!r)return 0;let i=0;for(let o of r)Rn(t,o)&&xn(t,o,n)&&++i;return i}function lt(t,e){let n=t.parentElement,r=t.querySelectorAll("[id]");for(let i of r){let o=i;for(;o!==n&&o;){let s=e.get(o);s==null&&(s=new Set,e.set(o,s)),s.add(i.id),o=o.parentElement}}}function Mn(t,e){let n=new Map;return lt(t,n),lt(e,n),n}var ht={type:2,name:I.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");V(I.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:i=qe,settleDuration:o=`${300}`,useViewTransition:s=`${!1}`})=>{let a=Number.parseInt(o),l=O(s);e.innerHTML=n.trim();let c=[...e.content.children];for(let u of c){if(!(u instanceof Element))throw f("NoFragmentsFound");let m=r||`#${u.getAttribute("id")}`,g=[...document.querySelectorAll(m)||[]];if(!g.length)throw f("NoTargetsFound",{selectorOrID:m});J&&l?Z.startViewTransition(()=>gt(t,i,a,u,g)):gt(t,i,a,u,g)}})}};function gt(t,e,n,r,i){for(let o of i){o.classList.add(W);let s=o.outerHTML,a=o;switch(e){case k.Morph:{let u=ut(a,r,{callbacks:{beforeNodeRemoved:(m,g)=>(t.cleanup(m),!0)}});if(!u?.length)throw f("MorphFailed");a=u[0];break}case k.Inner:a.innerHTML=r.innerHTML;break;case k.Outer:a.replaceWith(r);break;case k.Prepend:a.prepend(r);break;case k.Append:a.append(r);break;case k.Before:a.before(r);break;case k.After:a.after(r);break;case k.UpsertAttributes:for(let u of r.getAttributeNames()){let m=r.getAttribute(u);a.setAttribute(u,m)}break;default:throw f("InvalidMergeMode",{mergeMode:e})}t.cleanup(a);let l=a.classList;l.add(W),t.apply(document.body),setTimeout(()=>{o.classList.remove(W),l.remove(W)},n);let c=a.outerHTML;s!==c&&(l.add(Ae),setTimeout(()=>{l.remove(Ae)},n))}}var yt={type:2,name:I.MergeSignals,onGlobalInit:async t=>{V(I.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${!1}`})=>{let{signals:r}=t,i=O(n);r.merge(ne(e),i),t.apply(document.body)})}};var vt={type:2,name:I.RemoveFragments,onGlobalInit:async()=>{V(I.RemoveFragments,({selector:t,settleDuration:e=`${300}`,useViewTransition:n=`${!1}`})=>{if(!t.length)throw f("NoSelectorProvided");let r=Number.parseInt(e),i=O(n),o=document.querySelectorAll(t),s=()=>{for(let a of o)a.classList.add(W);setTimeout(()=>{for(let a of o)a.remove()},r)};J&&i?Z.startViewTransition(()=>s()):s()})}};var bt={type:2,name:I.RemoveSignals,onGlobalInit:async t=>{V(I.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw f("NoPathsProvided");t.signals.remove(...n),t.apply(document.body)})}};var Et={type:3,name:"clipboard",fn:(t,e)=>{if(!navigator.clipboard)throw f("ClipboardNotAvailable");navigator.clipboard.writeText(e)}};var St="once",Tt="half",At="full",_t={type:1,name:"intersects",keyReq:2,mods:new Set([St,Tt,At]),onLoad:({el:t,rawKey:e,mods:n,genRX:r})=>{let i={threshold:0};n.has(At)?i.threshold=1:n.has(Tt)&&(i.threshold=.5);let o=r(),s=new IntersectionObserver(a=>{for(let l of a)l.isIntersecting&&(o(),n.has(St)&&(s.disconnect(),delete t.dataset[e]))},i);return s.observe(t),()=>s.disconnect()}};var wt="session",Rt={type:1,name:"persist",mods:new Set([wt]),onLoad:({key:t,value:e,signals:n,effect:r,mods:i})=>{t===""&&(t=L);let o=i.has(wt)?sessionStorage:localStorage,s=e.split(/\s+/).filter(c=>c!==""),a=()=>{let c=o.getItem(t)||"{}",u=JSON.parse(c);n.merge(u)},l=()=>{let c;s.length?c=n.subset(...s):c=n.values(),o.setItem(t,JSON.stringify(c))};return a(),r(()=>{l()})}};var xt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:t,genRX:e})=>{let n=e();return t(()=>{let r=n(),i=window.location.href,o=new URL(r,i).toString();window.history.replaceState({},"",o)})}};var ge="smooth",xe="instant",Me="auto",Mt="hstart",Nt="hcenter",Pt="hend",It="hnearest",Lt="vstart",kt="vcenter",Ct="vend",Dt="vnearest",Pn="focus",he="center",Vt="start",Ot="end",Ft="nearest",Ht={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([ge,xe,Me,Mt,Nt,Pt,It,Lt,kt,Ct,Dt,Pn]),onLoad:({el:t,mods:e,rawKey:n})=>{t.tabIndex||t.setAttribute("tabindex","0");let r={behavior:ge,block:he,inline:he};if(e.has(ge)&&(r.behavior=ge),e.has(xe)&&(r.behavior=xe),e.has(Me)&&(r.behavior=Me),e.has(Mt)&&(r.inline=Vt),e.has(Nt)&&(r.inline=he),e.has(Pt)&&(r.inline=Ot),e.has(It)&&(r.inline=Ft),e.has(Lt)&&(r.block=Vt),e.has(kt)&&(r.block=he),e.has(Ct)&&(r.block=Ot),e.has(Dt)&&(r.block=Ft),!(t instanceof HTMLElement||t instanceof SVGElement))throw f("NotHtmlSvgElement, el");return t.tabIndex||t.setAttribute("tabindex","0"),t.scrollIntoView(r),e.has("focus")&&t.focus(),delete t.dataset[n],()=>{}}};var qt="none",Wt="display",$t={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:t},genRX:e,effect:n})=>{let r=e();return n(async()=>{r()?t.display===qt&&t.removeProperty(Wt):t.setProperty(Wt,qt)})}};var Ne="view-transition",Ut={type:1,name:Ne,keyReq:2,valReq:1,onGlobalInit(){let t=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===Ne&&(t=!0);if(!t){let e=document.createElement("meta");e.name=Ne,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:t,el:e,genRX:n})=>{if(!J){console.error("Browser does not support view transitions");return}let r=n();return t(()=>{let i=r();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var Bt={type:1,name:"attributes",valReq:1,onLoad:({el:t,genRX:e,key:n,effect:r})=>{let i=e();return n===""?r(async()=>{let o=i();for(let[s,a]of Object.entries(o))t.setAttribute(s,a)}):(n=U(n),r(async()=>{let o=!1;try{o=i()}catch{}let s;typeof o=="string"?s=o:s=JSON.stringify(o),!s||s==="false"||s==="null"||s==="undefined"?t.removeAttribute(n):t.setAttribute(n,s)}))}};var In=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,Gt=["change","input","keydown"],jt={type:1,name:"bind",keyReq:3,valReq:3,onLoad:t=>{let{el:e,value:n,key:r,signals:i,effect:o}=t,s=r||n,a=()=>{},l=()=>{};if(typeof s!="string")throw f("InvalidExpression");let c=e.tagName.toLowerCase(),u="",m=c.includes("input"),g=e.getAttribute("type"),p=c.includes("checkbox")||m&&g==="checkbox";p&&(u=!1),m&&g==="number"&&(u=0);let T=c.includes("select"),_=c.includes("radio")||m&&g==="radio",x=m&&g==="file";_&&(e.getAttribute("name")?.length||e.setAttribute("name",s)),i.upsert(s,u),a=()=>{let b="value"in e,y=i.value(s),h=`${y}`;if(p||_){let R=e;p?R.checked=!!y||y==="true":_&&(R.checked=h===R.value)}else if(!x)if(T){let R=e;if(R.multiple)for(let A of R.options){if(A?.disabled)return;Array.isArray(y)||typeof y=="string"?A.selected=y.includes(A.value):typeof y=="number"?A.selected=y===Number(A.value):A.selected=y}else R.value=h}else b?e.value=h:e.setAttribute("value",h)},l=async()=>{if(x){let h=[...e?.files||[]],R=[],A=[],M=[];await Promise.all(h.map(te=>new Promise(nn=>{let H=new FileReader;H.onload=()=>{if(typeof H.result!="string")throw f("InvalidFileResultType",{type:typeof H.result});let ye=H.result.match(In);if(!ye?.groups)throw f("InvalidDataUri",{result:H.result});R.push(ye.groups.contents),A.push(ye.groups.mime),M.push(te.name)},H.onloadend=()=>nn(void 0),H.readAsDataURL(te)}))),i.setValue(s,R);let C=`${s}Mimes`,N=`${s}Names`;C in i&&i.upsert(C,A),N in i&&i.upsert(N,M);return}let b=i.value(s),y=e||e;if(typeof b=="number"){let h=Number(y.value||y.getAttribute("value"));i.setValue(s,h)}else if(typeof b=="string"){let h=y.value||y.getAttribute("value")||"";i.setValue(s,h)}else if(typeof b=="boolean")if(p){let h=y.checked||y.getAttribute("checked")==="true";i.setValue(s,h)}else{let h=!!(y.value||y.getAttribute("value"));i.setValue(s,h)}else if(!(typeof b>"u"))if(Array.isArray(b))if(T){let A=[...e.selectedOptions].filter(M=>M.selected).map(M=>M.value);i.setValue(s,A)}else{let h=JSON.stringify(y.value.split(","));i.setValue(s,h)}else throw f("UnsupportedSignalType",{current:typeof b})};for(let b of Gt)e.addEventListener(b,l);let v=o(()=>a());return()=>{v();for(let b of Gt)e.removeEventListener(b,l)}}};var Kt={type:1,name:"class",valReq:1,onLoad:({key:t,el:e,genRX:n,effect:r})=>{let i=e.classList,o=n();return r(()=>{if(t===""){let s=o();for(let[a,l]of Object.entries(s)){let c=a.split(/\s+/);l?i.add(...c):i.remove(...c)}}else{let s=o(),a=U(t);s?i.add(a):i.remove(a)}})}};function Pe(t){if(!t||t.size<=0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ee(t,e,n=!1){return t?t.has(e.toLowerCase()):n}function Jt(t,e,n=!1,r=!0){let i=-1,o=()=>i&&clearTimeout(i);return function(...a){o(),n&&!i&&t(...a),i=setTimeout(()=>{r&&t(...a),o()},e)}}function zt(t,e,n=!0,r=!1){let i=!1;return function(...s){i||(n&&t(...s),i=!0,setTimeout(()=>{i=!1,r&&t(...s)},e))}}var Ie=new Map,Ln="evt",Xt={type:1,name:"on",keyReq:1,valReq:1,argNames:[Ln],macros:{pre:[{type:0,name:"evtEsc",fn:t=>t.replaceAll(/evt.([\w\.]+)value/gm,"EVT_$1_VALUE")}],post:[{type:0,name:"evtUnesc",fn:t=>t.replaceAll(/EVT_([\w\.]+)_VALUE/gm,"evt.$1value")}]},onLoad:({el:t,key:e,genRX:n,mods:r,signals:i,effect:o})=>{let s=n(),a=t;r.has("window")&&(a=window);let l=p=>{p&&(r.has("prevent")&&p.preventDefault(),r.has("stop")&&p.stopPropagation()),s(p)},c=r.get("debounce");if(c){let p=Pe(c),d=ee(c,"leading",!1),T=!ee(c,"notrail",!1);l=Jt(l,p,d,T)}let u=r.get("throttle");if(u){let p=Pe(u),d=!ee(u,"noleading",!1),T=ee(u,"trail",!1);l=zt(l,p,d,T)}let m={capture:!0,passive:!1,once:!1};r.has("capture")||(m.capture=!1),r.has("passive")&&(m.passive=!0),r.has("once")&&(m.once=!0);let g=U(e).toLowerCase();switch(g){case"load":return l(),delete t.dataset.onLoad,()=>{};case"raf":{let p,d=()=>{l(),p=requestAnimationFrame(d)};return p=requestAnimationFrame(d),()=>{p&&cancelAnimationFrame(p)}}case"signals-change":return $e(t,()=>{Ie.delete(t.id)}),o(()=>{let p=r.has("remote"),d=i.JSON(!1,p);(Ie.get(t.id)||"")!==d&&(Ie.set(t.id,d),l())});default:{if(r.has("outside")){a=document;let d=l,T=!1;l=x=>{let v=x?.target;if(!v)return;let b=t.id===v.id;b&&T&&(T=!1),!b&&!T&&(d(x),T=!0)}}return a.addEventListener(g,l,m),()=>{a.removeEventListener(g,l)}}}}};var Yt={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:t,key:e,value:n,signals:r})=>{let i=e||n;return r.upsert(i,t),()=>r.setValue(i,null)}};var Zt={type:1,name:"text",keyReq:2,valReq:1,onLoad:t=>{let{el:e,genRX:n,effect:r}=t,i=n();return e instanceof HTMLElement||f("NotHtmlElement"),r(()=>{let o=i(t);e.textContent=`${o}`})}};var{round:kn,max:Cn,min:Dn}=Math,Qt={type:3,name:"fit",fn:(t,e,n,r,i,o,s=!1,a=!1)=>{let l=(e-n)/(r-n)*(o-i)+i;return a&&(l=kn(l)),s&&(l=Cn(i,Dn(o,l))),l}};var en={type:3,name:"setAll",fn:({signals:t},e,n)=>{t.walk((r,i)=>{r.startsWith(e)&&(i.value=n)})}};var tn={type:3,name:"toggleAll",fn:({signals:t},e)=>{t.walk((n,r)=>{n.startsWith(e)&&(r.value=!r.value)})}};Te.load(jt,it,Yt,Bt,Kt,Xt,$t,Zt,rt,ht,yt,vt,bt,ot,Et,_t,Rt,xt,Ht,Ut,Qt,en,tn);var Co=Te;export{Co as Datastar};
//# sourceMappingURL=datastar.js.map
