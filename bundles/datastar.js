// Datastar v1.0.0-beta.9
var We=/🖕JS_DS🚀/.source,ce=We.slice(0,5),Re=We.slice(4),H="datastar",$e="Datastar-Request",Ge=1e3,_e="type module",ue=!1,Ue=!1,je=!0,_={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ke=_.Morph,F={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var M=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(M||{});var W=`${H}-signals`;var B=n=>n.trim()==="true",K=n=>n.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,t)=>(t?"-":"")+e.toLowerCase()),Y=n=>K(n).replace(/-./g,e=>e[1].toUpperCase()),we=n=>K(n).replace(/-/g,"_"),un=n=>Y(n).replace(/^./,e=>e[0].toUpperCase()),pe=n=>new Function(`return Object.assign({}, ${n})`)(),J=n=>n.startsWith("$")?n.slice(1):n,pn={kebab:K,snake:we,pascal:un};function C(n,e){for(let t of e.get("case")||[]){let r=pn[t];r&&(n=r(n))}return n}var fn="computed",Be={type:1,name:fn,keyReq:1,valReq:1,onLoad:({key:n,mods:e,signals:t,genRX:r})=>{n=C(n,e);let{deps:s,rxFn:i}=r();t.setComputed(n,s,i)}};var Je={type:1,name:"signals",onLoad:n=>{let{key:e,mods:t,signals:r,value:s,genRX:i}=n,o=t.has("ifmissing"),{rxFn:u}=i();if(e!==""){let l=C(e,t),v=s===""?s:u();o?r.upsertIfMissing(l,v):r.setValue(l,v)}else{let l=pe(n.value);n.value=JSON.stringify(l);let v=u();r.merge(v,o)}}};var ze={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ee=class{#e=0;#t;constructor(e=H){this.#t=e}with(e){if(typeof e=="string")for(let t of e.split(""))this.with(t.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function fe(n){if(n.id)return n.id;let e=new ee,t=n;for(;t;){if(e.with(t.tagName||""),t.id){e.with(t.id);break}let r=t?.parentNode;r&&e.with([...r.children].indexOf(t)),t=r}return e.string}function de(n,e){return new ee().with(n).with(e).value}function oe(n,e){if(!n||!(n instanceof HTMLElement||n instanceof SVGElement))return null;let t=n.dataset;if("starIgnore"in t)return null;"starIgnore__self"in t||e(n);let r=n.firstElementChild;for(;r;)oe(r,e),r=r.nextElementSibling}var dn="https://data-star.dev/errors";function xe(n,e,t={}){let r=new Error;r.name=`${H} ${n} error`;let s=we(e),i=new URLSearchParams({metadata:JSON.stringify(t)}).toString(),o=JSON.stringify(t,null,2);return r.message=`${e}
More info: ${dn}/${n}/${s}?${i}
Context: ${o}`,r}function Me(n,e,t={}){return xe("internal",e,Object.assign({from:n},t))}function q(n,e,t={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]}};return xe("init",n,Object.assign(r,t))}function P(n,e,t={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return xe("runtime",n,Object.assign(r,t))}var Xe="namespacedSignals",te=n=>{document.dispatchEvent(new CustomEvent(W,{detail:Object.assign({added:[],removed:[],updated:[]},n)}))};function Ye(n,e=!1){let t={};for(let r in n)if(Object.hasOwn(n,r)){if(e&&r.startsWith("_"))continue;let s=n[r];s instanceof ne?t[r]=s.value:t[r]=Ye(s)}return t}function Ze(n,e,t,r=!1){let s={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw Me(Xe,"InvalidSignalKey",{key:i});let o=t?`${t}.${i}`:i,u=e[i];if(u instanceof Object&&!Array.isArray(u)){n[i]||(n[i]={});let l=Ze(n[i],u,o,r);s.added.push(...l.added.map(v=>`${o}.${v}`)),s.removed.push(...l.removed.map(v=>`${o}.${v}`)),s.updated.push(...l.updated.map(v=>`${o}.${v}`))}else{if(Object.hasOwn(n,i)){if(r)continue;let E=n[i];if(E instanceof re){let L=E.value;E.value=u,L!==u&&s.updated.push(o);continue}}let v=new re(u,()=>te({updated:[o]}));n[i]=v,s.added.push(o)}}return s}function Qe(n,e){for(let t in n)if(Object.hasOwn(n,t)){let r=n[t];r instanceof ne?e(t,r):Qe(r,(s,i)=>{e(`${t}.${s}`,i)})}}function mn(n,...e){let t={};for(let r of e){let s=r.split("."),i=n,o=t;for(let l=0;l<s.length-1;l++){let v=s[l];if(!i[v])return{};o[v]||(o[v]={}),i=i[v],o=o[v]}let u=s[s.length-1];o[u]=i[u]}return t}var me=class{#e={};exists(e){return!!this.signal(e)}signal(e){let t=e.split("."),r=this.#e;for(let o=0;o<t.length-1;o++){let u=t[o];if(!r[u])return null;r=r[u]}let s=t[t.length-1],i=r[s];if(!i)throw Me(Xe,"SignalNotFound",{path:e});return i}setSignal(e,t){let r=e.split("."),s=this.#e;for(let o=0;o<r.length-1;o++){let u=r[o];s[u]||(s[u]={}),s=s[u]}let i=r[r.length-1];s[i]=t}setComputed(e,t,r){let s=gn(t,r);this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,t){let{signal:r}=this.upsertIfMissing(e,t),s=r.value;r.value=t,s!==t&&te({updated:[e]})}upsertIfMissing(e,t){let r=e.split("."),s=this.#e;for(let l=0;l<r.length-1;l++){let v=r[l];s[v]||(s[v]={}),s=s[v]}let i=r[r.length-1],o=s[i];if(o instanceof re)return{signal:o,inserted:!1};let u=new re(t);return u.onChange=()=>{te({updated:[e]})},s[i]=u,te({added:[e]}),{signal:u,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let t=Array();for(let r of e){let s=r.split("."),i=this.#e;for(let u=0;u<s.length-1;u++){let l=s[u];if(!i[l])return;i=i[l]}let o=s[s.length-1];delete i[o],t.push(r)}te({removed:t})}merge(e,t=!1){let r=Ze(this.#e,e,"",t);(r.added.length||r.removed.length||r.updated.length)&&te(r)}subset(...e){return mn(this.values(),...e)}walk(e){Qe(this.#e,e)}paths(){let e=new Array;return this.walk(t=>e.push(t)),e}values(e=!1){return Ye(this.#e,e)}JSON(e=!0,t=!1){let r=this.values(t);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var ne=class{},re=class extends ne{constructor(t,r){super();this.val=t;this.onChange=r;this.subs=new Set;this.ver=1}set value(t){this.val!==t&&(this.val=t,this.ver++,this.markDirty(),this.onChange?.(t))}markDirty(){for(let t of this.subs)t.markDirty()}get value(){return this.val}version(){return this.ver}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};var De=class extends ne{constructor(t,r){super();this.deps=t;this.fn=r;this.subs=new Set;this.isDirty=!0;this.ver=1;this.versionSum=0;for(let s of t)s.addSubscribers(this)}get value(){if(!this.isDirty)return this.val;this.isDirty=!1;let t=0;for(let i of this.deps)t+=i.version();if(t===this.versionSum)return this.val;this.versionSum=t;let r=this.deps.map(i=>i.value),s=this.fn(...r);return this.val===s?this.val:(this.val=s,this.ver++,this.val)}version(){return this.ver}markDirty(){this.isDirty=!0;for(let t of this.subs)t.markDirty()}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};function gn(n,e){return new De(n,e)}var Pe=class{constructor(e,t){this.deps=e;this.fn=t;this.depsVersionSum=-1;for(let r of e)r.addSubscribers(this)}markDirty(){let e=0;for(let r of this.deps)e+=r.version();if(e===this.depsVersionSum)return;this.depsVersionSum=e;let t=this.deps.map(r=>r.value);this.fn(...t)}};function Le(n,e){let t=new Pe(n,e);return t.markDirty(),()=>{for(let r of n)r.removeSubscribers(t)}}var et=new me,ge={},Ie=[],Z=new Map,Ne=null,Ve="";function tt(n){Ve=n}function he(...n){for(let e of n){let t={plugin:e,signals:et,effect:(s,i)=>Le(s,i),actions:ge,removals:Z,applyToElement:ye},r;switch(e.type){case 3:{ge[e.name]=e;break}case 1:{let s=e;Ie.push(s),r=s.onGlobalInit;break}case 2:{r=e.onGlobalInit;break}default:throw q("InvalidPluginType",t)}r&&r(t)}Ie.sort((e,t)=>{let r=t.name.length-e.name.length;return r!==0?r:e.name.localeCompare(t.name)})}function Ce(){queueMicrotask(()=>{ye(document.documentElement),hn()})}function ye(n){oe(n,e=>{let t=new Array,r=Z.get(e.id)||new Map,s=new Map([...r]),i=new Map;for(let o of Object.keys(e.dataset)){if(!o.startsWith(Ve))break;let u=e.dataset[o]||"",l=de(o,u);i.set(o,l),r.has(l)?s.delete(l):t.push(o)}for(let[o,u]of s)u();for(let o of t){let u=i.get(o);yn(e,o,u)}})}function hn(){Ne||(Ne=new MutationObserver(n=>{let e=new Set,t=new Set;for(let{target:r,type:s,addedNodes:i,removedNodes:o}of n)switch(s){case"childList":{for(let u of o)e.add(u);for(let u of i)t.add(u)}break;case"attributes":{t.add(r);break}}for(let r of e){let s=Z.get(r.id);if(s){for(let[i,o]of s)o(),s.delete(i);s.size===0&&Z.delete(r.id)}}for(let r of t)ye(r)}),Ne.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function yn(n,e,t){let r=Y(e.slice(Ve.length)),s=Ie.find(y=>new RegExp(`^${y.name}([A-Z]|_|$)`).test(r));if(!s)return;n.id.length||(n.id=fe(n));let[i,...o]=r.slice(s.name.length).split(/\_\_+/),u=i.length>0;u&&(i=Y(i));let l=n.dataset[e]||"",v=l.length>0,E={signals:et,applyToElement:ye,effect:(y,A)=>Le(y,A),actions:ge,removals:Z,genRX:()=>vn(E,...s.argNames||[]),plugin:s,el:n,rawKey:r,key:i,value:l,mods:new Map},L=s.keyReq||0;if(u){if(L===2)throw P(`${s.name}KeyNotAllowed`,E)}else if(L===1)throw P(`${s.name}KeyRequired`,E);let b=s.valReq||0;if(v){if(b===2)throw P(`${s.name}ValueNotAllowed`,E)}else if(b===1)throw P(`${s.name}ValueRequired`,E);if(L===3||b===3){if(u&&v)throw P(`${s.name}KeyAndValueProvided`,E);if(!u&&!v)throw P(`${s.name}KeyOrValueRequired`,E)}for(let y of o){let[A,...T]=y.split(".");E.mods.set(Y(A),new Set(T.map(c=>c.toLowerCase())))}let S=s.onLoad(E)??(()=>{}),g=Z.get(n.id);g||(g=new Map,Z.set(n.id,g)),g.set(t,S)}function vn(n,...e){let t=new Array,r="",s=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=n.value.trim().match(s);if(i){let g=i.length-1,y=i[g].trim();y.startsWith("return")||(i[g]=`return (${y});`),r=i.join(`;
`)}let o=new Map,u=new RegExp(`(?:${ce})(.*?)(?:${Re})`,"gm");for(let g of r.matchAll(u)){let y=g[1],A=new ee("dsEscaped").with(y).string;o.set(A,y),r=r.replace(ce+y+Re,A)}let l=/@(\w*)\(/gm,v=r.matchAll(l),E=new Set;for(let g of v)E.add(g[1]);let L=new RegExp(`@(${Object.keys(ge).join("|")})\\(`,"gm");r=r.replaceAll(L,"ctx.actions.$1.fn(ctx,");let b=n.signals.paths();if(b.length){let g=new RegExp(`\\$(${b.join("|")})(\\W|$)`,"gm");r=r.replaceAll(g,"ctx.signals.signal('$1').value$2");let y=/ctx.signals.signal\('(.+?)'\).value/gm;for(let A of r.matchAll(y)){let T=A[1],c=n.signals.signal(T);c&&t.push(c)}}for(let[g,y]of o)r=r.replace(g,y);let S=`return (() => {
${r}
})()`;n.fnContent=S;try{let g=new Function("ctx",...e,S);return{deps:t,rxFn:(...y)=>{try{return g(n,...y)}catch(A){throw P("ExecuteExpression",n,{error:A.message})}}}}catch(g){throw P("GenerateExpression",n,{error:g.message})}}he(ze,Je,Be);var Q=`${H}-sse`,ve="started",be="finished",nt="error",rt="retrying",Se="all-retries-failed";function U(n,e){document.addEventListener(Q,t=>{if(t.detail.type!==n)return;let{argsRaw:r}=t.detail;e(r)})}function st(n,e,t){n.dispatchEvent(new CustomEvent(Q,{detail:{type:e,argsRaw:t},bubbles:!0}))}async function bn(n,e){let t=n.getReader(),r;for(;!(r=await t.read()).done;)e(r.value)}function Sn(n){let e,t,r,s=!1;return function(o){e===void 0?(e=o,t=0,r=-1):e=En(e,o);let u=e.length,l=0;for(;t<u;){s&&(e[t]===10&&(l=++t),s=!1);let v=-1;for(;t<u&&v===-1;++t)switch(e[t]){case 58:r===-1&&(r=t-l);break;case 13:s=!0;case 10:v=t;break}if(v===-1)break;n(e.subarray(l,v),r),l=t,r=-1}l===u?e=void 0:l!==0&&(e=e.subarray(l),t-=l)}}function An(n,e,t){let r=it(),s=new TextDecoder;return function(o,u){if(o.length===0)t?.(r),r=it();else if(u>0){let l=s.decode(o.subarray(0,u)),v=u+(o[u+1]===32?2:1),E=s.decode(o.subarray(v));switch(l){case"data":r.data=r.data?`${r.data}
${E}`:E;break;case"event":r.event=E;break;case"id":n(r.id=E);break;case"retry":{let L=Number.parseInt(E,10);Number.isNaN(L)||e(r.retry=L);break}}}}}function En(n,e){let t=new Uint8Array(n.length+e.length);return t.set(n),t.set(e,n.length),t}function it(){return{data:"",event:"",id:"",retry:void 0}}var Tn="text/event-stream",ot="last-event-id";function at(n,e,{signal:t,headers:r,onopen:s,onmessage:i,onclose:o,onerror:u,openWhenHidden:l,fetch:v,retryInterval:E=1e3,retryScaler:L=2,retryMaxWaitMs:b=3e4,retryMaxCount:S=10,...g}){return new Promise((y,A)=>{let T=0,c={...r};c.accept||(c.accept=Tn);let d;function p(){d.abort(),document.hidden||R()}l||document.addEventListener("visibilitychange",p);let a=0;function h(){document.removeEventListener("visibilitychange",p),window.clearTimeout(a),d.abort()}t?.addEventListener("abort",()=>{h(),y()});let m=v??window.fetch,f=s??function(){};async function R(){d=new AbortController;try{let x=await m(e,{...g,headers:c,signal:d.signal});await f(x),await bn(x.body,Sn(An(w=>{w?c[ot]=w:delete c[ot]},w=>{E=w},i))),o?.(),h(),y()}catch(x){if(!d.signal.aborted)try{let w=u?.(x)??E;window.clearTimeout(a),a=window.setTimeout(R,w),E*=L,E=Math.min(E,b),T++,T>S?(st(n,Se,{}),h(),A("Max retries reached.")):console.error(`Datastar failed to reach ${e.toString()} retrying in ${w}ms.`)}catch(w){h(),A(w)}}}R()})}function se(n,e,t){n.dispatchEvent(new CustomEvent(Q,{detail:{type:e,argsRaw:t},bubbles:!0}))}var lt=n=>`${n}`.includes("text/event-stream"),j=async(n,e,t,r)=>{let{el:{id:s},el:i,signals:o}=n,{headers:u,contentType:l,includeLocal:v,selector:E,openWhenHidden:L,retryInterval:b,retryScaler:S,retryMaxWaitMs:g,retryMaxCount:y,abort:A}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:Ge,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),T=e.toLowerCase(),c=()=>{};try{if(se(i,ve,{elId:s}),!t?.length)throw P("SseNoUrlProvided",n,{action:T});let d={};d[$e]=!0,l==="json"&&(d["Content-Type"]="application/json");let p=Object.assign({},d,u),a={method:e,headers:p,openWhenHidden:L,retryInterval:b,retryScaler:S,retryMaxWaitMs:g,retryMaxCount:y,signal:A,onopen:async f=>{if(f.status>=400){let R=f.status.toString();se(i,nt,{status:R})}},onmessage:f=>{if(!f.event.startsWith(H))return;let R=f.event,x={},w=f.data.split(`
`);for(let I of w){let N=I.indexOf(" "),k=I.slice(0,N),V=x[k];V||(V=[],x[k]=V);let $=I.slice(N+1);V.push($)}let D={};for(let[I,N]of Object.entries(x))D[I]=N.join(`
`);se(i,R,D)},onerror:f=>{if(lt(f))throw P("InvalidContentType",n,{url:t});f&&(console.error(f.message),se(i,rt,{message:f.message}))}},h=new URL(t,window.location.origin),m=new URLSearchParams(h.search);if(l==="json"){let f=o.JSON(!1,!v);e==="GET"?m.set(H,f):a.body=f}else if(l==="form"){let f=E?document.querySelector(E):i.closest("form");if(f===null)throw E?P("SseFormNotFound",n,{action:T,selector:E}):P("SseClosestFormNotFound",n,{action:T});if(i!==f){let x=w=>w.preventDefault();f.addEventListener("submit",x),c=()=>f.removeEventListener("submit",x)}if(!f.checkValidity()){f.reportValidity(),c();return}let R=new FormData(f);if(e==="GET"){let x=new URLSearchParams(R);for(let[w,D]of x)m.set(w,D)}else a.body=R}else throw P("SseInvalidContentType",n,{action:T,contentType:l});h.search=m.toString();try{await at(i,h.toString(),a)}catch(f){if(!lt(f))throw P("SseFetchFailed",n,{method:e,url:t,error:f});f==="Max retries reached."&&(se(i,Se,{message:f}),console.error(f))}}finally{se(i,be,{elId:s}),c()}};var ct={type:3,name:"delete",fn:async(n,e,t)=>j(n,"DELETE",e,{...t})};var ut={type:3,name:"get",fn:async(n,e,t)=>j(n,"GET",e,{...t})};var pt={type:3,name:"patch",fn:async(n,e,t)=>j(n,"PATCH",e,{...t})};var ft={type:3,name:"post",fn:async(n,e,t)=>j(n,"POST",e,{...t})};var dt={type:3,name:"put",fn:async(n,e,t)=>j(n,"PUT",e,{...t})};var mt={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?C(e,t):J(s),{signal:o}=r.upsertIfMissing(i,!1),u=l=>{let{type:v,argsRaw:{elId:E}}=l.detail;if(E===n.id)switch(v){case ve:o.value=!0;break;case be:o.value=!1;break}};return document.addEventListener(Q,u),()=>{o.value=!1,document.removeEventListener(Q,u)}}};var gt={type:2,name:F.ExecuteScript,onGlobalInit:async n=>{U(F.ExecuteScript,({autoRemove:e=`${je}`,attributes:t=_e,script:r})=>{let s=B(e);if(!r?.length)throw q("NoScriptProvided",n);let i=document.createElement("script");for(let o of t.split(`
`)){let u=o.indexOf(" "),l=u?o.slice(0,u):o,v=u?o.slice(u):"";i.setAttribute(l.trim(),v.trim())}i.text=r,document.head.appendChild(i),s&&i.remove()})}};var ae=document,z=!!ae.startViewTransition;var ht=function(){"use strict";let n=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:n,afterNodeAdded:n,beforeNodeMorphed:n,afterNodeMorphed:n,beforeNodeRemoved:n,afterNodeRemoved:n,beforeAttributeUpdated:n},head:{style:"merge",shouldPreserve:b=>b.getAttribute("im-preserve")==="true",shouldReAppend:b=>b.getAttribute("im-re-append")==="true",shouldRemove:n,afterHeadMorphed:n},restoreFocus:!0};function t(b,S,g={}){b=E(b);let y=L(S),A=v(b,y,g),T=s(A,()=>u(A,b,y,c=>c.morphStyle==="innerHTML"?(i(c,b,y),Array.from(b.childNodes)):r(c,b,y)));return A.pantry.remove(),T}function r(b,S,g){let y=L(S);return i(b,y,g,S,S.nextSibling),Array.from(y.childNodes)}function s(b,S){if(!b.config.restoreFocus)return S();let g=document.activeElement;if(!(g instanceof HTMLInputElement||g instanceof HTMLTextAreaElement))return S();let{id:y,selectionStart:A,selectionEnd:T}=g,c=S();return y&&y!==document.activeElement?.id&&(g=b.target.querySelector(`[id="${y}"]`),g?.focus()),g&&!g.selectionEnd&&T&&g.setSelectionRange(A,T),c}let i=function(){function b(p,a,h,m=null,f=null){a instanceof HTMLTemplateElement&&h instanceof HTMLTemplateElement&&(a=a.content,h=h.content),m||=a.firstChild;for(let R of h.childNodes){if(m&&m!=f){let w=g(p,R,m,f);if(w){w!==m&&A(p,m,w),o(w,R,p),m=w.nextSibling;continue}}if(R instanceof Element&&p.persistentIds.has(R.id)){let w=T(a,R.id,m,p);o(w,R,p),m=w.nextSibling;continue}let x=S(a,R,m,p);x&&(m=x.nextSibling)}for(;m&&m!=f;){let R=m;m=m.nextSibling,y(p,R)}}function S(p,a,h,m){if(m.callbacks.beforeNodeAdded(a)===!1)return null;if(m.idMap.has(a)){let f=document.createElement(a.tagName);return p.insertBefore(f,h),o(f,a,m),m.callbacks.afterNodeAdded(f),f}else{let f=document.importNode(a,!0);return p.insertBefore(f,h),m.callbacks.afterNodeAdded(f),f}}let g=function(){function p(m,f,R,x){let w=null,D=f.nextSibling,I=0,N=R;for(;N&&N!=x;){if(h(N,f)){if(a(m,N,f))return N;w===null&&(m.idMap.has(N)||(w=N))}if(w===null&&D&&h(N,D)&&(I++,D=D.nextSibling,I>=2&&(w=void 0)),N.contains(document.activeElement))break;N=N.nextSibling}return w||null}function a(m,f,R){let x=m.idMap.get(f),w=m.idMap.get(R);if(!w||!x)return!1;for(let D of x)if(w.has(D))return!0;return!1}function h(m,f){let R=m,x=f;return R.nodeType===x.nodeType&&R.tagName===x.tagName&&(!R.id||R.id===x.id)}return p}();function y(p,a){if(p.idMap.has(a))d(p.pantry,a,null);else{if(p.callbacks.beforeNodeRemoved(a)===!1)return;a.parentNode?.removeChild(a),p.callbacks.afterNodeRemoved(a)}}function A(p,a,h){let m=a;for(;m&&m!==h;){let f=m;m=m.nextSibling,y(p,f)}return m}function T(p,a,h,m){let f=m.target.id===a&&m.target||m.target.querySelector(`[id="${a}"]`)||m.pantry.querySelector(`[id="${a}"]`);return c(f,m),d(p,f,h),f}function c(p,a){let h=p.id;for(;p=p.parentNode;){let m=a.idMap.get(p);m&&(m.delete(h),m.size||a.idMap.delete(p))}}function d(p,a,h){if(p.moveBefore)try{p.moveBefore(a,h)}catch{p.insertBefore(a,h)}else p.insertBefore(a,h)}return b}(),o=function(){function b(c,d,p){return p.ignoreActive&&c===document.activeElement?null:(p.callbacks.beforeNodeMorphed(c,d)===!1||(c instanceof HTMLHeadElement&&p.head.ignore||(c instanceof HTMLHeadElement&&p.head.style!=="morph"?l(c,d,p):(S(c,d,p),T(c,p)||i(p,c,d))),p.callbacks.afterNodeMorphed(c,d)),c)}function S(c,d,p){let a=d.nodeType;if(a===1){let h=c,m=d,f=h.attributes,R=m.attributes;for(let x of R)A(x.name,h,"update",p)||h.getAttribute(x.name)!==x.value&&h.setAttribute(x.name,x.value);for(let x=f.length-1;0<=x;x--){let w=f[x];if(w&&!m.hasAttribute(w.name)){if(A(w.name,h,"remove",p))continue;h.removeAttribute(w.name)}}T(h,p)||g(h,m,p)}(a===8||a===3)&&c.nodeValue!==d.nodeValue&&(c.nodeValue=d.nodeValue)}function g(c,d,p){if(c instanceof HTMLInputElement&&d instanceof HTMLInputElement&&d.type!=="file"){let a=d.value,h=c.value;y(c,d,"checked",p),y(c,d,"disabled",p),d.hasAttribute("value")?h!==a&&(A("value",c,"update",p)||(c.setAttribute("value",a),c.value=a)):A("value",c,"remove",p)||(c.value="",c.removeAttribute("value"))}else if(c instanceof HTMLOptionElement&&d instanceof HTMLOptionElement)y(c,d,"selected",p);else if(c instanceof HTMLTextAreaElement&&d instanceof HTMLTextAreaElement){let a=d.value,h=c.value;if(A("value",c,"update",p))return;a!==h&&(c.value=a),c.firstChild&&c.firstChild.nodeValue!==a&&(c.firstChild.nodeValue=a)}}function y(c,d,p,a){let h=d[p],m=c[p];if(h!==m){let f=A(p,c,"update",a);f||(c[p]=d[p]),h?f||c.setAttribute(p,""):A(p,c,"remove",a)||c.removeAttribute(p)}}function A(c,d,p,a){return c==="value"&&a.ignoreActiveValue&&d===document.activeElement?!0:a.callbacks.beforeAttributeUpdated(c,d,p)===!1}function T(c,d){return!!d.ignoreActiveValue&&c===document.activeElement&&c!==document.body}return b}();function u(b,S,g,y){if(b.head.block){let A=S.querySelector("head"),T=g.querySelector("head");if(A&&T){let c=l(A,T,b);return Promise.all(c).then(()=>{let d=Object.assign(b,{head:{block:!1,ignore:!0}});return y(d)})}}return y(b)}function l(b,S,g){let y=[],A=[],T=[],c=[],d=new Map;for(let a of S.children)d.set(a.outerHTML,a);for(let a of b.children){let h=d.has(a.outerHTML),m=g.head.shouldReAppend(a),f=g.head.shouldPreserve(a);h||f?m?A.push(a):(d.delete(a.outerHTML),T.push(a)):g.head.style==="append"?m&&(A.push(a),c.push(a)):g.head.shouldRemove(a)!==!1&&A.push(a)}c.push(...d.values());let p=[];for(let a of c){let h=document.createRange().createContextualFragment(a.outerHTML).firstChild;if(g.callbacks.beforeNodeAdded(h)!==!1){if("href"in h&&h.href||"src"in h&&h.src){let m,f=new Promise(function(R){m=R});h.addEventListener("load",function(){m()}),p.push(f)}b.appendChild(h),g.callbacks.afterNodeAdded(h),y.push(h)}}for(let a of A)g.callbacks.beforeNodeRemoved(a)!==!1&&(b.removeChild(a),g.callbacks.afterNodeRemoved(a));return g.head.afterHeadMorphed(b,{added:y,kept:T,removed:A}),p}let v=function(){function b(d,p,a){let{persistentIds:h,idMap:m}=T(d,p),f=S(a),R=f.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(R))throw`Do not understand how to morph style ${R}`;return{target:d,newContent:p,config:f,morphStyle:R,ignoreActive:f.ignoreActive,ignoreActiveValue:f.ignoreActiveValue,restoreFocus:f.restoreFocus,idMap:m,persistentIds:h,pantry:g(),callbacks:f.callbacks,head:f.head}}function S(d){let p=Object.assign({},e);return Object.assign(p,d),p.callbacks=Object.assign({},e.callbacks,d.callbacks),p.head=Object.assign({},e.head,d.head),p}function g(){let d=document.createElement("div");return d.hidden=!0,document.body.insertAdjacentElement("afterend",d),d}function y(d){let p=Array.from(d.querySelectorAll("[id]"));return d.id&&p.push(d),p}function A(d,p,a,h){for(let m of h)if(p.has(m.id)){let f=m;for(;f;){let R=d.get(f);if(R==null&&(R=new Set,d.set(f,R)),R.add(m.id),f===a)break;f=f.parentElement}}}function T(d,p){let a=y(d),h=y(p),m=c(a,h),f=new Map;A(f,m,d,a);let R=p.__idiomorphRoot||p;return A(f,m,R,h),{persistentIds:m,idMap:f}}function c(d,p){let a=new Set,h=new Map;for(let{id:f,tagName:R}of d)h.has(f)?a.add(f):h.set(f,R);let m=new Set;for(let{id:f,tagName:R}of p)m.has(f)?a.add(f):h.get(f)===R&&m.add(f);for(let f of a)m.delete(f);return m}return b}(),{normalizeElement:E,normalizeParent:L}=function(){let b=new WeakSet;function S(T){return T instanceof Document?T.documentElement:T}function g(T){if(T==null)return document.createElement("div");if(typeof T=="string")return g(A(T));if(b.has(T))return T;if(T instanceof Node){if(T.parentNode)return new y(T);{let c=document.createElement("div");return c.append(T),c}}else{let c=document.createElement("div");for(let d of[...T])c.append(d);return c}}class y{constructor(c){this.originalNode=c,this.realParentNode=c.parentNode,this.previousSibling=c.previousSibling,this.nextSibling=c.nextSibling}get childNodes(){let c=[],d=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;d&&d!=this.nextSibling;)c.push(d),d=d.nextSibling;return c}querySelectorAll(c){return this.childNodes.reduce((d,p)=>{if(p instanceof Element){p.matches(c)&&d.push(p);let a=p.querySelectorAll(c);for(let h=0;h<a.length;h++)d.push(a[h])}return d},[])}insertBefore(c,d){return this.realParentNode.insertBefore(c,d)}moveBefore(c,d){return this.realParentNode.moveBefore(c,d)}get __idiomorphRoot(){return this.originalNode}}function A(T){let c=new DOMParser,d=T.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(d.match(/<\/html>/)||d.match(/<\/head>/)||d.match(/<\/body>/)){let p=c.parseFromString(T,"text/html");if(d.match(/<\/html>/))return b.add(p),p;{let a=p.firstChild;return a&&b.add(a),a}}else{let a=c.parseFromString("<body><template>"+T+"</template></body>","text/html").body.querySelector("template").content;return b.add(a),a}}return{normalizeElement:S,normalizeParent:g}}();return{morph:t,defaults:e}}();var vt={type:2,name:F.MergeFragments,onGlobalInit:async n=>{let e=document.createElement("template");U(F.MergeFragments,({fragments:t="<div></div>",selector:r="",mergeMode:s=Ke,useViewTransition:i=`${ue}`})=>{let o=B(i);e.innerHTML=t.trim();let u=[...e.content.children];for(let l of u){if(!(l instanceof Element))throw q("NoFragmentsFound",n);let v=r||`#${l.getAttribute("id")}`,E=[...document.querySelectorAll(v)||[]];if(!E.length)throw q("NoTargetsFound",n,{selectorOrID:v});o&&z?ae.startViewTransition(()=>yt(n,s,l,E)):yt(n,s,l,E)}})}};function yt(n,e,t,r){for(let s of r){let i=t.cloneNode(!0);switch(e){case _.Morph:{oe(i,o=>{!o.id?.length&&Object.keys(o.dataset).length&&(o.id=fe(o));let u=n.removals.get(o.id);if(u){let l=new Map;for(let[v,E]of u){let L=de(v,v);l.set(L,E),u.delete(v)}n.removals.set(o.id,l)}}),ht.morph(s,i);break}case _.Inner:s.innerHTML=i.outerHTML;break;case _.Outer:s.replaceWith(i);break;case _.Prepend:s.prepend(i);break;case _.Append:s.append(i);break;case _.Before:s.before(i);break;case _.After:s.after(i);break;case _.UpsertAttributes:for(let o of i.getAttributeNames()){let u=i.getAttribute(o);s.setAttribute(o,u)}break;default:throw q("InvalidMergeMode",n,{mergeMode:e})}}}var bt={type:2,name:F.MergeSignals,onGlobalInit:async n=>{U(F.MergeSignals,({signals:e="{}",onlyIfMissing:t=`${Ue}`})=>{let{signals:r}=n,s=B(t);r.merge(pe(e),s)})}};var St={type:2,name:F.RemoveFragments,onGlobalInit:async n=>{U(F.RemoveFragments,({selector:e,useViewTransition:t=`${ue}`})=>{if(!e.length)throw q("NoSelectorProvided",n);let r=B(t),s=document.querySelectorAll(e),i=()=>{for(let o of s)o.remove()};r&&z?ae.startViewTransition(()=>i()):i()})}};var At={type:2,name:F.RemoveSignals,onGlobalInit:async n=>{U(F.RemoveSignals,({paths:e=""})=>{let t=e.split(`
`).map(r=>r.trim());if(!t?.length)throw q("NoPathsProvided",n);n.signals.remove(...t)})}};var Et={type:3,name:"clipboard",fn:(n,e)=>{if(!navigator.clipboard)throw P("ClipboardNotAvailable",n);navigator.clipboard.writeText(e)}};var Tt={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:n=>{let{el:e,genRX:t,effect:r}=n;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw P("CustomValidityInvalidElement",n);let{deps:s,rxFn:i}=t();return r(s,()=>{let o=i();if(typeof o!="string")throw P("CustomValidityInvalidExpression",n,{result:o});e.setCustomValidity(o)})}};var Rt="once",wt="half",xt="full",Mt={type:1,name:"intersects",keyReq:2,mods:new Set([Rt,wt,xt]),onLoad:({el:n,rawKey:e,mods:t,genRX:r})=>{let s={threshold:0};t.has(xt)?s.threshold=1:t.has(wt)&&(s.threshold=.5);let{rxFn:i}=r(),o=new IntersectionObserver(u=>{for(let l of u)l.isIntersecting&&(i(),t.has(Rt)&&(o.disconnect(),delete n.dataset[e]))},s);return o.observe(n),()=>o.disconnect()}};var Dt="session",Pt={type:1,name:"persist",mods:new Set([Dt]),onLoad:({key:n,mods:e,signals:t,value:r})=>{n=C(n,e),n===""&&(n=H);let s=e.has(Dt)?sessionStorage:localStorage,i=r.split(/\s+/).filter(l=>l!=="");i=i.map(l=>J(l));let o=()=>{let l=s.getItem(n)||"{}",v=JSON.parse(l);t.merge(v)},u=()=>{let l;i.length?l=t.subset(...i):l=t.values(),s.setItem(n,JSON.stringify(l))};return o(),document.addEventListener(W,u),()=>{document.removeEventListener(W,u)}}};var Lt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:n,genRX:e})=>{let{deps:t,rxFn:r}=e();return n(t,()=>{let s=r(),i=window.location.href,o=new URL(s,i).toString();window.history.replaceState({},"",o)})}};var Ae="smooth",ke="instant",Oe="auto",Nt="hstart",It="hcenter",Vt="hend",Ct="hnearest",kt="vstart",Ot="vcenter",Ft="vend",Ht="vnearest",Rn="focus",Ee="center",qt="start",Wt="end",$t="nearest",Gt={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ae,ke,Oe,Nt,It,Vt,Ct,kt,Ot,Ft,Ht,Rn]),onLoad:n=>{let{el:e,mods:t,rawKey:r}=n;e.tabIndex||e.setAttribute("tabindex","0");let s={behavior:Ae,block:Ee,inline:Ee};if(t.has(Ae)&&(s.behavior=Ae),t.has(ke)&&(s.behavior=ke),t.has(Oe)&&(s.behavior=Oe),t.has(Nt)&&(s.inline=qt),t.has(It)&&(s.inline=Ee),t.has(Vt)&&(s.inline=Wt),t.has(Ct)&&(s.inline=$t),t.has(kt)&&(s.block=qt),t.has(Ot)&&(s.block=Ee),t.has(Ft)&&(s.block=Wt),t.has(Ht)&&(s.block=$t),!(e instanceof HTMLElement||e instanceof SVGElement))throw P("ScrollIntoViewInvalidElement",n);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(s),t.has("focus")&&e.focus(),delete e.dataset[r]}};var _t="none",Ut="display",jt={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:n},genRX:e,effect:t})=>{let{deps:r,rxFn:s}=e();return t(r,async()=>{s()?n.display===_t&&n.removeProperty(Ut):n.setProperty(Ut,_t)})}};var Kt="view-transition",Bt={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let n=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===Kt&&(n=!0);if(!n){let e=document.createElement("meta");e.name=Kt,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:n,el:e,genRX:t})=>{if(!z){console.error("Browser does not support view transitions");return}let{deps:r,rxFn:s}=t();return n(r,()=>{let i=s();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var Jt={type:1,name:"attr",valReq:1,onLoad:({el:n,key:e,effect:t,genRX:r})=>{let{deps:s,rxFn:i}=r();return e===""?t(s,async()=>{let o=i();for(let[u,l]of Object.entries(o))l===!1?n.removeAttribute(u):n.setAttribute(u,l)}):(e=K(e),t(s,async()=>{let o=!1;try{o=i()}catch{}let u;typeof o=="string"?u=o:u=JSON.stringify(o),!u||u==="false"||u==="null"||u==="undefined"?n.removeAttribute(e):n.setAttribute(e,u)}))}};var wn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,zt=["change","input","keydown"],Xt={type:1,name:"bind",keyReq:3,valReq:3,onLoad:n=>{let{el:e,key:t,mods:r,signals:s,value:i,effect:o}=n,u=e,l=t?C(t,r):J(i),v=e.tagName.toLowerCase(),E=v.includes("input"),L=v.includes("select"),b=e.getAttribute("type"),S=e.hasAttribute("value"),g="",y=E&&b==="checkbox";y&&(g=S?"":!1);let A=E&&b==="number";A&&(g=0);let T=E&&b==="radio";T&&(e.getAttribute("name")?.length||e.setAttribute("name",l));let c=E&&b==="file",{signal:d,inserted:p}=s.upsertIfMissing(l,g),a=-1;Array.isArray(d.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",l),a=[...document.querySelectorAll(`[name="${l}"]`)].findIndex(D=>D===n.el));let h=a>=0,m=()=>[...s.value(l)],f=()=>{let D=s.value(l);h&&!L&&(D=D[a]||g);let I=`${D}`;if(y||T)typeof D=="boolean"?u.checked=D:u.checked=I===u.value;else if(L){let N=e;if(N.multiple){if(!h)throw P("BindSelectMultiple",n);for(let k of N.options){if(k?.disabled)return;let V=A?Number(k.value):k.value;k.selected=D.includes(V)}}else N.value=I}else c||("value"in e?e.value=I:e.setAttribute("value",I))},R=async()=>{let D=s.value(l);if(h){let V=D;for(;a>=V.length;)V.push(g);D=V[a]||g}let I=(V,$)=>{let G=$;h&&!L&&(G=m(),G[a]=$),s.setValue(V,G)};if(c){let V=[...u?.files||[]],$=[],G=[],He=[];await Promise.all(V.map(qe=>new Promise(cn=>{let X=new FileReader;X.onload=()=>{if(typeof X.result!="string")throw P("InvalidFileResultType",n,{resultType:typeof X.result});let Te=X.result.match(wn);if(!Te?.groups)throw P("InvalidDataUri",n,{result:X.result});$.push(Te.groups.contents),G.push(Te.groups.mime),He.push(qe.name)},X.onloadend=()=>cn(void 0),X.readAsDataURL(qe)}))),I(l,$),I(`${l}Mimes`,G),I(`${l}Names`,He);return}let N=u.value||"",k;if(y){let V=u.checked||u.getAttribute("checked")==="true";S?k=V?N:"":k=V}else if(L){let $=[...e.selectedOptions];h?k=$.filter(G=>G.selected).map(G=>G.value):k=$[0]?.value||g}else typeof D=="boolean"?k=!!N:typeof D=="number"?k=Number(N):k=N||"";I(l,k)};p&&R();for(let D of zt)e.addEventListener(D,R);let x=D=>{D.persisted&&R()};window.addEventListener("pageshow",x);let w=o([d],()=>{f()});return()=>{w();for(let D of zt)e.removeEventListener(D,R);window.removeEventListener("pageshow",x)}}};var Yt={type:1,name:"class",valReq:1,onLoad:({el:n,key:e,mods:t,effect:r,genRX:s})=>{let i=n.classList,{deps:o,rxFn:u}=s();return r(o,()=>{if(e===""){let l=u();for(let[v,E]of Object.entries(l)){let L=v.split(/\s+/);E?i.add(...L):i.remove(...L)}}else{let l=K(e);l=C(l,t),u()?i.add(l):i.remove(l)}})}};var Zt={type:1,name:"json",keyReq:2,valReq:2,onLoad:n=>{let{el:e,signals:t}=n;e instanceof HTMLElement||P("JsonInvalidElement",n);let r=()=>{e.textContent=t.JSON()};return document.addEventListener(W,r),r(),()=>{document.removeEventListener(W,r)}}};function le(n){if(!n||n.size<=0)return 0;for(let e of n){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function ie(n,e,t=!1){return n?n.has(e.toLowerCase()):t}function Qt(n,e){return(...t)=>{setTimeout(()=>{n(...t)},e)}}function en(n,e,t=!1,r=!0){let s=-1,i=()=>s&&clearTimeout(s);return(...o)=>{i(),t&&!s&&n(...o),s=setTimeout(()=>{r&&n(...o),i()},e)}}function tn(n,e,t=!0,r=!1){let s=!1;return(...i)=>{s||(t&&n(...i),s=!0,setTimeout(()=>{s=!1,r&&n(...i)},e))}}var xn="evt",Fe="signalsChange",Mn=Fe.length,nn={type:1,name:"on",keyReq:1,valReq:1,argNames:[xn],onLoad:({el:n,key:e,mods:t,genRX:r})=>{let{rxFn:s}=r(),i=n;t.has("window")&&(i=window);let o=S=>{S&&((t.has("prevent")||e==="submit")&&S.preventDefault(),t.has("stop")&&S.stopPropagation()),s(S)},u=t.get("delay");if(u){let S=le(u);o=Qt(o,S)}let l=t.get("debounce");if(l){let S=le(l),g=ie(l,"leading",!1),y=!ie(l,"notrail",!1);o=en(o,S,g,y)}let v=t.get("throttle");if(v){let S=le(v),g=!ie(v,"noleading",!1),y=ie(v,"trail",!1);o=tn(o,S,g,y)}if(t.has("viewtransition")&&z){let S=o;o=(...g)=>document.startViewTransition(()=>S(...g))}let E={capture:!0,passive:!1,once:!1};if(t.has("capture")||(E.capture=!1),t.has("passive")&&(E.passive=!0),t.has("once")&&(E.once=!0),e==="load")return setTimeout(o,0),()=>{};if(e==="interval"){let S=1e3,g=t.get("duration");g&&(S=le(g),ie(g,"leading",!1)&&o());let y=setInterval(o,S);return()=>{clearInterval(y)}}if(e==="raf"){let S,g=()=>{o(),S=requestAnimationFrame(g)};return S=requestAnimationFrame(g),()=>{S&&cancelAnimationFrame(S)}}if(e.startsWith(Fe)){let S=e!==Fe,g=C(Y(e.slice(Mn)),t),y=A=>{if(S){let{added:T,removed:c,updated:d}=A.detail;if(![...T,...c,...d].some(p=>p.startsWith(g)))return}o(A)};return document.addEventListener(W,y),()=>{document.removeEventListener(W,y)}}if(t.has("outside")){i=document;let S=o;o=y=>{let A=y?.target;n.contains(A)||S(y)}}let b=K(e);return b=C(b,t),i.addEventListener(b,o,E),()=>{i.removeEventListener(b,o)}}};var rn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?C(e,t):J(s);r.setValue(i,n)}};var sn={type:1,name:"text",keyReq:2,valReq:1,onLoad:n=>{let{el:e,effect:t,genRX:r}=n,{deps:s,rxFn:i}=r();return e instanceof HTMLElement||P("TextInvalidElement",n),t(s,()=>{let o=i(n);e.textContent=`${o}`})}};var{round:Dn,max:Pn,min:Ln}=Math,on={type:3,name:"fit",fn:(n,e,t,r,s,i,o=!1,u=!1)=>{let l=(e-t)/(r-t)*(i-s)+s;return u&&(l=Dn(l)),o&&(l=Pn(s,Ln(i,l))),l}};var an={type:3,name:"setAll",fn:({signals:n},e,t)=>{n.walk((r,s)=>{r.startsWith(e)&&(s.value=t)})}};var ln={type:3,name:"toggleAll",fn:({signals:n},e)=>{n.walk((t,r)=>{t.startsWith(e)&&(r.value=!r.value)})}};he(Jt,Xt,Yt,Zt,nn,rn,jt,sn,mt,ut,ft,dt,pt,ct,vt,bt,St,At,gt,Et,Tt,Mt,Pt,Lt,Gt,Bt,on,an,ln);Ce();export{Ce as apply,he as load,tt as setAlias};
//# sourceMappingURL=datastar.js.map
