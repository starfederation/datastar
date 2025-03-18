// Datastar v1.0.0-beta.9
var We=/🖕JS_DS🚀/.source,ue=We.slice(0,5),we=We.slice(4),H="datastar",$e="Datastar-Request",Ge=1e3,Ue="type module",pe=!1,_e=!1,je=!0,G={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ke=G.Morph,O={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var M=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(M||{});var B=`${H}-signals`;var J=n=>n.trim()==="true",j=n=>n.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,t)=>(t?"-":"")+e.toLowerCase()),K=n=>j(n).replace(/-./g,e=>e[1].toUpperCase()),Re=n=>j(n).replace(/-/g,"_"),an=n=>K(n).replace(/^./,e=>e[0].toUpperCase()),fe=n=>new Function(`return Object.assign({}, ${n})`)(),z=n=>n.startsWith("$")?n.slice(1):n,ln={kebab:j,snake:Re,pascal:an};function L(n,e){for(let t of e.get("case")||[]){let r=ln[t];r&&(n=r(n))}return n}var cn="computed",Be={type:1,name:cn,keyReq:1,valReq:1,onLoad:({key:n,mods:e,signals:t,genRX:r})=>{n=L(n,e);let{deps:s,rxFn:i}=r();t.setComputed(n,s,i)}};var Je={type:1,name:"signals",onLoad:n=>{let{key:e,mods:t,signals:r,value:s,genRX:i}=n,o=t.has("ifmissing"),{rxFn:u}=i();if(e!==""){let p=L(e,t),v=s===""?s:u();o?r.upsertIfMissing(p,v):r.setValue(p,v)}else{let p=fe(n.value);n.value=JSON.stringify(p);let v=u();r.merge(v,o)}}};var ze={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var Q=class{#e=0;#t;constructor(e=H){this.#t=e}with(e){if(typeof e=="string")for(let t of e.split(""))this.with(t.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function de(n){if(n.id)return n.id;let e=new Q,t=n;for(;t;){if(e.with(t.tagName||""),t.id){e.with(t.id);break}let r=t?.parentNode;r&&e.with([...r.children].indexOf(t)),t=r}return e.string}function me(n,e){return new Q().with(n).with(e).value}function oe(n,e){if(!n||!(n instanceof HTMLElement||n instanceof SVGElement))return null;let t=n.dataset;if("starIgnore"in t)return null;"starIgnore__self"in t||e(n);let r=n.firstElementChild;for(;r;)oe(r,e),r=r.nextElementSibling}var un="https://data-star.dev/errors";function xe(n,e,t={}){let r=new Error;r.name=`${H} ${n} error`;let s=Re(e),i=new URLSearchParams({metadata:JSON.stringify(t)}).toString(),o=JSON.stringify(t,null,2);return r.message=`${e}
More info: ${un}/${n}/${s}?${i}
Context: ${o}`,r}function Me(n,e,t={}){return xe("internal",e,Object.assign({from:n},t))}function q(n,e,t={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]}};return xe("init",n,Object.assign(r,t))}function N(n,e,t={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return xe("runtime",n,Object.assign(r,t))}var Xe="namespacedSignals",ee=n=>{document.dispatchEvent(new CustomEvent(B,{detail:Object.assign({added:[],removed:[],updated:[]},n)}))};function Ye(n,e=!1){let t={};for(let r in n)if(Object.hasOwn(n,r)){if(e&&r.startsWith("_"))continue;let s=n[r];s instanceof te?t[r]=s.value:t[r]=Ye(s)}return t}function Ze(n,e,t,r=!1){let s={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw Me(Xe,"InvalidSignalKey",{key:i});let o=t?`${t}.${i}`:i,u=e[i];if(u instanceof Object&&!Array.isArray(u)){n[i]||(n[i]={});let p=Ze(n[i],u,o,r);s.added.push(...p.added.map(v=>`${o}.${v}`)),s.removed.push(...p.removed.map(v=>`${o}.${v}`)),s.updated.push(...p.updated.map(v=>`${o}.${v}`))}else{if(Object.hasOwn(n,i)){if(r)continue;let w=n[i];if(w instanceof ne){let x=w.value;w.value=u,x!==u&&s.updated.push(o);continue}}let v=new ne(u,()=>ee({updated:[o]}));n[i]=v,s.added.push(o)}}return s}function Qe(n,e){for(let t in n)if(Object.hasOwn(n,t)){let r=n[t];r instanceof te?e(t,r):Qe(r,(s,i)=>{e(`${t}.${s}`,i)})}}function pn(n,...e){let t={};for(let r of e){let s=r.split("."),i=n,o=t;for(let p=0;p<s.length-1;p++){let v=s[p];if(!i[v])return{};o[v]||(o[v]={}),i=i[v],o=o[v]}let u=s[s.length-1];o[u]=i[u]}return t}var ge=class{#e={};exists(e){return!!this.signal(e)}signal(e){let t=e.split("."),r=this.#e;for(let o=0;o<t.length-1;o++){let u=t[o];if(!r[u])return null;r=r[u]}let s=t[t.length-1],i=r[s];if(!i)throw Me(Xe,"SignalNotFound",{path:e});return i}setSignal(e,t){let r=e.split("."),s=this.#e;for(let o=0;o<r.length-1;o++){let u=r[o];s[u]||(s[u]={}),s=s[u]}let i=r[r.length-1];s[i]=t}setComputed(e,t,r){let s=fn(t,r);this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,t){let{signal:r}=this.upsertIfMissing(e,t),s=r.value;r.value=t,s!==t&&ee({updated:[e]})}upsertIfMissing(e,t){let r=e.split("."),s=this.#e;for(let p=0;p<r.length-1;p++){let v=r[p];s[v]||(s[v]={}),s=s[v]}let i=r[r.length-1],o=s[i];if(o instanceof ne)return{signal:o,inserted:!1};let u=new ne(t);return u.onChange=()=>{ee({updated:[e]})},s[i]=u,ee({added:[e]}),{signal:u,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let t=Array();for(let r of e){let s=r.split("."),i=this.#e;for(let u=0;u<s.length-1;u++){let p=s[u];if(!i[p])return;i=i[p]}let o=s[s.length-1];delete i[o],t.push(r)}ee({removed:t})}merge(e,t=!1){let r=Ze(this.#e,e,"",t);(r.added.length||r.removed.length||r.updated.length)&&ee(r)}subset(...e){return pn(this.values(),...e)}walk(e){Qe(this.#e,e)}paths(){let e=new Array;return this.walk(t=>e.push(t)),e}values(e=!1){return Ye(this.#e,e)}JSON(e=!0,t=!1){let r=this.values(t);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var te=class{},ne=class extends te{constructor(t,r){super();this.val=t;this.onChange=r;this.subs=new Set;this.ver=1}set value(t){this.val!==t&&(this.val=t,this.ver++,this.markDirty(),this.onChange?.(t))}markDirty(){for(let t of this.subs)t.markDirty()}get value(){return this.val}version(){return this.ver}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};var Pe=class extends te{constructor(t,r){super();this.deps=t;this.fn=r;this.subs=new Set;this.isDirty=!0;this.ver=1;this.versionSum=0;for(let s of t)s.addSubscribers(this)}get value(){if(!this.isDirty)return this.val;this.isDirty=!1;let t=0;for(let i of this.deps)t+=i.version();if(t===this.versionSum)return this.val;this.versionSum=t;let r=this.deps.map(i=>i.value),s=this.fn(...r);return this.val===s?this.val:(this.val=s,this.ver++,this.val)}version(){return this.ver}markDirty(){this.isDirty=!0;for(let t of this.subs)t.markDirty()}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};function fn(n,e){return new Pe(n,e)}var De=class{constructor(e,t){this.deps=e;this.fn=t;this.depsVersionSum=-1;for(let r of e)r.addSubscribers(this)}markDirty(){let e=0;for(let r of this.deps)e+=r.version();if(e===this.depsVersionSum)return;this.depsVersionSum=e;let t=this.deps.map(r=>r.value);this.fn(...t)}};function Ne(n,e){let t=new De(n,e);return t.markDirty(),()=>{for(let r of n)r.removeSubscribers(t)}}var et=new ge,he={},Ce=[],Z=new Map,Ie=null,Ve="";function Le(n){Ve=n}function ye(...n){for(let e of n){let t={plugin:e,signals:et,effect:(s,i)=>Ne(s,i),actions:he,removals:Z,applyToElement:ve},r;switch(e.type){case 3:{he[e.name]=e;break}case 1:{let s=e;Ce.push(s),r=s.onGlobalInit;break}case 2:{r=e.onGlobalInit;break}default:throw q("InvalidPluginType",t)}r&&r(t)}Ce.sort((e,t)=>{let r=t.name.length-e.name.length;return r!==0?r:e.name.localeCompare(t.name)})}function ke(){queueMicrotask(()=>{ve(document.documentElement),dn()})}function ve(n){oe(n,e=>{let t=new Array,r=Z.get(e.id)||new Map,s=new Map([...r]),i=new Map;for(let o of Object.keys(e.dataset)){if(!o.startsWith(Ve))break;let u=e.dataset[o]||"",p=me(o,u);i.set(o,p),r.has(p)?s.delete(p):t.push(o)}for(let[o,u]of s)u();for(let o of t){let u=i.get(o);mn(e,o,u)}})}function dn(){Ie||(Ie=new MutationObserver(n=>{let e=new Set,t=new Set;for(let{target:r,type:s,addedNodes:i,removedNodes:o}of n)switch(s){case"childList":{for(let u of o)e.add(u);for(let u of i)t.add(u)}break;case"attributes":{t.add(r);break}}for(let r of e){let s=Z.get(r.id);if(s){for(let[i,o]of s)o(),s.delete(i);s.size===0&&Z.delete(r.id)}}for(let r of t)ve(r)}),Ie.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function mn(n,e,t){let r=K(e.slice(Ve.length)),s=Ce.find(y=>new RegExp(`^${y.name}([A-Z]|_|$)`).test(r));if(!s)return;n.id.length||(n.id=de(n));let[i,...o]=r.slice(s.name.length).split(/\_\_+/),u=i.length>0;u&&(i=K(i));let p=n.dataset[e]||"",v=p.length>0,w={signals:et,applyToElement:ve,effect:(y,E)=>Ne(y,E),actions:he,removals:Z,genRX:()=>gn(w,...s.argNames||[]),plugin:s,el:n,rawKey:r,key:i,value:p,mods:new Map},x=s.keyReq||0;if(u){if(x===2)throw N(`${s.name}KeyNotAllowed`,w)}else if(x===1)throw N(`${s.name}KeyRequired`,w);let b=s.valReq||0;if(v){if(b===2)throw N(`${s.name}ValueNotAllowed`,w)}else if(b===1)throw N(`${s.name}ValueRequired`,w);if(x===3||b===3){if(u&&v)throw N(`${s.name}KeyAndValueProvided`,w);if(!u&&!v)throw N(`${s.name}KeyOrValueRequired`,w)}for(let y of o){let[E,...A]=y.split(".");w.mods.set(K(E),new Set(A.map(l=>l.toLowerCase())))}let S=s.onLoad(w)??(()=>{}),m=Z.get(n.id);m||(m=new Map,Z.set(n.id,m)),m.set(t,S)}function gn(n,...e){let t=new Array,r="",s=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=n.value.trim().match(s);if(i){let m=i.length-1,y=i[m].trim();y.startsWith("return")||(i[m]=`return (${y});`),r=i.join(`;
`)}let o=new Map,u=new RegExp(`(?:${ue})(.*?)(?:${we})`,"gm");for(let m of r.matchAll(u)){let y=m[1],E=new Q("dsEscaped").with(y).string;o.set(E,y),r=r.replace(ue+y+we,E)}let p=/@(\w*)\(/gm,v=r.matchAll(p),w=new Set;for(let m of v)w.add(m[1]);let x=new RegExp(`@(${Object.keys(he).join("|")})\\(`,"gm");r=r.replaceAll(x,"ctx.actions.$1.fn(ctx,");let b=n.signals.paths();if(b.length){let m=new RegExp(`\\$(${b.join("|")})(\\W|$)`,"gm");r=r.replaceAll(m,"ctx.signals.signal('$1').value$2");let y=/ctx.signals.signal\('(.+?)'\).value/gm;for(let E of r.matchAll(y)){let A=E[1],l=n.signals.signal(A);l&&t.push(l)}if(r.includes("ctx.signals.JSON()"))for(let E of n.signals.paths()){let A=n.signals.signal(E);A&&t.push(A)}}for(let[m,y]of o)r=r.replace(m,y);let S=`return (() => {
${r}
})()`;n.fnContent=S;try{let m=new Function("ctx",...e,S);return{deps:t,rxFn:(...y)=>{try{return m(n,...y)}catch(E){throw N("ExecuteExpression",n,{error:E.message})}}}}catch(m){throw N("GenerateExpression",n,{error:m.message})}}ye(ze,Je,Be);async function hn(n,e){let t=n.getReader(),r;for(;!(r=await t.read()).done;)e(r.value)}function yn(n){let e,t,r,s=!1;return function(o){e===void 0?(e=o,t=0,r=-1):e=bn(e,o);let u=e.length,p=0;for(;t<u;){s&&(e[t]===10&&(p=++t),s=!1);let v=-1;for(;t<u&&v===-1;++t)switch(e[t]){case 58:r===-1&&(r=t-p);break;case 13:s=!0;case 10:v=t;break}if(v===-1)break;n(e.subarray(p,v),r),p=t,r=-1}p===u?e=void 0:p!==0&&(e=e.subarray(p),t-=p)}}function vn(n,e,t){let r=tt(),s=new TextDecoder;return function(o,u){if(o.length===0)t?.(r),r=tt();else if(u>0){let p=s.decode(o.subarray(0,u)),v=u+(o[u+1]===32?2:1),w=s.decode(o.subarray(v));switch(p){case"data":r.data=r.data?`${r.data}
${w}`:w;break;case"event":r.event=w;break;case"id":n(r.id=w);break;case"retry":{let x=Number.parseInt(w,10);Number.isNaN(x)||e(r.retry=x);break}}}}}function bn(n,e){let t=new Uint8Array(n.length+e.length);return t.set(n),t.set(e,n.length),t}function tt(){return{data:"",event:"",id:"",retry:void 0}}var Sn="text/event-stream",nt="last-event-id";function rt(n,{signal:e,headers:t,onopen:r,onmessage:s,onclose:i,onerror:o,openWhenHidden:u,fetch:p,retryInterval:v=1e3,retryScaler:w=2,retryMaxWaitMs:x=3e4,retryMaxCount:b=10,...S}){return new Promise((m,y)=>{let E=0,A={...t};A.accept||(A.accept=Sn);let l;function d(){l.abort(),document.hidden||f()}u||document.addEventListener("visibilitychange",d);let c=0;function a(){document.removeEventListener("visibilitychange",d),window.clearTimeout(c),l.abort()}e?.addEventListener("abort",()=>{a(),m()});let h=p??window.fetch,g=r??function(){};async function f(){l=new AbortController;try{let T=await h(n,{...S,headers:A,signal:l.signal});await g(T),await hn(T.body,yn(vn(R=>{R?A[nt]=R:delete A[nt]},R=>{v=R},s))),i?.(),a(),m()}catch(T){if(!l.signal.aborted)try{let R=o?.(T)??v;window.clearTimeout(c),c=window.setTimeout(f,R),v*=w,v=Math.min(v,x),E++,E>b?(a(),y("Max retries reached.")):console.error(`Datastar failed to reach ${n.toString()} retrying in ${R}ms.`)}catch(R){a(),y(R)}}}f()})}var re=`${H}-sse`,be="started",Se="finished",st="error",it="retrying";function U(n,e){document.addEventListener(re,t=>{if(t.detail.type!==n)return;let{argsRaw:r}=t.detail;e(r)})}function ae(n,e,t){n.dispatchEvent(new CustomEvent(re,{detail:{type:e,argsRaw:t},bubbles:!0}))}var ot=n=>`${n}`.includes("text/event-stream"),_=async(n,e,t,r)=>{let{el:{id:s},el:i,signals:o}=n,{headers:u,contentType:p,includeLocal:v,selector:w,openWhenHidden:x,retryInterval:b,retryScaler:S,retryMaxWaitMs:m,retryMaxCount:y,abort:E}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:Ge,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),A=e.toLowerCase(),l=()=>{};try{if(ae(i,be,{elId:s}),!t?.length)throw N("SseNoUrlProvided",n,{action:A});let d={};d[$e]=!0,p==="json"&&(d["Content-Type"]="application/json");let c=Object.assign({},d,u),a={method:e,headers:c,openWhenHidden:x,retryInterval:b,retryScaler:S,retryMaxWaitMs:m,retryMaxCount:y,signal:E,onopen:async f=>{if(f.status>=400){let T=f.status.toString();ae(i,st,{status:T})}},onmessage:f=>{if(!f.event.startsWith(H))return;let T=f.event,R={},D=f.data.split(`
`);for(let C of D){let I=C.indexOf(" "),k=C.slice(0,I),V=R[k];V||(V=[],R[k]=V);let W=C.slice(I+1);V.push(W)}let P={};for(let[C,I]of Object.entries(R))P[C]=I.join(`
`);ae(i,T,P)},onerror:f=>{if(ot(f))throw N("InvalidContentType",n,{url:t});f&&(console.error(f.message),ae(i,it,{message:f.message}))}},h=new URL(t,window.location.origin),g=new URLSearchParams(h.search);if(p==="json"){let f=o.JSON(!1,!v);e==="GET"?g.set(H,f):a.body=f}else if(p==="form"){let f=w?document.querySelector(w):i.closest("form");if(f===null)throw w?N("SseFormNotFound",n,{action:A,selector:w}):N("SseClosestFormNotFound",n,{action:A});if(i!==f){let R=D=>D.preventDefault();f.addEventListener("submit",R),l=()=>f.removeEventListener("submit",R)}if(!f.checkValidity()){f.reportValidity(),l();return}let T=new FormData(f);if(e==="GET"){let R=new URLSearchParams(T);for(let[D,P]of R)g.set(D,P)}else a.body=T}else throw N("SseInvalidContentType",n,{action:A,contentType:p});h.search=g.toString();try{await rt(h.toString(),a)}catch(f){if(!ot(f))throw N("SseFetchFailed",n,{method:e,url:t,error:f})}}finally{ae(i,Se,{elId:s}),l()}};var at={type:3,name:"delete",fn:async(n,e,t)=>_(n,"DELETE",e,{...t})};var lt={type:3,name:"get",fn:async(n,e,t)=>_(n,"GET",e,{...t})};var ct={type:3,name:"patch",fn:async(n,e,t)=>_(n,"PATCH",e,{...t})};var ut={type:3,name:"post",fn:async(n,e,t)=>_(n,"POST",e,{...t})};var pt={type:3,name:"put",fn:async(n,e,t)=>_(n,"PUT",e,{...t})};var ft={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?L(e,t):z(s),{signal:o}=r.upsertIfMissing(i,!1),u=p=>{let{type:v,argsRaw:{elId:w}}=p.detail;if(w===n.id)switch(v){case be:o.value=!0;break;case Se:o.value=!1;break}};return document.addEventListener(re,u),()=>{o.value=!1,document.removeEventListener(re,u)}}};var dt={type:2,name:O.ExecuteScript,onGlobalInit:async n=>{U(O.ExecuteScript,({autoRemove:e=`${je}`,attributes:t=Ue,script:r})=>{let s=J(e);if(!r?.length)throw q("NoScriptProvided",n);let i=document.createElement("script");for(let o of t.split(`
`)){let u=o.indexOf(" "),p=u?o.slice(0,u):o,v=u?o.slice(u):"";i.setAttribute(p.trim(),v.trim())}i.text=r,document.head.appendChild(i),s&&i.remove()})}};var le=document,X=!!le.startViewTransition;var mt=function(){"use strict";let n=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:n,afterNodeAdded:n,beforeNodeMorphed:n,afterNodeMorphed:n,beforeNodeRemoved:n,afterNodeRemoved:n,beforeAttributeUpdated:n},head:{style:"merge",shouldPreserve:b=>b.getAttribute("im-preserve")==="true",shouldReAppend:b=>b.getAttribute("im-re-append")==="true",shouldRemove:n,afterHeadMorphed:n},restoreFocus:!0};function t(b,S,m={}){b=w(b);let y=x(S),E=v(b,y,m),A=s(E,()=>u(E,b,y,l=>l.morphStyle==="innerHTML"?(i(l,b,y),Array.from(b.childNodes)):r(l,b,y)));return E.pantry.remove(),A}function r(b,S,m){let y=x(S);return i(b,y,m,S,S.nextSibling),Array.from(y.childNodes)}function s(b,S){if(!b.config.restoreFocus)return S();let m=document.activeElement;if(!(m instanceof HTMLInputElement||m instanceof HTMLTextAreaElement))return S();let{id:y,selectionStart:E,selectionEnd:A}=m,l=S();return y&&y!==document.activeElement?.id&&(m=b.target.querySelector(`[id="${y}"]`),m?.focus()),m&&!m.selectionEnd&&A&&m.setSelectionRange(E,A),l}let i=function(){function b(c,a,h,g=null,f=null){a instanceof HTMLTemplateElement&&h instanceof HTMLTemplateElement&&(a=a.content,h=h.content),g||=a.firstChild;for(let T of h.childNodes){if(g&&g!=f){let D=m(c,T,g,f);if(D){D!==g&&E(c,g,D),o(D,T,c),g=D.nextSibling;continue}}if(T instanceof Element&&c.persistentIds.has(T.id)){let D=A(a,T.id,g,c);o(D,T,c),g=D.nextSibling;continue}let R=S(a,T,g,c);R&&(g=R.nextSibling)}for(;g&&g!=f;){let T=g;g=g.nextSibling,y(c,T)}}function S(c,a,h,g){if(g.callbacks.beforeNodeAdded(a)===!1)return null;if(g.idMap.has(a)){let f=document.createElement(a.tagName);return c.insertBefore(f,h),o(f,a,g),g.callbacks.afterNodeAdded(f),f}else{let f=document.importNode(a,!0);return c.insertBefore(f,h),g.callbacks.afterNodeAdded(f),f}}let m=function(){function c(g,f,T,R){let D=null,P=f.nextSibling,C=0,I=T;for(;I&&I!=R;){if(h(I,f)){if(a(g,I,f))return I;D===null&&(g.idMap.has(I)||(D=I))}if(D===null&&P&&h(I,P)&&(C++,P=P.nextSibling,C>=2&&(D=void 0)),I.contains(document.activeElement))break;I=I.nextSibling}return D||null}function a(g,f,T){let R=g.idMap.get(f),D=g.idMap.get(T);if(!D||!R)return!1;for(let P of R)if(D.has(P))return!0;return!1}function h(g,f){let T=g,R=f;return T.nodeType===R.nodeType&&T.tagName===R.tagName&&(!T.id||T.id===R.id)}return c}();function y(c,a){if(c.idMap.has(a))d(c.pantry,a,null);else{if(c.callbacks.beforeNodeRemoved(a)===!1)return;a.parentNode?.removeChild(a),c.callbacks.afterNodeRemoved(a)}}function E(c,a,h){let g=a;for(;g&&g!==h;){let f=g;g=g.nextSibling,y(c,f)}return g}function A(c,a,h,g){let f=g.target.id===a&&g.target||g.target.querySelector(`[id="${a}"]`)||g.pantry.querySelector(`[id="${a}"]`);return l(f,g),d(c,f,h),f}function l(c,a){let h=c.id;for(;c=c.parentNode;){let g=a.idMap.get(c);g&&(g.delete(h),g.size||a.idMap.delete(c))}}function d(c,a,h){if(c.moveBefore)try{c.moveBefore(a,h)}catch{c.insertBefore(a,h)}else c.insertBefore(a,h)}return b}(),o=function(){function b(l,d,c){return c.ignoreActive&&l===document.activeElement?null:(c.callbacks.beforeNodeMorphed(l,d)===!1||(l instanceof HTMLHeadElement&&c.head.ignore||(l instanceof HTMLHeadElement&&c.head.style!=="morph"?p(l,d,c):(S(l,d,c),A(l,c)||i(c,l,d))),c.callbacks.afterNodeMorphed(l,d)),l)}function S(l,d,c){let a=d.nodeType;if(a===1){let h=l,g=d,f=h.attributes,T=g.attributes;for(let R of T)E(R.name,h,"update",c)||h.getAttribute(R.name)!==R.value&&h.setAttribute(R.name,R.value);for(let R=f.length-1;0<=R;R--){let D=f[R];if(D&&!g.hasAttribute(D.name)){if(E(D.name,h,"remove",c))continue;h.removeAttribute(D.name)}}A(h,c)||m(h,g,c)}(a===8||a===3)&&l.nodeValue!==d.nodeValue&&(l.nodeValue=d.nodeValue)}function m(l,d,c){if(l instanceof HTMLInputElement&&d instanceof HTMLInputElement&&d.type!=="file"){let a=d.value,h=l.value;y(l,d,"checked",c),y(l,d,"disabled",c),d.hasAttribute("value")?h!==a&&(E("value",l,"update",c)||(l.setAttribute("value",a),l.value=a)):E("value",l,"remove",c)||(l.value="",l.removeAttribute("value"))}else if(l instanceof HTMLOptionElement&&d instanceof HTMLOptionElement)y(l,d,"selected",c);else if(l instanceof HTMLTextAreaElement&&d instanceof HTMLTextAreaElement){let a=d.value,h=l.value;if(E("value",l,"update",c))return;a!==h&&(l.value=a),l.firstChild&&l.firstChild.nodeValue!==a&&(l.firstChild.nodeValue=a)}}function y(l,d,c,a){let h=d[c],g=l[c];if(h!==g){let f=E(c,l,"update",a);f||(l[c]=d[c]),h?f||l.setAttribute(c,""):E(c,l,"remove",a)||l.removeAttribute(c)}}function E(l,d,c,a){return l==="value"&&a.ignoreActiveValue&&d===document.activeElement?!0:a.callbacks.beforeAttributeUpdated(l,d,c)===!1}function A(l,d){return!!d.ignoreActiveValue&&l===document.activeElement&&l!==document.body}return b}();function u(b,S,m,y){if(b.head.block){let E=S.querySelector("head"),A=m.querySelector("head");if(E&&A){let l=p(E,A,b);return Promise.all(l).then(()=>{let d=Object.assign(b,{head:{block:!1,ignore:!0}});return y(d)})}}return y(b)}function p(b,S,m){let y=[],E=[],A=[],l=[],d=new Map;for(let a of S.children)d.set(a.outerHTML,a);for(let a of b.children){let h=d.has(a.outerHTML),g=m.head.shouldReAppend(a),f=m.head.shouldPreserve(a);h||f?g?E.push(a):(d.delete(a.outerHTML),A.push(a)):m.head.style==="append"?g&&(E.push(a),l.push(a)):m.head.shouldRemove(a)!==!1&&E.push(a)}l.push(...d.values());let c=[];for(let a of l){let h=document.createRange().createContextualFragment(a.outerHTML).firstChild;if(m.callbacks.beforeNodeAdded(h)!==!1){if("href"in h&&h.href||"src"in h&&h.src){let g,f=new Promise(function(T){g=T});h.addEventListener("load",function(){g()}),c.push(f)}b.appendChild(h),m.callbacks.afterNodeAdded(h),y.push(h)}}for(let a of E)m.callbacks.beforeNodeRemoved(a)!==!1&&(b.removeChild(a),m.callbacks.afterNodeRemoved(a));return m.head.afterHeadMorphed(b,{added:y,kept:A,removed:E}),c}let v=function(){function b(d,c,a){let{persistentIds:h,idMap:g}=A(d,c),f=S(a),T=f.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(T))throw`Do not understand how to morph style ${T}`;return{target:d,newContent:c,config:f,morphStyle:T,ignoreActive:f.ignoreActive,ignoreActiveValue:f.ignoreActiveValue,restoreFocus:f.restoreFocus,idMap:g,persistentIds:h,pantry:m(),callbacks:f.callbacks,head:f.head}}function S(d){let c=Object.assign({},e);return Object.assign(c,d),c.callbacks=Object.assign({},e.callbacks,d.callbacks),c.head=Object.assign({},e.head,d.head),c}function m(){let d=document.createElement("div");return d.hidden=!0,document.body.insertAdjacentElement("afterend",d),d}function y(d){let c=Array.from(d.querySelectorAll("[id]"));return d.id&&c.push(d),c}function E(d,c,a,h){for(let g of h)if(c.has(g.id)){let f=g;for(;f;){let T=d.get(f);if(T==null&&(T=new Set,d.set(f,T)),T.add(g.id),f===a)break;f=f.parentElement}}}function A(d,c){let a=y(d),h=y(c),g=l(a,h),f=new Map;E(f,g,d,a);let T=c.__idiomorphRoot||c;return E(f,g,T,h),{persistentIds:g,idMap:f}}function l(d,c){let a=new Set,h=new Map;for(let{id:f,tagName:T}of d)h.has(f)?a.add(f):h.set(f,T);let g=new Set;for(let{id:f,tagName:T}of c)g.has(f)?a.add(f):h.get(f)===T&&g.add(f);for(let f of a)g.delete(f);return g}return b}(),{normalizeElement:w,normalizeParent:x}=function(){let b=new WeakSet;function S(A){return A instanceof Document?A.documentElement:A}function m(A){if(A==null)return document.createElement("div");if(typeof A=="string")return m(E(A));if(b.has(A))return A;if(A instanceof Node){if(A.parentNode)return new y(A);{let l=document.createElement("div");return l.append(A),l}}else{let l=document.createElement("div");for(let d of[...A])l.append(d);return l}}class y{constructor(l){this.originalNode=l,this.realParentNode=l.parentNode,this.previousSibling=l.previousSibling,this.nextSibling=l.nextSibling}get childNodes(){let l=[],d=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;d&&d!=this.nextSibling;)l.push(d),d=d.nextSibling;return l}querySelectorAll(l){return this.childNodes.reduce((d,c)=>{if(c instanceof Element){c.matches(l)&&d.push(c);let a=c.querySelectorAll(l);for(let h=0;h<a.length;h++)d.push(a[h])}return d},[])}insertBefore(l,d){return this.realParentNode.insertBefore(l,d)}moveBefore(l,d){return this.realParentNode.moveBefore(l,d)}get __idiomorphRoot(){return this.originalNode}}function E(A){let l=new DOMParser,d=A.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(d.match(/<\/html>/)||d.match(/<\/head>/)||d.match(/<\/body>/)){let c=l.parseFromString(A,"text/html");if(d.match(/<\/html>/))return b.add(c),c;{let a=c.firstChild;return a&&b.add(a),a}}else{let a=l.parseFromString("<body><template>"+A+"</template></body>","text/html").body.querySelector("template").content;return b.add(a),a}}return{normalizeElement:S,normalizeParent:m}}();return{morph:t,defaults:e}}();var ht={type:2,name:O.MergeFragments,onGlobalInit:async n=>{let e=document.createElement("template");U(O.MergeFragments,({fragments:t="<div></div>",selector:r="",mergeMode:s=Ke,useViewTransition:i=`${pe}`})=>{let o=J(i);e.innerHTML=t.trim();let u=[...e.content.children];for(let p of u){if(!(p instanceof Element))throw q("NoFragmentsFound",n);let v=r||`#${p.getAttribute("id")}`,w=[...document.querySelectorAll(v)||[]];if(!w.length)throw q("NoTargetsFound",n,{selectorOrID:v});o&&X?le.startViewTransition(()=>gt(n,s,p,w)):gt(n,s,p,w)}})}};function gt(n,e,t,r){for(let s of r)switch(e){case G.Morph:{let i=t.cloneNode(!0);oe(i,o=>{!o.id?.length&&Object.keys(o.dataset).length&&(o.id=de(o));let u=n.removals.get(o.id);if(u){let p=new Map;for(let[v,w]of u){let x=me(v,v);p.set(x,w),u.delete(v)}n.removals.set(o.id,p)}}),mt.morph(s,i);break}case G.Inner:s.innerHTML=t.outerHTML;break;case G.Outer:s.replaceWith(t);break;case G.Prepend:s.prepend(t);break;case G.Append:s.append(t);break;case G.Before:s.before(t);break;case G.After:s.after(t);break;case G.UpsertAttributes:for(let i of t.getAttributeNames()){let o=t.getAttribute(i);s.setAttribute(i,o)}break;default:throw q("InvalidMergeMode",n,{mergeMode:e})}}var yt={type:2,name:O.MergeSignals,onGlobalInit:async n=>{U(O.MergeSignals,({signals:e="{}",onlyIfMissing:t=`${_e}`})=>{let{signals:r}=n,s=J(t);r.merge(fe(e),s)})}};var vt={type:2,name:O.RemoveFragments,onGlobalInit:async n=>{U(O.RemoveFragments,({selector:e,useViewTransition:t=`${pe}`})=>{if(!e.length)throw q("NoSelectorProvided",n);let r=J(t),s=document.querySelectorAll(e),i=()=>{for(let o of s)o.remove()};r&&X?le.startViewTransition(()=>i()):i()})}};var bt={type:2,name:O.RemoveSignals,onGlobalInit:async n=>{U(O.RemoveSignals,({paths:e=""})=>{let t=e.split(`
`).map(r=>r.trim());if(!t?.length)throw q("NoPathsProvided",n);n.signals.remove(...t)})}};var St={type:3,name:"clipboard",fn:(n,e)=>{if(!navigator.clipboard)throw N("ClipboardNotAvailable",n);navigator.clipboard.writeText(e)}};var At={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:n=>{let{el:e,genRX:t,effect:r}=n;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw N("CustomValidityInvalidElement",n);let{deps:s,rxFn:i}=t();return r(s,()=>{let o=i();if(typeof o!="string")throw N("CustomValidityInvalidExpression",n,{result:o});e.setCustomValidity(o)})}};var Et="once",Tt="half",wt="full",Rt={type:1,name:"intersects",keyReq:2,mods:new Set([Et,Tt,wt]),onLoad:({el:n,rawKey:e,mods:t,genRX:r})=>{let s={threshold:0};t.has(wt)?s.threshold=1:t.has(Tt)&&(s.threshold=.5);let{rxFn:i}=r(),o=new IntersectionObserver(u=>{for(let p of u)p.isIntersecting&&(i(),t.has(Et)&&(o.disconnect(),delete n.dataset[e]))},s);return o.observe(n),()=>o.disconnect()}};function ce(n){if(!n||n.size<=0)return 0;for(let e of n){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function se(n,e,t=!1){return n?n.has(e.toLowerCase()):t}function xt(n,e){return(...t)=>{setTimeout(()=>{n(...t)},e)}}function Mt(n,e,t=!1,r=!0){let s=-1,i=()=>s&&clearTimeout(s);return(...o)=>{i(),t&&!s&&n(...o),s=setTimeout(()=>{r&&n(...o),i()},e)}}function Pt(n,e,t=!0,r=!1){let s=!1;return(...i)=>{s||(t&&n(...i),s=!0,setTimeout(()=>{s=!1,r&&n(...i)},e))}}var An="evt",ie="signalsChange",En=ie.length,Dt={type:1,name:"on",keyReq:1,valReq:1,argNames:[An],onLoad:({el:n,key:e,mods:t,genRX:r})=>{let{rxFn:s}=r(),i=n;t.has("window")&&(i=window);let o=S=>{S&&((t.has("prevent")||e==="submit")&&S.preventDefault(),t.has("stop")&&S.stopPropagation()),s(S)},u=t.get("delay");if(u){let S=ce(u);o=xt(o,S)}let p=t.get("debounce");if(p){let S=ce(p),m=se(p,"leading",!1),y=!se(p,"notrail",!1);o=Mt(o,S,m,y)}let v=t.get("throttle");if(v){let S=ce(v),m=!se(v,"noleading",!1),y=se(v,"trail",!1);o=Pt(o,S,m,y)}if(t.has("viewtransition")&&X){let S=o;o=(...m)=>document.startViewTransition(()=>S(...m))}let w={capture:!0,passive:!1,once:!1};if(t.has("capture")||(w.capture=!1),t.has("passive")&&(w.passive=!0),t.has("once")&&(w.once=!0),e==="load")return setTimeout(o,0),()=>{};if(e==="interval"){let S=1e3,m=t.get("duration");m&&(S=ce(m),se(m,"leading",!1)&&o());let y=setInterval(o,S);return()=>{clearInterval(y)}}if(e==="raf"){let S,m=()=>{o(),S=requestAnimationFrame(m)};return S=requestAnimationFrame(m),()=>{S&&cancelAnimationFrame(S)}}if(e.startsWith(ie)){let S=e!==ie,m=L(K(e.slice(En)),t),y=E=>{if(S){let{added:A,removed:l,updated:d}=E.detail;if(![...A,...l,...d].some(c=>c.startsWith(m)))return}o(E)};return document.addEventListener(B,y),()=>{document.removeEventListener(B,y)}}if(t.has("outside")){i=document;let S=o;o=y=>{let E=y?.target;n.contains(E)||S(y)}}let b=j(e);return b=L(b,t),i.addEventListener(b,o,w),()=>{i.removeEventListener(b,o)}}};var Nt="session",It={type:1,name:"persist",mods:new Set([Nt]),onLoad:({key:n,mods:e,signals:t,value:r})=>{n=L(n,e),n===""&&(n=H);let s=e.has(Nt)?sessionStorage:localStorage,i=r.split(/\s+/).filter(x=>x!=="");i=i.map(x=>z(x));let o=()=>{let x=s.getItem(n)||"{}",b=JSON.parse(x);t.merge(b)},u=()=>{let x;i.length?x=t.subset(...i):x=t.values(),s.setItem(n,JSON.stringify(x))},p=n!==ie,v=L(K(n.slice(ie.length)),e),w=x=>{if(p){let{added:b,removed:S,updated:m}=x.detail;if(![...b,...S,...m].some(y=>y.startsWith(v)))return}u()};return document.addEventListener(B,w),o(),()=>{document.removeEventListener(B,w)}}};var Ct={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:n,genRX:e})=>{let{deps:t,rxFn:r}=e();return n(t,()=>{let s=r(),i=window.location.href,o=new URL(s,i).toString();window.history.replaceState({},"",o)})}};var Ae="smooth",Oe="instant",Fe="auto",Vt="hstart",Lt="hcenter",kt="hend",Ot="hnearest",Ft="vstart",Ht="vcenter",qt="vend",Wt="vnearest",Tn="focus",Ee="center",$t="start",Gt="end",Ut="nearest",_t={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ae,Oe,Fe,Vt,Lt,kt,Ot,Ft,Ht,qt,Wt,Tn]),onLoad:n=>{let{el:e,mods:t,rawKey:r}=n;e.tabIndex||e.setAttribute("tabindex","0");let s={behavior:Ae,block:Ee,inline:Ee};if(t.has(Ae)&&(s.behavior=Ae),t.has(Oe)&&(s.behavior=Oe),t.has(Fe)&&(s.behavior=Fe),t.has(Vt)&&(s.inline=$t),t.has(Lt)&&(s.inline=Ee),t.has(kt)&&(s.inline=Gt),t.has(Ot)&&(s.inline=Ut),t.has(Ft)&&(s.block=$t),t.has(Ht)&&(s.block=Ee),t.has(qt)&&(s.block=Gt),t.has(Wt)&&(s.block=Ut),!(e instanceof HTMLElement||e instanceof SVGElement))throw N("ScrollIntoViewInvalidElement",n);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(s),t.has("focus")&&e.focus(),delete e.dataset[r]}};var jt="none",Kt="display",Bt={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:n},genRX:e,effect:t})=>{let{deps:r,rxFn:s}=e();return t(r,async()=>{s()?n.display===jt&&n.removeProperty(Kt):n.setProperty(Kt,jt)})}};var Jt="view-transition",zt={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let n=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===Jt&&(n=!0);if(!n){let e=document.createElement("meta");e.name=Jt,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:n,el:e,genRX:t})=>{if(!X){console.error("Browser does not support view transitions");return}let{deps:r,rxFn:s}=t();return n(r,()=>{let i=s();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var Xt={type:1,name:"attr",valReq:1,onLoad:({el:n,key:e,effect:t,genRX:r})=>{let{deps:s,rxFn:i}=r();return e===""?t(s,async()=>{let o=i();for(let[u,p]of Object.entries(o))p===!1?n.removeAttribute(u):n.setAttribute(u,p)}):(e=j(e),t(s,async()=>{let o=!1;try{o=i()}catch{}let u;typeof o=="string"?u=o:u=JSON.stringify(o),!u||u==="false"||u==="null"||u==="undefined"?n.removeAttribute(e):n.setAttribute(e,u)}))}};var wn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,Yt=["change","input","keydown"],Zt={type:1,name:"bind",keyReq:3,valReq:3,onLoad:n=>{let{el:e,key:t,mods:r,signals:s,value:i,effect:o}=n,u=e,p=t?L(t,r):z(i),v=e.tagName.toLowerCase(),w=v.includes("input"),x=v.includes("select"),b=e.getAttribute("type"),S=e.hasAttribute("value"),m="",y=w&&b==="checkbox";y&&(m=S?"":!1);let E=w&&b==="number";E&&(m=0);let A=w&&b==="radio";A&&(e.getAttribute("name")?.length||e.setAttribute("name",p));let l=w&&b==="file",{signal:d,inserted:c}=s.upsertIfMissing(p,m),a=-1;Array.isArray(d.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",p),a=[...document.querySelectorAll(`[name="${p}"]`)].findIndex(P=>P===n.el));let h=a>=0,g=()=>[...s.value(p)],f=()=>{let P=s.value(p);h&&!x&&(P=P[a]||m);let C=`${P}`;if(y||A)typeof P=="boolean"?u.checked=P:u.checked=C===u.value;else if(x){let I=e;if(I.multiple){if(!h)throw N("BindSelectMultiple",n);for(let k of I.options){if(k?.disabled)return;let V=E?Number(k.value):k.value;k.selected=P.includes(V)}}else I.value=C}else l||("value"in e?e.value=C:e.setAttribute("value",C))},T=async()=>{let P=s.value(p);if(h){let V=P;for(;a>=V.length;)V.push(m);P=V[a]||m}let C=(V,W)=>{let $=W;h&&!x&&($=g(),$[a]=W),s.setValue(V,$)};if(l){let V=[...u?.files||[]],W=[],$=[],He=[];await Promise.all(V.map(qe=>new Promise(on=>{let Y=new FileReader;Y.onload=()=>{if(typeof Y.result!="string")throw N("InvalidFileResultType",n,{resultType:typeof Y.result});let Te=Y.result.match(wn);if(!Te?.groups)throw N("InvalidDataUri",n,{result:Y.result});W.push(Te.groups.contents),$.push(Te.groups.mime),He.push(qe.name)},Y.onloadend=()=>on(void 0),Y.readAsDataURL(qe)}))),C(p,W),C(`${p}Mimes`,$),C(`${p}Names`,He);return}let I=u.value||"",k;if(y){let V=u.checked||u.getAttribute("checked")==="true";S?k=V?I:"":k=V}else if(x){let W=[...e.selectedOptions];h?k=W.filter($=>$.selected).map($=>$.value):k=W[0]?.value||m}else typeof P=="boolean"?k=!!I:typeof P=="number"?k=Number(I):k=I||"";C(p,k)};c&&T();for(let P of Yt)e.addEventListener(P,T);let R=P=>{P.persisted&&T()};window.addEventListener("pageshow",R);let D=o([d],()=>{f()});return()=>{D();for(let P of Yt)e.removeEventListener(P,T);window.removeEventListener("pageshow",R)}}};var Qt={type:1,name:"class",valReq:1,onLoad:({el:n,key:e,mods:t,effect:r,genRX:s})=>{let i=n.classList,{deps:o,rxFn:u}=s();return r(o,()=>{if(e===""){let p=u();for(let[v,w]of Object.entries(p)){let x=v.split(/\s+/);w?i.add(...x):i.remove(...x)}}else{let p=j(e);p=L(p,t),u()?i.add(p):i.remove(p)}})}};var en={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?L(e,t):z(s);r.setValue(i,n)}};var tn={type:1,name:"text",keyReq:2,valReq:1,onLoad:n=>{let{el:e,effect:t,genRX:r}=n,{deps:s,rxFn:i}=r();return e instanceof HTMLElement||N("TextInvalidElement",n),t(s,()=>{let o=i(n);e.textContent=`${o}`})}};var{round:Rn,max:xn,min:Mn}=Math,nn={type:3,name:"fit",fn:(n,e,t,r,s,i,o=!1,u=!1)=>{let p=(e-t)/(r-t)*(i-s)+s;return u&&(p=Rn(p)),o&&(p=xn(s,Mn(i,p))),p}};var rn={type:3,name:"setAll",fn:({signals:n},e,t)=>{n.walk((r,s)=>{r.startsWith(e)&&(s.value=t)})}};var sn={type:3,name:"toggleAll",fn:({signals:n},e)=>{n.walk((t,r)=>{t.startsWith(e)&&(r.value=!r.value)})}};Le("star");ye(Xt,Zt,Qt,Dt,en,Bt,tn,ft,lt,ut,pt,ct,at,ht,yt,vt,bt,dt,St,At,Rt,It,Ct,_t,zt,nn,rn,sn);ke();export{ke as apply,ye as load,Le as setAlias};
//# sourceMappingURL=datastar-aliased.js.map
