// Datastar v1.0.0-beta.9
var Ge=/🖕JS_DS🚀/.source,ue=Ge.slice(0,5),Re=Ge.slice(4),O="datastar";var Ue="Datastar-Request",pe=300,_e=1e3,je="type module",fe=!1,Ke=!1,Be=!0,G={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},ze=G.Morph,F={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var D=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(D||{});var se=`${O}-signals`;var j=n=>n.trim()==="true",ie=n=>n.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,t)=>(t?"-":"")+e.toLowerCase()),J=n=>ie(n).replace(/-./g,e=>e[1].toUpperCase()),xe=n=>ie(n).replace(/-/g,"_"),pn=n=>J(n).replace(/^./,e=>e[0].toUpperCase()),de=n=>new Function(`return Object.assign({}, ${n})`)(),K=n=>n.startsWith("$")?n.slice(1):n,fn={kebab:ie,snake:xe,pascal:pn};function V(n,e){for(let t of e.get("case")||[]){let r=fn[t];r&&(n=r(n))}return n}var dn="computed",Je={type:1,name:dn,keyReq:1,valReq:1,onLoad:({key:n,mods:e,signals:t,genRX:r})=>{n=V(n,e);let s=r();t.setComputed(n,s)}};var Xe={type:1,name:"signals",onLoad:n=>{let{key:e,mods:t,signals:r,value:s,genRX:i}=n,c=t.has("ifmissing"),{rxFn:a}=i();if(e!==""){let f=V(e,t),g=s===""?s:a();c?r.upsertIfMissing(f,g):r.setValue(f,g)}else{let f=de(n.value);n.value=JSON.stringify(f);let g=a();r.merge(g,c)}}};var Ye={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ee=class{#e=0;#t;constructor(e=O){this.#t=e}with(e){if(typeof e=="string")for(let t of e.split(""))this.with(t.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function me(n){if(n.id)return n.id;let e=new ee,t=n;for(;t;){if(e.with(t.tagName||""),t.id){e.with(t.id);break}let r=t?.parentNode;r&&e.with([...r.children].indexOf(t)),t=r}return e.string}function ge(n,e){return new ee().with(n).with(e).value}function oe(n,e){if(!n||!(n instanceof HTMLElement||n instanceof SVGElement))return null;let t=n.dataset;if("starIgnore"in t)return null;"starIgnore__self"in t||e(n);let r=n.firstElementChild;for(;r;)oe(r,e),r=r.nextElementSibling}var X=class{},Y=class extends X{constructor(t,r){super();this.val=t;this.onChange=r;this.subs=new Set;this.ver=1}set value(t){this.val!==t&&(this.val=t,this.ver++,this.markDirty(),this.onChange?.(t))}markDirty(){for(let t of this.subs)t.markDirty()}get value(){return this.val}version(){return this.ver}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};var Me=class extends X{constructor(t,r){super();this.deps=t;this.fn=r;this.subs=new Set;this.isDirty=!0;this.ver=1;this.versionSum=0;for(let s of t)s.addSubscribers(this)}get value(){if(!this.isDirty)return this.val;this.isDirty=!1;let t=0;for(let i of this.deps)t+=i.version();if(t===this.versionSum)return this.val;this.versionSum=t;let r=this.deps.map(i=>i.value),s=this.fn(...r);return this.val===s?this.val:(this.val=s,this.ver++,this.val)}version(){return this.ver}markDirty(){this.isDirty=!0;for(let t of this.subs)t.markDirty()}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};function Qe(n,e){return new Me(n,e)}var De=class{constructor(e,t){this.deps=e;this.fn=t;this.depsVersionSum=-1;for(let r of e)r.addSubscribers(this)}markDirty(){let e=0;for(let r of this.deps)e+=r.version();if(e===this.depsVersionSum)return;this.depsVersionSum=e;let t=this.deps.map(r=>r.value);this.fn(...t)}};function Pe(n,e){let t=new De(n,e);return t.markDirty(),()=>{for(let r of n)r.removeSubscribers(t)}}var mn="https://data-star.dev/errors";function Ne(n,e,t={}){let r=new Error;r.name=`${O} ${n} error`;let s=xe(e),i=new URLSearchParams({metadata:JSON.stringify(t)}).toString(),c=JSON.stringify(t,null,2);return r.message=`${e}
More info: ${mn}/${n}/${s}?${i}
Context: ${c}`,r}function Ie(n,e,t={}){return Ne("internal",e,Object.assign({from:n},t))}function q(n,e,t={}){let r={plugin:{name:e.plugin.name,type:D[e.plugin.type]}};return Ne("init",n,Object.assign(r,t))}function N(n,e,t={}){let r={plugin:{name:e.plugin.name,type:D[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return Ne("runtime",n,Object.assign(r,t))}var Ze="namespacedSignals",te=n=>{document.dispatchEvent(new CustomEvent(se,{detail:Object.assign({added:[],removed:[],updated:[]},n)}))};function et(n,e=!1){let t={};for(let r in n)if(Object.hasOwn(n,r)){if(e&&r.startsWith("_"))continue;let s=n[r];s instanceof X?t[r]=s.value:t[r]=et(s)}return t}function tt(n,e,t,r=!1){let s={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw Ie(Ze,"InvalidSignalKey",{key:i});let c=t?`${t}.${i}`:i,a=e[i];if(a instanceof Object&&!Array.isArray(a)){n[i]||(n[i]={});let f=tt(n[i],a,c,r);s.added.push(...f.added.map(g=>`${c}.${g}`)),s.removed.push(...f.removed.map(g=>`${c}.${g}`)),s.updated.push(...f.updated.map(g=>`${c}.${g}`))}else{if(Object.hasOwn(n,i)){if(r)continue;let E=n[i];if(E instanceof Y){let w=E.value;E.value=a,w!==a&&s.updated.push(c);continue}}let g=new Y(a,()=>te({updated:[c]}));n[i]=g,s.added.push(c)}}return s}function nt(n,e){for(let t in n)if(Object.hasOwn(n,t)){let r=n[t];r instanceof X?e(t,r):nt(r,(s,i)=>{e(`${t}.${s}`,i)})}}function gn(n,...e){let t={};for(let r of e){let s=r.split("."),i=n,c=t;for(let f=0;f<s.length-1;f++){let g=s[f];if(!i[g])return{};c[g]||(c[g]={}),i=i[g],c=c[g]}let a=s[s.length-1];c[a]=i[a]}return t}var he=class{#e={};exists(e){return!!this.signal(e)}signal(e){let t=e.split("."),r=this.#e;for(let c=0;c<t.length-1;c++){let a=t[c];if(!r[a])return null;r=r[a]}let s=t[t.length-1],i=r[s];if(!i)throw Ie(Ze,"SignalNotFound",{path:e});return i}setSignal(e,t){let r=e.split("."),s=this.#e;for(let c=0;c<r.length-1;c++){let a=r[c];s[a]||(s[a]={}),s=s[a]}let i=r[r.length-1];s[i]=t}setComputed(e,t,r){let s=Qe(t,r);this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,t){let{signal:r}=this.upsertIfMissing(e,t),s=r.value;r.value=t,s!==t&&te({updated:[e]})}upsertIfMissing(e,t){let r=e.split("."),s=this.#e;for(let f=0;f<r.length-1;f++){let g=r[f];s[g]||(s[g]={}),s=s[g]}let i=r[r.length-1],c=s[i];if(c instanceof Y)return{signal:c,inserted:!1};let a=new Y(t);return a.onChange=()=>{te({updated:[e]})},s[i]=a,te({added:[e]}),{signal:a,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let t=Array();for(let r of e){let s=r.split("."),i=this.#e;for(let a=0;a<s.length-1;a++){let f=s[a];if(!i[f])return;i=i[f]}let c=s[s.length-1];delete i[c],t.push(r)}te({removed:t})}merge(e,t=!1){let r=tt(this.#e,e,"",t);(r.added.length||r.removed.length||r.updated.length)&&te(r)}subset(...e){return gn(this.values(),...e)}walk(e){nt(this.#e,e)}paths(){let e=new Array;return this.walk(t=>e.push(t)),e}values(e=!1){return et(this.#e,e)}JSON(e=!0,t=!1){let r=this.values(t);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var rt=new he,Ce=[],ye={},hn=[],Q=new Map,Le=null,Ve="";function st(n){Ve=n}function ve(...n){for(let e of n){let t={plugin:e,signals:rt,effect:(s,i)=>Pe(s,i),actions:ye,removals:Q,applyToElement:be},r;switch(e.type){case 2:{let s=e;hn.push(s),r=s.onGlobalInit;break}case 3:{ye[e.name]=e;break}case 1:{let s=e;Ce.push(s),r=s.onGlobalInit;break}default:throw q("InvalidPluginType",t)}r&&r(t)}Ce.sort((e,t)=>{let r=t.name.length-e.name.length;return r!==0?r:e.name.localeCompare(t.name)})}function ke(){queueMicrotask(()=>{be(document.documentElement),yn()})}function be(n){oe(n,e=>{let t=new Array,r=Q.get(e.id)||new Map,s=new Map([...r]),i=new Map;for(let c of Object.keys(e.dataset)){if(!c.startsWith(Ve))break;let a=e.dataset[c]||"",f=ge(c,a);i.set(c,f),r.has(f)?s.delete(f):t.push(c)}for(let[c,a]of s)a();for(let c of t){let a=i.get(c);vn(e,c,a)}})}function yn(){Le||(Le=new MutationObserver(n=>{let e=new Set,t=new Set;for(let{target:r,type:s,addedNodes:i,removedNodes:c}of n)switch(s){case"childList":{for(let a of c)e.add(a);for(let a of i)t.add(a)}break;case"attributes":{t.add(r);break}}for(let r of e){let s=Q.get(r.id);if(s){for(let[i,c]of s)c(),s.delete(i);s.size===0&&Q.delete(r.id)}}for(let r of t)be(r)}),Le.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function vn(n,e,t){let r=J(e.slice(Ve.length)),s=Ce.find(A=>r.startsWith(A.name));if(!s)return;n.id.length||(n.id=me(n));let[i,...c]=r.slice(s.name.length).split(/\_\_+/),a=i.length>0;a&&(i=J(i));let f=n.dataset[e]||"",g=f.length>0,E={signals:rt,applyToElement:be,effect:(A,y)=>Pe(A,y),actions:ye,removals:Q,genRX:()=>bn(E,...s.argNames||[]),plugin:s,el:n,rawKey:r,key:i,value:f,mods:new Map},w=s.keyReq||0;if(a){if(w===2)throw N(`${s.name}KeyNotAllowed`,E)}else if(w===1)throw N(`${s.name}KeyRequired`,E);let S=s.valReq||0;if(g){if(S===2)throw N(`${s.name}ValueNotAllowed`,E)}else if(S===1)throw N(`${s.name}ValueRequired`,E);if(w===3||S===3){if(a&&g)throw N(`${s.name}KeyAndValueProvided`,E);if(!a&&!g)throw N(`${s.name}KeyOrValueRequired`,E)}for(let A of c){let[y,...b]=A.split(".");E.mods.set(J(y),new Set(b.map(o=>o.toLowerCase())))}let R=s.onLoad(E)??(()=>{}),v=Q.get(n.id);v||(v=new Map,Q.set(n.id,v)),v.set(t,R)}function bn(n,...e){let t=new Array,r="",s=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=n.value.trim().match(s);if(i){let v=i.length-1,A=i[v].trim();A.startsWith("return")||(i[v]=`return (${A});`),r=i.join(`;
`)}let c=new Map,a=new RegExp(`(?:${ue})(.*?)(?:${Re})`,"gm");for(let v of r.matchAll(a)){let A=v[1],y=new ee("dsEscaped").with(A).string;c.set(y,A),r=r.replace(ue+A+Re,y)}let f=/@(\w*)\(/gm,g=r.matchAll(f),E=new Set;for(let v of g)E.add(v[1]);let w=new RegExp(`@(${Object.keys(ye).join("|")})\\(`,"gm");r=r.replaceAll(w,"ctx.actions.$1.fn(ctx,");let S=n.signals.paths();if(S.length){let v=new RegExp(`\\$(${S.join("|")})(\\W|$)`,"gm");for(let A of r.matchAll(v)){let y=A[1],b=n.signals.signal(y);b&&t.push(b)}r=r.replaceAll(v,"ctx.signals.signal('$1').value$2")}for(let[v,A]of c)r=r.replace(v,A);let R=`return (()=> {
${r}
})()`;n.fnContent=R;try{let v=new Function("ctx",...e,R);return{deps:t,rxFn:(...A)=>{try{return v(n,...A)}catch(y){throw N("ExecuteExpression",n,{error:y.message})}}}}catch(v){throw N("GenerateExpression",n,{error:v.message})}}ve(Ye,Xe,Je);async function Sn(n,e){let t=n.getReader(),r;for(;!(r=await t.read()).done;)e(r.value)}function An(n){let e,t,r,s=!1;return function(c){e===void 0?(e=c,t=0,r=-1):e=Tn(e,c);let a=e.length,f=0;for(;t<a;){s&&(e[t]===10&&(f=++t),s=!1);let g=-1;for(;t<a&&g===-1;++t)switch(e[t]){case 58:r===-1&&(r=t-f);break;case 13:s=!0;case 10:g=t;break}if(g===-1)break;n(e.subarray(f,g),r),f=t,r=-1}f===a?e=void 0:f!==0&&(e=e.subarray(f),t-=f)}}function En(n,e,t){let r=it(),s=new TextDecoder;return function(c,a){if(c.length===0)t?.(r),r=it();else if(a>0){let f=s.decode(c.subarray(0,a)),g=a+(c[a+1]===32?2:1),E=s.decode(c.subarray(g));switch(f){case"data":r.data=r.data?`${r.data}
${E}`:E;break;case"event":r.event=E;break;case"id":n(r.id=E);break;case"retry":{let w=Number.parseInt(E,10);Number.isNaN(w)||e(r.retry=w);break}}}}}function Tn(n,e){let t=new Uint8Array(n.length+e.length);return t.set(n),t.set(e,n.length),t}function it(){return{data:"",event:"",id:"",retry:void 0}}var wn="text/event-stream",ot="last-event-id";function at(n,e,{signal:t,headers:r,onopen:s,onmessage:i,onclose:c,onerror:a,openWhenHidden:f,fetch:g,retryInterval:E=1e3,retryScaler:w=2,retryMaxWaitMs:S=3e4,retryMaxCount:R=10,...v}){return new Promise((A,y)=>{let b=0,o={...r};o.accept||(o.accept=wn);let p;function u(){p.abort(),document.hidden||T()}f||document.addEventListener("visibilitychange",u);let l=0;function h(){document.removeEventListener("visibilitychange",u),window.clearTimeout(l),p.abort()}t?.addEventListener("abort",()=>{h(),A()});let m=g??window.fetch,d=s??function(){};async function T(){p=new AbortController;try{let M=await m(e,{...v,headers:o,signal:p.signal});await d(M),await Sn(M.body,An(En(x=>{x?o[ot]=x:delete o[ot]},x=>{E=x},i))),c?.(),h(),A()}catch(M){if(!p.signal.aborted)try{let x=a?.(M)??E;window.clearTimeout(l),l=window.setTimeout(T,x),E*=w,E=Math.min(E,S),b++,b>=R?(h(),y(N("SseMaxRetries",n,{retryMaxCount:R}))):console.error(`Datastar failed to reach ${v.method}: ${e.toString()} retry in ${x}ms`)}catch(x){h(),y(x)}}}T()})}var ne=`${O}-sse`,Oe=`${O}-settling`,Z=`${O}-swapping`,Se="started",Ae="finished",lt="error",ct="retrying";function U(n,e){document.addEventListener(ne,t=>{if(t.detail.type!==n)return;let{argsRaw:r}=t.detail;e(r)})}function ae(n,e){document.dispatchEvent(new CustomEvent(ne,{detail:{type:n,argsRaw:e}}))}var ut=n=>`${n}`.includes("text/event-stream"),_=async(n,e,t,r)=>{let{el:{id:s},el:i,signals:c}=n,{headers:a,contentType:f,includeLocal:g,selector:E,openWhenHidden:w,retryInterval:S,retryScaler:R,retryMaxWaitMs:v,retryMaxCount:A,abort:y}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:_e,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),b=e.toLowerCase(),o=()=>{};try{if(ae(Se,{elId:s}),!t?.length)throw N("SseNoUrlProvided",n,{action:b});let p={};p[Ue]=!0,f==="json"&&(p["Content-Type"]="application/json");let u=Object.assign({},p,a),l={method:e,headers:u,openWhenHidden:w,retryInterval:S,retryScaler:R,retryMaxWaitMs:v,retryMaxCount:A,signal:y,onopen:async d=>{if(d.status>=400){let T=d.status.toString();ae(lt,{status:T})}},onmessage:d=>{if(!d.event.startsWith(O))return;let T=d.event,M={},x=d.data.split(`
`);for(let L of x){let I=L.indexOf(" "),k=L.slice(0,I),C=M[k];C||(C=[],M[k]=C);let W=L.slice(I+1);C.push(W)}let P={};for(let[L,I]of Object.entries(M))P[L]=I.join(`
`);ae(T,P)},onerror:d=>{if(ut(d))throw N("InvalidContentType",n,{url:t});d&&(console.error(d.message),ae(ct,{message:d.message}))}},h=new URL(t,window.location.origin),m=new URLSearchParams(h.search);if(f==="json"){let d=c.JSON(!1,!g);e==="GET"?m.set(O,d):l.body=d}else if(f==="form"){let d=E?document.querySelector(E):i.closest("form");if(d===null)throw E?N("SseFormNotFound",n,{action:b,selector:E}):N("SseClosestFormNotFound",n,{action:b});if(i!==d){let M=x=>x.preventDefault();d.addEventListener("submit",M),o=()=>d.removeEventListener("submit",M)}if(!d.checkValidity()){d.reportValidity(),o();return}let T=new FormData(d);if(e==="GET"){let M=new URLSearchParams(T);for(let[x,P]of M)m.set(x,P)}else l.body=T}else throw N("SseInvalidContentType",n,{action:b,contentType:f});h.search=m.toString();try{await at(n,h.toString(),l)}catch(d){if(!ut(d))throw N("SseFetchFailed",n,{method:e,url:t,error:d})}}finally{ae(Ae,{elId:s}),o()}};var pt={type:3,name:"delete",fn:async(n,e,t)=>_(n,"DELETE",e,{...t})};var ft={type:3,name:"get",fn:async(n,e,t)=>_(n,"GET",e,{...t})};var dt={type:3,name:"patch",fn:async(n,e,t)=>_(n,"PATCH",e,{...t})};var mt={type:3,name:"post",fn:async(n,e,t)=>_(n,"POST",e,{...t})};var gt={type:3,name:"put",fn:async(n,e,t)=>_(n,"PUT",e,{...t})};var ht={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?V(e,t):K(s),{signal:c}=r.upsertIfMissing(i,!1),a=f=>{let{type:g,argsRaw:{elId:E}}=f.detail;if(E===n.id)switch(g){case Se:c.value=!0;break;case Ae:c.value=!1;break}};return document.addEventListener(ne,a),()=>{c.value=!1,document.removeEventListener(ne,a)}}};var yt={type:2,name:F.ExecuteScript,onGlobalInit:async n=>{U(F.ExecuteScript,({autoRemove:e=`${Be}`,attributes:t=je,script:r})=>{let s=j(e);if(!r?.length)throw q("NoScriptProvided",n);let i=document.createElement("script");for(let c of t.split(`
`)){let a=c.indexOf(" "),f=a?c.slice(0,a):c,g=a?c.slice(a):"";i.setAttribute(f.trim(),g.trim())}i.text=r,document.head.appendChild(i),s&&i.remove()})}};var le=document,B=!!le.startViewTransition;var vt=function(){"use strict";let n=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:n,afterNodeAdded:n,beforeNodeMorphed:n,afterNodeMorphed:n,beforeNodeRemoved:n,afterNodeRemoved:n,beforeAttributeUpdated:n},head:{style:"merge",shouldPreserve:S=>S.getAttribute("im-preserve")==="true",shouldReAppend:S=>S.getAttribute("im-re-append")==="true",shouldRemove:n,afterHeadMorphed:n},restoreFocus:!0};function t(S,R,v={}){S=E(S);let A=w(R),y=g(S,A,v),b=s(y,()=>a(y,S,A,o=>o.morphStyle==="innerHTML"?(i(o,S,A),Array.from(S.childNodes)):r(o,S,A)));return y.pantry.remove(),b}function r(S,R,v){let A=w(R);return i(S,A,v,R,R.nextSibling),Array.from(A.childNodes)}function s(S,R){if(!S.config.restoreFocus)return R();let v=document.activeElement;if(!(v instanceof HTMLInputElement||v instanceof HTMLTextAreaElement))return R();let{id:A,selectionStart:y,selectionEnd:b}=v,o=R();return A&&A!==document.activeElement?.id&&(v=S.target.querySelector(`[id="${A}"]`),v?.focus()),v&&!v.selectionEnd&&b&&v.setSelectionRange(y,b),o}let i=function(){function S(u,l,h,m=null,d=null){l instanceof HTMLTemplateElement&&h instanceof HTMLTemplateElement&&(l=l.content,h=h.content),m||=l.firstChild;for(let T of h.childNodes){if(m&&m!=d){let x=v(u,T,m,d);if(x){x!==m&&y(u,m,x),c(x,T,u),m=x.nextSibling;continue}}if(T instanceof Element&&u.persistentIds.has(T.id)){let x=b(l,T.id,m,u);c(x,T,u),m=x.nextSibling;continue}let M=R(l,T,m,u);M&&(m=M.nextSibling)}for(;m&&m!=d;){let T=m;m=m.nextSibling,A(u,T)}}function R(u,l,h,m){if(m.callbacks.beforeNodeAdded(l)===!1)return null;if(m.idMap.has(l)){let d=document.createElement(l.tagName);return u.insertBefore(d,h),c(d,l,m),m.callbacks.afterNodeAdded(d),d}else{let d=document.importNode(l,!0);return u.insertBefore(d,h),m.callbacks.afterNodeAdded(d),d}}let v=function(){function u(m,d,T,M){let x=null,P=d.nextSibling,L=0,I=T;for(;I&&I!=M;){if(h(I,d)){if(l(m,I,d))return I;x===null&&(m.idMap.has(I)||(x=I))}if(x===null&&P&&h(I,P)&&(L++,P=P.nextSibling,L>=2&&(x=void 0)),I.contains(document.activeElement))break;I=I.nextSibling}return x||null}function l(m,d,T){let M=m.idMap.get(d),x=m.idMap.get(T);if(!x||!M)return!1;for(let P of M)if(x.has(P))return!0;return!1}function h(m,d){let T=m,M=d;return T.nodeType===M.nodeType&&T.tagName===M.tagName&&(!T.id||T.id===M.id)}return u}();function A(u,l){if(u.idMap.has(l))p(u.pantry,l,null);else{if(u.callbacks.beforeNodeRemoved(l)===!1)return;l.parentNode?.removeChild(l),u.callbacks.afterNodeRemoved(l)}}function y(u,l,h){let m=l;for(;m&&m!==h;){let d=m;m=m.nextSibling,A(u,d)}return m}function b(u,l,h,m){let d=m.target.id===l&&m.target||m.target.querySelector(`[id="${l}"]`)||m.pantry.querySelector(`[id="${l}"]`);return o(d,m),p(u,d,h),d}function o(u,l){let h=u.id;for(;u=u.parentNode;){let m=l.idMap.get(u);m&&(m.delete(h),m.size||l.idMap.delete(u))}}function p(u,l,h){if(u.moveBefore)try{u.moveBefore(l,h)}catch{u.insertBefore(l,h)}else u.insertBefore(l,h)}return S}(),c=function(){function S(o,p,u){return u.ignoreActive&&o===document.activeElement?null:(u.callbacks.beforeNodeMorphed(o,p)===!1||(o instanceof HTMLHeadElement&&u.head.ignore||(o instanceof HTMLHeadElement&&u.head.style!=="morph"?f(o,p,u):(R(o,p,u),b(o,u)||i(u,o,p))),u.callbacks.afterNodeMorphed(o,p)),o)}function R(o,p,u){let l=p.nodeType;if(l===1){let h=o,m=p,d=h.attributes,T=m.attributes;for(let M of T)y(M.name,h,"update",u)||h.getAttribute(M.name)!==M.value&&h.setAttribute(M.name,M.value);for(let M=d.length-1;0<=M;M--){let x=d[M];if(x&&!m.hasAttribute(x.name)){if(y(x.name,h,"remove",u))continue;h.removeAttribute(x.name)}}b(h,u)||v(h,m,u)}(l===8||l===3)&&o.nodeValue!==p.nodeValue&&(o.nodeValue=p.nodeValue)}function v(o,p,u){if(o instanceof HTMLInputElement&&p instanceof HTMLInputElement&&p.type!=="file"){let l=p.value,h=o.value;A(o,p,"checked",u),A(o,p,"disabled",u),p.hasAttribute("value")?h!==l&&(y("value",o,"update",u)||(o.setAttribute("value",l),o.value=l)):y("value",o,"remove",u)||(o.value="",o.removeAttribute("value"))}else if(o instanceof HTMLOptionElement&&p instanceof HTMLOptionElement)A(o,p,"selected",u);else if(o instanceof HTMLTextAreaElement&&p instanceof HTMLTextAreaElement){let l=p.value,h=o.value;if(y("value",o,"update",u))return;l!==h&&(o.value=l),o.firstChild&&o.firstChild.nodeValue!==l&&(o.firstChild.nodeValue=l)}}function A(o,p,u,l){let h=p[u],m=o[u];if(h!==m){let d=y(u,o,"update",l);d||(o[u]=p[u]),h?d||o.setAttribute(u,""):y(u,o,"remove",l)||o.removeAttribute(u)}}function y(o,p,u,l){return o==="value"&&l.ignoreActiveValue&&p===document.activeElement?!0:l.callbacks.beforeAttributeUpdated(o,p,u)===!1}function b(o,p){return!!p.ignoreActiveValue&&o===document.activeElement&&o!==document.body}return S}();function a(S,R,v,A){if(S.head.block){let y=R.querySelector("head"),b=v.querySelector("head");if(y&&b){let o=f(y,b,S);return Promise.all(o).then(()=>{let p=Object.assign(S,{head:{block:!1,ignore:!0}});return A(p)})}}return A(S)}function f(S,R,v){let A=[],y=[],b=[],o=[],p=new Map;for(let l of R.children)p.set(l.outerHTML,l);for(let l of S.children){let h=p.has(l.outerHTML),m=v.head.shouldReAppend(l),d=v.head.shouldPreserve(l);h||d?m?y.push(l):(p.delete(l.outerHTML),b.push(l)):v.head.style==="append"?m&&(y.push(l),o.push(l)):v.head.shouldRemove(l)!==!1&&y.push(l)}o.push(...p.values());let u=[];for(let l of o){let h=document.createRange().createContextualFragment(l.outerHTML).firstChild;if(v.callbacks.beforeNodeAdded(h)!==!1){if("href"in h&&h.href||"src"in h&&h.src){let m,d=new Promise(function(T){m=T});h.addEventListener("load",function(){m()}),u.push(d)}S.appendChild(h),v.callbacks.afterNodeAdded(h),A.push(h)}}for(let l of y)v.callbacks.beforeNodeRemoved(l)!==!1&&(S.removeChild(l),v.callbacks.afterNodeRemoved(l));return v.head.afterHeadMorphed(S,{added:A,kept:b,removed:y}),u}let g=function(){function S(p,u,l){let{persistentIds:h,idMap:m}=b(p,u),d=R(l),T=d.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(T))throw`Do not understand how to morph style ${T}`;return{target:p,newContent:u,config:d,morphStyle:T,ignoreActive:d.ignoreActive,ignoreActiveValue:d.ignoreActiveValue,restoreFocus:d.restoreFocus,idMap:m,persistentIds:h,pantry:v(),callbacks:d.callbacks,head:d.head}}function R(p){let u=Object.assign({},e);return Object.assign(u,p),u.callbacks=Object.assign({},e.callbacks,p.callbacks),u.head=Object.assign({},e.head,p.head),u}function v(){let p=document.createElement("div");return p.hidden=!0,document.body.insertAdjacentElement("afterend",p),p}function A(p){let u=Array.from(p.querySelectorAll("[id]"));return p.id&&u.push(p),u}function y(p,u,l,h){for(let m of h)if(u.has(m.id)){let d=m;for(;d;){let T=p.get(d);if(T==null&&(T=new Set,p.set(d,T)),T.add(m.id),d===l)break;d=d.parentElement}}}function b(p,u){let l=A(p),h=A(u),m=o(l,h),d=new Map;y(d,m,p,l);let T=u.__idiomorphRoot||u;return y(d,m,T,h),{persistentIds:m,idMap:d}}function o(p,u){let l=new Set,h=new Map;for(let{id:d,tagName:T}of p)h.has(d)?l.add(d):h.set(d,T);let m=new Set;for(let{id:d,tagName:T}of u)m.has(d)?l.add(d):h.get(d)===T&&m.add(d);for(let d of l)m.delete(d);return m}return S}(),{normalizeElement:E,normalizeParent:w}=function(){let S=new WeakSet;function R(b){return b instanceof Document?b.documentElement:b}function v(b){if(b==null)return document.createElement("div");if(typeof b=="string")return v(y(b));if(S.has(b))return b;if(b instanceof Node){if(b.parentNode)return new A(b);{let o=document.createElement("div");return o.append(b),o}}else{let o=document.createElement("div");for(let p of[...b])o.append(p);return o}}class A{constructor(o){this.originalNode=o,this.realParentNode=o.parentNode,this.previousSibling=o.previousSibling,this.nextSibling=o.nextSibling}get childNodes(){let o=[],p=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;p&&p!=this.nextSibling;)o.push(p),p=p.nextSibling;return o}querySelectorAll(o){return this.childNodes.reduce((p,u)=>{if(u instanceof Element){u.matches(o)&&p.push(u);let l=u.querySelectorAll(o);for(let h=0;h<l.length;h++)p.push(l[h])}return p},[])}insertBefore(o,p){return this.realParentNode.insertBefore(o,p)}moveBefore(o,p){return this.realParentNode.moveBefore(o,p)}get __idiomorphRoot(){return this.originalNode}}function y(b){let o=new DOMParser,p=b.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(p.match(/<\/html>/)||p.match(/<\/head>/)||p.match(/<\/body>/)){let u=o.parseFromString(b,"text/html");if(p.match(/<\/html>/))return S.add(u),u;{let l=u.firstChild;return l&&S.add(l),l}}else{let l=o.parseFromString("<body><template>"+b+"</template></body>","text/html").body.querySelector("template").content;return S.add(l),l}}return{normalizeElement:R,normalizeParent:v}}();return{morph:t,defaults:e}}();var St={type:2,name:F.MergeFragments,onGlobalInit:async n=>{let e=document.createElement("template");U(F.MergeFragments,({fragments:t="<div></div>",selector:r="",mergeMode:s=ze,settleDuration:i=`${pe}`,useViewTransition:c=`${fe}`})=>{let a=Number.parseInt(i),f=j(c);e.innerHTML=t.trim();let g=[...e.content.children];for(let E of g){if(!(E instanceof Element))throw q("NoFragmentsFound",n);let w=r||`#${E.getAttribute("id")}`,S=[...document.querySelectorAll(w)||[]];if(!S.length)throw q("NoTargetsFound",n,{selectorOrID:w});f&&B?le.startViewTransition(()=>bt(n,s,a,E,S)):bt(n,s,a,E,S)}})}};function bt(n,e,t,r,s){for(let i of s){i.classList.add(Z);let c=i.outerHTML,a=i;switch(e){case G.Morph:{let E=r.cloneNode(!0);oe(E,w=>{!w.id?.length&&Object.keys(w.dataset).length&&(w.id=me(w));let S=n.removals.get(w.id);if(S){let R=new Map;for(let[v,A]of S){let y=ge(v,v);R.set(y,A),S.delete(v)}n.removals.set(w.id,R)}}),vt.morph(a,E);break}case G.Inner:a.innerHTML=r.outerHTML;break;case G.Outer:a.replaceWith(r);break;case G.Prepend:a.prepend(r);break;case G.Append:a.append(r);break;case G.Before:a.before(r);break;case G.After:a.after(r);break;case G.UpsertAttributes:for(let E of r.getAttributeNames()){let w=r.getAttribute(E);a.setAttribute(E,w)}break;default:throw q("InvalidMergeMode",n,{mergeMode:e})}let f=a.classList;f?.add(Z),setTimeout(()=>{i.classList.remove(Z),f?.remove(Z)},t);let g=a.outerHTML;f&&c!==g&&(f.add(Oe),setTimeout(()=>{f.remove(Oe)},t))}}var At={type:2,name:F.MergeSignals,onGlobalInit:async n=>{U(F.MergeSignals,({signals:e="{}",onlyIfMissing:t=`${Ke}`})=>{let{signals:r}=n,s=j(t);r.merge(de(e),s)})}};var Et={type:2,name:F.RemoveFragments,onGlobalInit:async n=>{U(F.RemoveFragments,({selector:e,settleDuration:t=`${pe}`,useViewTransition:r=`${fe}`})=>{if(!e.length)throw q("NoSelectorProvided",n);let s=Number.parseInt(t),i=j(r),c=document.querySelectorAll(e),a=()=>{for(let f of c)f.classList.add(Z);setTimeout(()=>{for(let f of c)f.remove()},s)};i&&B?le.startViewTransition(()=>a()):a()})}};var Tt={type:2,name:F.RemoveSignals,onGlobalInit:async n=>{U(F.RemoveSignals,({paths:e=""})=>{let t=e.split(`
`).map(r=>r.trim());if(!t?.length)throw q("NoPathsProvided",n);n.signals.remove(...t)})}};var wt={type:3,name:"clipboard",fn:(n,e)=>{if(!navigator.clipboard)throw N("ClipboardNotAvailable",n);navigator.clipboard.writeText(e)}};var Rt={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:n=>{let{el:e,genRX:t,effect:r}=n;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw N("CustomValidityInvalidElement",n);let s=t();return r(()=>{let i=s();if(typeof i!="string")throw N("CustomValidityInvalidExpression",n,{result:i});e.setCustomValidity(i)})}};var xt="once",Mt="half",Dt="full",Pt={type:1,name:"intersects",keyReq:2,mods:new Set([xt,Mt,Dt]),onLoad:({el:n,rawKey:e,mods:t,genRX:r})=>{let s={threshold:0};t.has(Dt)?s.threshold=1:t.has(Mt)&&(s.threshold=.5);let i=r(),c=new IntersectionObserver(a=>{for(let f of a)f.isIntersecting&&(i(),t.has(xt)&&(c.disconnect(),delete n.dataset[e]))},s);return c.observe(n),()=>c.disconnect()}};var Nt="session",It={type:1,name:"persist",mods:new Set([Nt]),onLoad:({key:n,effect:e,mods:t,signals:r,value:s})=>{n=V(n,t),n===""&&(n=O);let i=t.has(Nt)?sessionStorage:localStorage,c=s.split(/\s+/).filter(g=>g!=="");c=c.map(g=>K(g));let a=()=>{let g=i.getItem(n)||"{}",E=JSON.parse(g);r.merge(E)},f=()=>{let g;c.length?g=r.subset(...c):g=r.values(),i.setItem(n,JSON.stringify(g))};return a(),e(()=>{f()})}};var Lt={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:n,genRX:e})=>{let t=e();return n(()=>{let r=t(),s=window.location.href,i=new URL(r,s).toString();window.history.replaceState({},"",i)})}};var Ee="smooth",Fe="instant",He="auto",Ct="hstart",Vt="hcenter",kt="hend",Ot="hnearest",Ft="vstart",Ht="vcenter",qt="vend",Wt="vnearest",Rn="focus",Te="center",$t="start",Gt="end",Ut="nearest",_t={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ee,Fe,He,Ct,Vt,kt,Ot,Ft,Ht,qt,Wt,Rn]),onLoad:n=>{let{el:e,mods:t,rawKey:r}=n;e.tabIndex||e.setAttribute("tabindex","0");let s={behavior:Ee,block:Te,inline:Te};if(t.has(Ee)&&(s.behavior=Ee),t.has(Fe)&&(s.behavior=Fe),t.has(He)&&(s.behavior=He),t.has(Ct)&&(s.inline=$t),t.has(Vt)&&(s.inline=Te),t.has(kt)&&(s.inline=Gt),t.has(Ot)&&(s.inline=Ut),t.has(Ft)&&(s.block=$t),t.has(Ht)&&(s.block=Te),t.has(qt)&&(s.block=Gt),t.has(Wt)&&(s.block=Ut),!(e instanceof HTMLElement||e instanceof SVGElement))throw N("ScrollIntoViewInvalidElement",n);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(s),t.has("focus")&&e.focus(),delete e.dataset[r]}};var jt="none",Kt="display",Bt={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:n},genRX:e,effect:t})=>{let{deps:r,rxFn:s}=e();return t(r,async()=>{s()?n.display===jt&&n.removeProperty(Kt):n.setProperty(Kt,jt)})}};var zt="view-transition",Jt={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let n=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===zt&&(n=!0);if(!n){let e=document.createElement("meta");e.name=zt,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:n,el:e,genRX:t})=>{if(!B){console.error("Browser does not support view transitions");return}let r=t();return n(()=>{let s=r();if(!s?.length)return;let i=e.style;i.viewTransitionName=s})}};var Xt={type:1,name:"attr",valReq:1,onLoad:({el:n,key:e,effect:t,genRX:r})=>{let{deps:s,rxFn:i}=r();return e===""?t(s,async()=>{let c=i();for(let[a,f]of Object.entries(c))f===!1?n.removeAttribute(a):n.setAttribute(a,f)}):(e=ie(e),t(s,async()=>{let c=!1;try{c=i()}catch{}let a;typeof c=="string"?a=c:a=JSON.stringify(c),!a||a==="false"||a==="null"||a==="undefined"?n.removeAttribute(e):n.setAttribute(e,a)}))}};var xn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,Yt=["change","input","keydown"],Qt={type:1,name:"bind",keyReq:3,valReq:3,onLoad:n=>{let{el:e,key:t,mods:r,signals:s,value:i,effect:c}=n,a=e,f=t?V(t,r):K(i),g=e.tagName.toLowerCase(),E=g.includes("input"),w=g.includes("select"),S=e.getAttribute("type"),R=e.hasAttribute("value"),v="",A=E&&S==="checkbox";A&&(v=R?"":!1);let y=E&&S==="number";y&&(v=0);let b=E&&S==="radio";b&&(e.getAttribute("name")?.length||e.setAttribute("name",f));let o=E&&S==="file",{signal:p,inserted:u}=s.upsertIfMissing(f,v),l=-1;Array.isArray(p.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",f),l=[...document.querySelectorAll(`[name="${f}"]`)].findIndex(P=>P===n.el));let h=l>=0,m=()=>[...s.value(f)],d=()=>{let P=s.value(f);h&&!w&&(P=P[l]||v);let L=`${P}`;if(A||b)typeof P=="boolean"?a.checked=P:a.checked=L===a.value;else if(w){let I=e;if(I.multiple){if(!h)throw N("BindSelectMultiple",n);for(let k of I.options){if(k?.disabled)return;let C=y?Number(k.value):k.value;k.selected=P.includes(C)}}else I.value=L}else o||("value"in e?e.value=L:e.setAttribute("value",L))},T=async()=>{let P=s.value(f);if(h){let C=P;for(;l>=C.length;)C.push(v);P=C[l]||v}let L=(C,W)=>{let $=W;h&&!w&&($=m(),$[l]=W),s.setValue(C,$)};if(o){let C=[...a?.files||[]],W=[],$=[],We=[];await Promise.all(C.map($e=>new Promise(un=>{let z=new FileReader;z.onload=()=>{if(typeof z.result!="string")throw N("InvalidFileResultType",n,{resultType:typeof z.result});let we=z.result.match(xn);if(!we?.groups)throw N("InvalidDataUri",n,{result:z.result});W.push(we.groups.contents),$.push(we.groups.mime),We.push($e.name)},z.onloadend=()=>un(void 0),z.readAsDataURL($e)}))),L(f,W),L(`${f}Mimes`,$),L(`${f}Names`,We);return}let I=a.value||"",k;if(A){let C=a.checked||a.getAttribute("checked")==="true";R?k=C?I:"":k=C}else if(w){let W=[...e.selectedOptions];h?k=W.filter($=>$.selected).map($=>$.value):k=W[0]?.value||v}else typeof P=="boolean"?k=!!I:typeof P=="number"?k=Number(I):k=I||"";L(f,k)};u&&T();for(let P of Yt)e.addEventListener(P,T);let M=P=>{P.persisted&&T()};window.addEventListener("pageshow",M);let x=c([p],()=>{d()});return()=>{x();for(let P of Yt)e.removeEventListener(P,T);window.removeEventListener("pageshow",M)}}};var Zt={type:1,name:"class",valReq:1,onLoad:({el:n,key:e,mods:t,effect:r,genRX:s})=>{let i=n.classList,{deps:c,rxFn:a}=s();return r(c,()=>{if(e===""){let f=a();for(let[g,E]of Object.entries(f)){let w=g.split(/\s+/);E?i.add(...w):i.remove(...w)}}else e=V(e,t),a()?i.add(e):i.remove(e)})}};function ce(n){if(!n||n.size<=0)return 0;for(let e of n){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function re(n,e,t=!1){return n?n.has(e.toLowerCase()):t}function en(n,e){return(...t)=>{setTimeout(()=>{n(...t)},e)}}function tn(n,e,t=!1,r=!0){let s=-1,i=()=>s&&clearTimeout(s);return(...c)=>{i(),t&&!s&&n(...c),s=setTimeout(()=>{r&&n(...c),i()},e)}}function nn(n,e,t=!0,r=!1){let s=!1;return(...i)=>{s||(t&&n(...i),s=!0,setTimeout(()=>{s=!1,r&&n(...i)},e))}}var Mn="evt",qe="signalsChange",Dn=qe.length,rn={type:1,name:"on",keyReq:1,valReq:1,argNames:[Mn],onLoad:({el:n,key:e,mods:t,signals:r,effect:s,genRX:i})=>{let{deps:c,rxFn:a}=i(),f=n;t.has("window")&&(f=window);let g=y=>{y&&((t.has("prevent")||e==="submit")&&y.preventDefault(),t.has("stop")&&y.stopPropagation()),a(y)},E=t.get("delay");if(E){let y=ce(E);g=en(g,y)}let w=t.get("debounce");if(w){let y=ce(w),b=re(w,"leading",!1),o=!re(w,"notrail",!1);g=tn(g,y,b,o)}let S=t.get("throttle");if(S){let y=ce(S),b=!re(S,"noleading",!1),o=re(S,"trail",!1);g=nn(g,y,b,o)}if(t.has("viewtransition")&&B){let y=g;g=(...b)=>document.startViewTransition(()=>y(...b))}let R={capture:!0,passive:!1,once:!1};if(t.has("capture")||(R.capture=!1),t.has("passive")&&(R.passive=!0),t.has("once")&&(R.once=!0),e==="load")return setTimeout(g,0),()=>{};if(e==="interval"){let y=1e3,b=t.get("duration");b&&(y=ce(b),re(b,"leading",!1)&&g());let o=setInterval(g,y);return()=>{clearInterval(o)}}if(e==="raf"){let y,b=()=>{g(),y=requestAnimationFrame(b)};return y=requestAnimationFrame(b),()=>{y&&cancelAnimationFrame(y)}}if(e.startsWith(qe)){let y=e!==qe,b=V(J(e.slice(Dn)),t),o=p=>{if(y){let{added:u,removed:l,updated:h}=p.detail;if(![...u,...l,...h].some(m=>m.startsWith(b)))return}g(p)};return document.addEventListener(se,o),()=>{document.removeEventListener(se,o)}}if(t.has("outside")){f=document;let y=g;g=o=>{let p=o?.target;n.contains(p)||y(o)}}let A=V(e,t);return f.addEventListener(A,g,R),()=>{f.removeEventListener(A,g)}}};var sn={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?V(e,t):K(s);r.setValue(i,n)}};var on={type:1,name:"text",keyReq:2,valReq:1,onLoad:n=>{let{el:e,effect:t,genRX:r}=n,{deps:s,rxFn:i}=r();return e instanceof HTMLElement||N("TextInvalidElement",n),t(s,()=>{let c=i(n);e.textContent=`${c}`})}};var{round:Pn,max:Nn,min:In}=Math,an={type:3,name:"fit",fn:(n,e,t,r,s,i,c=!1,a=!1)=>{let f=(e-t)/(r-t)*(i-s)+s;return a&&(f=Pn(f)),c&&(f=Nn(s,In(i,f))),f}};var ln={type:3,name:"setAll",fn:({signals:n},e,t)=>{n.walk((r,s)=>{r.startsWith(e)&&(s.value=t)})}};var cn={type:3,name:"toggleAll",fn:({signals:n},e)=>{n.walk((t,r)=>{t.startsWith(e)&&(r.value=!r.value)})}};ve(Xt,Qt,Zt,rn,sn,Bt,on,ht,ft,mt,gt,dt,pt,St,At,Et,Tt,yt,wt,Rt,Pt,It,Lt,_t,Jt,an,ln,cn);ke();export{ke as apply,ve as load,st as setAlias};
//# sourceMappingURL=datastar.js.map
