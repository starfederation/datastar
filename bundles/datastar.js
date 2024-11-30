"use strict";(()=>{var L="datastar",Te="datastar-event",We="Datastar-Request";var $e="type module";var I={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Be=I.Morph,N={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var G=t=>{let e=new Error;return e.name=`${L}${t}`,e},d=G(400),X=G(409),V=G(404),j=G(403),se=G(405),Ue=G(503);function W(t){let e={};for(let[n,r]of Object.entries(t))n.startsWith("_")||(typeof r=="object"&&!Array.isArray(r)?e[n]=W(r):e[n]=r);return e}function ae(t,e,n){let r={};if(!n)Object.assign(r,e);else for(let o in e){let i=t[o]?.value;i==null&&(r[o]=e[o])}return r}async function je(t,e){let n=t.getReader(),r;for(;!(r=await n.read()).done;)e(r.value)}function Ke(t){let e,n,r,o=!1;return function(s){e===void 0?(e=s,n=0,r=-1):e=bn(e,s);let a=e.length,u=0;for(;n<a;){o&&(e[n]===10&&(u=++n),o=!1);let m=-1;for(;n<a&&m===-1;++n)switch(e[n]){case 58:r===-1&&(r=n-u);break;case 13:o=!0;case 10:m=n;break}if(m===-1)break;t(e.subarray(u,m),r),u=n,r=-1}u===a?e=void 0:u!==0&&(e=e.subarray(u),n-=u)}}function qe(t,e,n){let r=Ge(),o=new TextDecoder;return function(s,a){if(s.length===0)n?.(r),r=Ge();else if(a>0){let u=o.decode(s.subarray(0,a)),m=a+(s[a+1]===32?2:1),c=o.decode(s.subarray(m));switch(u){case"data":r.data=r.data?r.data+`
`+c:c;break;case"event":r.event=c;break;case"id":t(r.id=c);break;case"retry":let l=parseInt(c,10);isNaN(l)||e(r.retry=l);break}}}}function bn(t,e){let n=new Uint8Array(t.length+e.length);return n.set(t),n.set(e,t.length),n}function Ge(){return{data:"",event:"",id:"",retry:void 0}}var ze="text/event-stream",yn=1e3,Je="last-event-id";function _e(t,{signal:e,headers:n,onopen:r,onmessage:o,onclose:i,onerror:s,openWhenHidden:a,fetch:u,retryScaler:m=2,retryMaxWaitMs:c=3e4,retryMaxCount:l=10,...g}){return new Promise((p,A)=>{let v=0,b={...n};b.accept||(b.accept=ze);let S;function w(){S.abort(),document.hidden||M()}a||document.addEventListener("visibilitychange",w);let f=yn,T=0;function E(){document.removeEventListener("visibilitychange",w),window.clearTimeout(T),S.abort()}e?.addEventListener("abort",()=>{E(),p()});let x=u??window.fetch,_=r??function(){};async function M(){S=new AbortController;try{let D=await x(t,{...g,headers:b,signal:S.signal});await _(D),await je(D.body,Ke(qe(R=>{R?b[Je]=R:delete b[Je]},R=>{f=R},o))),i?.(),E(),p()}catch(D){if(!S.signal.aborted)try{let R=s?.(D)??f;window.clearTimeout(T),T=window.setTimeout(M,R),f*=m,f=Math.min(f,c),v++,v>=l?(E(),A(Ue)):console.error(`Datastar failed to reach ${g.method}:${t.toString()} retry in ${R}ms`)}catch(R){E(),A(R)}}}M()})}var K=`${L}-sse`,we=`${L}-settling`,$=`${L}-swapping`,le="started",ue="finished";function k(t,e){document.addEventListener(K,n=>{if(n.detail.type!=t)return;let{argsRaw:r}=n.detail;e(r)})}var Ye=t=>`${t}`.includes("text/event-stream");function xe(t,e){document.dispatchEvent(new CustomEvent(K,{detail:{type:t,argsRaw:e}}))}function C(t){return async(e,n,r)=>{if(!n?.length)throw d;let o=r?.onlyRemoteSignals??!0,i=Object.assign({"Content-Type":"application/json",[We]:!0},r?.headers),s=Object.assign({},e.signals);o&&(s=W(s));let a=JSON.stringify(s),{el:{id:u}}=e;xe(le,{elID:u});let m=new URL(n,window.location.origin);t=t.toUpperCase();let c={method:t,headers:i,onmessage:l=>{if(!l.event.startsWith(L))return;let g=l.event,p={},A=l.data.split(`
`);for(let b of A){let S=b.indexOf(" "),w=b.slice(0,S),f=p[w];f||(f=[],p[w]=f);let T=b.slice(S+1).trim();f.push(T)}let v={};for(let[b,S]of Object.entries(p))v[b]=S.join(`
`);xe(g,v)},onerror:l=>{if(Ye(l))throw l;l&&console.error(l.message)},onclose:()=>{xe(ue,{elID:u})}};if(t==="GET"){let l=new URLSearchParams(m.search);l.append(L,a),m.search=l.toString()}else c.body=a;try{let l=m.toString();await _e(l,c)}catch(l){if(!Ye(l))throw l}}}var Ze={pluginType:3,name:"delete",method:C("delete")};var Xe={pluginType:3,name:"get",method:C("get")};var Qe={pluginType:3,name:"patch",method:C("patch")};var et={pluginType:3,name:"post",method:C("post")};var tt={pluginType:3,name:"put",method:C("put")};var nt={pluginType:3,name:"clipboard",method:(t,e)=>{if(!navigator.clipboard)throw j;navigator.clipboard.writeText(e)}};var rt={pluginType:3,name:"setAll",method:(t,e,n)=>{let r=new RegExp(e);t.walkSignals((o,i)=>r.test(o)&&(i.value=n))}};var ot={pluginType:3,name:"toggleAll",method:(t,e)=>{let n=new RegExp(e);t.walkSignals((r,o)=>n.test(r)&&(o.value=!o.value))}};var it={pluginType:3,name:"clampFit",method:(t,e,n,r,o,i)=>Math.max(o,Math.min(i,(e-n)/(r-n)*(i-o)+o))};var st={pluginType:3,name:"clampFitInt",method:(t,e,n,r,o,i)=>Math.round(Math.max(o,Math.min(i,(e-n)/(r-n)*(i-o)+o)))};var at={pluginType:3,name:"fit",method:(t,e,n,r,o,i)=>(e-n)/(r-n)*(i-o)+o};var lt={pluginType:3,name:"fitInt",method:(t,e,n,r,o,i)=>Math.round((e-n)/(r-n)*(i-o)+o)};var An=`${L}-indicator`,vo=`${An}-loading`,ut={pluginType:1,name:"indicator",mustHaveEmptyKey:!0,onLoad:t=>{let{expression:e,upsertSignal:n,el:r}=t,i=n(e,!1),s=a=>{let{type:u,argsRaw:{elID:m}}=a.detail;if(m===r.id)switch(u){case le:i.value=!0;break;case ue:i.value=!1;break}};return document.addEventListener(K,s),()=>{document.removeEventListener(K,s)}}};var ct={pluginType:1,name:"computed",mustNotEmptyKey:!0,onLoad:t=>{let{signals:e,key:n,expressionFn:r,reactivity:{computed:o}}=t;return e[n]=o(()=>r(t)),()=>{delete e[t.key]}}};var ft={pluginType:1,name:"mergeSignals",removeNewLines:!0,macros:{pre:[{pluginType:0,name:"signals",regexp:/(?<whole>.+)/g,replacer:t=>{let{whole:e}=t;return`Object.assign({...ctx.signals}, ${e})`}}]},allowedModifiers:new Set(["ifmissing"]),onLoad:t=>{let{el:e,signals:n,expressionFn:r,modifiers:o,mergeSignals:i}=t,s=r(t),a=ae(n,s,o.has("ifmissing"));i(a),delete e.dataset[t.rawKey]}};var mt={pluginType:1,name:"star",onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ce=t=>t.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,n)=>(n?"-":"")+e.toLowerCase()),F=t=>t.trim()==="true";var Tn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,pt=["change","input","keydown"],dt={pluginType:1,name:"bind",onLoad:t=>{let{el:e,expression:n,expressionFn:r,key:o,upsertSignal:i,reactivity:{effect:s}}=t,a=()=>{},u=()=>{},m=o==="";if(m){if(typeof n!="string")throw new Error("Invalid expression");if(n.includes("$"))throw new Error("Not an expression");let l=e.tagName.toLowerCase(),g="",p=l.includes("input"),A=e.getAttribute("type"),v=l.includes("checkbox")||p&&A==="checkbox";v&&(g=!1);let b=l.includes("select"),S=l.includes("radio")||p&&A==="radio",w=p&&A==="file";S&&(e.getAttribute("name")?.length||e.setAttribute("name",n));let f=i(n,g);a=()=>{let T="value"in e,E=f.value,x=`${E}`;if(v||S){let _=e;v?_.checked=E:S&&(_.checked=x===_.value)}else if(!w)if(b){let _=e;_.multiple?Array.from(_.options).forEach(M=>{M?.disabled||(M.selected=E.includes(M.value))}):_.value=x}else T?e.value=x:e.setAttribute("value",x)},u=async()=>{if(w){let x=[...e?.files||[]],_=[],M=[],D=[];await Promise.all(x.map(Ve=>new Promise(Sn=>{let Z=new FileReader;Z.onload=()=>{if(typeof Z.result!="string")throw d;let Ae=Z.result.match(Tn);if(!Ae?.groups)throw d;_.push(Ae.groups.contents),M.push(Ae.groups.mime),D.push(Ve.name)},Z.onloadend=()=>Sn(void 0),Z.readAsDataURL(Ve)}))),f.value=_;let R=t.signals,ie=`${n}Mimes`,He=`${n}Names`;ie in R&&(R[`${ie}`].value=M),He in R&&(R[`${He}`].value=D);return}let T=f.value,E=e||e;if(typeof T=="number")f.value=Number(E.value||E.getAttribute("value"));else if(typeof T=="string")f.value=E.value||E.getAttribute("value")||"";else if(typeof T=="boolean")v?f.value=E.checked||E.getAttribute("checked")==="true":f.value=!!(E.value||E.getAttribute("value"));else if(!(typeof T>"u"))if(typeof T=="bigint")f.value=BigInt(E.value||E.getAttribute("value")||"0");else if(Array.isArray(T)){if(b){let M=[...e.selectedOptions].map(D=>D.value);f.value=M}else f.value=JSON.parse(E.value).split(",");console.log(E.value)}else throw se}}else{let l=ce(o);a=()=>{let g=r(t),p;typeof g=="string"?p=g:p=JSON.stringify(g),!p||p==="false"||p==="null"||p==="undefined"?e.removeAttribute(l):e.setAttribute(l,p)}}m&&pt.forEach(l=>{e.addEventListener(l,u)});let c=s(async()=>{a()});return()=>{c(),m&&pt.forEach(l=>{e.removeEventListener(l,u)})}}};var gt={pluginType:1,name:"class",mustHaveEmptyKey:!0,mustNotEmptyExpression:!0,onLoad:t=>t.reactivity.effect(()=>{let e=t.expressionFn(t);for(let[n,r]of Object.entries(e)){let o=n.split(" ");r?t.el.classList.add(...o):t.el.classList.remove(...o)}})};function Re(t){if(!t||t?.length===0)return 0;for(let e of t){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return parseFloat(e)}catch{}}return 0}function Q(t,e,n=!1){return t?t.includes(e)||n:!1}function ht(t,e,n=!1,r=!0){let o=-1,i=()=>o&&clearTimeout(o);return function(...a){i(),n&&!o&&t(...a),o=setTimeout(()=>{r&&t(...a),i()},e)}}function Et(t,e,n=!0,r=!1){let o=!1;return function(...s){o||(n&&t(...s),o=!0,setTimeout(()=>{o=!1,r&&t(...s)},e))}}var _n=new Set(["window","once","passive","capture","debounce","throttle","remote","outside"]),vt="",St={pluginType:1,name:"on",mustNotEmptyKey:!0,mustNotEmptyExpression:!0,argumentNames:["evt"],onLoad:t=>{let{el:e,key:n,expressionFn:r}=t,o=t.el;t.modifiers.get("window")&&(o=window);let i=l=>{r(t,l)},s=t.modifiers.get("debounce");if(s){let l=Re(s),g=Q(s,"leading",!1),p=Q(s,"noTrail",!0);i=ht(i,l,g,p)}let a=t.modifiers.get("throttle");if(a){let l=Re(a),g=Q(a,"noLead",!0),p=Q(a,"noTrail",!1);i=Et(i,l,g,p)}let u={capture:!0,passive:!1,once:!1};t.modifiers.has("capture")||(u.capture=!1),t.modifiers.has("passive")&&(u.passive=!0),t.modifiers.has("once")&&(u.once=!0),[...t.modifiers.keys()].filter(l=>!_n.has(l)).forEach(l=>{let g=t.modifiers.get(l)||[],p=i;i=()=>{let v=event,b=v[l],S;if(typeof b=="function")S=b(...g);else if(typeof b=="boolean")S=b;else if(typeof b=="string"){let w=b.toLowerCase().trim(),f=g.join("").toLowerCase().trim();S=w===f}else throw d;S&&p(v)}});let c=ce(n).toLowerCase();switch(c){case"load":return i(),delete t.el.dataset.onLoad,()=>{};case"raf":let l,g=()=>{i(),l=requestAnimationFrame(g)};return l=requestAnimationFrame(g),()=>{l&&cancelAnimationFrame(l)};case"signals-change":return t.reactivity.effect(()=>{let A=t.signals;t.modifiers.has("remote")&&(A=W(A));let v=JSON.stringify(A);vt!==v&&(vt=v,i())});default:if(t.modifiers.has("outside")){o=document;let A=i,v=!1;i=S=>{let w=S?.target;if(!w)return;let f=e.id===w.id;f&&v&&(v=!1),!f&&!v&&(A(S),v=!0)}}return o.addEventListener(c,i,u),()=>{o.removeEventListener(c,i)}}}};var bt={pluginType:1,name:"ref",mustHaveEmptyKey:!0,mustNotEmptyExpression:!0,bypassExpressionFunctionCreation:()=>!0,onLoad:t=>{let e=t.expression;return t.upsertSignal(e,t.el),()=>{t.removeSignals(e)}}};var yt={pluginType:1,name:"text",mustHaveEmptyKey:!0,onLoad:t=>{let{el:e,expressionFn:n}=t;if(!(e instanceof HTMLElement))throw d;return t.reactivity.effect(()=>{let r=n(t);e.textContent=`${r}`})}};var At={pluginType:1,name:"persist",allowedModifiers:new Set(["local","session","remote"]),onLoad:t=>{let e=t.key||L,n=t.expression,r=new Set;if(n.trim()!==""){let c=t.expressionFn(t).split(" ");for(let l of c)r.add(l)}let o="",i=t.modifiers.has("session")?"session":"local",s=t.modifiers.has("remote"),a=m=>{let c=t.signals;if(s&&(c=W(c)),r.size>0){let g={};for(let p of r){let A=p.split("."),v=g,b=c;for(let w=0;w<A.length-1;w++){let f=A[w];v[f]||(v[f]={}),v=v[f],b=b[f]}let S=A[A.length-1];v[S]=b[S]}c=g}let l=JSON.stringify(c);l!==o&&(i==="session"?window.sessionStorage.setItem(e,l):window.localStorage.setItem(e,l),o=l)};window.addEventListener(Te,a);let u;if(i==="session"?u=window.sessionStorage.getItem(e):u=window.localStorage.getItem(e),u){let m=JSON.parse(u);for(let c in m)t.upsertSignal(c,m[c])}return()=>{window.removeEventListener(Te,a)}}};var Tt={pluginType:1,name:"replaceUrl",mustHaveEmptyKey:!0,mustNotEmptyExpression:!0,onLoad:t=>t.reactivity.effect(()=>{let e=t.expressionFn(t),n=window.location.href,r=new URL(e,n).toString();window.history.replaceState({},"",r)})};var _t="once",wt="half",xt="full",Rt={pluginType:1,name:"intersects",allowedModifiers:new Set([_t,wt,xt]),mustHaveEmptyKey:!0,onLoad:t=>{let{modifiers:e}=t,n={threshold:0};e.has(xt)?n.threshold=1:e.has(wt)&&(n.threshold=.5);let r=new IntersectionObserver(o=>{o.forEach(i=>{i.isIntersecting&&(t.expressionFn(t),e.has(_t)&&(r.disconnect(),delete t.el.dataset[t.rawKey]))})},n);return r.observe(t.el),()=>r.disconnect()}};function Pt(t){if(t.id)return t.id;let e=0,n=o=>(e=(e<<5)-e+o,e&e),r=o=>o.split("").forEach(i=>n(i.charCodeAt(0)));for(;t.parentNode;){if(t.id){r(`${t.id}`);break}else if(t===t.ownerDocument.documentElement)r(t.tagName);else{for(let o=1,i=t;i.previousElementSibling;i=i.previousElementSibling,o++)n(o);t=t.parentNode}t=t.parentNode}return L+e}function Mt(t,e,n=!0){if(!(t instanceof HTMLElement||t instanceof SVGElement))throw V;t.tabIndex||t.setAttribute("tabindex","0"),t.scrollIntoView(e),n&&t.focus()}var fe="smooth",Pe="instant",Me="auto",Lt="hstart",Nt="hcenter",Dt="hend",It="hnearest",kt="vstart",Ct="vcenter",Ot="vend",Ft="vnearest",wn="focus",me="center",Ht="start",Vt="end",Wt="nearest",$t={pluginType:1,name:"scrollIntoView",mustHaveEmptyKey:!0,mustHaveEmptyExpression:!0,allowedModifiers:new Set([fe,Pe,Me,Lt,Nt,Dt,It,kt,Ct,Ot,Ft,wn]),onLoad:({el:t,modifiers:e,rawKey:n})=>{t.tabIndex||t.setAttribute("tabindex","0");let r={behavior:fe,block:me,inline:me};return e.has(fe)&&(r.behavior=fe),e.has(Pe)&&(r.behavior=Pe),e.has(Me)&&(r.behavior=Me),e.has(Lt)&&(r.inline=Ht),e.has(Nt)&&(r.inline=me),e.has(Dt)&&(r.inline=Vt),e.has(It)&&(r.inline=Wt),e.has(kt)&&(r.block=Ht),e.has(Ct)&&(r.block=me),e.has(Ot)&&(r.block=Vt),e.has(Ft)&&(r.block=Wt),Mt(t,r,e.has("focus")),delete t.dataset[n],()=>{}}};var Bt={pluginType:1,name:"show",mustHaveEmptyKey:!0,mustNotEmptyExpression:!0,onLoad:t=>t.reactivity.effect(async()=>{t.expressionFn(t)?t.el.style.display==="none"&&t.el.style.removeProperty("display"):t.el.style.setProperty("display","none")})};var ee=document,q=!!ee.startViewTransition;var Le="view-transition",Ut={pluginType:1,name:Le,onGlobalInit(){let t=!1;if(document.head.childNodes.forEach(e=>{e instanceof HTMLMetaElement&&e.name===Le&&(t=!0)}),!t){let e=document.createElement("meta");e.name=Le,e.content="same-origin",document.head.appendChild(e)}},onLoad:t=>{if(!q){console.error("Browser does not support view transitions");return}return t.reactivity.effect(()=>{let{el:e,expressionFn:n}=t,r=n(t);if(!r)return;let o=e.style;o.viewTransitionName=r})}};var Gt="[a-zA-Z_$]+",xn=Gt+"[0-9a-zA-Z_$.]*";function pe(t,e,n,r=!0){let o=r?xn:Gt;return new RegExp(`(?<whole>${t}(?<${e}>${o})${n})`,"g")}var jt={name:"action",pluginType:0,regexp:pe("@","action","(?<call>\\((?<args>.*)\\))",!1),replacer:({action:t,args:e})=>{let n=["ctx"];e&&n.push(...e.split(",").map(o=>o.trim()));let r=n.join(",");return`ctx.actions.${t}.method(${r})`}};var Kt={name:"signal",pluginType:0,regexp:pe("\\$","signal","(?<method>\\([^\\)]*\\))?"),replacer:t=>{let{signal:e,method:n}=t,r="ctx.signals";if(!n?.length)return`${r}.${e}.value`;let o=e.split("."),i=o.pop(),s=o.join(".");return`${r}.${s}.value.${i}${n}`}};var qt={pluginType:2,name:N.ExecuteScript,onGlobalInit:async()=>{k(N.ExecuteScript,({autoRemove:t=`${!0}`,attributes:e=$e,script:n})=>{let r=F(t);if(!n?.length)throw d;let o=document.createElement("script");e.split(`
`).forEach(i=>{let s=i.indexOf(" "),a=s?i.slice(0,s):i,u=s?i.slice(s):"";o.setAttribute(a.trim(),u.trim())}),o.text=n,document.head.appendChild(o),r&&o.remove()})}};var ge=new WeakSet;function Zt(t,e,n={}){t instanceof Document&&(t=t.documentElement);let r;typeof e=="string"?r=Dn(e):r=e;let o=In(r),i=Mn(t,o,n);return Xt(t,o,i)}function Xt(t,e,n){if(n.head.block){let r=t.querySelector("head"),o=e.querySelector("head");if(r&&o){let i=en(o,r,n);Promise.all(i).then(()=>{Xt(t,e,Object.assign(n,{head:{block:!1,ignore:!0}}))});return}}if(n.morphStyle==="innerHTML")return Qt(e,t,n),t.children;if(n.morphStyle==="outerHTML"||n.morphStyle==null){let r=Cn(e,t,n);if(!r)throw V;let o=r?.previousSibling,i=r?.nextSibling,s=he(t,r,n);return r?kn(o,s,i):[]}else throw d}function he(t,e,n){if(!(n.ignoreActive&&t===document.activeElement))if(e==null){if(n.callbacks.beforeNodeRemoved(t)===!1)return;t.remove(),n.callbacks.afterNodeRemoved(t);return}else{if(Ee(t,e))return n.callbacks.beforeNodeMorphed(t,e)===!1?void 0:(t instanceof HTMLHeadElement&&n.head.ignore||(e instanceof HTMLHeadElement&&t instanceof HTMLHeadElement&&n.head.style!==I.Morph?en(e,t,n):(Pn(e,t),Qt(e,t,n))),n.callbacks.afterNodeMorphed(t,e),t);if(n.callbacks.beforeNodeRemoved(t)===!1||n.callbacks.beforeNodeAdded(e)===!1)return;if(!t.parentElement)throw d;return t.parentElement.replaceChild(e,t),n.callbacks.afterNodeAdded(e),n.callbacks.afterNodeRemoved(t),e}}function Qt(t,e,n){let r=t.firstChild,o=e.firstChild,i;for(;r;){if(i=r,r=i.nextSibling,o==null){if(n.callbacks.beforeNodeAdded(i)===!1)return;e.appendChild(i),n.callbacks.afterNodeAdded(i),B(n,i);continue}if(tn(i,o,n)){he(o,i,n),o=o.nextSibling,B(n,i);continue}let s=Ln(t,e,i,o,n);if(s){o=Jt(o,s,n),he(s,i,n),B(n,i);continue}let a=Nn(t,i,o,n);if(a){o=Jt(o,a,n),he(a,i,n),B(n,i);continue}if(n.callbacks.beforeNodeAdded(i)===!1)return;e.insertBefore(i,o),n.callbacks.afterNodeAdded(i),B(n,i)}for(;o!==null;){let s=o;o=o.nextSibling,nn(s,n)}}function Pn(t,e){let n=t.nodeType;if(n===1){for(let r of t.attributes)e.getAttribute(r.name)!==r.value&&e.setAttribute(r.name,r.value);for(let r of e.attributes)t.hasAttribute(r.name)||e.removeAttribute(r.name)}if((n===Node.COMMENT_NODE||n===Node.TEXT_NODE)&&e.nodeValue!==t.nodeValue&&(e.nodeValue=t.nodeValue),t instanceof HTMLInputElement&&e instanceof HTMLInputElement&&t.type!=="file")e.value=t.value||"",de(t,e,"value"),de(t,e,"checked"),de(t,e,"disabled");else if(t instanceof HTMLOptionElement)de(t,e,"selected");else if(t instanceof HTMLTextAreaElement&&e instanceof HTMLTextAreaElement){let r=t.value,o=e.value;r!==o&&(e.value=r),e.firstChild&&e.firstChild.nodeValue!==r&&(e.firstChild.nodeValue=r)}}function de(t,e,n){let r=t.getAttribute(n),o=e.getAttribute(n);r!==o&&(r?e.setAttribute(n,r):e.removeAttribute(n))}function en(t,e,n){let r=[],o=[],i=[],s=[],a=n.head.style,u=new Map;for(let c of t.children)u.set(c.outerHTML,c);for(let c of e.children){let l=u.has(c.outerHTML),g=n.head.shouldReAppend(c),p=n.head.shouldPreserve(c);l||p?g?o.push(c):(u.delete(c.outerHTML),i.push(c)):a===I.Append?g&&(o.push(c),s.push(c)):n.head.shouldRemove(c)!==!1&&o.push(c)}s.push(...u.values());let m=[];for(let c of s){let l=document.createRange().createContextualFragment(c.outerHTML).firstChild;if(!l)throw d;if(n.callbacks.beforeNodeAdded(l)){if(l.hasAttribute("href")||l.hasAttribute("src")){let g,p=new Promise(A=>{g=A});l.addEventListener("load",function(){g(void 0)}),m.push(p)}e.appendChild(l),n.callbacks.afterNodeAdded(l),r.push(l)}}for(let c of o)n.callbacks.beforeNodeRemoved(c)!==!1&&(e.removeChild(c),n.callbacks.afterNodeRemoved(c));return n.head.afterHeadMorphed(e,{added:r,kept:i,removed:o}),m}function H(){}function Mn(t,e,n){return{target:t,newContent:e,config:n,morphStyle:n.morphStyle,ignoreActive:n.ignoreActive,idMap:Vn(t,e),deadIds:new Set,callbacks:Object.assign({beforeNodeAdded:H,afterNodeAdded:H,beforeNodeMorphed:H,afterNodeMorphed:H,beforeNodeRemoved:H,afterNodeRemoved:H},n.callbacks),head:Object.assign({style:"merge",shouldPreserve:r=>r.getAttribute("im-preserve")==="true",shouldReAppend:r=>r.getAttribute("im-re-append")==="true",shouldRemove:H,afterHeadMorphed:H},n.head)}}function tn(t,e,n){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName?t?.id?.length&&t.id===e.id?!0:te(n,t,e)>0:!1}function Ee(t,e){return!t||!e?!1:t.nodeType===e.nodeType&&t.tagName===e.tagName}function Jt(t,e,n){for(;t!==e;){let r=t;if(t=t?.nextSibling,!r)throw d;nn(r,n)}return B(n,e),e.nextSibling}function Ln(t,e,n,r,o){let i=te(o,n,e),s=null;if(i>0){s=r;let a=0;for(;s!=null;){if(tn(n,s,o))return s;if(a+=te(o,s,t),a>i)return null;s=s.nextSibling}}return s}function Nn(t,e,n,r){let o=n,i=e.nextSibling,s=0;for(;o&&i;){if(te(r,o,t)>0)return null;if(Ee(e,o))return o;if(Ee(i,o)&&(s++,i=i.nextSibling,s>=2))return null;o=o.nextSibling}return o}var zt=new DOMParser;function Dn(t){let e=t.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(e.match(/<\/html>/)||e.match(/<\/head>/)||e.match(/<\/body>/)){let n=zt.parseFromString(t,"text/html");if(e.match(/<\/html>/))return ge.add(n),n;{let r=n.firstChild;return r?(ge.add(r),r):null}}else{let r=zt.parseFromString(`<body><template>${t}</template></body>`,"text/html").body.querySelector("template")?.content;if(!r)throw V;return ge.add(r),r}}function In(t){if(t==null)return document.createElement("div");if(ge.has(t))return t;if(t instanceof Node){let e=document.createElement("div");return e.append(t),e}else{let e=document.createElement("div");for(let n of[...t])e.append(n);return e}}function kn(t,e,n){let r=[],o=[];for(;t;)r.push(t),t=t.previousSibling;for(;r.length>0;){let i=r.pop();o.push(i),e?.parentElement?.insertBefore(i,e)}for(o.push(e);n;)r.push(n),o.push(n),n=n.nextSibling;for(;r.length;)e?.parentElement?.insertBefore(r.pop(),e.nextSibling);return o}function Cn(t,e,n){let r=t.firstChild,o=r,i=0;for(;r;){let s=On(r,e,n);s>i&&(o=r,i=s),r=r.nextSibling}return o}function On(t,e,n){return Ee(t,e)?.5+te(n,t,e):0}function nn(t,e){B(e,t),e.callbacks.beforeNodeRemoved(t)!==!1&&(t.remove(),e.callbacks.afterNodeRemoved(t))}function Fn(t,e){return!t.deadIds.has(e)}function Hn(t,e,n){return t.idMap.get(n)?.has(e)||!1}function B(t,e){let n=t.idMap.get(e);if(n)for(let r of n)t.deadIds.add(r)}function te(t,e,n){let r=t.idMap.get(e);if(!r)return 0;let o=0;for(let i of r)Fn(t,i)&&Hn(t,i,n)&&++o;return o}function Yt(t,e){let n=t.parentElement,r=t.querySelectorAll("[id]");for(let o of r){let i=o;for(;i!==n&&i;){let s=e.get(i);s==null&&(s=new Set,e.set(i,s)),s.add(o.id),i=i.parentElement}}}function Vn(t,e){let n=new Map;return Yt(t,n),Yt(e,n),n}var on={pluginType:2,name:N.MergeFragments,onGlobalInit:async t=>{let e=document.createElement("template");k(N.MergeFragments,({fragments:n="<div></div>",selector:r="",mergeMode:o=Be,settleDuration:i=`${300}`,useViewTransition:s=`${!1}`})=>{let a=parseInt(i),u=F(s);e.innerHTML=n.trim(),[...e.content.children].forEach(c=>{if(!(c instanceof Element))throw d;let l=r||`#${c.getAttribute("id")}`,p=[...document.querySelectorAll(l)||[]];if(!p.length)throw d;q&&u?ee.startViewTransition(()=>rn(t,o,a,c,p)):rn(t,o,a,c,p)})})}};function rn(t,e,n,r,o){for(let i of o){i.classList.add($);let s=i.outerHTML,a=i;switch(e){case I.Morph:let m=Zt(a,r,{callbacks:{beforeNodeRemoved:(c,l)=>(t.cleanup(c),!0)}});if(!m?.length)throw d;a=m[0];break;case I.Inner:a.innerHTML=r.innerHTML;break;case I.Outer:a.replaceWith(r);break;case I.Prepend:a.prepend(r);break;case I.Append:a.append(r);break;case I.Before:a.before(r);break;case I.After:a.after(r);break;case I.UpsertAttributes:r.getAttributeNames().forEach(c=>{let l=r.getAttribute(c);a.setAttribute(c,l)});break;default:throw d}t.cleanup(a),a.classList.add($),t.applyPlugins(document.body),setTimeout(()=>{i.classList.remove($),a.classList.remove($)},n);let u=a.outerHTML;s!==u&&(a.classList.add(we),setTimeout(()=>{a.classList.remove(we)},n))}}var sn={pluginType:2,name:N.MergeSignals,onGlobalInit:async t=>{k(N.MergeSignals,({signals:e="{}",onlyIfMissing:n=`${!1}`})=>{let r=F(n),o=` return Object.assign({...ctx.signals}, ${e})`;try{let s=new Function("ctx",o)(t),a=ae(t.signals,s,r);t.mergeSignals(a),t.applyPlugins(document.body)}catch(i){console.log(o),console.error(i);debugger}})}};var an={pluginType:2,name:N.RemoveFragments,onGlobalInit:async()=>{k(N.RemoveFragments,({selector:t,settleDuration:e=`${300}`,useViewTransition:n=`${!1}`})=>{if(!t.length)throw d;let r=parseInt(e),o=F(n),i=document.querySelectorAll(t),s=()=>{for(let a of i)a.classList.add($);setTimeout(()=>{for(let a of i)a.remove()},r)};q&&o?ee.startViewTransition(()=>s()):s()})}};var ln={pluginType:2,name:N.RemoveSignals,onGlobalInit:async t=>{k(N.RemoveSignals,({paths:e=""})=>{let n=e.split(`
`).map(r=>r.trim());if(!n?.length)throw d;t.removeSignals(...n)})}};var $n=Symbol.for("preact-signals"),O=1,J=2,re=4,Y=8,ve=16,z=32;function ke(){Se++}function Ce(){if(Se>1){Se--;return}let t,e=!1;for(;ne!==void 0;){let n=ne;for(ne=void 0,Ie++;n!==void 0;){let r=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~J,!(n._flags&Y)&&fn(n))try{n._callback()}catch(o){e||(t=o,e=!0)}n=r}}if(Ie=0,Se--,e)throw t}var y;var ne,Se=0,Ie=0,be=0;function un(t){if(y===void 0)return;let e=t._node;if(e===void 0||e._target!==y)return e={_version:0,_source:t,_prevSource:y._sources,_nextSource:void 0,_target:y,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},y._sources!==void 0&&(y._sources._nextSource=e),y._sources=e,t._node=e,y._flags&z&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=y._sources,e._nextSource=void 0,y._sources._nextSource=e,y._sources=e),e}function P(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}P.prototype.brand=$n;P.prototype._refresh=function(){return!0};P.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};P.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};P.prototype.subscribe=function(t){return Fe(()=>{let e=this.value,n=y;y=void 0;try{t(e)}finally{y=n}})};P.prototype.valueOf=function(){return this.value};P.prototype.toString=function(){return this.value+""};P.prototype.toJSON=function(){return this.value};P.prototype.peek=function(){let t=y;y=void 0;try{return this.value}finally{y=t}};Object.defineProperty(P.prototype,"value",{get(){let t=un(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(Ie>100)throw d;this._value=t,this._version++,be++,ke();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{Ce()}}}});function cn(t){return new P(t)}function fn(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function mn(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function pn(t){let e=t._sources,n;for(;e!==void 0;){let r=e._prevSource;e._version===-1?(e._source._unsubscribe(e),r!==void 0&&(r._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=r)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=r}t._sources=n}function U(t){P.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=be-1,this._flags=re}U.prototype=new P;U.prototype._refresh=function(){if(this._flags&=~J,this._flags&O)return!1;if((this._flags&(re|z))===z||(this._flags&=~re,this._globalVersion===be))return!0;if(this._globalVersion=be,this._flags|=O,this._version>0&&!fn(this))return this._flags&=~O,!0;let t=y;try{mn(this),y=this;let e=this._fn();(this._flags&ve||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~ve,this._version++)}catch(e){this._value=e,this._flags|=ve,this._version++}return y=t,pn(this),this._flags&=~O,!0};U.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=re|z;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}P.prototype._subscribe.call(this,t)};U.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(P.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~z;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};U.prototype._notify=function(){if(!(this._flags&J)){this._flags|=re|J;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(U.prototype,"value",{get(){if(this._flags&O)throw d;let t=un(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&ve)throw this._value;return this._value}});function dn(t){return new U(t)}function gn(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){ke();let n=y;y=void 0;try{e()}catch(r){throw t._flags&=~O,t._flags|=Y,Oe(t),r}finally{y=n,Ce()}}}function Oe(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,gn(t)}function Bn(t){if(y!==this)throw d;pn(this),y=t,this._flags&=~O,this._flags&Y&&Oe(this),Ce()}function oe(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=z}oe.prototype._callback=function(){let t=this._start();try{if(this._flags&Y||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};oe.prototype._start=function(){if(this._flags&O)throw d;this._flags|=O,this._flags&=~Y,gn(this),mn(this),ke();let t=y;return y=this,Bn.bind(this,t)};oe.prototype._notify=function(){this._flags&J||(this._flags|=J,this._nextBatchedEffect=ne,ne=this)};oe.prototype._dispose=function(){this._flags|=Y,this._flags&O||Oe(this)};function Fe(t){let e=new oe(t);try{e._callback()}catch(n){throw e._dispose(),n}return e._dispose.bind(e)}var hn="0.20.1";var Un=t=>t.pluginType===0,Gn=t=>t.pluginType===2,jn=t=>t.pluginType===1,Kn=t=>t.pluginType===3,ye=class{constructor(){this.plugins=[];this._signals={};this.macros=new Array;this.actions={};this.watchers=new Array;this.refs={};this.reactivity={signal:cn,computed:dn,effect:Fe};this.removals=new Map;this.mergeRemovals=new Array;this.lastMarshalledSignals=""}get version(){return hn}load(...e){let n=new Set(this.plugins);e.forEach(r=>{if(r.requiredPlugins){for(let i of r.requiredPlugins)if(!n.has(i))throw j}let o;if(Un(r)){if(this.macros.includes(r))throw X;this.macros.push(r)}else if(Gn(r)){if(this.watchers.includes(r))throw X;this.watchers.push(r),o=r.onGlobalInit}else if(Kn(r)){if(this.actions[r.name])throw X;this.actions[r.name]=r}else if(jn(r)){if(this.plugins.includes(r))throw X;this.plugins.push(r),o=r.onGlobalInit}else throw V;if(o){let{_signals:i}=this;o({get signals(){return i},upsertSignal:this.upsertSignal.bind(this),mergeSignals:this.mergeSignals.bind(this),removeSignals:this.removeSignals.bind(this),actions:this.actions,reactivity:this.reactivity,applyPlugins:this.applyPlugins.bind(this),cleanup:this.cleanup.bind(this)})}n.add(r)}),this.applyPlugins(document.body)}cleanup(e){let n=this.removals.get(e);if(n){for(let r of n.set)r();this.removals.delete(e)}}mergeSignals(e){this.mergeRemovals.forEach(o=>o()),this.mergeRemovals=this.mergeRemovals.slice(0);let n=(o,i="")=>{let s=[];return Object.entries(o).forEach(([a,u])=>{a=i?`${i}.${a}`:a,typeof u=="object"?s.push(...n(u,a)):s.push({k:a,v:u})}),s};n(e).forEach(({k:o,v:i})=>this.upsertSignal(o,i));let r=JSON.stringify(this._signals);r!==this.lastMarshalledSignals&&(this.lastMarshalledSignals=r)}removeSignals(...e){let n={...this._signals},r=!1;for(let o of e){let i=o.split("."),s=i[0],a=n;for(let u=1;u<i.length;u++)s=i[u],a=a[s];delete a[s],r=!0}r&&(this._signals=n,this.applyPlugins(document.body))}upsertSignal(e,n){let r=e.split("."),o=this._signals;for(let u=0;u<r.length-1;u++){let m=r[u];o[m]||(o[m]={}),o=o[m]}let i=r[r.length-1],s=o[i];if(s)return s;let a=this.reactivity.signal(n);return o[i]=a,a}applyPlugins(e){let n=new Set;this.plugins.forEach((r,o)=>{this.walkDownDOM(e,i=>{o||this.cleanup(i);for(let s in i.dataset){let a=`${i.dataset[s]}`||"",u=a;if(!s.startsWith(r.name))continue;if(i.id.length||(i.id=Pt(i)),n.clear(),r.allowedTagRegexps){let f=i.tagName.toLowerCase();if(![...r.allowedTagRegexps].some(E=>f.match(E)))throw j}let m=s.slice(r.name.length),[c,...l]=m.split(".");if(r.mustHaveEmptyKey&&c.length>0)throw d;if(r.mustNotEmptyKey&&c.length===0)throw d;c.length&&(c=c[0].toLowerCase()+c.slice(1));let g=l.map(f=>{let[T,...E]=f.split("_");return{label:T,args:E}});if(r.allowedModifiers){for(let f of g)if(!r.allowedModifiers.has(f.label))throw j}let p=new Map;for(let f of g)p.set(f.label,f.args);if(r.mustHaveEmptyExpression&&u.length)throw d;if(r.mustNotEmptyExpression&&!u.length)throw d;let A=/;|\n/;r.removeNewLines&&(u=u.split(`
`).map(f=>f.trim()).join(" "));let v=[...r.macros?.pre||[],...this.macros,...r.macros?.post||[]];for(let f of v){if(n.has(f))continue;n.add(f);let T=u.split(A),E=[];T.forEach(x=>{let _=x,M=[..._.matchAll(f.regexp)];if(M.length)for(let D of M){if(!D.groups)continue;let{groups:R}=D,{whole:ie}=R;_=_.replace(ie,f.replacer(R))}E.push(_)}),u=E.join("; ")}let{_signals:b}=this,S={get signals(){return b},mergeSignals:this.mergeSignals.bind(this),upsertSignal:this.upsertSignal.bind(this),removeSignals:this.removeSignals.bind(this),applyPlugins:this.applyPlugins.bind(this),cleanup:this.cleanup.bind(this),walkSignals:this.walkMySignals.bind(this),actions:this.actions,reactivity:this.reactivity,el:i,rawKey:s,key:c,rawExpression:a,expression:u,expressionFn:()=>{throw se},modifiers:p};if(!r.bypassExpressionFunctionCreation?.(S)&&!r.mustHaveEmptyExpression&&u.length){let f=u.split(A).map(x=>x.trim()).filter(x=>x.length);f[f.length-1]=`return ${f[f.length-1]}`;let T=f.map(x=>`  ${x}`).join(`;
`),E=`try{${T}}catch(e){console.error(\`Error evaluating Datastar expression:
${T.replaceAll("`","\\`")}

Error: \${e.message}

Check if the expression is valid before raising an issue.\`.trim());debugger}`;try{let x=r.argumentNames||[],_=new Function("ctx",...x,E);S.expressionFn=_}catch(x){let _=new Error(`${x}
with
${E}`);console.error(_);debugger}}let w=r.onLoad(S);w&&(this.removals.has(i)||this.removals.set(i,{id:i.id,set:new Set}),this.removals.get(i).set.add(w))}})})}walkSignals(e,n){let r=Object.keys(e);for(let o=0;o<r.length;o++){let i=r[o],s=e[i],a=s instanceof P,u=typeof s=="object"&&Object.keys(s).length>0;if(a){n(i,s);continue}u&&this.walkSignals(s,n)}}walkMySignals(e){this.walkSignals(this._signals,e)}walkDownDOM(e,n,r=0){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;for(n(e),r=0,e=e.firstElementChild;e;)this.walkDownDOM(e,n,r++),e=e.nextElementSibling}};var En=new ye;En.load(mt,jt,Kt,ft,ct);var vn=En;vn.load(dt,bt,ut,Tt,gt,St,yt,At,Rt,$t,Bt,Ut,Ze,Xe,Qe,et,tt,nt,rt,ot,it,st,at,lt,on,sn,an,ln,qt);})();
//# sourceMappingURL=datastar.js.map
