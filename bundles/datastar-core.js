"use strict";(()=>{var te={pluginType:"attribute",name:"computed",mustNotEmptyKey:!0,onLoad:t=>{let e=t.signals();return e[t.key]=t.reactivity.computed(()=>t.expressionFn(t)),()=>{let s=t.signals();delete s[t.key]}}};function ne(t,e,s){let n={};if(!s)Object.assign(n,e);else for(let r in e){let i=t[r]?.value;i==null&&(n[r]=e[r])}return n}var se={pluginType:"attribute",name:"mergeSignals",removeNewLines:!0,preprocessors:{pre:[{pluginType:"preprocessor",name:"store",regexp:/(?<whole>.+)/g,replacer:t=>{let{whole:e}=t;return`Object.assign({...ctx.signals()}, ${e})`}}]},allowedModifiers:new Set(["ifmissing"]),onLoad:t=>{let e=t.expressionFn(t),s=ne(t.signals(),e,t.modifiers.has("ifmissing"));t.mergeSignals(s),delete t.el.dataset[t.rawKey]}};var re={pluginType:"attribute",name:"star",onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var ie="[a-zA-Z_$]+",Te=ie+"[0-9a-zA-Z_$.]*";function L(t,e,s,n=!0){let r=n?Te:ie;return new RegExp(`(?<whole>${t}(?<${e}>${r})${s})`,"g")}var oe={name:"action",pluginType:"preprocessor",regexp:L("\\$","action","(?<call>\\((?<args>.*)\\))",!1),replacer:({action:t,args:e})=>{let s=["ctx"];e&&s.push(...e.split(",").map(r=>r.trim()));let n=s.join(",");return`ctx.actions.${t}.method(${n})`}};var ae={name:"signal",pluginType:"preprocessor",regexp:L("\\$","signal","(?<method>\\([^\\)]*\\))?"),replacer:t=>{let{signal:e,method:s}=t,n="ctx.signals()";if(!s?.length)return`${n}.${e}.value`;let r=e.split("."),i=r.pop(),a=r.join(".");return`${n}.${a}.value.${i}${s}`}};var k="datastar";var Ee={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Be=Ee.Morph;var v=t=>{let e=new Error;return e.name=`${k}${t}`,e},d=v(400),w=v(409),B=v(404),x=v(403),le=v(405),Je=v(503);function ue(t){if(t.id)return t.id;let e=0,s=r=>(e=(e<<5)-e+r,e&e),n=r=>r.split("").forEach(i=>s(i.charCodeAt(0)));for(;t.parentNode;){if(t.id){n(`${t.id}`);break}else if(t===t.ownerDocument.documentElement)n(t.tagName);else{for(let r=1,i=t;i.previousElementSibling;i=i.previousElementSibling,r++)s(r);t=t.parentNode}t=t.parentNode}return k+e}var Re=Symbol.for("preact-signals"),g=1,T=2,O=4,R=8,C=16,E=32;function j(){D++}function I(){if(D>1){D--;return}let t,e=!1;for(;A!==void 0;){let s=A;for(A=void 0,G++;s!==void 0;){let n=s._nextBatchedEffect;if(s._nextBatchedEffect=void 0,s._flags&=~T,!(s._flags&R)&&pe(s))try{s._callback()}catch(r){e||(t=r,e=!0)}s=n}}if(G=0,D--,e)throw t}function ce(t){if(D>0)return t();j();try{return t()}finally{I()}}var o;var A,D=0,G=0,M=0;function fe(t){if(o===void 0)return;let e=t._node;if(e===void 0||e._target!==o)return e={_version:0,_source:t,_prevSource:o._sources,_nextSource:void 0,_target:o,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},o._sources!==void 0&&(o._sources._nextSource=e),o._sources=e,t._node=e,o._flags&E&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=o._sources,e._nextSource=void 0,o._sources._nextSource=e,o._sources=e),e}function c(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}c.prototype.brand=Re;c.prototype._refresh=function(){return!0};c.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};c.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,s=t._nextTarget;e!==void 0&&(e._nextTarget=s,t._prevTarget=void 0),s!==void 0&&(s._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=s)}};c.prototype.subscribe=function(t){return J(()=>{let e=this.value,s=o;o=void 0;try{t(e)}finally{o=s}})};c.prototype.valueOf=function(){return this.value};c.prototype.toString=function(){return this.value+""};c.prototype.toJSON=function(){return this.value};c.prototype.peek=function(){let t=o;o=void 0;try{return this.value}finally{o=t}};Object.defineProperty(c.prototype,"value",{get(){let t=fe(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(G>100)throw d;this._value=t,this._version++,M++,j();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{I()}}}});function F(t){return new c(t)}function pe(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function de(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let s=e._source._node;if(s!==void 0&&(e._rollbackNode=s),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ge(t){let e=t._sources,s;for(;e!==void 0;){let n=e._prevSource;e._version===-1?(e._source._unsubscribe(e),n!==void 0&&(n._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=n)):s=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=n}t._sources=s}function b(t){c.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=M-1,this._flags=O}b.prototype=new c;b.prototype._refresh=function(){if(this._flags&=~T,this._flags&g)return!1;if((this._flags&(O|E))===E||(this._flags&=~O,this._globalVersion===M))return!0;if(this._globalVersion=M,this._flags|=g,this._version>0&&!pe(this))return this._flags&=~g,!0;let t=o;try{de(this),o=this;let e=this._fn();(this._flags&C||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~C,this._version++)}catch(e){this._value=e,this._flags|=C,this._version++}return o=t,ge(this),this._flags&=~g,!0};b.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=O|E;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}c.prototype._subscribe.call(this,t)};b.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(c.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~E;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};b.prototype._notify=function(){if(!(this._flags&T)){this._flags|=O|T;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(b.prototype,"value",{get(){if(this._flags&g)throw d;let t=fe(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&C)throw this._value;return this._value}});function he(t){return new b(t)}function _e(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){j();let s=o;o=void 0;try{e()}catch(n){throw t._flags&=~g,t._flags|=R,H(t),n}finally{o=s,I()}}}function H(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,_e(t)}function we(t){if(o!==this)throw d;ge(this),o=t,this._flags&=~g,this._flags&R&&H(this),I()}function P(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=E}P.prototype._callback=function(){let t=this._start();try{if(this._flags&R||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};P.prototype._start=function(){if(this._flags&g)throw d;this._flags|=g,this._flags&=~R,_e(this),de(this),j();let t=o;return o=this,we.bind(this,t)};P.prototype._notify=function(){this._flags&T||(this._flags|=T,this._nextBatchedEffect=A,A=this)};P.prototype._dispose=function(){this._flags|=R,this._flags&g||H(this)};function J(t){let e=new P(t);try{e._callback()}catch(s){throw e._dispose(),s}return e._dispose.bind(e)}var $=class{get value(){return K(this)}set value(e){ce(()=>Ae(this,e))}peek(){return K(this,{peek:!0})}},N=t=>Object.assign(new $,Object.entries(t).reduce((e,[s,n])=>{if(["value","peek"].some(r=>r===s))throw x;return typeof n!="object"||n===null||Array.isArray(n)?e[s]=F(n):e[s]=N(n),e},{})),Ae=(t,e)=>Object.keys(e).forEach(s=>t[s].value=e[s]),K=(t,{peek:e=!1}={})=>Object.entries(t).reduce((s,[n,r])=>(r instanceof c?s[n]=e?r.peek():r.value:r instanceof $&&(s[n]=K(r,{peek:e})),s),{});function W(t,e){if(typeof e!="object"||Array.isArray(e)||!e)return JSON.parse(JSON.stringify(e));if(typeof e=="object"&&e.toJSON!==void 0&&typeof e.toJSON=="function")return e.toJSON();let s=t;return typeof t!="object"&&(s={...e}),Object.keys(e).forEach(n=>{s.hasOwnProperty(n)||(s[n]=e[n]),e[n]===null?delete s[n]:s[n]=W(s[n],e[n])}),s}var me="0.20.1";var De=t=>t.pluginType==="preprocessor",Oe=t=>t.pluginType==="watcher",Pe=t=>t.pluginType==="attribute",Ne=t=>t.pluginType==="action",V=class{constructor(){this.plugins=[];this.signals=N({});this.preprocessors=new Array;this.actions={};this.watchers=new Array;this.refs={};this.reactivity={signal:F,computed:he,effect:J};this.removals=new Map;this.mergeRemovals=new Array;this.lastMarshalledSignals=""}get version(){return me}load(...e){let s=new Set(this.plugins);e.forEach(n=>{if(n.requiredPlugins){for(let i of n.requiredPlugins)if(!s.has(i))throw x}let r;if(De(n)){if(this.preprocessors.includes(n))throw w;this.preprocessors.push(n)}else if(Oe(n)){if(this.watchers.includes(n))throw w;this.watchers.push(n),r=n.onGlobalInit}else if(Ne(n)){if(this.actions[n.name])throw w;this.actions[n.name]=n}else if(Pe(n)){if(this.plugins.includes(n))throw w;this.plugins.push(n),r=n.onGlobalInit}else throw B;r&&r({signals:()=>this.signals,upsertSignal:this.upsertSignal.bind(this),mergeSignals:this.mergeSignals.bind(this),removeSignals:this.removeSignals.bind(this),actions:this.actions,reactivity:this.reactivity,applyPlugins:this.applyPlugins.bind(this),cleanup:this.cleanup.bind(this)}),s.add(n)}),this.applyPlugins(document.body)}cleanup(e){let s=this.removals.get(e);if(s){for(let n of s.set)n();this.removals.delete(e)}}mergeSignals(e){this.mergeRemovals.forEach(r=>r()),this.mergeRemovals=this.mergeRemovals.slice(0);let s=W(this.signals.value,e);this.signals=N(s),JSON.stringify(this.signals.value),this.lastMarshalledSignals}removeSignals(...e){let s={...this.signals.value},n=!1;for(let r of e){let i=r.split("."),a=i[0],f=s;for(let u=1;u<i.length;u++){let m=i[u];f[a]||(f[a]={}),f=f[a],a=m}delete f[a],n=!0}n&&(this.signals=N(s),this.applyPlugins(document.body))}upsertSignal(e,s){let n=e.split("."),r=this.signals;for(let u=0;u<n.length-1;u++){let m=n[u];r[m]||(r[m]={}),r=r[m]}let i=n[n.length-1],a=r[i];if(a)return a;let f=this.reactivity.signal(s);return r[i]=f,f}applyPlugins(e){let s=new Set;this.plugins.forEach((n,r)=>{this.walkDownDOM(e,i=>{r||this.cleanup(i);for(let a in i.dataset){let f=`${i.dataset[a]}`||"",u=f;if(!a.startsWith(n.name))continue;if(i.id.length||(i.id=ue(i)),s.clear(),n.allowedTagRegexps){let l=i.tagName.toLowerCase();if(![...n.allowedTagRegexps].some(h=>l.match(h)))throw x}let m=a.slice(n.name.length),[y,...be]=m.split(".");if(n.mustHaveEmptyKey&&y.length>0)throw d;if(n.mustNotEmptyKey&&y.length===0)throw d;y.length&&(y=y[0].toLowerCase()+y.slice(1));let Y=be.map(l=>{let[S,...h]=l.split("_");return{label:S,args:h}});if(n.allowedModifiers){for(let l of Y)if(!n.allowedModifiers.has(l.label))throw x}let q=new Map;for(let l of Y)q.set(l.label,l.args);if(n.mustHaveEmptyExpression&&u.length)throw d;if(n.mustNotEmptyExpression&&!u.length)throw d;let z=/;|\n/;n.removeNewLines&&(u=u.split(`
`).map(l=>l.trim()).join(" "));let ve=[...n.preprocessors?.pre||[],...this.preprocessors,...n.preprocessors?.post||[]];for(let l of ve){if(s.has(l))continue;s.add(l);let S=u.split(z),h=[];S.forEach(p=>{let _=p,Z=[..._.matchAll(l.regexp)];if(Z.length)for(let Q of Z){if(!Q.groups)continue;let{groups:ee}=Q,{whole:xe}=ee;_=_.replace(xe,l.replacer(ee))}h.push(_)}),u=h.join("; ")}let U={signals:()=>this.signals,mergeSignals:this.mergeSignals.bind(this),upsertSignal:this.upsertSignal.bind(this),removeSignals:this.removeSignals.bind(this),applyPlugins:this.applyPlugins.bind(this),cleanup:this.cleanup.bind(this),walkSignals:this.walkMySignals.bind(this),actions:this.actions,reactivity:this.reactivity,el:i,rawKey:a,key:y,rawExpression:f,expression:u,expressionFn:()=>{throw le},modifiers:q};if(!n.bypassExpressionFunctionCreation?.(U)&&!n.mustHaveEmptyExpression&&u.length){let l=u.split(z).map(p=>p.trim()).filter(p=>p.length);l[l.length-1]=`return ${l[l.length-1]}`;let S=l.map(p=>`  ${p}`).join(`;
`),h=`try{${S}}catch(e){console.error(\`Error evaluating Datastar expression:
${S.replaceAll("`","\\`")}

Error: \${e.message}

Check if the expression is valid before raising an issue.\`.trim());debugger}`;try{let p=n.argumentNames||[],_=new Function("ctx",...p,h);U.expressionFn=_}catch(p){let _=new Error(`${p}
with
${h}`);console.error(_);debugger}}let X=n.onLoad(U);X&&(this.removals.has(i)||this.removals.set(i,{id:i.id,set:new Set}),this.removals.get(i).set.add(X))}})})}walkSignals(e,s){let n=Object.keys(e);for(let r=0;r<n.length;r++){let i=n[r],a=e[i],f=a instanceof c,u=typeof a=="object"&&Object.keys(a).length>0;if(f){s(i,a);continue}u&&this.walkSignals(a,s)}}walkMySignals(e){this.walkSignals(this.signals,e)}walkDownDOM(e,s,n=0){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;for(s(e),n=0,e=e.firstElementChild;e;)this.walkDownDOM(e,s,n++),e=e.nextElementSibling}};var ye=new V;ye.load(re,oe,ae,se,te);var Se=ye;Se.load();})();
//# sourceMappingURL=datastar-core.js.map
