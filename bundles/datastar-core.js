"use strict";(()=>{var te={pluginType:"attribute",name:"computed",mustNotEmptyKey:!0,onLoad:t=>{let e=t.store();return e[t.key]=t.reactivity.computed(()=>t.expressionFn(t)),()=>{let r=t.store();delete r[t.key]}}};var ne={pluginType:"attribute",name:"star",onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};function re(t,e,r){let n={};if(!r)Object.assign(n,e);else for(let s in e){let o=t[s]?.value;o==null&&(n[s]=e[s])}return n}var se={pluginType:"attribute",name:"store",removeNewLines:!0,preprocessors:{pre:[{pluginType:"preprocessor",name:"store",regexp:/(?<whole>.+)/g,replacer:t=>{let{whole:e}=t;return`Object.assign({...ctx.store()}, ${e})`}}]},allowedModifiers:new Set(["ifmissing"]),onLoad:t=>{let e=t.expressionFn(t),r=re(t.store(),e,t.modifiers.has("ifmissing"));t.mergeSignals(r),delete t.el.dataset[t.rawKey]}};var w="datastar";var Te={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},Ie=Te.Morph;var oe="[a-zA-Z_$]+",Ee=oe+"[0-9a-zA-Z_$.]*";function k(t,e,r,n=!0){let s=n?Ee:oe;return new RegExp(`(?<whole>${t}(?<${e}>${s})${r})`,"g")}var ie={name:"action",pluginType:"preprocessor",regexp:k("\\$","action","(?<call>\\((?<args>.*)\\))",!1),replacer:({action:t,args:e})=>{let r=["ctx"];e&&r.push(...e.split(",").map(s=>s.trim()));let n=r.join(",");return`ctx.actions.${t}.method(${n})`}};var ae={name:"signal",pluginType:"preprocessor",regexp:k("\\$","signal","(?<method>\\([^\\)]*\\))?"),replacer:t=>{let{signal:e,method:r}=t,n="ctx.store()";if(!r?.length)return`${n}.${e}.value`;let s=e.split("."),o=s.pop(),a=s.join(".");return`${n}.${a}.value.${o}${r}`}};var v=t=>{let e=new Error;return e.name=`${w}${t}`,e},d=v(400),D=v(409),B=v(404),x=v(403),ue=v(405),Je=v(503);function le(t){return t instanceof HTMLElement||t instanceof SVGElement?t:null}var Re=Symbol.for("preact-signals"),g=1,T=2,P=4,R=8,C=16,E=32;function M(){O++}function I(){if(O>1){O--;return}let t,e=!1;for(;A!==void 0;){let r=A;for(A=void 0,U++;r!==void 0;){let n=r._nextBatchedEffect;if(r._nextBatchedEffect=void 0,r._flags&=~T,!(r._flags&R)&&pe(r))try{r._callback()}catch(s){e||(t=s,e=!0)}r=n}}if(U=0,O--,e)throw t}function ce(t){if(O>0)return t();M();try{return t()}finally{I()}}var i;var A,O=0,U=0,j=0;function fe(t){if(i===void 0)return;let e=t._node;if(e===void 0||e._target!==i)return e={_version:0,_source:t,_prevSource:i._sources,_nextSource:void 0,_target:i,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},i._sources!==void 0&&(i._sources._nextSource=e),i._sources=e,t._node=e,i._flags&E&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=i._sources,e._nextSource=void 0,i._sources._nextSource=e,i._sources=e),e}function c(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}c.prototype.brand=Re;c.prototype._refresh=function(){return!0};c.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};c.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,r=t._nextTarget;e!==void 0&&(e._nextTarget=r,t._prevTarget=void 0),r!==void 0&&(r._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=r)}};c.prototype.subscribe=function(t){return W(()=>{let e=this.value,r=i;i=void 0;try{t(e)}finally{i=r}})};c.prototype.valueOf=function(){return this.value};c.prototype.toString=function(){return this.value+""};c.prototype.toJSON=function(){return this.value};c.prototype.peek=function(){let t=i;i=void 0;try{return this.value}finally{i=t}};Object.defineProperty(c.prototype,"value",{get(){let t=fe(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(U>100)throw d;this._value=t,this._version++,j++,M();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{I()}}}});function V(t){return new c(t)}function pe(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function de(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let r=e._source._node;if(r!==void 0&&(e._rollbackNode=r),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ge(t){let e=t._sources,r;for(;e!==void 0;){let n=e._prevSource;e._version===-1?(e._source._unsubscribe(e),n!==void 0&&(n._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=n)):r=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=n}t._sources=r}function b(t){c.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=j-1,this._flags=P}b.prototype=new c;b.prototype._refresh=function(){if(this._flags&=~T,this._flags&g)return!1;if((this._flags&(P|E))===E||(this._flags&=~P,this._globalVersion===j))return!0;if(this._globalVersion=j,this._flags|=g,this._version>0&&!pe(this))return this._flags&=~g,!0;let t=i;try{de(this),i=this;let e=this._fn();(this._flags&C||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~C,this._version++)}catch(e){this._value=e,this._flags|=C,this._version++}return i=t,ge(this),this._flags&=~g,!0};b.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=P|E;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}c.prototype._subscribe.call(this,t)};b.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(c.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~E;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};b.prototype._notify=function(){if(!(this._flags&T)){this._flags|=P|T;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(b.prototype,"value",{get(){if(this._flags&g)throw d;let t=fe(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&C)throw this._value;return this._value}});function he(t){return new b(t)}function _e(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){M();let r=i;i=void 0;try{e()}catch(n){throw t._flags&=~g,t._flags|=R,H(t),n}finally{i=r,I()}}}function H(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,_e(t)}function we(t){if(i!==this)throw d;ge(this),i=t,this._flags&=~g,this._flags&R&&H(this),I()}function N(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=E}N.prototype._callback=function(){let t=this._start();try{if(this._flags&R||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};N.prototype._start=function(){if(this._flags&g)throw d;this._flags|=g,this._flags&=~R,_e(this),de(this),M();let t=i;return i=this,we.bind(this,t)};N.prototype._notify=function(){this._flags&T||(this._flags|=T,this._nextBatchedEffect=A,A=this)};N.prototype._dispose=function(){this._flags|=R,this._flags&g||H(this)};function W(t){let e=new N(t);try{e._callback()}catch(r){throw e._dispose(),r}return e._dispose.bind(e)}var $=class{get value(){return J(this)}set value(e){ce(()=>De(this,e))}peek(){return J(this,{peek:!0})}},L=t=>Object.assign(new $,Object.entries(t).reduce((e,[r,n])=>{if(["value","peek"].some(s=>s===r))throw x;return typeof n!="object"||n===null||Array.isArray(n)?e[r]=V(n):e[r]=L(n),e},{})),De=(t,e)=>Object.keys(e).forEach(r=>t[r].value=e[r]),J=(t,{peek:e=!1}={})=>Object.entries(t).reduce((r,[n,s])=>(s instanceof c?r[n]=e?s.peek():s.value:s instanceof $&&(r[n]=J(s,{peek:e})),r),{});function K(t,e){if(typeof e!="object"||Array.isArray(e)||!e)return JSON.parse(JSON.stringify(e));if(typeof e=="object"&&e.toJSON!==void 0&&typeof e.toJSON=="function")return e.toJSON();let r=t;return typeof t!="object"&&(r={...e}),Object.keys(e).forEach(n=>{r.hasOwnProperty(n)||(r[n]=e[n]),e[n]===null?delete r[n]:r[n]=K(r[n],e[n])}),r}var me="0.20.0-beta4";var Ae=t=>t.pluginType==="preprocessor",Oe=t=>t.pluginType==="watcher",Pe=t=>t.pluginType==="attribute",Ne=t=>t.pluginType==="action",F=class{constructor(){this.plugins=[];this.store=L({});this.preprocessors=new Array;this.actions={};this.watchers=new Array;this.refs={};this.reactivity={signal:V,computed:he,effect:W};this.parentID="";this.missingIDNext=0;this.removals=new Map;this.mergeRemovals=new Array;this.lastMarshalledStore=""}get version(){return me}load(...e){let r=new Set(this.plugins);e.forEach(n=>{if(n.requiredPlugins){for(let o of n.requiredPlugins)if(!r.has(o))throw x}let s;if(Ae(n)){if(this.preprocessors.includes(n))throw D;this.preprocessors.push(n)}else if(Oe(n)){if(this.watchers.includes(n))throw D;this.watchers.push(n),s=n.onGlobalInit}else if(Ne(n)){if(this.actions[n.name])throw D;this.actions[n.name]=n}else if(Pe(n)){if(this.plugins.includes(n))throw D;this.plugins.push(n),s=n.onGlobalInit}else throw B;s&&s({store:()=>this.store,upsertSignal:this.upsertSignal.bind(this),mergeSignals:this.mergeSignals.bind(this),removeSignals:this.removeSignals.bind(this),actions:this.actions,reactivity:this.reactivity,applyPlugins:this.applyPlugins.bind(this),cleanup:this.cleanup.bind(this)}),r.add(n)}),this.applyPlugins(document.body)}cleanup(e){let r=this.removals.get(e);if(r){for(let n of r.set)n();this.removals.delete(e)}}mergeSignals(e){this.mergeRemovals.forEach(s=>s()),this.mergeRemovals=this.mergeRemovals.slice(0);let r=K(this.store.value,e);this.store=L(r),JSON.stringify(this.store.value),this.lastMarshalledStore}removeSignals(...e){let r={...this.store.value},n=!1;for(let s of e){let o=s.split("."),a=o[0],f=r;for(let l=1;l<o.length;l++){let m=o[l];f[a]||(f[a]={}),f=f[a],a=m}delete f[a],n=!0}n&&(this.store=L(r),this.applyPlugins(document.body))}upsertSignal(e,r){let n=e.split("."),s=this.store;for(let l=0;l<n.length-1;l++){let m=n[l];s[m]||(s[m]={}),s=s[m]}let o=n[n.length-1],a=s[o];if(a)return a;let f=this.reactivity.signal(r);return s[o]=f,f}applyPlugins(e){let r=new Set;this.plugins.forEach((n,s)=>{this.walkDownDOM(e,o=>{s||this.cleanup(o);for(let a in o.dataset){let f=`${o.dataset[a]}`||"",l=f;if(!a.startsWith(n.name))continue;if(o.id.length===0&&(o.id=`${w}-${this.parentID}-${this.missingIDNext++}`),r.clear(),n.allowedTagRegexps){let u=o.tagName.toLowerCase();if(![...n.allowedTagRegexps].some(h=>u.match(h)))throw x}let m=a.slice(n.name.length),[y,...be]=m.split(".");if(n.mustHaveEmptyKey&&y.length>0)throw d;if(n.mustNotEmptyKey&&y.length===0)throw d;y.length&&(y=y[0].toLowerCase()+y.slice(1));let Y=be.map(u=>{let[S,...h]=u.split("_");return{label:S,args:h}});if(n.allowedModifiers){for(let u of Y)if(!n.allowedModifiers.has(u.label))throw x}let q=new Map;for(let u of Y)q.set(u.label,u.args);if(n.mustHaveEmptyExpression&&l.length)throw d;if(n.mustNotEmptyExpression&&!l.length)throw d;let z=/;|\n/;n.removeNewLines&&(l=l.split(`
`).map(u=>u.trim()).join(" "));let ve=[...n.preprocessors?.pre||[],...this.preprocessors,...n.preprocessors?.post||[]];for(let u of ve){if(r.has(u))continue;r.add(u);let S=l.split(z),h=[];S.forEach(p=>{let _=p,Z=[..._.matchAll(u.regexp)];if(Z.length)for(let Q of Z){if(!Q.groups)continue;let{groups:ee}=Q,{whole:xe}=ee;_=_.replace(xe,u.replacer(ee))}h.push(_)}),l=h.join("; ")}let G={store:()=>this.store,mergeSignals:this.mergeSignals.bind(this),upsertSignal:this.upsertSignal.bind(this),removeSignals:this.removeSignals.bind(this),applyPlugins:this.applyPlugins.bind(this),cleanup:this.cleanup.bind(this),walkSignals:this.walkSignals.bind(this),actions:this.actions,reactivity:this.reactivity,el:o,rawKey:a,key:y,rawExpression:f,expression:l,expressionFn:()=>{throw ue},modifiers:q};if(!n.bypassExpressionFunctionCreation?.(G)&&!n.mustHaveEmptyExpression&&l.length){let u=l.split(z).map(p=>p.trim()).filter(p=>p.length);u[u.length-1]=`return ${u[u.length-1]}`;let S=u.map(p=>`  ${p}`).join(`;
`),h=`
  try {
    const _datastarExpression = () => {
  ${S}
    }
    const _datastarReturnVal = _datastarExpression()
    return _datastarReturnVal
  } catch (e) {
   const msg = \`
  Error evaluating Datastar expression:
  ${S.replaceAll("`","\\`")}

  Error: \${e.message}

  Check if the expression is valid before raising an issue.
  \`.trim()
   console.error(msg)
   debugger
  }
              `;try{let p=n.argumentNames||[],_=new Function("ctx",...p,h);G.expressionFn=_}catch(p){let _=new Error(`${p}
with
${h}`);console.error(_);debugger}}let X=n.onLoad(G);X&&(this.removals.has(o)||this.removals.set(o,{id:o.id,set:new Set}),this.removals.get(o).set.add(X))}})})}walkSignalsStore(e,r){let n=Object.keys(e);for(let s=0;s<n.length;s++){let o=n[s],a=e[o],f=a instanceof c,l=typeof a=="object"&&Object.keys(a).length>0;if(f){r(o,a);continue}l&&this.walkSignalsStore(a,r)}}walkSignals(e){this.walkSignalsStore(this.store,e)}walkDownDOM(e,r,n=0){if(!e)return;let s=le(e);if(s)for(r(s),n=0,e=e.firstElementChild;e;)this.walkDownDOM(e,r,n++),e=e.nextElementSibling}};var ye=new F;ye.load(ie,ae,se,te,ne);var Se=ye;Se.load();})();
//# sourceMappingURL=datastar-core.js.map
