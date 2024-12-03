"use strict";(()=>{var b="datastar";var W="0.20.1";var ce={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},_e=ce.Morph;var fe="https://data-star.dev/errors";var f=(t,e)=>{let n=new Error;n.name=`${b}${t}`;let i=`${fe}/code?${new URLSearchParams(e)}`;return n.message=`${b}${t}, for more info see ${i}`,n};var de="computed",H={type:1,name:de,purge:!0,onLoad:({key:t,signals:e,genRX:n})=>{let i=n();if(!t.length)throw f("P1");e.setComputed(t,i)}};var J={type:1,name:"signals",purge:!0,onLoad:({key:t,signals:e,genRX:n})=>{let s=n()();t?e.setValue(t,s):e.merge(s)}};var j={type:1,name:"star",onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var Z={name:"signalValue",type:0,fn:t=>{let e=/(?<path>[\w0-9.]*)(.value)/gm;return t.replaceAll(e,"ctx.signals.signal('$1').value")}};function Q(t){if(t.id)return t.id;let e=0,n=s=>(e=(e<<5)-e+s,e&e),i=s=>s.split("").forEach(o=>n(o.charCodeAt(0)));for(;t.parentNode;){if(t.id){i(`${t.id}`);break}else if(t===t.ownerDocument.documentElement)i(t.tagName);else{for(let s=1,o=t;o.previousElementSibling;o=o.previousElementSibling,s++)n(s);t=t.parentNode}t=t.parentNode}return b+e}var pe=Symbol.for("preact-signals"),g=1,v=2,T=4,S=8,P=16,y=32;function C(){M++}function U(){if(M>1){M--;return}let t,e=!1;for(;N!==void 0;){let n=N;for(N=void 0,G++;n!==void 0;){let i=n._nextBatchedEffect;if(n._nextBatchedEffect=void 0,n._flags&=~v,!(n._flags&S)&&q(n))try{n._callback()}catch(s){e||(t=s,e=!0)}n=i}}if(G=0,M--,e)throw f("Z1",t)}var r;var N,M=0,G=0,V=0;function X(t){if(r===void 0)return;let e=t._node;if(e===void 0||e._target!==r)return e={_version:0,_source:t,_prevSource:r._sources,_nextSource:void 0,_target:r,_prevTarget:void 0,_nextTarget:void 0,_rollbackNode:e},r._sources!==void 0&&(r._sources._nextSource=e),r._sources=e,t._node=e,r._flags&y&&t._subscribe(e),e;if(e._version===-1)return e._version=0,e._nextSource!==void 0&&(e._nextSource._prevSource=e._prevSource,e._prevSource!==void 0&&(e._prevSource._nextSource=e._nextSource),e._prevSource=r._sources,e._nextSource=void 0,r._sources._nextSource=e,r._sources=e),e}function u(t){this._value=t,this._version=0,this._node=void 0,this._targets=void 0}u.prototype.brand=pe;u.prototype._refresh=function(){return!0};u.prototype._subscribe=function(t){this._targets!==t&&t._prevTarget===void 0&&(t._nextTarget=this._targets,this._targets!==void 0&&(this._targets._prevTarget=t),this._targets=t)};u.prototype._unsubscribe=function(t){if(this._targets!==void 0){let e=t._prevTarget,n=t._nextTarget;e!==void 0&&(e._nextTarget=n,t._prevTarget=void 0),n!==void 0&&(n._prevTarget=e,t._nextTarget=void 0),t===this._targets&&(this._targets=n)}};u.prototype.subscribe=function(t){return D(()=>{let e=this.value,n=r;r=void 0;try{t(e)}finally{r=n}})};u.prototype.valueOf=function(){return this.value};u.prototype.toString=function(){return this.value+""};u.prototype.toJSON=function(){return this.value};u.prototype.peek=function(){let t=r;r=void 0;try{return this.value}finally{r=t}};Object.defineProperty(u.prototype,"value",{get(){let t=X(this);return t!==void 0&&(t._version=this._version),this._value},set(t){if(t!==this._value){if(G>100)throw f("Z2");this._value=t,this._version++,V++,C();try{for(let e=this._targets;e!==void 0;e=e._nextTarget)e._target._notify()}finally{U()}}}});function q(t){for(let e=t._sources;e!==void 0;e=e._nextSource)if(e._source._version!==e._version||!e._source._refresh()||e._source._version!==e._version)return!0;return!1}function z(t){for(let e=t._sources;e!==void 0;e=e._nextSource){let n=e._source._node;if(n!==void 0&&(e._rollbackNode=n),e._source._node=e,e._version=-1,e._nextSource===void 0){t._sources=e;break}}}function ee(t){let e=t._sources,n;for(;e!==void 0;){let i=e._prevSource;e._version===-1?(e._source._unsubscribe(e),i!==void 0&&(i._nextSource=e._nextSource),e._nextSource!==void 0&&(e._nextSource._prevSource=i)):n=e,e._source._node=e._rollbackNode,e._rollbackNode!==void 0&&(e._rollbackNode=void 0),e=i}t._sources=n}function m(t){u.call(this,void 0),this._fn=t,this._sources=void 0,this._globalVersion=V-1,this._flags=T}m.prototype=new u;m.prototype._refresh=function(){if(this._flags&=~v,this._flags&g)return!1;if((this._flags&(T|y))===y||(this._flags&=~T,this._globalVersion===V))return!0;if(this._globalVersion=V,this._flags|=g,this._version>0&&!q(this))return this._flags&=~g,!0;let t=r;try{z(this),r=this;let e=this._fn();(this._flags&P||this._value!==e||this._version===0)&&(this._value=e,this._flags&=~P,this._version++)}catch(e){this._value=e,this._flags|=P,this._version++}return r=t,ee(this),this._flags&=~g,!0};m.prototype._subscribe=function(t){if(this._targets===void 0){this._flags|=T|y;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._subscribe(e)}u.prototype._subscribe.call(this,t)};m.prototype._unsubscribe=function(t){if(this._targets!==void 0&&(u.prototype._unsubscribe.call(this,t),this._targets===void 0)){this._flags&=~y;for(let e=this._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e)}};m.prototype._notify=function(){if(!(this._flags&v)){this._flags|=T|v;for(let t=this._targets;t!==void 0;t=t._nextTarget)t._target._notify()}};Object.defineProperty(m.prototype,"value",{get(){if(this._flags&g)throw f("Z2");let t=X(this);if(this._refresh(),t!==void 0&&(t._version=this._version),this._flags&P)throw f("Z3",{value:this._value});return this._value}});function te(t){return new m(t)}function ne(t){let e=t._cleanup;if(t._cleanup=void 0,typeof e=="function"){C();let n=r;r=void 0;try{e()}catch(i){throw t._flags&=~g,t._flags|=S,$(t),f("Z4",{error:i})}finally{r=n,U()}}}function $(t){for(let e=t._sources;e!==void 0;e=e._nextSource)e._source._unsubscribe(e);t._fn=void 0,t._sources=void 0,ne(t)}function ge(t){if(r!==this)throw f("Z5");ee(this),r=t,this._flags&=~g,this._flags&S&&$(this),U()}function A(t){this._fn=t,this._cleanup=void 0,this._sources=void 0,this._nextBatchedEffect=void 0,this._flags=y}A.prototype._callback=function(){let t=this._start();try{if(this._flags&S||this._fn===void 0)return;let e=this._fn();typeof e=="function"&&(this._cleanup=e)}finally{t()}};A.prototype._start=function(){if(this._flags&g)throw f("Z2");this._flags|=g,this._flags&=~S,ne(this),z(this),C();let t=r;return r=this,ge.bind(this,t)};A.prototype._notify=function(){this._flags&v||(this._flags|=v,this._nextBatchedEffect=N,N=this)};A.prototype._dispose=function(){this._flags|=S,this._flags&g||$(this)};function D(t){let e=new A(t);try{e._callback()}catch(n){throw e._dispose(),f("Z6",{error:n})}return e._dispose.bind(e)}function ie(t,e=!1){let n={};for(let i in t)if(t.hasOwnProperty(i)){let s=t[i];if(s instanceof u){if(e&&i.startsWith("_"))continue;n[i]=s.value}else n[i]=ie(s)}return n}function se(t,e,n=!1){for(let i in e)if(e.hasOwnProperty(i)){let s=e[i];if(s instanceof Object&&!Array.isArray(s))t[i]||(t[i]={}),se(t[i],s,n);else{if(n&&t[i])continue;t[i]=new u(s)}}}function oe(t,e){for(let n in t)if(t.hasOwnProperty(n)){let i=t[n];i instanceof u?e(n,i):oe(i,e)}}function he(t,...e){let n={};for(let i of e){let s=i.split("."),o=t,a=n;for(let d=0;d<s.length-1;d++){let p=s[d];if(!o[p])return{};a[p]||(a[p]={}),o=o[p],a=a[p]}let c=s[s.length-1];a[c]=o[c]}return n}var O=class{constructor(){this._signals={}}exists(e){return!!this.signal(e)}signal(e){let n=e.split("."),i=this._signals;for(let o=0;o<n.length-1;o++){let a=n[o];if(!i[a])return null;i=i[a]}let s=n[n.length-1];return i[s]}setSignal(e,n){let i=e.split("."),s=this._signals;for(let a=0;a<i.length-1;a++){let c=i[a];s[c]||(s[c]={}),s=s[c]}let o=i[i.length-1];s[o]=n}setComputed(e,n){let i=te(()=>n());this.setSignal(e,i)}value(e){return this.signal(e)?.value}setValue(e,n){let i=this.upsert(e,n);i.value=n}upsert(e,n){let i=e.split("."),s=this._signals;for(let d=0;d<i.length-1;d++){let p=i[d];s[p]||(s[p]={}),s=s[p]}let o=i[i.length-1],a=s[o];if(a)return a;let c=new u(n);return s[o]=c,c}remove(...e){for(let n of e){let i=n.split("."),s=this._signals;for(let a=0;a<i.length-1;a++){let c=i[a];if(!s[c])return;s=s[c]}let o=i[i.length-1];delete s[o]}}merge(e,n=!1){se(this._signals,e,n)}subset(...e){return he(this.values(),...e)}walk(e){oe(this._signals,e)}values(e=!1){return ie(this._signals,e)}JSON(e=!0,n=!1){let i=this.values(n);return e?JSON.stringify(i,null,2):JSON.stringify(i)}toString(){return this.JSON()}};var I=class{constructor(){this._signals=new O;this.plugins=[];this.macros=[];this.actions={};this.watchers=[];this.removals=new Map}get version(){return W}load(...e){e.forEach(n=>{let i;switch(n.type){case 0:this.macros.push(n);break;case 2:let s=n;this.watchers.push(s),i=s.onGlobalInit;break;case 3:this.actions[n.name]=n;break;case 1:let o=n;this.plugins.push(o),i=o.onGlobalInit;break;default:throw f("A3",{name:n.name,type:n.type})}if(i){let s=this;i({get signals(){return s._signals},effect:o=>D(o),actions:this.actions,apply:this.apply.bind(this),cleanup:this.cleanup.bind(this)})}}),this.apply(document.body)}cleanup(e){let n=this.removals.get(e);if(n){for(let i of n.set)i();this.removals.delete(e)}}apply(e){let n=new Set;this.plugins.forEach((i,s)=>{this.walkDownDOM(e,o=>{s||this.cleanup(o);for(let a in o.dataset){let c=`${o.dataset[a]}`||"",d=c;if(!a.startsWith(i.name))continue;o.id.length||(o.id=Q(o)),n.clear();let p=a.slice(i.name.length),[h,...w]=p.split(".");h.length&&(h=h[0].toLowerCase()+h.slice(1));let F=new Map;w.forEach(_=>{let[le,...ue]=_.split("_");F.set(le,new Set(ue))});let R=[...i.macros?.pre||[],...this.macros,...i.macros?.post||[]];for(let _ of R)n.has(_)||(n.add(_),d=_.fn(d));let{actions:x,apply:l,cleanup:k}=this,B=this,L;L={get signals(){return B._signals},effect:_=>D(_),apply:l.bind(this),cleanup:k.bind(this),actions:x,genRX:()=>this.genRX(L,...i.argNames||[]),el:o,rawKey:a,rawValue:c,key:h,value:d,mods:F};let K=i.onLoad(L);K&&(this.removals.has(o)||this.removals.set(o,{id:o.id,set:new Set}),this.removals.get(o).set.add(K)),i?.purge&&delete o.dataset[a]}})})}genRX(e,...n){let i=e.value.split(/;|\n/).map(l=>l.trim()).filter(l=>l.length),s=i.length-1,o="return";i[s].startsWith(o)||(i[s]=`${o} ${i[s]};`);let c=i.join(`;
`),d=/(\w*)\(/gm,p=c.matchAll(d),h=new Set;for(let l of p)h.add(l[1]);let w=Object.keys(this.actions).filter(l=>h.has(l)),R=`${w.map(l=>`const ${l} = ctx.actions.${l}.fn;`).join(`
`)}
${c}`,x=R;w.forEach(l=>{x=x.replaceAll(l+"(",l+"(ctx,")});try{let l=n||[],k=new Function("ctx",...l,x);return(...B)=>k(e,...B)}catch(l){throw f("A5",{error:l,fnContent:R})}}walkDownDOM(e,n){if(!e||!(e instanceof HTMLElement||e instanceof SVGElement))return null;for(n(e),e=e.firstElementChild;e;)this.walkDownDOM(e,n),e=e.nextElementSibling}};var re=new I;re.load(j,Z,J,H);var ae=re;ae.load();})();
//# sourceMappingURL=datastar-core.js.map
