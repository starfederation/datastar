# Merge Fragment Input Value

Tests merging a fragment containing a bound input value.

<div>
  <div id="content"><input id="populatable" type="text" data-bind-result class="input input-bordered" /></div>
  <button id="clickable" data-on-click="@get('/tests/merge_fragment_input_value/data')" class="btn">Merge</button>
  <hr />
  Result:
  <code id="result">0</code>
  <hr />
  Expected result on entering `foo` in input and then click: <code>1</code>
</div>