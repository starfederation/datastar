# Persist Signals Wildcard

Tests persisting signals with a path using a wildcard.

<div data-signals="{foo: {bar: {baz: 0}}}" data-persist="foo.*.*" data-on-load="$foo.bar.baz = 1">
  Expected value in local storage (in alphabetical order): 
  <pre><code>datastar: {"foo":{"bar":{"baz":1}}}</code></pre>
</div>