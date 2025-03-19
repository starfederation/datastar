# Persist Signal Key

Tests persisting a signal using a key.

<div data-signals="{foo: 0, bar: 0}" data-persist-custom="foo" data-on-load="$foo = 1; $bar = 1">
  Expected value in local storage: 
  <pre><code>custom: {"foo":1}</code></pre>
</div>