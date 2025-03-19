# Persist Signal

Tests persisting a signal.

<div data-signals="{foo: 0, bar: 0}" data-persist="foo" data-on-load="$foo = 1; $bar = 1">
  Expected value in local storage: 
  <pre><code>datastar: {"foo":1}</code></pre>
</div>