# Persist Signals Path

Tests persisting signals with a path.

<div data-signals="{foo: 0, bar: 0, baz: 0}" data-persist="foo bar" data-on-load="$foo = 1; $bar = 1; $baz = 1">
  Expected value in local storage (in alphabetical order): 
  <pre><code>datastar: {"bar":1,"foo":1}</code></pre>
</div>