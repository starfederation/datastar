# Key Casing

Tests that keys are correctly cased when using the `__case` modifier, and are assignable in expressions.

<div data-signals-foo-bar="0" data-signals-foo-bar__case.kebab="0" data-signals-foo-bar__case.snake="0"
  data-signals-foo-bar__case.pascal="0">
  Result:
  <code id="result" data-on-load="$fooBar=0.25; $foo-bar=0.25; $foo_bar=0.25; $FooBar=0.25"
    data-text="$fooBar + $foo-bar + $foo_bar + $FooBar"></code>
  <hr />
  Expected result on load: <code>1</code>
</div>