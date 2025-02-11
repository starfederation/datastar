## Key Casing

Tests that keys are correctly cased when using the `__case` modifier.

<div data-signals-total="0" data-signals-foo-bar="0.25" data-signals-foo-bar__case.kebab="0.25" data-signals-foo-bar__case.snake="0.25" data-signals-foo-bar__case.pascal="0.25">
  <button data-on-click="$total = $fooBar + $foo-bar + $foo_bar + $FooBar" class="btn">Sum</button>
  <hr />
  Result:
  <code id="result" data-text="$total"></code>
  <hr />
  Expected result on click: <code>1</code>
</div>