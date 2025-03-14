# Custom Plugin

Tests that loading a custom plugin works.

<script type="module">
  import { load } from '/static/js/datastar.js'
  
  load({
      type: 3,
      name: 'test',
      fn: () => {
          result.innerText = 1
      },
  })
</script>

<div data-on-load="@test()">
  Result:
  <code id="result">0</code>
  <hr />
  Expected result on load: <code>1</code>
</div>