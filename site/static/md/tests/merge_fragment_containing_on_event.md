# Merge Fragment Containing On Event

Tests merging a fragment containing an `on` event.

<div>
  <div id="content" data-signals-hidden="false" data-show="!$hidden"><button data-on-click="$hidden = true" data-show="!$hidden" class="btn">Hide</button><input data-bind-name class="input input-bordered" /><button data-on-click="@get('/tests/merge_fragment_containing_on_event/data')" class="btn">Merge</button></div>
  <hr />
  <button id="clickable" data-on-click="@get('/tests/merge_fragment_containing_on_event/data')" class="btn">Merge</button>
  <pre data-text="ctx.signals.JSON()"></pre>
</div>