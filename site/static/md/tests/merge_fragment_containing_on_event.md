# Merge Fregment Containing On Event

Tests that merging a fragment containing an `on` event works.

<div>
  <div id="content" data-signals-hidden="false" data-show="!$hidden"><button data-on-click="$hidden = true" data-show="!$hidden" class="btn">Hide</button></div>
  <hr />
  <button id="clickable" data-on-click="@get('/tests/merge_fragment_containing_on_event/data')" class="btn">Merge</button>
  <pre data-text="ctx.signals.JSON()"></pre>
</div>