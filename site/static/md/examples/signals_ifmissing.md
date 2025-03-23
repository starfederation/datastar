## Merge Signals If Missing

## Demo

<div data-on-load="@get('/examples/signals_ifmissing/updates')">
  <div>
    Should always be 1234:
    <span id="placeholder"></span>
  </div>
</div>

## Explanation

```html
<div id="demo" data-signals="{id: 1234}" data-on-load="@get('/examples/signals_ifmissing/updates')">
  <div>
    Should always be 1234:
    <span data-text="$id">should be replaced</span>
  </div>
</div>
```

The `data-signals` attribute is used to set the initial state of the signals. You can check your browser's Network
Devtools to see the `updates` endpoint alternates between sending fragments and direct signal merges like the following:

```md
...

event: datastar-merge-signals
data: onlyIfMissing true
data: signals {id:73}

event: datastar-merge-fragments
data: mergeMode upsertAttributes
data: fragments <div id="demo" data-signals__ifmissing="{id:74}"></div>

event: datastar-merge-signals
data: onlyIfMissing true
data: signals {id:75}

event: datastar-merge-fragments
data: mergeMode upsertAttributes
data: fragments <div id="demo" data-signals__ifmissing="{id:76}"></div>

event: datastar-merge-signals
data: onlyIfMissing true
data: signals {id:77}

...
```

Since the signals is already set, the `data-signals__ifmissing` attribute will not overwrite the existing value in
either case.