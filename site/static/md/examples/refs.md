## Refs

## Demo

<div>
     <div data-ref="foo">I'm a div that is getting referenced</div>
     <div class="card bg-primary text-primary-content">
          <div class="card-body">
               <div class="card-title" data-text="`I'm using content of '${$foo.innerHTML}'`">
                    I should be replaced with the content of the referenced div
               </div>
          </div>
     </div>
</div>

## Explanation

```html
<div>
  <div data-ref="foo">I'm a div that is getting referenced</div>
  <div data-text="`I'm using content of '${$foo.innerHTML}'`"></div>
</div>
```

Adding `data-ref="foo"` to an element creates a signal called `$foo` that points to that element.

***Note:*** We have to wrap the reference in `()` to avoid parsing issues.  This is most do to supported nested signals.  To the parser `foo.bar.bar` and `foo.bar.length` look the same.  We are looking at efficient ways to handle this but for now this is the solution.
