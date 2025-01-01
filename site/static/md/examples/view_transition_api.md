<style>
@keyframes fade-in {
  from { opacity: 0; }
}

@keyframes fade-out {
to { opacity: 0; }
}

@keyframes slide-from-right {
from { transform: translateX(90px); }
}

@keyframes slide-to-left {
to { transform: translateX(-90px); }
}

/_ Define animations for the old and new content _/
::view-transition-old(slide-it) {
animation: 180ms cubic-bezier(0.4, 0, 1, 1) both fade-out,
600ms cubic-bezier(0.4, 0, 0.2, 1) both slide-to-left;
}

::view-transition-new(slide-it) {
animation: 420ms cubic-bezier(0, 0, 0.2, 1) 90ms both fade-in,
600ms cubic-bezier(0.4, 0, 0.2, 1) both slide-from-right;
}

::view-transition-old(root),
::view-transition-new(root) {
animation-duration: 0.5s;
}
</style>

## View Transition API

## Demo

<div class="flex flex-col gap-4" data-signals="{supportsViewTransitionAPI:!!document.startViewTransition, useSlide: false}">
<div data-text="`View Transition API supported in browser? ${$supportsViewTransitionAPI}`"></div>
<div id="stuff" class="flex gap-4">
<button class="btn btn-accent" data-show="$supportsViewTransitionAPI" data-on-click="@get('/examples/view_transition_api/watch')">
    Fade transition
</button>
<button class="btn btn-accent" data-show="$supportsViewTransitionAPI" data-on-click="$useSlide = true; @get('/examples/view_transition_api/watch')">
    Slide transition
</button>
</div>
</div>

## Explanation

This comes from an [issue](https://github.com/starfederation/datastar/issues/19) on how to use the `data-view-transition`. This lead to simplifying the API.

## Full page transitions

Datastar automatically adds the proper `<meta/>` tags to properly transition full page updates any elements with shared ids will automatically transition.

## Inter-page transitions

By default, if `docuument.startViewTransition` exists each SSE fragment update will be wrapped in a `startViewTransition` async call. This will automatically fade in/out. If you want custom animation, such as

```html
<style>
  @keyframes fade-in {
    from {
      opacity: 0;
    }
  }

  @keyframes fade-out {
    to {
      opacity: 0;
    }
  }

  @keyframes slide-from-right {
    from {
      transform: translateX(90px);
    }
  }

  @keyframes slide-to-left {
    to {
      transform: translateX(-90px);
    }
  }

  /_
    Define
    animations
    for
    the
    old
    and
    new
    content
    _/
    ::view-transition-old(slide-it) {
    animation: 180ms cubic-bezier(0.4, 0, 1, 1) both fade-out, 600ms
        cubic-bezier(0.4, 0, 0.2, 1) both slide-to-left;
  }

  ::view-transition-new(slide-it) {
    animation: 420ms cubic-bezier(0, 0, 0.2, 1) 90ms both fade-in, 600ms
        cubic-bezier(0.4, 0, 0.2, 1) both slide-from-right;
  }

  ::view-transition-old(root),
  ::view-transition-new(root) {
    animation-duration: 0.5s;
  }
</style>
```

You can add `data-view-transition="slide-it"` to your elements to use the custom transition
