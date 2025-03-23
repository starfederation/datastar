## Prefetch using ExecuteScript

## Demo

<div id="carousel" data-init="@get('/examples/prefetch/load')"></div>

## Explanation

We are loading Pokemon images in a carousel. Know how much people love Pokemon, we are prefetching the images before
they are needed. This way, the images are already in the browser cache when they are needed.

```html
<div id="carousel" data-init="@get('/examples/prefetch/load')">
  ```

  Since we have SSE we can easily load the initial page content then prefetch the images after

  The interesting part of the backend (Go) is how simple it is.

  ```go
  examplesRouter.Get("/prefetch/load", func(w http.ResponseWriter, r *http.Request) {
  sse := datastar.NewSSE(w, r)

  // render the carousel with the first image
  sse.MergeFragmentTempl(prefetchCarousel(1))

  // create a list of URLs to prefetch
  prefetchURLs := make([]string, 0, pokemonCount)
  for i := 2; i <= pokemonCount; i++ { prefetchURLs=append(prefetchURLs, fmt.Sprintf(pokemonURLFormat, i)) } // prefetch
    the images sse.Prefetch(prefetchURLs...) }) ``` So we get fast initial page load and prefetch the images after. This
    is a great way to make your site feel faster, especially when you know what the user is going to need next via the
    [speculation api](https://developer.mozilla.org/en-US/docs/Web/API/Speculation_Rules_API). If you are interested in
    the details of the script that get generated and loaded, look at your head element or the Network tab in the browser
    dev tools.