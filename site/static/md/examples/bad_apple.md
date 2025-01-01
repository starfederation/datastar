## Bad Apple ASCII Animation

## Demo

<div
    id="contents"
    class="flex flex-col gap-4 p-4 w-full"
    data-signals="{_contents: 'bad apple frames go here', percentage: 0}"
    data-on-load="@sse('/examples/bad_apple/updates')"
>
    <div class="flex gap-4 items-center font-mono">
        <input
            disabled
            class="range range-neutral flex-1"
            type="range" min="1" max="100"
            data-bind="percentage"
         />
        <div>
            <span data-text="$percentage.toFixed(2)"></span>%
        </div>
    </div>
    <div class="aspect-square font-mono font-bold text-[11px] leading-[0.25rem] flex justify-center items-center">
        <pre
            style="background-color: black"
            data-text="$_contents"
        ></pre>
    </div>
</div>

## Explanation

Per a conversation on the HTMX meme discord channel there was an [offhand remark](https://discordapp.com/channels/725789699527933952/996832027083026563/1276380165613813894) about adding the [Bad Apple Music video](https://www.youtube.com/watch?v=FtutLA63Cp8) as a benchmark. Thought it'd be fun to do so.

We take the [already converted](https://github.com/trung-kieen/bad-apple-ascii) frames of video and turn them into a ZSTD compressed Gob file that's embedded in the server binary. This makes the whole animation about 1.9MB. We then stream the frames to the client and update the contents of a pre tag with the frames. The percentage is updated with the current frame number.

## Code

```html
<div
  id="contents"
  data-signals="{_contents: 'bad apple frames go here', percentage: 0}"
  data-on-load="@sse('/examples/bad_apple/updates')"
>
  <div>
    <input type="range" min="1" max="100" data-bind="percentage" disabled />
    <div><span data-text="$percentage.toFixed(2)"></span>%</div>
  </div>
  <div>
    <pre data-text="$_contents"></pre>
  </div>
</div>
```

### Note

This is using Datastar's ability to patch signal directly. **_No need to generate html fragments, as the contents are already bound to existing elements._**

We could also stream down the raster frames using base64 encoded images and update the src of an image tag. Either way works, you would just have to use `data-attr-src` on an image tag.

Open your DevTool's Elements tab for the contents of the pre tag. You'll see the frames being updated in real-time (in this case 30fps).
