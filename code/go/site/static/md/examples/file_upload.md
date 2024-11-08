## File Upload

[Original HTMX Version](https://htmx.org/examples/file-upload/)

## Demo

<div
    id="file_upload"
    data-on-load="$$get('/examples/file_upload/data')"
>
</div>

## Explanation

In this example we show how to create a file upload form that will be submitted via fetch.

```html
<!-- Removed styling for brevity -->
<div
  id="file_upload" data-on-load="$$get"
  data-store="{"files": [],"filesMimes": [],"filesNames": []}"
>
  <div>
    <label>
      <span class="label-text">Pick anything reasonably sized</span>
    </label>
    <input type="file" data-model="files" multiple>
    <button
      data-fetch-url="'/examples/file_upload/upload'"
      data-on-click="$$post">
      Submit
    </button>
  </div>
</div>
```

We don't need a form because everything is encoded as signals and automatically sent to the server.
We `POST` the form to /upload, since the `input` is using `data-model` the file will be automatically encoded as base64. If your store includes `${signalName}Mimes` and `${signalName}Names` then those will be sent as well. All three signals are arrays and files / metainfo will be appended in the order of selection.

**Note:** If you try to upload a file that is too large you will get an error message in the console.

## Differences from HTMX

Since HTMX uses standard forms the [file upload is lost](https://htmx.org/examples/file-upload-input/) when the form is submitted. This is a limitation of the browser and not HTMX. However, since we are using signals we can send the file back down to the client and hook up to the signal if desired.
