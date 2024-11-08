package site

import (
	"encoding/base64"

	"fmt"
	"io"
	"net/http"

	"github.com/dustin/go-humanize"
	"github.com/go-chi/chi/v5"
	"github.com/goccy/go-json"
	datastar "github.com/starfederation/datastar/code/go/sdk"
	"github.com/zeebo/xxh3"
)

func setupExamplesFileUpload(examplesRouter chi.Router) error {

	examplesRouter.Get("/file_upload/data", func(w http.ResponseWriter, r *http.Request) {
		store := &FileUploadStore{
			FilesBase64: []string{},
			FileMimes:   []string{},
			FileNames:   []string{},
		}
		sse := datastar.NewSSE(w, r)
		sse.RenderFragmentTempl(FileUploadView(store))
	})

	examplesRouter.Post("/file_upload/upload", func(w http.ResponseWriter, r *http.Request) {
		maxBytesSize := 4 * 1024 * 1024
		r.Body = http.MaxBytesReader(w, r.Body, int64(maxBytesSize))
		data, err := io.ReadAll(r.Body)
		if err != nil {
			datastar.NewSSE(w, r).ConsoleErrorf("error reading body: %v", err)
			return
		}

		if len(data) >= maxBytesSize {
			sse := datastar.NewSSE(w, r)
			sse.RenderFragmentTempl(FileUpdateAlert(err))
			return
		}

		store := &FileUploadStore{}
		sse := datastar.NewSSE(w, r)
		if err := json.Unmarshal(data, store); err != nil {
			sse.RenderFragmentTempl(FileUpdateAlert(err))
			return
		}

		files := make([][]byte, len(store.FilesBase64))
		for i, file := range store.FilesBase64 {
			data, err := base64.StdEncoding.DecodeString(file)
			if err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}
			files[i] = data
		}

		humanizedHashes := make([]string, len(files))
		humainzeByteCount := make([]string, len(files))
		for i, file := range files {
			h := xxh3.Hash(file)
			humanizedHashes[i] = fmt.Sprintf("%x", h)
			humainzeByteCount[i] = humanize.Bytes(uint64(len(file)))
		}

		sse.RenderFragmentTempl(FileUploadResults(store, humainzeByteCount, humanizedHashes))
	})

	return nil
}
