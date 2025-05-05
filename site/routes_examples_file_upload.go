package site

import (
	"encoding/base64"

	"fmt"
	"io"
	"net/http"

	"github.com/dustin/go-humanize"
	"github.com/go-chi/chi/v5"
	"github.com/goccy/go-json"
	"github.com/starfederation/datastar/sdk/go/datastar"
	"github.com/zeebo/xxh3"
)

func setupExamplesFileUpload(examplesRouter chi.Router) error {

	examplesRouter.Get("/file_upload/data", func(w http.ResponseWriter, r *http.Request) {
		signals := &FileUploadSignals{
			FilesBase64: []string{},
			FileMimes:   []string{},
			FileNames:   []string{},
		}
		sse := datastar.NewSSE(w, r)
		sse.MergeFragmentTempl(FileUploadView(signals))
	})

	examplesRouter.Post("/file_upload/upload", func(w http.ResponseWriter, r *http.Request) {
		maxBytesSize := 4 * 1024 * 1024
		r.Body = http.MaxBytesReader(w, r.Body, int64(maxBytesSize))
		data, err := io.ReadAll(r.Body)
		if err != nil {
			if len(data) >= maxBytesSize {
				http.Error(w, "file too large", http.StatusRequestEntityTooLarge)
				return
			}

			datastar.NewSSE(w, r).ConsoleError(fmt.Errorf("error reading body: %w", err))
			return
		}

		sse := datastar.NewSSE(w, r)
		signals := &FileUploadSignals{}
		if err := json.Unmarshal(data, signals); err != nil {
			sse.ConsoleError(fmt.Errorf("error unmarshalling json: %w", err))
			return
		}

		files := make([][]byte, len(signals.FilesBase64))
		for i, file := range signals.FilesBase64 {
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

		sse.MergeFragmentTempl(FileUploadResults(signals, humainzeByteCount, humanizedHashes))
	})

	return nil
}
