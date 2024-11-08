package datastar

import (
	"fmt"
	"net/http"

	"github.com/goccy/go-json"
	"github.com/valyala/bytebufferpool"
)

func ParseIncoming(r *http.Request, store any) error {
	var dsInput []byte

	if r.Method == "GET" {
		dsJSON := r.URL.Query().Get("datastar")
		if dsJSON == "" {
			return nil
		} else {
			dsInput = []byte(dsJSON)
		}
	} else {
		buf := bytebufferpool.Get()
		defer bytebufferpool.Put(buf)
		if _, err := buf.ReadFrom(r.Body); err != nil {
			return fmt.Errorf("failed to read body: %w", err)
		}
		dsInput = buf.Bytes()
	}

	if err := json.Unmarshal(dsInput, store); err != nil {
		return fmt.Errorf("failed to unmarshal: %w", err)
	}
	return nil
}
