package main

import (
	"compress/gzip"
	"context"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/dustin/go-humanize"
	"github.com/evanw/esbuild/pkg/api"
	"github.com/goccy/go-json"
	datastar "github.com/starfederation/datastar/code/go/sdk"
	"github.com/valyala/bytebufferpool"
	"github.com/valyala/fasttemplate"
)

const (
	DefaultSettleTime        = 300 * time.Millisecond
	DefaultSseSendRetry      = 1 * time.Second
	DefaultFragmentMergeMode = datastar.FragmentMergeModeMorph
)

func main() {
	start := time.Now()
	log.Print("Datastar built in TS compiler!")
	defer func() {
		log.Printf("Datastar built in %s", time.Since(start))
	}()

	ctx := context.Background()
	if err := run(ctx); err != nil {
		log.Fatal(err)
	}

}

func run(ctx context.Context) error {
	if err := errors.Join(
		createBundles(ctx),
		extractVersion(ctx),
	); err != nil {
		return fmt.Errorf("error creating bundles: %w", err)
	}

	return nil
}

func createBundles(ctx context.Context) error {
	_ = ctx

	log.Print("Creating bundles...")
	defer log.Print("Bundles created!")

	outDir := "./bundles"
	os.RemoveAll(outDir)

	result := api.Build(api.BuildOptions{
		EntryPoints: []string{
			"code/ts/library/src/bundles/datastar-core.ts",
			"code/ts/library/src/bundles/datastar.ts",
		},
		Outdir:            outDir,
		Bundle:            true,
		Write:             true,
		LogLevel:          api.LogLevelInfo,
		MinifyWhitespace:  true,
		MinifyIdentifiers: true,
		MinifySyntax:      true,
		Sourcemap:         api.SourceMapLinked,
		Target:            api.ES2023,
	})

	if len(result.Errors) > 0 {
		errs := make([]error, len(result.Errors))
		for i, err := range result.Errors {
			errs[i] = errors.New(err.Text)
		}
		return errors.Join(errs...)
	}

	return nil
}

func extractVersion(ctx context.Context) error {
	log.Print("Extracting version...")

	packageJSONPath := "code/ts/library/package.json"
	packageJSON, err := os.ReadFile(packageJSONPath)
	if err != nil {
		return fmt.Errorf("error reading package.json: %w", err)
	}

	type PackageJSON struct {
		Version string `json:"version"`
	}
	pj := &PackageJSON{}
	if err := json.Unmarshal(packageJSON, pj); err != nil {
		return fmt.Errorf("error unmarshalling package.json: %w", err)
	}

	build, err := os.ReadFile("bundles/datastar.js")
	if err != nil {
		return fmt.Errorf("error reading datastar.js: %w", err)
	}
	datastarSize := len(build)

	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)

	w, err := gzip.NewWriterLevel(buf, gzip.BestCompression)
	if err != nil {
		panic(err)
	}
	if _, err := w.Write(build); err != nil {
		panic(err)
	}
	w.Close()
	datastarGzipSize := buf.Len()

	constsData := map[string]any{
		"version":                    pj.Version,
		"defaultSettleTimeMs":        strconv.Itoa(int(DefaultSettleTime.Milliseconds())),
		"defaultSSESendRetryMs":      strconv.Itoa(int(DefaultSseSendRetry.Milliseconds())),
		"defaultFragmentMergeMode":   DefaultFragmentMergeMode,
		"datastarSizeBytes":          strconv.Itoa(datastarSize),
		"datastarGzipSizeBytes":      strconv.Itoa(datastarGzipSize),
		"datastarGzipSizByteseHuman": humanize.IBytes(uint64(datastarGzipSize)),
	}

	for path, tmpl := range templates {
		t, err := fasttemplate.NewTemplate(strings.TrimSpace(tmpl), "{{", "}}")
		if err != nil {
			return fmt.Errorf("error creating template: %w", err)
		}
		s := t.ExecuteString(constsData)
		if err := os.WriteFile(path, []byte(s), 0644); err != nil {
			return fmt.Errorf("error writing version file: %w", err)
		}
	}

	return nil
}

var templates = map[string]string{
	"code/go/sdk/consts.go": `
package datastar

import "time"

const (
	Version                        = "{{version}}"
	VersionClientByteSize          = {{datastarSizeBytes}}
	VersionClientByteSizeGzip      = {{datastarGzipSizeBytes}}
	VersionClientByteSizeGzipHuman = "{{datastarGzipSizByteseHuman}}"

	DefaultSettleTime = {{defaultSettleTimeMs}} * time.Millisecond
	DefaultSseSendRetry = {{defaultSSESendRetryMs}} * time.Millisecond
	DefaultFragmentMergeMode = FragmentMergeMode("{{defaultFragmentMergeMode}}")
)
`,
}
