package build

import (
	"bytes"
	"errors"
	"fmt"
	"log"
	"os"
	"os/exec"
	"slices"
	"strings"
	"time"

	"github.com/andybalholm/brotli"
	"github.com/delaneyj/toolbelt"
	"github.com/valyala/bytebufferpool"
)

func Build() error {
	version, err := extractVersion()
	if err != nil {
		return fmt.Errorf("error extracting version: %w", err)
	}

	if err := errors.Join(
		writeOutConsts(version),
	); err != nil {
		return fmt.Errorf("error creating bundles: %w", err)
	}

	return nil
}

func extractVersion() (string, error) {
	versionFilepath := "VERSION"
	versionBytes, err := os.ReadFile(versionFilepath)
	if err != nil {
		return "", fmt.Errorf("error reading package.json: %w", err)
	}

	version := strings.TrimSpace(string(versionBytes))

	return version, nil
}

func writeOutConsts(version string) error {
	Consts.Version = version

	build, err := os.ReadFile("bundles/datastar.js")
	if err != nil {
		return fmt.Errorf("error reading datastar.js: %w", err)
	}
	Consts.VersionClientByteSize = len(build)

	compressed, err := compressWithBrotli(build)
	if err != nil {
		return fmt.Errorf("error compressing with brotli: %w", err)
	}
	Consts.VersionClientByteSizeBrotli = len(compressed)

	log.Printf("Datastar client bundle size: %d bytes (Brotli: %d bytes)", Consts.VersionClientByteSize, Consts.VersionClientByteSizeBrotli)

	var zeroCased toolbelt.CasedString
	// Make sure all enums are set up.
	for _, enum := range Consts.Enums {
		for _, value := range enum.Values {
			if value.Name == zeroCased {
				value.Name = toolbelt.ToCasedString(value.Value)
			}
		}
		if enum.DefaultIndex >= 0 {
			enum.Default = enum.Values[enum.DefaultIndex]
		}
	}

	slices.SortFunc(Consts.SDKLanguages, func(a, b Language) int {
		return strings.Compare(a.Name, b.Name)
	})

	templates := map[string]func(data *ConstTemplateData) string{
		"README.md":                    datastarREADME,
		"library/README.md":            datastarREADME,
		"library/src/engine/consts.ts": datastarClientConsts,
		"library/package.json":         datastarClientPackageJSON,
		"sdk/clojure/sdk/src/main/starfederation/datastar/clojure/consts.clj": clojureConsts,
		"sdk/go/datastar/consts.go":                                                        goConsts,
		"sdk/dotnet/fsharp/src/Consts.fs":                                                  fsharpConsts,
		"sdk/dotnet/csharp/src/Consts.cs":                                                  csharpConsts,
		"sdk/php/src/Consts.php":                                                           phpConsts,
		"sdk/php/src/enums/EventType.php":                                                  phpEventType,
		"sdk/php/src/enums/ElementPatchMode.php":                                           phpElementPatchMode,
		"sdk/java/core/src/main/java/starfederation/datastar/Consts.java":                  javaConsts,
		"sdk/java/core/src/main/java/starfederation/datastar/enums/EventType.java":         javaEventType,
		"sdk/java/core/src/main/java/starfederation/datastar/enums/ElementPatchMode.java":  javaElementPatchMode,
		"sdk/python/src/datastar_py/consts.py":                                             pythonConsts,
		"sdk/typescript/src/consts.ts":                                                     typescriptConsts,
		"sdk/ruby/lib/datastar/consts.rb":                                                  rubyConsts,
		"sdk/rust/src/consts.rs":                                                           rustConsts,
		"sdk/zig/src/consts.zig":                                                           zigConsts,
		"examples/clojure/hello-world/resources/public/hello-world.html":                   helloWorldExample,
		"examples/dotnet/csharp/HelloWorld/wwwroot/hello-world.html":                       helloWorldExample,
		"examples/dotnet/fsharp/HelloWorld/wwwroot/hello-world.html":                       helloWorldExample,
		"examples/go/hello-world/hello-world.html":                                         helloWorldExample,
		"examples/php/hello-world/public/hello-world.html":                                 helloWorldExamplePHP,
		"examples/zig/httpz/hello-world/src/hello-world.html":                              helloWorldExample,
		"examples/zig/tokamak/hello-world/hello-world.html":                                helloWorldExample,
		"examples/ruby/hello-world/hello-world.html":                                       helloWorldExample,
		"examples/rust/axum/hello-world/hello-world.html":                                  helloWorldExample,
		"examples/rust/rocket/hello-world/hello-world.html":                                helloWorldExample,
	}

	for path, tmplFn := range templates {
		log.Printf("Writing %s...", path)
		contents := strings.TrimSpace(tmplFn(Consts))
		if err := os.WriteFile(path, []byte(contents), 0o644); err != nil {
			return fmt.Errorf("error writing version file: %w", err)
		}
	}

	return nil
}

func durationToMs(d time.Duration) int {
	return int(d.Milliseconds())
}

func compressWithBrotli(data []byte) ([]byte, error) {
	// Check if brotli CLI is available
	if _, err := exec.LookPath("brotli"); err == nil {
		// Use CLI version of brotli
		cmd := exec.Command("brotli", "-c", "-q", "11", "-")
		cmd.Stdin = bytes.NewReader(data)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			log.Print("Using brotli CLI for compression")
			return out.Bytes(), nil
		}
	}

	// Fallback to Go library
	log.Print("Using Go brotli library for compression")
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)

	w := brotli.NewWriterV2(buf, brotli.BestCompression)
	if _, err := w.Write(data); err != nil {
		return nil, err
	}
	w.Close()

	result := make([]byte, buf.Len())
	copy(result, buf.Bytes())
	return result, nil
}
