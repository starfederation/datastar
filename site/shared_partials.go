package site

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"github.com/a-h/templ"
	"github.com/alecthomas/chroma"
	"github.com/alecthomas/chroma/formatters/html"
	"github.com/alecthomas/chroma/lexers"
	"github.com/alecthomas/chroma/styles"
	"github.com/delaneyj/toolbelt"
	"github.com/gomarkdown/markdown"
	"github.com/gomarkdown/markdown/ast"
	mdhtml "github.com/gomarkdown/markdown/html"
	"github.com/gomarkdown/markdown/parser"
	build "github.com/starfederation/datastar/build"
	datastar "github.com/starfederation/datastar/sdk/go"
	"github.com/valyala/bytebufferpool"
)

var (
	mdRenderer    func() *mdhtml.Renderer
	htmlHighlight func(w io.Writer, source, lang, defaultLang string) error
)

type CodeSnippet struct {
	Path               toolbelt.CasedString
	Extension          string
	Icon               string
	Content            string
	ContentHighlighted string
}

type CodeSnippetBlock struct {
	BasePath toolbelt.CasedString
	Snippets []CodeSnippet
}

type MarkdownData struct {
	Anchors     []string
	Title       string
	Description string
	Contents    string
}
type MarkdownDataset map[string]*MarkdownData

func markdownRenders(ctx context.Context, staticMdPath string) (MarkdownDataset, error) {
	if mdRenderer == nil {
		htmlFormatter := html.New(html.WithClasses(true), html.TabWidth(2))
		if htmlFormatter == nil {
			return nil, fmt.Errorf("couldn't create html formatter")
		}
		styleName := "nord"
		highlightStyle := styles.Get(styleName)
		if highlightStyle == nil {
			return nil, fmt.Errorf("couldn't find style %s", styleName)
		}
		highlightCSSBuffer := &bytes.Buffer{}
		if err := htmlFormatter.WriteCSS(highlightCSSBuffer, highlightStyle); err != nil {
			return nil, fmt.Errorf("error writing highlight css: %w", err)
		}
		highlightCSS = templ.ComponentFunc(func(ctx context.Context, w io.Writer) error {
			_, err := io.WriteString(w, fmt.Sprintf(`<style>%s</style>`, highlightCSSBuffer.String()))
			return err
		})
		// based on https://github.com/alecthomas/chroma/blob/master/quick/quick.go
		htmlHighlight = func(w io.Writer, source, lang, defaultLang string) error {
			if lang == "" {
				lang = defaultLang
			}
			l := lexers.Get(lang)
			if l == nil {
				l = lexers.Analyse(source)
			}
			if l == nil {
				l = lexers.Fallback
			}
			l = chroma.Coalesce(l)

			it, err := l.Tokenise(nil, source)
			if err != nil {
				return err
			}
			return htmlFormatter.Format(w, highlightStyle, it)
		}

		mdRenderer = func() *mdhtml.Renderer {
			return mdhtml.NewRenderer(mdhtml.RendererOptions{
				Flags: mdhtml.CommonFlags | mdhtml.HrefTargetBlank,
				RenderNodeHook: func(w io.Writer, node ast.Node, entering bool) (ast.WalkStatus, bool) {
					skipDefaultRenderer := false
					switch n := node.(type) {
					case *ast.CodeBlock:
						defaultLang := ""
						lang := string(n.Info)
						htmlHighlight(w, string(n.Literal), lang, defaultLang)
						skipDefaultRenderer = true
					case *ast.Heading:
						if entering {
							break
						}
						buf := bytebufferpool.Get()
						defer bytebufferpool.Put(buf)
						level := strconv.Itoa(n.Level)
						if level != "1" {
							buf.WriteString(`<a class="prose link-neutral" href="#`)
							buf.WriteString(n.HeadingID)
							buf.WriteString(`">#</a>`)
						}
						buf.WriteString(`</h`)
						buf.WriteString(level)
						buf.WriteString(`>`)
						buf.WriteTo(w)
						skipDefaultRenderer = true
					}
					return ast.GoToNext, skipDefaultRenderer
				},
			})
		}
	}

	mdDir := "static/md/" + staticMdPath
	docs, err := staticFS.ReadDir(mdDir)
	if err != nil {
		return nil, fmt.Errorf("error reading docs dir: %w", err)
	}

	// regExpImg := regexp.MustCompile(`(?P<whole>!\[[^\]]+]\((?P<path>[^)]+)\))`)
	// prefix := []byte("/static/")

	codeSnippets := regexp.MustCompile(`!!!CODE_SNIPPET:(?<basePath>[^!]*)!!!`)
	// Icon or mascot from https://icones.js.org/collection/vscode-icons

	res := MarkdownDataset{}

	titleTrimmer := regexp.MustCompile(`^#+\s*`)

	for _, de := range docs {
		fullPath := mdDir + "/" + de.Name()

		b, err := staticFS.ReadFile(fullPath)
		if err != nil {
			return nil, fmt.Errorf("error reading doc %s: %w", de.Name(), err)
		}

		// Package version
		b = bytes.ReplaceAll(b, []byte("PACKAGE_VERSION"), []byte(datastar.Version))

		// code snippets
		for _, matches := range codeSnippets.FindAllSubmatch(b, -1) {
			fullMatch := matches[0]
			basePath := string(matches[1])
			fullBasePath := "static/code_snippets/" + basePath
			fullWithTestExtension := fullBasePath + ".txt"
			baseDir := filepath.Dir(fullWithTestExtension)
			fileEntries, err := staticFS.ReadDir(baseDir)
			if err != nil {
				return nil, fmt.Errorf("error reading code snippet dir %s: %w", baseDir, err)
			}
			if len(fileEntries) == 0 {
				return nil, fmt.Errorf("no files found in code snippet dir %s", baseDir)
			}

			snippetBlock := CodeSnippetBlock{
				BasePath: toolbelt.ToCasedString(basePath),
				Snippets: make([]CodeSnippet, 0, len(fileEntries)),
			}

			// Find the file with the full base path prefix
			for _, fileEntry := range fileEntries {
				name := fileEntry.Name()
				fileFullPath := filepath.Join(baseDir, name)
				if !strings.HasPrefix(fileFullPath, fullBasePath) {
					continue
				}

				ext := strings.TrimSuffix(filepath.Ext(name), "snippet")[1:] // remove the dot

				codeSnippetRaw, err := staticFS.ReadFile(fileFullPath)
				if err != nil {
					return nil, fmt.Errorf("error reading code snippet %s: %w", fileFullPath, err)
				}
				codeSnippet := string(codeSnippetRaw)

				buf := bytebufferpool.Get()
				defer bytebufferpool.Put(buf)

				if err := htmlHighlight(buf, codeSnippet, ext, ""); err != nil {
					return nil, fmt.Errorf("error highlighting code snippet %s: %w", fileFullPath, err)
				}

				icon := ""
				for _, lang := range build.Consts.SDKLanguages {
					if lang.FileExtension == ext {
						icon = lang.Icon
						break
					}
				}
				if icon == "" {
					icon = "vscode-icons:file-type-text"
				}

				snippet := CodeSnippet{
					Extension:          ext,
					Icon:               icon,
					Content:            codeSnippet,
					ContentHighlighted: buf.String(),
				}
				snippetBlock.Snippets = append(snippetBlock.Snippets, snippet)
			}
			slices.SortFunc(snippetBlock.Snippets, func(a, b CodeSnippet) int {
				return strings.Compare(a.Extension, b.Extension)
			})
			buf := bytebufferpool.Get()
			defer bytebufferpool.Put(buf)

			c := codeSnippetFragment(snippetBlock)
			c.Render(ctx, buf)

			b = bytes.ReplaceAll(b, fullMatch, buf.Bytes())
		}

		title := ""

		// Get all anchors
		anchors := []string{}
		lines := strings.Split(string(b), "\n")
		for _, line := range lines {
			if strings.HasPrefix(line, "#") {
				if title == "" {
					title = titleTrimmer.ReplaceAllString(line, "")
				}
				parts := strings.Split(line, " ")
				anchor := strings.Join(parts[1:], " ")
				anchors = append(anchors, anchor)
			}
		}

		mdParser := parser.NewWithExtensions(parser.CommonExtensions | parser.AutoHeadingIDs | parser.NoEmptyLineBeforeBlock | parser.Footnotes)
		doc := mdParser.Parse(b)
		renderedHTML := string(markdown.Render(doc, mdRenderer()))

		name := de.Name()[0 : len(de.Name())-3]

		res[name] = &MarkdownData{
			Anchors:     anchors,
			Title:       title,
			Description: "",
			Contents:    renderedHTML,
		}
	}

	return res, nil
}

func KVPairsAttrs(kvPairs ...string) templ.Attributes {
	if len(kvPairs)%2 != 0 {
		panic("kvPairs must be a multiple of 2")
	}
	attrs := templ.Attributes{}
	for i := 0; i < len(kvPairs); i += 2 {
		attrs[kvPairs[i]] = kvPairs[i+1]
	}
	return attrs
}
