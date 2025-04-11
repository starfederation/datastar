package datastar

import (
	"context"
	"fmt"
	"io"

	"github.com/valyala/bytebufferpool"
)

var ValidFragmentMergeTypes = []FragmentMergeMode{
	FragmentMergeModeMorph,
	FragmentMergeModeInner,
	FragmentMergeModeOuter,
	FragmentMergeModePrepend,
	FragmentMergeModeAppend,
	FragmentMergeModeBefore,
	FragmentMergeModeAfter,
	FragmentMergeModeUpsertAttributes,
}

func FragmentMergeTypeFromString(s string) (FragmentMergeMode, error) {
	for _, t := range ValidFragmentMergeTypes {
		if string(t) == s {
			return t, nil
		}
	}
	return "", fmt.Errorf("invalid fragment merge type: %s", s)
}

func WithMergeMorph() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModeMorph)
}

func WithMergeInner() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModeInner)
}

func WithMergeOuter() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModeOuter)
}

func WithMergePrepend() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModePrepend)
}

func WithMergeAppend() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModeAppend)
}

func WithMergeBefore() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModeBefore)
}

func WithMergeAfter() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModeAfter)
}

func WithMergeUpsertAttributes() MergeFragmentOption {
	return WithMergeMode(FragmentMergeModeUpsertAttributes)
}

func WithSelectorID(id string) MergeFragmentOption {
	return WithSelector("#" + id)
}

func WithViewTransitions() MergeFragmentOption {
	return func(o *MergeFragmentOptions) {
		o.UseViewTransitions = true
	}
}

func WithoutViewTransitions() MergeFragmentOption {
	return func(o *MergeFragmentOptions) {
		o.UseViewTransitions = false
	}
}

func (sse *ServerSentEventGenerator) MergeFragmentf(format string, args ...any) error {
	return sse.MergeFragments(fmt.Sprintf(format, args...))
}

// TemplComponent satisfies the component rendering interface for HTML template engine [Templ].
// This separate type ensures compatibility with [Templ] without imposing a dependency requirement
// on those who prefer to use a different template engine.
//
// [Templ]: https://templ.guide/
type TemplComponent interface {
	Render(ctx context.Context, w io.Writer) error
}

// MergeFragmentTempl is a convenience adaptor of [sse.MergeFragments] for [TemplComponent].
func (sse *ServerSentEventGenerator) MergeFragmentTempl(c TemplComponent, opts ...MergeFragmentOption) error {
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)
	if err := c.Render(sse.Context(), buf); err != nil {
		return fmt.Errorf("failed to merge fragment: %w", err)
	}
	if err := sse.MergeFragments(buf.String(), opts...); err != nil {
		return fmt.Errorf("failed to merge fragment: %w", err)
	}
	return nil
}

// GoStarElementRenderer satisfies the component rendering interface for HTML template engine [GoStar].
// This separate type ensures compatibility with [GoStar] without imposing a dependency requirement
// on those who prefer to use a different template engine.
//
// [GoStar]: https://github.com/delaneyj/gostar
type GoStarElementRenderer interface {
	Render(w io.Writer) error
}

// MergeFragmentGostar is a convenience adaptor of [sse.MergeFragments] for [GoStarElementRenderer].
func (sse *ServerSentEventGenerator) MergeFragmentGostar(child GoStarElementRenderer, opts ...MergeFragmentOption) error {
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)
	if err := child.Render(buf); err != nil {
		return fmt.Errorf("failed to render: %w", err)
	}
	if err := sse.MergeFragments(buf.String(), opts...); err != nil {
		return fmt.Errorf("failed to merge fragment: %w", err)
	}
	return nil
}

func GetSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@get('%s')`, fmt.Sprintf(urlFormat, args...))
}

func PostSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@post('%s')`, fmt.Sprintf(urlFormat, args...))
}

func PutSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@put('%s')`, fmt.Sprintf(urlFormat, args...))
}

func PatchSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@patch('%s')`, fmt.Sprintf(urlFormat, args...))
}

func DeleteSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@delete('%s')`, fmt.Sprintf(urlFormat, args...))
}
