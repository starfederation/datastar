package datastar

import (
	"fmt"

	"github.com/a-h/templ"
	"github.com/delaneyj/gostar/elements"
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

func (sse *ServerSentEventGenerator) MergeFragmentTempl(c templ.Component, opts ...MergeFragmentOption) error {
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

func (sse *ServerSentEventGenerator) MergeFragmentGostar(child elements.ElementRenderer, opts ...MergeFragmentOption) error {
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
	return fmt.Sprintf(`sse('%s',{method:'get'})`, fmt.Sprintf(urlFormat, args...))
}

func PostSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`sse('%s',{method:'post'})`, fmt.Sprintf(urlFormat, args...))
}

func PutSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`sse('%s',{method:'put'})`, fmt.Sprintf(urlFormat, args...))
}

func PatchSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`sse('%s',{method:'patch'})`, fmt.Sprintf(urlFormat, args...))
}

func DeleteSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`sse('%s',{method:'delete'})`, fmt.Sprintf(urlFormat, args...))
}
