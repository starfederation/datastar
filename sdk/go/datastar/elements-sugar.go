package datastar

import (
	"context"
	"fmt"
	"io"

	"github.com/valyala/bytebufferpool"
)

// ValidElementMergeTypes is a list of valid element merge modes.
var ValidElementMergeTypes = []ElementMergeMode{
	ElementMergeModeOuter,
	ElementMergeModeInner,
	ElementMergeModeRemove,
	ElementMergeModePrepend,
	ElementMergeModeAppend,
	ElementMergeModeBefore,
	ElementMergeModeAfter,
	ElementMergeModeReplace,
}

// ElementMergeTypeFromString converts a string to a [ElementMergeMode].
func ElementMergeTypeFromString(s string) (ElementMergeMode, error) {
	switch s {
	case "outer":
		return ElementMergeModeOuter, nil
	case "inner":
		return ElementMergeModeInner, nil
	case "remove":
		return ElementMergeModeRemove, nil
	case "prepend":
		return ElementMergeModePrepend, nil
	case "append":
		return ElementMergeModeAppend, nil
	case "before":
		return ElementMergeModeBefore, nil
	case "after":
		return ElementMergeModeAfter, nil
	case "replace":
		return ElementMergeModeReplace, nil
	default:
		return "", fmt.Errorf("invalid element merge type: %s", s)
	}
}

// WithMergeOuter creates a MergeElementOption that merges elements using the outer mode.
func WithMergeOuter() MergeElementOption {
	return WithMergeMode(ElementMergeModeOuter)
}

// WithMergeInner creates a MergeElementOption that merges elements using the inner mode.
func WithMergeInner() MergeElementOption {
	return WithMergeMode(ElementMergeModeInner)
}

// WithMergeRemove creates a MergeElementOption that removes elements from the DOM.
func WithMergeRemove() MergeElementOption {
	return WithMergeMode(ElementMergeModeRemove)
}

// WithMergePrepend creates a MergeElementOption that merges elements using the prepend mode.
func WithMergePrepend() MergeElementOption {
	return WithMergeMode(ElementMergeModePrepend)
}

// WithMergeAppend creates a MergeElementOption that merges elements using the append mode.
func WithMergeAppend() MergeElementOption {
	return WithMergeMode(ElementMergeModeAppend)
}

// WithMergeBefore creates a MergeElementOption that merges elements using the before mode.
func WithMergeBefore() MergeElementOption {
	return WithMergeMode(ElementMergeModeBefore)
}

// WithMergeAfter creates a MergeElementOption that merges elements using the after mode.
func WithMergeAfter() MergeElementOption {
	return WithMergeMode(ElementMergeModeAfter)
}

// WithMergeReplace creates a MergeElementOption that replaces elements without morphing.
// This mode does not use morphing and will completely replace the element, resetting any related state.
func WithMergeReplace() MergeElementOption {
	return WithMergeMode(ElementMergeModeReplace)
}

// WithSelectorID is a convenience wrapper for [WithSelector] option
// equivalent to calling `WithSelector("#"+id)`.
func WithSelectorID(id string) MergeElementOption {
	return WithSelector("#" + id)
}

// WithViewTransitions enables the use of view transitions when merging elements.
func WithViewTransitions() MergeElementOption {
	return func(o *mergeElementOptions) {
		o.UseViewTransitions = true
	}
}

// WithoutViewTransitions disables the use of view transitions when merging elements.
func WithoutViewTransitions() MergeElementOption {
	return func(o *mergeElementOptions) {
		o.UseViewTransitions = false
	}
}

// MergeElementf is a convenience wrapper for [MergeElements] option
// equivalent to calling `MergeElements(fmt.Sprintf(format, args...))`.
func (sse *ServerSentEventGenerator) MergeElementf(format string, args ...any) error {
	return sse.MergeElements(fmt.Sprintf(format, args...))
}

// TemplComponent satisfies the component rendering interface for HTML template engine [Templ].
// This separate type ensures compatibility with [Templ] without imposing a dependency requirement
// on those who prefer to use a different template engine.
//
// [Templ]: https://templ.guide/
type TemplComponent interface {
	Render(ctx context.Context, w io.Writer) error
}

// MergeElementTempl is a convenience adaptor of [sse.MergeElements] for [TemplComponent].
func (sse *ServerSentEventGenerator) MergeElementTempl(c TemplComponent, opts ...MergeElementOption) error {
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)
	if err := c.Render(sse.Context(), buf); err != nil {
		return fmt.Errorf("failed to merge element: %w", err)
	}
	if err := sse.MergeElements(buf.String(), opts...); err != nil {
		return fmt.Errorf("failed to merge element: %w", err)
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

// MergeElementGostar is a convenience adaptor of [sse.MergeElements] for [GoStarElementRenderer].
func (sse *ServerSentEventGenerator) MergeElementGostar(child GoStarElementRenderer, opts ...MergeElementOption) error {
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)
	if err := child.Render(buf); err != nil {
		return fmt.Errorf("failed to render: %w", err)
	}
	if err := sse.MergeElements(buf.String(), opts...); err != nil {
		return fmt.Errorf("failed to merge element: %w", err)
	}
	return nil
}

// GetSSE is a convenience method for generating Datastar backend [get] action attribute.
//
// [get]: https://data-star.dev/reference/action_plugins#get
func GetSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@get('%s')`, fmt.Sprintf(urlFormat, args...))
}

// PostSSE is a convenience method for generating Datastar backend [post] action attribute.
//
// [post]: https://data-star.dev/reference/action_plugins#post
func PostSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@post('%s')`, fmt.Sprintf(urlFormat, args...))
}

// PutSSE is a convenience method for generating Datastar backend [put] action attribute.
//
// [put]: https://data-star.dev/reference/action_plugins#put
func PutSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@put('%s')`, fmt.Sprintf(urlFormat, args...))
}

// PatchSSE is a convenience method for generating Datastar backend [patch] action attribute.
//
// [patch]: https://data-star.dev/reference/action_plugins#patch
func PatchSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@patch('%s')`, fmt.Sprintf(urlFormat, args...))
}

// DeleteSSE is a convenience method for generating Datastar backend [delete] action attribute.
//
// [delete]: https://data-star.dev/reference/action_plugins#delete
func DeleteSSE(urlFormat string, args ...any) string {
	return fmt.Sprintf(`@delete('%s')`, fmt.Sprintf(urlFormat, args...))
}

// RemoveElement is a convenience method for removing elements from the DOM.
// It uses MergeElements with the remove merge mode and the specified selector.
func (sse *ServerSentEventGenerator) RemoveElement(selector string, opts ...MergeElementOption) error {
	// Prepend the remove mode option
	allOpts := append([]MergeElementOption{WithMergeRemove(), WithSelector(selector)}, opts...)
	return sse.MergeElements("", allOpts...)
}

// RemoveElementf is a convenience wrapper for RemoveElement that formats the selector string
// using the provided format and arguments similar to fmt.Sprintf.
func (sse *ServerSentEventGenerator) RemoveElementf(selectorFormat string, args ...any) error {
	selector := fmt.Sprintf(selectorFormat, args...)
	return sse.RemoveElement(selector)
}

// RemoveElementByID is a convenience wrapper for RemoveElement that removes an element by its ID.
// Equivalent to calling RemoveElement("#"+id).
func (sse *ServerSentEventGenerator) RemoveElementByID(id string) error {
	return sse.RemoveElement("#" + id)
}
