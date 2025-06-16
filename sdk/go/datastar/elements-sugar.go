package datastar

import (
	"context"
	"fmt"
	"io"

	"github.com/valyala/bytebufferpool"
)

// ValidElementMergeTypes is a list of valid element merge modes.
var ValidElementMergeTypes = []ElementPatchMode{
	ElementPatchModeOuter,
	ElementPatchModeInner,
	ElementPatchModeRemove,
	ElementPatchModePrepend,
	ElementPatchModeAppend,
	ElementPatchModeBefore,
	ElementPatchModeAfter,
	ElementPatchModeReplace,
}

// ElementMergeTypeFromString converts a string to a [ElementPatchMode].
func ElementMergeTypeFromString(s string) (ElementPatchMode, error) {
	switch s {
	case "outer":
		return ElementPatchModeOuter, nil
	case "inner":
		return ElementPatchModeInner, nil
	case "remove":
		return ElementPatchModeRemove, nil
	case "prepend":
		return ElementPatchModePrepend, nil
	case "append":
		return ElementPatchModeAppend, nil
	case "before":
		return ElementPatchModeBefore, nil
	case "after":
		return ElementPatchModeAfter, nil
	case "replace":
		return ElementPatchModeReplace, nil
	default:
		return "", fmt.Errorf("invalid element merge type: %s", s)
	}
}

// WithMergeOuter creates a PatchElementOption that merges elements using the outer mode.
func WithMergeOuter() PatchElementOption {
	return WithMergeMode(ElementPatchModeOuter)
}

// WithMergeInner creates a PatchElementOption that merges elements using the inner mode.
func WithMergeInner() PatchElementOption {
	return WithMergeMode(ElementPatchModeInner)
}

// WithMergeRemove creates a PatchElementOption that removes elements from the DOM.
func WithMergeRemove() PatchElementOption {
	return WithMergeMode(ElementPatchModeRemove)
}

// WithMergePrepend creates a PatchElementOption that merges elements using the prepend mode.
func WithMergePrepend() PatchElementOption {
	return WithMergeMode(ElementPatchModePrepend)
}

// WithMergeAppend creates a PatchElementOption that merges elements using the append mode.
func WithMergeAppend() PatchElementOption {
	return WithMergeMode(ElementPatchModeAppend)
}

// WithMergeBefore creates a PatchElementOption that merges elements using the before mode.
func WithMergeBefore() PatchElementOption {
	return WithMergeMode(ElementPatchModeBefore)
}

// WithMergeAfter creates a PatchElementOption that merges elements using the after mode.
func WithMergeAfter() PatchElementOption {
	return WithMergeMode(ElementPatchModeAfter)
}

// WithMergeReplace creates a PatchElementOption that replaces elements without morphing.
// This mode does not use morphing and will completely replace the element, resetting any related state.
func WithMergeReplace() PatchElementOption {
	return WithMergeMode(ElementPatchModeReplace)
}

// WithSelectorID is a convenience wrapper for [WithSelector] option
// equivalent to calling `WithSelector("#"+id)`.
func WithSelectorID(id string) PatchElementOption {
	return WithSelector("#" + id)
}

// WithViewTransitions enables the use of view transitions when merging elements.
func WithViewTransitions() PatchElementOption {
	return func(o *patchElementOptions) {
		o.UseViewTransitions = true
	}
}

// WithoutViewTransitions disables the use of view transitions when merging elements.
func WithoutViewTransitions() PatchElementOption {
	return func(o *patchElementOptions) {
		o.UseViewTransitions = false
	}
}

// PatchElementf is a convenience wrapper for [PatchElements] option
// equivalent to calling `PatchElements(fmt.Sprintf(format, args...))`.
func (sse *ServerSentEventGenerator) PatchElementf(format string, args ...any) error {
	return sse.PatchElements(fmt.Sprintf(format, args...))
}

// TemplComponent satisfies the component rendering interface for HTML template engine [Templ].
// This separate type ensures compatibility with [Templ] without imposing a dependency requirement
// on those who prefer to use a different template engine.
//
// [Templ]: https://templ.guide/
type TemplComponent interface {
	Render(ctx context.Context, w io.Writer) error
}

// PatchElementTempl is a convenience adaptor of [sse.PatchElements] for [TemplComponent].
func (sse *ServerSentEventGenerator) PatchElementTempl(c TemplComponent, opts ...PatchElementOption) error {
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)
	if err := c.Render(sse.Context(), buf); err != nil {
		return fmt.Errorf("failed to merge element: %w", err)
	}
	if err := sse.PatchElements(buf.String(), opts...); err != nil {
		return fmt.Errorf("failed to patch element: %w", err)
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

// PatchElementGostar is a convenience adaptor of [sse.PatchElements] for [GoStarElementRenderer].
func (sse *ServerSentEventGenerator) PatchElementGostar(child GoStarElementRenderer, opts ...PatchElementOption) error {
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)
	if err := child.Render(buf); err != nil {
		return fmt.Errorf("failed to render: %w", err)
	}
	if err := sse.PatchElements(buf.String(), opts...); err != nil {
		return fmt.Errorf("failed to patch element: %w", err)
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
// It uses PatchElements with the remove merge mode and the specified selector.
func (sse *ServerSentEventGenerator) RemoveElement(selector string, opts ...PatchElementOption) error {
	// Prepend the remove mode option
	allOpts := append([]PatchElementOption{WithMergeRemove(), WithSelector(selector)}, opts...)
	return sse.PatchElements("", allOpts...)
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
