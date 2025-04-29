package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/starfederation/datastar/sdk/go/datastar"
	"github.com/stretchr/testify/assert"
)

func TestExampleMergeOptions(t *testing.T) {

	getTarget := func(page *rod.Page) *rod.Element {
		target := page.MustElement("#imTheTarget")
		assert.NotNil(t, target)
		return target
	}

	setupMergeModeTest := func(
		t *testing.T, mergeMode datastar.FragmentMergeMode, page *rod.Page,
		fn func(t *testing.T, page *rod.Page),
	) {
		mmStr := string(mergeMode)
		assert.NotNil(t, page)

		initial, _ := getTarget(page).Attribute("style")
		assert.Nil(t, initial)

		mergeModeBtn := page.MustElement("#" + mmStr)

		wait := page.MustWaitRequestIdle()
		mergeModeBtn.MustClick()
		wait()

		fn(t, page)
	}

	t.Run(string(datastar.FragmentMergeModeMorph), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModeMorph), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModeMorph, page,
					func(t *testing.T, page *rod.Page) {
						target := getTarget(page)
						waitForElementWithIDToStartWith(t, page, target, "Update")
						result := target.MustAttribute("style")
						assert.Contains(t, *result, "background-color:#a6cee3")
					},
				)
			})
		})
	})

	t.Run(string(datastar.FragmentMergeModeInner), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModeInner), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModeInner, page,
					func(t *testing.T, page *rod.Page) {
						target := getTarget(page)
						waitForElementWithIDToStartWith(t, page, target, "Update")
						result := target.MustText()
						assert.Contains(t, result, "Update")
					},
				)
			})
		})
	})

	t.Run(string(datastar.FragmentMergeModeOuter), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModeOuter), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModeOuter, page,
					func(t *testing.T, page *rod.Page) {
						target := getTarget(page)
						result := target.MustText()
						assert.Contains(t, result, "Update")
					},
				)
			})
		})
	})

	t.Run(string(datastar.FragmentMergeModePrepend), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModePrepend), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModePrepend, page,
					func(t *testing.T, page *rod.Page) {
						target := getTarget(page)
						actualTarget := target.MustElements("div")[0]
						result := actualTarget.MustHTML()
						assert.Contains(t, result, "background-color:#33a02c")
					},
				)
			})
		})
	})

	t.Run(string(datastar.FragmentMergeModeAppend), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModeAppend), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModeAppend, page,
					func(t *testing.T, page *rod.Page) {
						target := getTarget(page)
						actualTarget := target.MustElement("div")
						result := actualTarget.MustHTML()
						assert.Contains(t, result, "background-color:#fb9a99")
					},
				)
			})
		})
	})

	t.Run(string(datastar.FragmentMergeModeBefore), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModeBefore), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModeBefore, page,
					func(t *testing.T, page *rod.Page) {
						target := getTarget(page)
						actualTarget := target.MustParent().MustElement("div")
						result := actualTarget.MustHTML()
						assert.Contains(t, result, "background-color:#e31a1c")
					},
				)
			})
		})
	})

	t.Run(string(datastar.FragmentMergeModeAfter), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModeAfter), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModeAfter, page,
					func(t *testing.T, page *rod.Page) {
						target := getTarget(page)
						actualTarget := target.MustNext()
						result := actualTarget.MustHTML()
						assert.Contains(t, result, "background-color:#fdbf6f")
					},
				)
			})
		})
	})

	t.Run(string(datastar.FragmentMergeModeUpsertAttributes), func(t *testing.T) {
		setupPageTest(t, "examples/merge_options", func(runner runnerFn) {
			runner(string(datastar.FragmentMergeModeUpsertAttributes), func(t *testing.T, page *rod.Page) {
				setupMergeModeTest(
					t, datastar.FragmentMergeModeUpsertAttributes, page,
					func(t *testing.T, page *rod.Page) {
						// waitForElementWithIDToStartWith(t, page, target, "Update")
						target := getTarget(page)
						result := target.MustAttribute("style")
						assert.NotNil(t, result)
						assert.Contains(t, *result, "background-color:#ff7f00")
					},
				)
			})
		})
	})

}
