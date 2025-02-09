package smoketests

import (
	"testing"
	"time"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleImgSrcBind(t *testing.T) {
	setupPageTest(t, "examples/img_src_bind", func(runner runnerFn) {
		runner("image source binding", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#file_upload > img").MustProperty("src").Str()
			assert.Equal(t, "https://picsum.photos/id/237/640/320", initial)

			page.MustElement("#file_upload > button").MustClick()
			page.MustWaitIdle()

			time.Sleep(100 * time.Millisecond)
			result := page.MustElement("#file_upload > img").MustProperty("src").Str()

			assert.NotEqual(t, initial, result)
		})
	})
}
