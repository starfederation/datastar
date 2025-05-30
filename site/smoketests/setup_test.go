package smoketests

import (
	"context"
	"fmt"
	"os"
	"runtime"
	"sync"
	"testing"
	"time"

	"github.com/Jeffail/gabs/v2"
	"github.com/delaneyj/toolbelt"
	"github.com/go-rod/rod"
	"github.com/starfederation/datastar/site"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var (
	baseURL     string
	browserPool rod.Pool[rod.Browser]
)

func TestMain(m *testing.M) {
	if err := os.Chdir("../../"); err != nil {
		panic(fmt.Errorf("could not change the working directory: %w", err))
	}

	ctx := context.Background()

	port, err := toolbelt.FreePort()
	if err != nil {
		panic(fmt.Errorf("could not obtain a free port: %w", err))
	}

	baseURL = fmt.Sprintf("http://localhost:%d", port)

	readyCh := make(chan struct{})
	go site.RunBlocking(port, readyCh)(ctx)
	<-readyCh

	browserPool = rod.NewBrowserPool(runtime.NumCPU())

	m.Run()

	ctx.Done()
}

func create() *rod.Browser {
	browser := rod.New().MustConnect()
	return browser
}

type runnerFn func(name string, fn func(t *testing.T, page *rod.Page))

func setupPageTest(t *testing.T, subURL string, gen func(runner runnerFn)) {
	t.Parallel()
	browser := browserPool.MustGet(create)
	defer browserPool.Put(browser)

	page := browser.MustIncognito().MustPage(fmt.Sprintf("%s/%s", baseURL, subURL))
	require.NotNil(t, page)
	t.Cleanup(page.MustClose)

	wg := &sync.WaitGroup{}

	runner := func(name string, fn func(t *testing.T, page *rod.Page)) {
		wg.Add(1)
		defer wg.Done()
		fn(t, page)
	}

	gen(runner)

	wg.Wait()
}

func setupPageTestOnLoad(t *testing.T, subURL string) {
	setupPageTest(t, subURL, func(runner runnerFn) {
		runner(subURL, func(t *testing.T, page *rod.Page) {
			result := page.MustElement("#result")
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "1")
		})
	})
}

func setupPageTestOnDelay(t *testing.T, subURL string) {
	setupPageTest(t, subURL, func(runner runnerFn) {
		runner(subURL, func(t *testing.T, page *rod.Page) {
			result := page.MustElement("#result")
			page.WaitStable(100 * time.Millisecond)
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "1")
		})
	})
}

func setupPageTestOnClick(t *testing.T, subURL string) {
	setupPageTest(t, subURL, func(runner runnerFn) {
		runner(subURL, func(t *testing.T, page *rod.Page) {
			result := page.MustElement("#result")
			before, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, before, "0")

			clickable := page.MustElement("#clickable")
			clickable.MustClick()
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "1")
		})
	})
}

func setupPageTestOnPopulate(t *testing.T, subURL string, value string) {
	setupPageTest(t, subURL, func(runner runnerFn) {
		runner(subURL, func(t *testing.T, page *rod.Page) {
			result := page.MustElement("#result")
			before, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, before, "0")

			populatable := page.MustElement("#populatable")
			populatable.MustInput(value)
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "1")
		})
	})
}

func setupPageTestOnPopulateAndClick(t *testing.T, subURL string, value string) {
	setupPageTest(t, subURL, func(runner runnerFn) {
		runner(subURL, func(t *testing.T, page *rod.Page) {
			result := page.MustElement("#result")
			before, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, before, "0")

			populatable := page.MustElement("#populatable")
			populatable.MustInput(value)
			clickable := page.MustElement("#clickable")
			clickable.MustClick()
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "1")
		})
	})
}

func setupPageTestOnSelect(t *testing.T, subURL string) {
	setupPageTest(t, subURL, func(runner runnerFn) {
		runner(subURL, func(t *testing.T, page *rod.Page) {
			result := page.MustElement("#result")
			before, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, before, "0")

			selectable := page.MustElement("#selectable")
			selectable.MustSelect("bar")
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "1")
		})
	})
}

func getLocalStoragePath(t *testing.T, page *rod.Page, path string) string {
	fromLocalStorage := page.MustEval(`k => localStorage[k]`, "datastar")
	marshalled := fromLocalStorage.String()
	c, err := gabs.ParseJSON([]byte(marshalled))
	assert.NoError(t, err)
	actual, ok := c.Path(path).Data().(string)
	assert.True(t, ok)
	return actual
}
