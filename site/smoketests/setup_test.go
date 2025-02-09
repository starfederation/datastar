package smoketests

import (
	"context"
	"fmt"
	"sync"
	"testing"
	"time"

	"github.com/delaneyj/toolbelt"
	"github.com/go-rod/rod"
	"github.com/starfederation/datastar/site"
	"github.com/stretchr/testify/require"
)

type runnerFn func(name string, fn func(t *testing.T, page *rod.Page))

func setupPageTest(t *testing.T, subURL string, gen func(runner runnerFn)) {
	t.Parallel()
	browser := rod.New().MustConnect()
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	port, err := toolbelt.FreePort()
	require.NoError(t, err)

	baseURL := fmt.Sprintf("http://localhost:%d", port)

	readyCh := make(chan struct{})
	go site.RunBlocking(port, readyCh)(ctx)
	<-readyCh

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
