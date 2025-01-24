package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"log/slog"
	"os"
	"os/signal"
	"syscall"

	"github.com/delaneyj/toolbelt"
	"github.com/joho/godotenv"
	"github.com/starfederation/datastar/site"
)

const (
	port = 8080
)

var (
	useSiteSearchFeature = false
	enabledFeatures      = make(site.FeatureFlags)
)

func main() {
	godotenv.Load()
	log.SetFlags(log.LstdFlags | log.Lshortfile)

	logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))
	logger.Info("Starting Docs Server", "url", fmt.Sprintf("http://localhost:%d", port))
	defer logger.Info("Stopping Docs Server")

	flag.BoolVar(&useSiteSearchFeature, "enable-search", true, "Enables the site search feature")

	if useSiteSearchFeature {
		logger.Info("Search feature enabled")
		enabledFeatures[site.EnableSearchFlag] = useSiteSearchFeature
	}

	ctx := context.Background()

	if err := run(ctx, enabledFeatures); err != nil {
		logger.Error("Error running docs server", slog.Any("err", err))
		os.Exit(1)
	}

}

func run(ctx context.Context, enabledFeatures site.FeatureFlags) error {
	ctx, stop := signal.NotifyContext(ctx, syscall.SIGINT, syscall.SIGTERM)
	defer stop()

	eg := toolbelt.NewErrGroupSharedCtx(
		ctx,
		site.RunBlocking(port, nil, enabledFeatures),
	)
	if err := eg.Wait(); err != nil {
		return fmt.Errorf("error running docs server: %w", err)
	}

	return nil
}
