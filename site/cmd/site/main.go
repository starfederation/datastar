package main

import (
	"context"
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

func main() {
	godotenv.Load()
	log.SetFlags(log.LstdFlags | log.Lshortfile)

	logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))
	logger.Info("Starting Docs Server", "url", fmt.Sprintf("http://localhost:%d", port))
	defer logger.Info("Stopping Docs Server")

	ctx := context.Background()

	if err := run(ctx); err != nil {
		logger.Error("Error running docs server", slog.Any("err", err))
		os.Exit(1)
	}

}

func run(ctx context.Context) error {
	ctx, stop := signal.NotifyContext(ctx, syscall.SIGINT, syscall.SIGTERM)
	defer stop()

	eg := toolbelt.NewErrGroupSharedCtx(
		ctx,
		site.RunBlocking(port, nil),
	)
	if err := eg.Wait(); err != nil {
		return fmt.Errorf("error running docs server: %w", err)
	}

	return nil
}
