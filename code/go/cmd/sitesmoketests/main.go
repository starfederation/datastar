package main

import (
	"context"
	"fmt"
	"log"

	"github.com/starfederation/datastar/code/go/site/smoketests"
)

func main() {
	ctx := context.Background()
	if err := run(ctx); err != nil {
		log.Fatal(err)
	}
}

func run(ctx context.Context) error {
	log.SetFlags(log.LstdFlags | log.Lshortfile)

	if err := smoketests.RunSmokeTests(ctx); err != nil {
		return fmt.Errorf("error running smoke tests: %w", err)
	}

	return nil
}
