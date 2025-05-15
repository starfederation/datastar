# Datastar Razor Pages

# Development
```shell
bun install
```

```shell
bun run dev
```

# Release

## Build
```shell
docker build -t starfederation-datastar-razorpages:latest -f Dockerfile ./../
```

## Deploy
```shell
fly deploy ../ --dockerfile ./Examples/StarFederation.Datastar.RazorPages/Dockerfile --config ./Examples/StarFederation.Datastar.RazorPages/fly.toml
```