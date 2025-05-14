# Datastar Razor Pages

## Build
```shell
docker build -t starfederation-datastar-razorpages:latest -f Dockerfile ./../
```

## Deploy
```shell
fly deploy ../ --dockerfile ./Examples/Starfederation.Datastar.RazorPages/Dockerfile --config ./Examples/Starfederation.Datastar.RazorPages/fly.toml
```