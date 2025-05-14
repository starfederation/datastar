# Datastar Razor Pages

## Build
```shell
docker build -t starfederation-datastar-razorpages:latest -f Dockerfile ./../
```

## Deploy
```shell
fly deploy ../../ --dockerfile ./Web/TerraScale.Web/Dockerfile --config ./Web/TerraScale.Web/fly.toml
```