# Building Datastar using Docker

Datastar comes with a development environment for developing Datastar via a Docker container.

It takes care of the setup listed here for you in a Dockerized environment, which allows you to get it up and running quickly & easily.

See [DOCKER.md](DOCKER.md) for detailed instructions on using it.

# Building Datastar Natively

To run the examples website you will need the following:

- [Go](https://go.dev/)
- [Taskfile](https://taskfile.dev/)

To publish the library or if you're using a BSD operating system, you will also need:

- [pnpm](https://pnpm.io/)
- [Node and npm](https://nodejs.org/)

`task tools` will install all required tools

`task build` will build the library as well as sdk constant templates

The compiled files can be found in `./bundles`

Finally, `task` will build the library and serve the reference documentation website.

The website should be available on http://localhost:8080 with hot-reloading enabled.

The following example runs the `build` task via Docker.
This task will rebuild SDK constants.

```
make task "build -f"
```

## End to end tests

Once you have the website running, you should be able to run the
[rod](https://go-rod.github.io) tests.

```
$ task test
```
