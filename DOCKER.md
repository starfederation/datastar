# Building Datastar using Docker

Datastar comes with a development environment for developing Datastar via a Docker container.

It takes care of the setup listed in the for you in a Dockerized environment, which allows you to get it up and running quickly & easily.

The only requirement is that you have [Docker](https://www.docker.com/products/docker-desktop) installed (you do not need `golang`, `tmpl`, `npm`, `pnpm` or any other project dependencies installed locally).

## Why Docker?

Developers who have adopted [Docker](https://www.docker.com/) for a containerized approach to development are used to not having to install a matching development infrastructure each time they approach a project.

This allows you to "shrink-wrap" the devops needed to run a project in a container, which will run anywhere, on any machine, without having to do any meticulous setup.

It also allows you to easily swap between basic things like Go versions, without affecting your local computer.

## Quick Start

In terminal, `cd` to the `datastar/` directory, and then type:

```
make dev
```

The first time you run this command, it may take a bit of time to build the Docker image, and download all of the appropriate packages, and cache them locally.

Then just navigate to `http://localhost:8080` in your browser, and Datastar site from `site` will be up and running.

You can freely make changes to the `library` Datastar codebase, and the changes will be rebuilt and reload automatically.

You can also make changes to the `site` Datastar website backend, and the backend will be rebuilt and reload automatically.

## Using Datastar Docker Dev

Datastar Docker Dev uses the venerable `make` command to automate setup and access to the Docker containers used.

It uses `make` because it's available pre-installed on any development machine.

See the [Using Make & Makefiles to Automate your Frontend Workflow](https://nystudio107.com/blog/using-make-makefiles-to-automate-your-frontend-workflow) article for more on `make`.

The make tool is available for just about every platform you can imagine, and is installed with the [XCode CLI Tools](https://www.embarcadero.com/starthere/xe5/mobdevsetup/ios/en/installing_the_commandline_tools.html) on the Mac, and [WSL2](https://docs.microsoft.com/en-us/windows/wsl/install-win10) on Windows. Probably you have these installed already if you’re doing development.

Below are details and options available in each of the provided `make` commands:

* `make dev` - starts up the Go website server for the backend with hot reloading as you make changes
* `make test` - run the project tests inside the Docker container
* `make task xxx` - runs the specified task from the `Taskfile.yml` inside of the Docker container
* `make ssh` - gives you a shell inside of the Docker container
* `make image-build` - rebuilds the Docker image from scratch (you will probably never need this)
* `make clean` - removes all build artifacts, including the Docker image & volumes, to start from scratch

### CLI Arguments

You can pass in optional CLI arguments to override the default settings Datastar dev uses:

* `TAG=` (default is defined in `Dockerfile-dev`) - allows you to specify the official [golang Docker image](https://hub.docker.com/_/golang) tag that should be used. Using this, you can change the version of Go the container runs, e.g.: `make image-build TAG="1.24"` will use the latest patch version of Go 1.24 official Docker image.

### Terminating

To terminate the `datastar-dev` Docker container, enter `Control-C` in the terminal.