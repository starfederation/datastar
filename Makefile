TAG?=1.24
CONTAINER?=$(shell basename $(CURDIR))-dev
DEV_PORT?=8080
IMAGE_INFO=$(shell docker image inspect $(CONTAINER):$(TAG))
IMAGE_NAME=${CONTAINER}:${TAG}
DOCKER_RUN=docker container run --rm -it -v "${CURDIR}":/app -v go-modules:/go/pkg/mod
ARCH=$(shell uname -m)

.PHONY: build clean dev image-build task test ssh

# Perform a dist build
build: image-check
	${DOCKER_RUN} --name ${CONTAINER}-$@ ${IMAGE_NAME} build
# Clean up all build artifacts to start from scratch
clean:
	docker image rm ${IMAGE_NAME}
	docker volume rm go-modules
# Run the development server
dev: --image-check
	${DOCKER_RUN} --name ${CONTAINER}-$@ -e DEV_PORT="${DEV_PORT}" -p ${DEV_PORT}:${DEV_PORT} ${IMAGE_NAME} -c 'task -w'
# Build the Docker image
image-build:
	docker build -f Dockerfile-dev . -t ${IMAGE_NAME} --build-arg TAG=${TAG} --no-cache
	${DOCKER_RUN} --name ${CONTAINER}-$@ ${IMAGE_NAME} -c 'task tools'
# Run the passed in task command
task: --image-check
	${DOCKER_RUN} --name ${CONTAINER}-$@ -e DEV_PORT="${DEV_PORT}" -p ${DEV_PORT}:${DEV_PORT} ${IMAGE_NAME} -c 'task $(filter-out $@,$(MAKECMDGOALS)) $(MAKEFLAGS)'
# Run the test suite
test: --image-check
	${DOCKER_RUN} --name ${CONTAINER}-$@ -e DEV_PORT="${DEV_PORT}" -p ${DEV_PORT}:${DEV_PORT} ${IMAGE_NAME} -c 'task test'
# Open a shell inside of the container
ssh: --image-check
	${DOCKER_RUN} --name ${CONTAINER}-$@ --entrypoint=/bin/sh ${IMAGE_NAME}
# Ensure the image has been created
--image-check:
ifeq ($(IMAGE_INFO), [])
--image-check: image-build
endif
%:
	@:
# ref: https://stackoverflow.com/questions/6273608/how-to-pass-argument-to-makefile-from-command-line
