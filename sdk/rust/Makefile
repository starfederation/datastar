.PHONY: all

all:
	@echo "Usage:"
	@echo "fmt                  - run the rust formatter"
	@echo "sort                 - sort TOML dependencies"
	@echo "lint                 - combine fmt+sort"
	@echo "check                - shallow check of rust code (pre-compile)"
	@echo "clippy               - run clippy checks"
	@echo "doc                  - doc checks"
	@echo "hack                 - test feature matrix compatibility"
	@echo "test                 - run all unit and doc tests"
	@echo "qa                   - combine lint+check+clippy+doc+hack+test"
	@echo "detect-unused-deps   - detect unused deps for removal"
	@echo "hello-axum           - run hello-world example using the Axum framework"
	@echo "hello-rama           - run hello-world example using the Rama framework"
	@echo "hello-rocket         - run hello-world example using the Rocket framework"
.PHONY:

fmt:
	cargo fmt --all

sort:
	cargo sort --grouped

lint: fmt sort

check:
	cargo check --all-targets --all-features

clippy:
	cargo clippy --all-targets --all-features

doc:
	RUSTDOCFLAGS="-D rustdoc::broken-intra-doc-links" cargo doc --all-features --no-deps

hack:
	cargo hack check --each-feature --no-dev-deps --workspace

test:
	cargo test --all-features --workspace

qa: lint check clippy doc test
	cd ../../examples/rust/axum/hello-world && make qa
	cd ../../examples/rust/rama/hello-world && make qa
	cd ../../examples/rust/rocket/hello-world && make qa

detect-unused-deps:
    # https://github.com/bnjbvr/cargo-machete
	cargo machete --skip-target-dir

hello-axum:
	cd ../../examples/rust/axum/hello-world && make run

hello-rama:
	cd ../../examples/rust/rama/hello-world && make run

hello-rocket:
	cd ../../examples/rust/rocket/hello-world && make run
