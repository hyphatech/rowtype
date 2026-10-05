# The opam switch is local (./_opam), so OCaml commands go through opam exec.
OPAM = opam exec --switch=$(CURDIR) --

PG_TEST_PORT ?= 5437
PG_TEST_URL  ?= postgres://rowtype:rowtype@localhost:$(PG_TEST_PORT)/rowtype
export PG_TEST_PORT

.DEFAULT_GOAL := help
.PHONY: help setup build test lint fmt doc db db-down

help: ## list targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

setup: ## create the local switch and install dependencies
	@docker compose version >/dev/null 2>&1 || echo "The tests need docker compose."
	@[ -d _opam ] || opam switch create . 5.5.1 --no-install -y
	opam install . --switch=$(CURDIR) --deps-only --with-test --with-dev-setup -y

build: ## build everything
	$(OPAM) dune build @all

# A squash dumps with the server's own pg_dump, the container's.
test: db ## run the tests
	ROWTYPE_TEST_PG=$(PG_TEST_URL) \
	  ROWTYPE_TEST_PG_DUMP="docker compose -f $(CURDIR)/compose.yaml exec -T db pg_dump -U rowtype -d {database}" \
	  $(OPAM) dune test --force

# Release is the profile opam installs with.
lint: ## check formatting, docs and the release build
	$(OPAM) dune build @all @fmt @doc
	$(OPAM) dune build --profile release @all

fmt: ## format the code
	$(OPAM) dune fmt

doc: ## build the API docs into _build/default/_doc/_html
	$(OPAM) dune build @doc

db: ## start the test database
	docker compose up -d --wait db

db-down: ## stop and delete the test database
	docker compose rm -sf db

