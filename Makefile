SWITCH ?= rowtype
OPAM = opam exec --switch=$(SWITCH) --

DOCS_PORT ?= 4321
# The container runtime the tests start Postgres in: podman or docker.
RUNTIME ?= podman
# The reference is odoc's, for rowtype's own packages and nothing under them.
REFERENCE = rowtype rowtype-postgres rowtype-sqlite rowtype-migrate odoc.support

.DEFAULT_GOAL := help
.PHONY: help setup build test lint fmt docs docs-serve release

help: ## list targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

setup: ## create the switch and install dependencies
	@$(RUNTIME) --version >/dev/null 2>&1 || echo "The tests need $(RUNTIME), or RUNTIME=docker for Docker."
	@command -v bun >/dev/null 2>&1 || echo "The documentation site needs Bun 1.3 or later."
	@opam switch list --short | grep -qx $(SWITCH) || opam switch create $(SWITCH) --empty -y
	opam repo add open-opam https://github.com/open-opam/opam-repository.git --switch=$(SWITCH) --rank=1
	opam install . --switch=$(SWITCH) --deps-only --with-test --with-dev-setup -y

build: ## build everything
	$(OPAM) dune build @all

# One Postgres for the whole run, in $(RUNTIME), removed as the run ends
# whatever its result: rowtype-test hands it to every suite as
# TEST_DATABASE_URL, and each makes its databases in it; the container it
# runs in is ROWTYPE_TEST_CONTAINER, where a dump runs pg_dump and
# rowtype-test's own suite starts its containers.
test: ## run the tests
	$(OPAM) dune build testing/bin/main.exe
	$(OPAM) ./_build/default/testing/bin/main.exe run --runtime $(RUNTIME) -- \
	  $(OPAM) dune test --force

# Release is the profile opam installs with.
lint: ## check formatting, docs and the release build
	$(OPAM) dune build @all @fmt @doc
	$(OPAM) dune build --profile release @all

fmt: ## format the code
	$(OPAM) dune fmt

# The site is Astro's, on Starlight in Hypha's theme. Bun's dependencies are
# installed again whenever the lockfile or the manifest changes, so no build
# runs against what an older checkout installed.
docs/node_modules: docs/package.json docs/bun.lock
	cd docs && bun install --frozen-lockfile
	@touch docs/node_modules

# odoc's reference from the .mli files is copied into public/, which
# Starlight serves as it is, then the site is built into _build/docs.
docs: docs/node_modules ## build the documentation site
	$(OPAM) dune build @doc
	rm -rf docs/public/reference
	mkdir -p docs/public/reference
	cd _build/default/_doc/_html && cp -R $(REFERENCE) $(CURDIR)/docs/public/reference/
	chmod -R u+w docs/public/reference
	cd docs && bun --bun astro build

# The built site, as Pages serves it: Pagefind's index exists only in a
# build, and a page served as it changes loads its styles after its text.
docs-serve: docs ## serve the built documentation site
	cd docs && bun --bun astro preview --open --port $(DOCS_PORT)

release: ## tag VERSION and make its release
	@test "$$(git show HEAD:CHANGES.md | grep -m1 '^## ' | cut -d' ' -f2)" = "$(VERSION)" \
	  || { echo "CHANGES.md must open with ## $(VERSION)" >&2; exit 2; }
	open-opam release $(VERSION)
