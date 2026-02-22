GSH        := $(CURDIR)/.gerbil/bin/gsh
OILS_DIR   := _vendor/oils
SH_SPEC    := python3 $(CURDIR)/test/run_spec.py
BASH       := /bin/bash

export GERBIL_BUILD_CORES := $(shell echo $$(( $$(nproc) / 2 )))

# Static binary build variables
ARCH := $(shell uname -m)
PWD := $(shell pwd)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"
UID := $(shell id -u)
GID := $(shell id -g)

# --- Gambitgsc (embedded compiler) ---

GAMBITGSC_DIR := _vendor/gambitgsc

gambitgsc:
	$(CURDIR)/scripts/generate-gambitgsc

# --- Build ---

build: gambitgsc
	GERBIL_GSC=$(CURDIR)/scripts/gsc-with-gambitgsc \
	GERBIL_LOADPATH="$$HOME/.gerbil/pkg/gerbil-pcre/.gerbil/lib:$$GERBIL_LOADPATH" \
	LIBRARY_PATH="$$(brew --prefix openssl@3 2>/dev/null)/lib:$$LIBRARY_PATH" \
	gerbil build

install:
	@if [ ! -f $(GSH) ]; then $(MAKE) build; fi
	@echo "Cleaning old gsh artifacts from ~/.gerbil/ and ~/.local/bin/..."
	@rm -f ~/.gerbil/bin/gsh
	@rm -f ~/.local/bin/gsh
	@rm -rf ~/.gerbil/lib/gsh/
	@rm -f ~/.gerbil/lib/static/gsh__*.scm
	@echo "Installing new gsh..."
	@mkdir -p ~/.local/bin
	@cp $(GSH) ~/.local/bin/gsh
	@chmod +x ~/.local/bin/gsh
	@echo "Installed gsh to ~/.local/bin/gsh"

# --- Spec test targets ---

# Run the smoke test (quick sanity check)
compat-smoke: build $(OILS_DIR)
	$(SH_SPEC) $(OILS_DIR)/spec/smoke.test.sh $(BASH) $(GSH)

# Run all Tier 0 spec tests
compat-tier0: build $(OILS_DIR)
	@for f in smoke pipeline redirect redirect-multi \
	          builtin-eval-source command-sub comments exit-status; do \
	  echo "=== $$f ==="; \
	  $(SH_SPEC) $(OILS_DIR)/spec/$$f.test.sh $(BASH) $(GSH) || true; \
	  echo; \
	done

# Run all Tier 1 spec tests
compat-tier1: build $(OILS_DIR)
	@for f in here-doc quote word-eval word-split var-sub var-sub-quote \
	          var-num var-op-test var-op-strip var-op-len assign tilde; do \
	  echo "=== $$f ==="; \
	  $(SH_SPEC) $(OILS_DIR)/spec/$$f.test.sh $(BASH) $(GSH) || true; \
	  echo; \
	done

# Run all Tier 2 spec tests
compat-tier2: build $(OILS_DIR)
	@for f in arith glob brace-expansion case_ if_ loop for-expr subshell \
	          sh-func builtin-echo builtin-printf builtin-read builtin-cd \
	          builtin-set builtin-type builtin-trap builtin-bracket \
	          builtin-misc builtin-process background command-parsing \
	          var-op-bash var-op-slice assign-extended; do \
	  echo "=== $$f ==="; \
	  $(SH_SPEC) $(OILS_DIR)/spec/$$f.test.sh $(BASH) $(GSH) || true; \
	  echo; \
	done

# Run a single spec file: make compat-one SPEC=smoke
compat-one: build $(OILS_DIR)
	$(SH_SPEC) $(OILS_DIR)/spec/$(SPEC).test.sh $(BASH) $(GSH)

# Run a specific test range: make compat-range SPEC=smoke RANGE=3-5
compat-range: build $(OILS_DIR)
	$(SH_SPEC) --range $(RANGE) $(OILS_DIR)/spec/$(SPEC).test.sh $(BASH) $(GSH)

# Run all tiers combined and update the compatibility report
compat: compat-tier0 compat-tier1 compat-tier2
	@python3 $(CURDIR)/test/gen_compat_report.py --output $(CURDIR)/bash-compatibility.md \
	  $(OILS_DIR) $(BASH) $(GSH) 2>&1 | grep -v "^  Running"
	@echo "Updated bash-compatibility.md"

# Verbose single spec: make compat-debug SPEC=smoke
compat-debug: build $(OILS_DIR)
	$(SH_SPEC) -v $(OILS_DIR)/spec/$(SPEC).test.sh $(BASH) $(GSH)

# --- Benchmarks ---

SHELLBENCH_DIR := $(HOME)/mine/shellbench

bench: install
	@if [ ! -d "$(SHELLBENCH_DIR)" ]; then \
	  echo "Error: shellbench not found at $(SHELLBENCH_DIR)"; \
	  exit 1; \
	fi
	@cd $(SHELLBENCH_DIR) && setsid ./shellbench -s $(HOME)/.gerbil/bin/gsh,bash sample/*

# --- Vendor management ---

$(OILS_DIR):
	git clone --depth 1 https://github.com/oils-for-unix/oils.git $(OILS_DIR)

vendor-update: $(OILS_DIR)
	cd $(OILS_DIR) && git pull --ff-only

clean:
	gerbil clean
	rm -rf .gerbil
	rm -f ~/.gerbil/bin/gsh
	rm -rf ~/.gerbil/lib/gsh/
	rm -f ~/.gerbil/lib/static/gsh__*.scm
	rm -f $(GAMBITGSC_DIR)/*.o $(GAMBITGSC_DIR)/*.c $(GAMBITGSC_DIR)/LINK_ORDER $(GAMBITGSC_DIR)/.gambit-version

# --- Static binary (Docker) ---

static: linux-static-docker

clean-docker:
	-rm -rf .gerbil 2>/dev/null || true
	docker run --rm -v $(PWD):/src:z alpine rm -rf /src/.gerbil

check-root:
	@if [ "${UID}" -eq 0 ]; then \
	  git config --global --add safe.directory /src; \
	fi

build-static: check-root gambitgsc
	gxpkg install github.com/ober/gerbil-pcre2
	GSH_STATIC=1 \
	GERBIL_GSC=$(CURDIR)/scripts/gsc-with-gambitgsc \
	GERBIL_LOADPATH="$$(pwd)/.gerbil/lib:$$GERBIL_LOADPATH" \
	gerbil build

linux-static-docker: clean-docker
	docker run --rm \
	  --ulimit nofile=1024:1024 \
	  -v $(PWD):/src:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache pcre2-dev pcre2-static && \
	         cd /src && \
	         make build-static && \
	         chown -R $(UID):$(GID) .gerbil"

.PHONY: build install clean compat compat-smoke compat-tier0 compat-tier1 compat-tier2 \
        compat-one compat-range compat-debug compat-report vendor-update bench \
        static clean-docker check-root build-static linux-static-docker gambitgsc
