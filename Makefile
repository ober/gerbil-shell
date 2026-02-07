GSH        := $(CURDIR)/.gerbil/bin/gsh
OILS_DIR   := _vendor/oils
SH_SPEC    := python3 $(CURDIR)/test/run_spec.py
BASH       := /bin/bash

# --- Build ---

build:
	LIBRARY_PATH="$$(brew --prefix openssl@3 2>/dev/null)/lib:$$LIBRARY_PATH" gerbil build

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
	          builtin-misc builtin-process background command-parsing; do \
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

# Run all tiers combined
compat: compat-tier0 compat-tier1 compat-tier2

# Verbose single spec: make compat-debug SPEC=smoke
compat-debug: build $(OILS_DIR)
	$(SH_SPEC) -v $(OILS_DIR)/spec/$(SPEC).test.sh $(BASH) $(GSH)

# --- Vendor management ---

$(OILS_DIR):
	git clone --depth 1 https://github.com/oils-for-unix/oils.git $(OILS_DIR)

vendor-update: $(OILS_DIR)
	cd $(OILS_DIR) && git pull --ff-only

clean:
	gerbil build --clean

.PHONY: build clean compat compat-smoke compat-tier0 compat-tier1 compat-tier2 \
        compat-one compat-range compat-debug vendor-update
