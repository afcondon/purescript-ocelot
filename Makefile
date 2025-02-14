# Vairables that might need to be overriden.
ROOT_DIR ?= $(shell pwd)
BUILD_DIR ?= $(ROOT_DIR)/.build
DIST_DIR ?= $(ROOT_DIR)/dist
OUTPUT_DIR ?= $(ROOT_DIR)/output
PARCEL_DIR ?= $(BUILD_DIR)/parcel
RTS_ARGS ?=
SRC_DIR ?= $(ROOT_DIR)/src
TEST_DIR ?= $(ROOT_DIR)/test
UI_GUIDE_DIR ?= $(ROOT_DIR)/ui-guide

# Variables that we control
CLEAN_DEPS :=
BUILD_DEPS := build-ui
DEPS := $(BUILD_DIR)/.deps
FIND_SRC_FILES_ARGS := \( -name '*.purs' -o -name '*.js' \) -type f
NODE_MODULES := $(ROOT_DIR)/node_modules/.stamp
PACKAGE_JSON := $(ROOT_DIR)/package.json
SRC_FILES := $(shell find $(SRC_DIR) $(FIND_SRC_FILES_ARGS))
TEST_FILES := $(shell find $(TEST_DIR) $(FIND_SRC_FILES_ARGS))
UI_GUIDE_FILES := $(shell find $(UI_GUIDE_DIR) $(FIND_SRC_FILES_ARGS))
YARN_LOCK := $(ROOT_DIR)/yarn.lock

# Colors for printing
CYAN := \033[0;36m
RESET := \033[0;0m

YARN := cd $(ROOT_DIR) && yarn

-include $(ROOT_DIR)/css/Makefile

.DEFAULT_GOAL := build

$(BUILD_DIR) $(DIST_DIR) $(PARCEL_DIR):
	mkdir -p $@

$(BUILD_DIR)/help-unsorted: $(MAKEFILE_LIST) | $(BUILD_DIR)
	@grep \
		--extended-regexp '^[A-Za-z_-]+:.*?## .*$$' \
	  --no-filename \
	  $(MAKEFILE_LIST) \
	  > $@

$(BUILD_DIR)/help: $(BUILD_DIR)/help-unsorted | $(BUILD_DIR)
	@sort $< > $@

$(BUILD_DIR)/test.js: $(OUTPUT_DIR)/Test.Main/index.js | $(BUILD_DIR)
	$(YARN) run purs bundle \
		$(RTS_ARGS) \
		$(OUTPUT_DIR)/*/*.js \
		--main Test.Main \
		--module Test.Main \
		--output $@

$(BUILD_DIR)/test.out: $(BUILD_DIR)/test.js
	node $< | tee $@.tmp # Store output in a temp file in case of a failure.
	mv $@.tmp $@ # Move the output where it belongs.

$(DEPS): packages.dhall spago.dhall $(NODE_MODULES) | $(BUILD_DIR)
	$(YARN) run spago install $(RTS_ARGS)
	touch $@

$(DIST_DIR)/bundled.js: $(OUTPUT_DIR)/Main/index.js
	$(YARN) run purs bundle \
		$(RTS_ARGS) \
		$(OUTPUT_DIR)/*/*.js \
		--main Main \
		--module Main \
		--output $@

$(DIST_DIR)/index.js: $(OUTPUT_DIR)/Main/index.js
	$(YARN) run browserify dist/main.js --outfile $@

$(NODE_MODULES): $(PACKAGE_JSON) $(YARN_LOCK)
	$(YARN) install
	touch $@

$(OUTPUT_DIR)/Main/index.js: $(SRC_FILES) $(UI_GUIDE_FILES) $(DEPS)
	$(YARN) run spago build -p "$(UI_GUIDE_DIR)/**/*.purs" -u "$(RTS_ARGS)"

$(OUTPUT_DIR)/Test.Main/index.js: $(SRC_FILES) $(TEST_FILES) $(DEPS)
	$(YARN) run spago build -p "$(TEST_DIR)/Main.purs $(TEST_DIR)/Test/**/*.purs" -u "$(RTS_ARGS)"

.PHONY: build
build: $(BUILD_DEPS) ## Build everything — all the CSS, and the UI Guide — installing any missing dependencies along the way

.PHONY: build-ui
build-ui: $(DIST_DIR)/index.js ## Build the UI Guide, installing any missing dependencies along the way

.PHONY: clean
clean: $(CLEAN_DEPS) ## Remove all dependencies and build artifacts, starting with a clean slate
	rm -fr \
		$(BUILD_DIR) \
		$(DIST_DIR)/bundled.js \
		$(DIST_DIR)/index.js \
		$(OUTPUT_DIR) \
		$(ROOT_DIR)/.spago \
		$(ROOT_DIR)/node_modules

.PHONY: help
help: $(BUILD_DIR)/help ## Display this help message
	@awk 'BEGIN {FS = ":.*?## "}; {printf "$(CYAN)%-30s$(RESET) %s\n", $$1, $$2}' $<

.PHONY: test
test: $(BUILD_DIR)/test.out ## Build and run tests

.PHONY: ui-guide
ui-guide: $(OUTPUT_DIR)/Main/index.js $(NODE_MODULES) | $(PARCEL_DIR) ## Build the UI Guide using ParcelJS for hot reloading (experimental, may not work on all environments)
	npx parcel $(DIST_DIR)/parcel.html --out-dir $(PARCEL_DIR) --no-cache
