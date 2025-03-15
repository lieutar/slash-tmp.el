EMACS      ?= emacs
CASK       ?= cask
TEST_TYPE  ?= ert
T          ?= $(shell find tests -name '*.test.el')

PWD	     := $(shell pwd)
PROJECT      := $(shell basename $(PWD))
INDEX	     ?= $(shell echo $(PROJECT) | sed -e 's/\.[^.]*$$//' )
INDEX_EL     := $(INDEX).el
TARGET	     := $(INDEX).elc
LT           := $(foreach f,$(T),-l $f)
SRC_DIR      := .
SRC_INDEX_EL := $(SRC_DIR)/$(INDEX_EL)
SUBMOD_DIR   := $(SRC_DIR)/$(INDEX)
EMACS_OPTS   := --batch -Q -L $(SRC_DIR)
PHONY	     := build clean test test-ert test-buttercup
-include custom.mk

.PHONY: $(PHONY)

build: $(TARGET)

clean:
	test -f "$(TARGET)" && rm $(TARGET)

test: test-$(TEST_TYPE)

test-ert:
	$(CASK) exec $(EMACS) $(EMACS_OPTS) -l ert $(LT)\
		-f ert-run-tests-batch-and-exit

test-buttercup:
	$(CASK) exec $(EMACS) $(EMACS_OPTS) -l buttercup $(LT)\
		-f buttercup-run

%.elc: %.el
	$(EMACS) $(EMACS_OPTS)\
		--eval "(setq byte-compile-error-on-warn t)"\
		-f batch-byte-compile $<
