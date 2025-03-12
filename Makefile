-include config.mk
PROJECT		?= $(shell basename "$(shell pwd)")
INDEX		?= $(shell basename -s .el $(PROJECT))
EMACS		?= emacs
PREFIX		?= $(HOME)/.emacs.d/lelde/repos/production
T		?= $(shell find tests -name '*.test.el')
DEPLOY_COMMAND  ?= git clone

PWD             := $(shell pwd)
DEPLOY_TO       := $(PREFIX)/$(INDEX)
EMACS_OPTS	:= --batch -Q -L . -l config/batch.el
EMACS_TEST_OPTS := $(EMACS_OPTS) -l config/test.el
BUNDLED		:= $(INDEX).el
TARGET		:= $(BUNDLED:.el=.elc)
-include custom.mk

#-------------------------------------------------------------------------------

.PHONY: help build clean test deploy

help:
	@echo build - Builds $(INDEX).elc.
	@echo clean - Removes $(INDEX).elc.
	@echo test - Runs tests. If the macro T specified,
	@echo	this task runs partial tests.
	@echo deploy - Installes into $(DEPLOY_TO).

build: $(TARGET)

clean:
	for f in $(TARGET) $(EL_GENERATED);do\
		if test -f "$$f"; then\
			rm "$$f";\
		fi;\
	done

test:
	$(EMACS) $(EMACS_TEST_OPTS) $(T)

deploy: $(DEPLOY_TO)
	cd $(DEPLOY_TO) &&\
	make clean &&\
	git pull origin main &&\
	git merge main
	make build

#-------------------------------------------------------------------------------
$(DEPLOY_TO): $(PREFIX)
	cd $(PREFIX);\
	$(DEPLOY_COMMAND) $(PWD) $(PROJECT)\
	cd $(DEPLOY_TO);\
	git checkout -b production;

$(PREFIX):
	mkdir -p $(PREFIX)

$(BUNDLED): src/$(PROJECT).el
	$(EMACS) $(EMACS_OPTS) $(LELDE_BUNDLE) $< $@

%.elc: %.el
	$(EMACS) $(EMACS_OPTS)\
		--eval "(setq byte-compile-error-on-warn t)"\
		-f batch-byte-compile $<

%.el: %.src.el
	$(EMACS) $(EMACS_OPTS) $(LELDE_SMACRO) $< $@
