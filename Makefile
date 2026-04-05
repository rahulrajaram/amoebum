.PHONY: build test test-ptui test-amoebum yarli-bootstrap-validate install-wrapper-validate check check-parens check-dist-ignore prepare-quicklisp-compat clean

REPO_ROOT := $(CURDIR)
QUICKLISP_SETUP ?= $(HOME)/quicklisp/setup.lisp
ifneq ($(wildcard ptui/.tools/quicklisp/setup.lisp),)
  QUICKLISP_SETUP := ptui/.tools/quicklisp/setup.lisp
endif
ifeq ($(filter /%,$(QUICKLISP_SETUP)),)
  QUICKLISP_SETUP_RESOLVED := $(abspath $(REPO_ROOT)/$(QUICKLISP_SETUP))
else
  QUICKLISP_SETUP_RESOLVED := $(QUICKLISP_SETUP)
endif
QUICKLISP_COMPAT_SETUP := $(REPO_ROOT)/ptui/.tools/quicklisp/setup.lisp
AMOEBUM_TEST_FAILURE_SUMMARY ?= $(REPO_ROOT)/tmp/amoebum-test-failures.log

build:
	bash bin/build-binary.sh

test:
	$(MAKE) test-ptui
	$(MAKE) test-amoebum

prepare-quicklisp-compat:
	@ptui/bin/ensure-quicklisp.sh >/dev/null
	@if [ ! -f "$(QUICKLISP_SETUP_RESOLVED)" ]; then \
	  echo "Quicklisp setup not found at $(QUICKLISP_SETUP_RESOLVED)" >&2; \
	  exit 1; \
	fi
	@if [ ! -f "$(QUICKLISP_COMPAT_SETUP)" ]; then \
	  mkdir -p "$(dir $(QUICKLISP_COMPAT_SETUP))"; \
	  ln -sf "$(QUICKLISP_SETUP_RESOLVED)" "$(QUICKLISP_COMPAT_SETUP)"; \
	fi

test-ptui: prepare-quicklisp-compat
	sbcl --noinform --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)) \
                     (handler-bind ((warning (lambda (c) (muffle-warning c)))) \
                       (load \"$(QUICKLISP_SETUP_RESOLVED)\") \
                       (setf asdf:*compile-file-warnings-behaviour* :ignore) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/ptui/ptui.asd\")) \
                       (asdf:load-system :ptui/tests) \
                       (asdf:test-system :ptui/tests)))"

test-amoebum: prepare-quicklisp-compat
	rm -f "$(AMOEBUM_TEST_FAILURE_SUMMARY)"
	AMOEBUM_TEST_FAILURE_SUMMARY="$(AMOEBUM_TEST_FAILURE_SUMMARY)" \
	sbcl --noinform --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)) \
                     (handler-bind ((warning (lambda (c) (muffle-warning c)))) \
                       (load \"$(QUICKLISP_SETUP_RESOLVED)\") \
                       (setf asdf:*compile-file-warnings-behaviour* :ignore) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/pseudopod/pseudopod.asd\")) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/sw4rm-sdk/sw4rm-sdk.asd\")) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/ptui/ptui.asd\")) \
	                       (asdf:load-asd (truename \"$(REPO_ROOT)/amoebum/amoebum.asd\")) \
	                       (asdf:test-system :amoebum/test)))"

yarli-bootstrap-validate:
	cd "$(REPO_ROOT)" && \
	  bash ./bin/yarli-bootstrap-local-state.sh && \
	  yarli plan validate

install-wrapper-validate:
	bash ./bin/install-wrapper-regression.sh

check-parens:
	bash ./bin/check-parens.sh

check-dist-ignore:
	bash ./bin/check-dist-ignore.sh

check:
	$(MAKE) check-parens
	$(MAKE) check-dist-ignore
	$(MAKE) test
	$(MAKE) build

clean:
	rm -rf dist/
