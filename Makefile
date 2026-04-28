.PHONY: build test test-ptui test-amoebum test-pseudopod test-sw4rm-sdk yarli-bootstrap-validate install-wrapper-validate check check-parens check-dist-ignore check-import-cycles check-package-export-goldens check-readme-makefile prepare-quicklisp-compat clean

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

build: check-import-cycles
	bash bin/build-binary.sh

# NXT-397: Fail-fast guardrail. Refuses to build if any directed
# cycle exists in the amoebum package import graph. Runs before the
# heavyweight SBCL build so a stray :use/:import-from cycle introduced
# by the post-delegation facade splits is caught in seconds.
check-import-cycles:
	bash bin/check-import-cycles.sh

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
                       (asdf:load-asd (truename \"$(REPO_ROOT)/ptui/ptui-preview.asd\")) \
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

# NXT-560: focused Pseudopod test entrypoint so dependency-layer tranches
# (NXT-460, NXT-547) can verify provider/streaming/tool/file behavior
# without running the broader test-amoebum bundle.
test-pseudopod: prepare-quicklisp-compat
	sbcl --noinform --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)) \
                     (handler-bind ((warning (lambda (c) (muffle-warning c)))) \
                       (load \"$(QUICKLISP_SETUP_RESOLVED)\") \
                       (setf asdf:*compile-file-warnings-behaviour* :ignore) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/pseudopod/pseudopod.asd\")) \
                       (asdf:test-system :pseudopod/test)))"

# NXT-560: focused SW4RM SDK test entrypoint so coordination tranches
# (NXT-466, NXT-467) can verify handoff/negotiation/workflow/policy
# behavior without rebuilding the full amoebum test surface.
test-sw4rm-sdk: prepare-quicklisp-compat
	sbcl --noinform --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)) \
                     (handler-bind ((warning (lambda (c) (muffle-warning c)))) \
                       (load \"$(QUICKLISP_SETUP_RESOLVED)\") \
                       (setf asdf:*compile-file-warnings-behaviour* :ignore) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/pseudopod/pseudopod.asd\")) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/sw4rm-sdk/sw4rm-sdk.asd\")) \
                       (asdf:test-system :sw4rm-sdk/tests)))"

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

# NXT-398: Subsystem-level public-symbol stability fixture. Runs the
# standalone package-export golden script which loads :amoebum, gathers the
# external symbols of each registered subsystem package, and diffs them
# against the checked-in goldens under amoebum/test/snapshots/package-exports/.
# Run with AMOEBUM_UPDATE_SNAPSHOTS=1 to refresh the goldens after a
# deliberate facade change.
check-package-export-goldens:
	timeout 600 sbcl --noinform \
	  --script amoebum/test/package-export-golden-test.lisp

check-readme-makefile:
	bash ./bin/check-readme-makefile.sh

check:
	$(MAKE) check-parens
	$(MAKE) check-dist-ignore
	$(MAKE) check-import-cycles
	$(MAKE) check-readme-makefile
	$(MAKE) test
	$(MAKE) check-package-export-goldens
	$(MAKE) build

clean:
	rm -rf dist/
