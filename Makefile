.PHONY: build test test-ptui test-amoebum check check-dist-ignore clean

REPO_ROOT := $(CURDIR)
QUICKLISP_SETUP ?= $(HOME)/quicklisp/setup.lisp
ifneq ($(wildcard ptui/.tools/quicklisp/setup.lisp),)
  QUICKLISP_SETUP := ptui/.tools/quicklisp/setup.lisp
endif

build:
	bash bin/build-binary.sh

test:
	$(MAKE) test-ptui
	$(MAKE) test-amoebum

test-ptui:
	sbcl --noinform --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)) \
                     (handler-bind ((warning (lambda (c) (muffle-warning c)))) \
                       (load \"$(abspath $(REPO_ROOT)/$(QUICKLISP_SETUP))\") \
                       (setf asdf:*compile-file-warnings-behaviour* :ignore) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/ptui/ptui.asd\")) \
                       (asdf:load-system :ptui/tests) \
                       (asdf:test-system :ptui/tests)))"

test-amoebum:
	sbcl --noinform --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)) \
                     (handler-bind ((warning (lambda (c) (muffle-warning c)))) \
                       (load \"$(abspath $(REPO_ROOT)/$(QUICKLISP_SETUP))\") \
                       (setf asdf:*compile-file-warnings-behaviour* :ignore) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/pseudopod/pseudopod.asd\")) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/sw4rm-sdk/sw4rm-sdk.asd\")) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/ptui/ptui.asd\")) \
                       (asdf:load-asd (truename \"$(REPO_ROOT)/amoebum/amoebum.asd\")) \
                       (asdf:test-system :amoebum/test)))"

check-dist-ignore:
	bash ./bin/check-dist-ignore.sh

check:
	$(MAKE) check-dist-ignore
	$(MAKE) test
	$(MAKE) build

clean:
	rm -rf dist/
