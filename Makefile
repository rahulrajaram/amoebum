.PHONY: build test test-ptui test-amoebum check clean

REPO_ROOT := $(CURDIR)
QUICKLISP_SETUP ?= $(HOME)/quicklisp/setup.lisp
ifneq ($(wildcard ptui/.tools/quicklisp/setup.lisp),)
  QUICKLISP_SETUP := ptui/.tools/quicklisp/setup.lisp
endif

build:
	bash bin/build-binary.sh

test:
	$(MAKE) check

test-ptui:
	sbcl --noinform --non-interactive \
	  --eval "(load \"$(abspath $(REPO_ROOT)/$(QUICKLISP_SETUP))\")" \
	--eval "(require :asdf)" \
	  --eval "(asdf:load-asd (truename \"$(REPO_ROOT)/ptui/ptui.asd\"))" \
	  --eval "(asdf:load-system :ptui/tests)" \
	  --eval "(asdf:test-system :ptui/tests)"

test-amoebum:
	sbcl --noinform --non-interactive \
	  --eval "(load \"$(abspath $(REPO_ROOT)/$(QUICKLISP_SETUP))\")" \
	  --eval "(require :asdf)" \
	  --eval "(asdf:load-asd (truename \"$(REPO_ROOT)/pseudopod/pseudopod.asd\"))" \
	  --eval "(asdf:load-asd (truename \"$(REPO_ROOT)/sw4rm-sdk/sw4rm-sdk.asd\"))" \
	  --eval "(asdf:load-asd (truename \"$(REPO_ROOT)/ptui/ptui.asd\"))" \
	  --eval "(asdf:load-asd (truename \"$(REPO_ROOT)/amoebum/amoebum.asd\"))" \
	  --eval "(asdf:test-system :amoebum/test)"

check:
	$(MAKE) test-ptui
	$(MAKE) test-amoebum

clean:
	rm -rf dist/
