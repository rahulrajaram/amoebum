.PHONY: build test clean

build:
	bash bin/build-binary.sh

test:
	bash ptui/bin/test.sh
	bash bin/amoebum test

clean:
	rm -rf dist/
