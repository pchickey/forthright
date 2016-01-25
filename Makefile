.PHONY: default build test

default: build

build:
	stack build

test: build
	stack test

clean:
	-rm -rf .stack-work
