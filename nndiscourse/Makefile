SHELL := /bin/bash
REPO := $(shell git rev-parse --show-toplevel)

.PHONY: test-compile
test-compile:
	bundle install
	bundle exec rake

.PHONY: clean
clean:
	bundle clean
