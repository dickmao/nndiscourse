SHELL := /bin/bash
REPO := $(shell git rev-parse --show-toplevel)

.PHONY: test-compile
test-compile:
	bundle install --quiet
	bundle exec rake

.PHONY: clean
clean:
	@echo Not running bundle clean
