export EMACS ?= $(shell which emacs)
export CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at https://cask.readthedocs.io/en/latest/guide/installation.html)
endif
CASK_DIR := $(shell $(CASK) package-directory || exit 1)
SRC = $(shell $(CASK) files)
PKBUILD = 2.3
VERSION = $(shell $(CASK) version)
ELCFILES = $(SRC:.el=.elc)
TESTS = $(shell ls tests/test*el)
TESTSSRC = $(TESTS) features/support/env.el tests/nndiscourse-test.el
ELCTESTS = $(TESTSSRC:.el=.elc)
.DEFAULT_GOAL := test-compile

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"nndiscourse\" \".\")"

README.rst: README.in.rst nndiscourse.el
	grep ';;' nndiscourse.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: clean
clean:
	$(CASK) clean-elc
	$(MAKE) -C nndiscourse $@
	rm -f ert-profile*
	rm -f tests/log/*
	rm -rf tests/test-install

.PHONY: bundler
bundler:
	$(MAKE) $(HOME)/.gem/ruby/2.6.0/gems/bundler-2.0.2/bundler.gemspec

$(HOME)/.gem/ruby/2.6.0/gems/bundler-2.0.2/bundler.gemspec:
	cd nndiscourse ; gem install --user-install bundler:2.0.2

.PHONY: cask
cask: bundler $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: test-compile
test-compile: cask autoloads
	$(MAKE) -C nndiscourse $@
	sh -e tools/package-lint.sh ./nndiscourse.el
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)

TESTFILES = $(shell $(CASK) files)

define TESTRUN
--eval "(custom-set-variables \
  (backquote (nndiscourse-test-dir ,(file-name-as-directory (make-temp-file \"testrun-\" t)))) \
  (quote (gnus-select-method (quote (nndiscourse \"meta.discourse.org\" (nndiscourse-scheme \"https\"))))) \
  (quote (gnus-verbose 8)))" \
--eval "(setq debug-on-error t)" \
--eval "(fset (quote gnus-y-or-n-p) (function ignore))" \
--eval "(dolist (f (mapcar (function symbol-name) (quote ($(TESTFILES))))) \
  (let* ((parent (file-name-directory f)) \
         (dest (concat (file-name-as-directory nndiscourse-test-dir) (or parent \".\")))) \
    (make-directory dest t) \
    (funcall (if (file-directory-p f) (function copy-directory) (function copy-file)) f (concat (file-name-as-directory dest) (file-name-nondirectory f)))))"
endef

.PHONY: test-run
test-run: cask
	$(CASK) emacs -Q --batch -l nndiscourse \
	  $(TESTRUN) \
	  --eval "(gnus-open-server gnus-select-method)" \
	  --eval "(sleep-for .43)" \
	  --eval "(cl-assert nndiscourse-processes)" \
	  --eval "(nndiscourse-dump-diagnostics (nth 1 gnus-select-method))"

.PHONY: test-run-interactive
test-run-interactive: cask autoloads
	$(CASK) emacs -Q -l nndiscourse \
	  $(TESTRUN) \
	  -f gnus

.PHONY: test-unit
test-unit: cask autoloads
	$(CASK) exec ert-runner -L . -L tests $(TESTS)

.PHONY: test-clean
test-clean:
	rm -rf tests/.emacs* tests/.newsrc* tests/Mail tests/News tests/request tests/request-log

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int: test-clean
	rm -f tests/.newsrc.eld
	$(CASK) exec ecukes --debug --reporter magnars

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: install
install: dist bundler
	$(EMACS) -Q --batch -l package \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/nndiscourse-$(VERSION).tar\")))"
