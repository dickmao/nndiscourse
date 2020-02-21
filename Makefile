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
	rm -f tests/log/*
	rm -rf tests/test-install

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: test-compile
test-compile: cask autoloads
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	$(MAKE) -C nndiscourse $@
	sh -e tools/package-lint.sh ./nndiscourse.el
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)

define TESTRUN
--eval "(custom-set-variables \
  (quote (gnus-select-method (quote (nndiscourse \"\")))) \
  (backquote (venv-location ,(file-name-as-directory (make-temp-file \"testrun-\" t)))) \
  (quote (gnus-verbose 8)) \
  (quote (nndiscourse-log-rpc t)))" \
--eval "(setq debug-on-error t)" \
--eval "(fset (quote gnus-y-or-n-p) (function ignore))"
endef

.PHONY: test-run
test-run:
	$(CASK) emacs -Q --batch \
	  $(TESTRUN) \
	  --eval "(require 'nndiscourse)" \
	  --eval "(cl-assert (nndiscourse-rpc-get))" \
	  --eval "(sleep-for 0 7300)" \
	  -f nndiscourse-dump-diagnostics \
	  --eval "(cl-assert nndiscourse-processes)"

.PHONY: test-run-interactive
test-run-interactive:
	$(CASK) emacs -Q \
	  $(TESTRUN) \
	  -f gnus

.PHONY: test-unit
test-unit:
	$(CASK) exec ert-runner -L . -L tests $(TESTS)

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int:
	rm -f tests/.newsrc.eld
	$(CASK) exec ecukes --debug --reporter magnars

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: backup-melpa
backup-melpa:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval \
	  "(with-temp-buffer \
	    (insert-file-contents-literally (car (file-expand-wildcards \"dist/nndiscourse-$(VERSION).tar\"))) \
	    (tar-mode) \
	    (let* ((my-desc (package-tar-file-info)) \
	           (name (package-desc-name my-desc)) \
	           (other-pkgs (cdr (assq name package-alist)))) \
	      (when other-pkgs \
	        (mapcar (lambda (odesc) \
	                  (let* ((odir (package-desc-dir odesc)) \
	                         (parent (file-name-directory odir)) \
	                         (leaf (file-name-nondirectory odir))) \
	                    (if (equal (package-desc-version my-desc) \
	                               (package-desc-version odesc)) \
	                        (delete-directory odir t) \
	                      (rename-file odir \
	                                   (expand-file-name (format \"BACKUP-%s\" leaf) parent) \
	                                   t)))) \
	                other-pkgs))))"

.PHONY: install
install: dist backup-melpa
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/nndiscourse-$(VERSION).tar\")))"
