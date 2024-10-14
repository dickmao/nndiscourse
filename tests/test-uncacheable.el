;;; -*- lexical-binding: t; coding: utf-8 -*-
(require 'nndiscourse-test)

;; since nndiscourse has fixed numbering, maybe we *can* use gnus-cache
(ert-deftest nndiscourse-could-cache ()
  (should (featurep 'gnus-cache))
  (should-not (string-match (or gnus-uncacheable-groups "$a") "nndiscourse+emacs-china.org:emacs")))
