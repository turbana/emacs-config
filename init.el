;; -*- lexical-binding: t -*-
;;; init.el --- Ian Clark's Emacs Initialization File
;;
;; Copyright (c) 2021 Ian Clark
;;
;; Author: Ian Clark <turbana@gmail.com>
;; URL: https://github.com/turbana/emacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; This file replaces itself with the actual configuration at first run.

;; load core emacs `org-mode', but don't native compile it
(let ((comp-deferred-compilation nil))
  (require 'org))

;; tangle/load our init file
(let* ((base-dir (file-name-directory load-file-name))
       (org-file (concat base-dir "README.org"))
       (el-file (concat base-dir "init.el")))
  (org-babel-tangle-file org-file el-file)
  ;; on first load we shadow the core emacs org-mode and when trying to quit
  ;; emacs calls the non-existent function `org-clocking-buffer'. Define a dummy
  ;; to allow us to exit cleanly on initial run
  (defun org-clocking-buffer (&rest _))
  (load-file el-file)
  (warn "This is the first load of init.el so core emacs `org-mode' is loaded. Restart emacs to use newest version."))
