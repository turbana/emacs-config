;;; init.el --- Ian Clark's Initialization File
;;
;; Copyright (c) 2020 Ian Clark
;;
;; Author: Ian Clark <turbana@gmail.com>
;; URL: https://github.com/turbana/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; prevent package.el from loading packages
(when (>= emacs-major-version 27)
  (setq package-enable-at-startup nil))

;;; setup use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;;; setup org
(straight-use-package 'org)

;;; tangle/load the real config
(load-file
 (let* ((org-file (expand-file-name "~/.etc/emacs/emacs.org"))
        (el-file (concat (substring org-file 0 -4) ".el")))
   (when (file-newer-than-file-p org-file el-file)
     (require 'ob-tangle)
     (org-babel-tangle-file org-file))
   el-file))
