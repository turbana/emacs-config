;; -*- lexical-binding: t -*-
;;; early-init.el --- Ian Clark's Emacs Initialization File
;;
;; Copyright (c) 2021 Ian Clark
;;
;; Author: Ian Clark <turbana@gmail.com>
;; URL: https://github.com/turbana/emacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; disable package.el
(setq package-enable-at-startup nil)

(require 'server)
;; keep the server files under `user-cache-directory'/server
(setq server-auth-dir (concat (file-name-directory load-file-name)
                              "cache/server"))
(unless (server-running-p)
  (server-start))
