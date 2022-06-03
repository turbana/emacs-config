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
;; keep the server files under `user-cache-directory'/server
(setq server-auth-dir (concat (file-name-directory load-file-name)
                              "cache/server/"))
;; keep the (minimal) server eln cache in the same directory
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-eln-load-path
        (cons server-auth-dir
              (last native-comp-eln-load-path))))

(require 'server)
(unless (server-running-p)
  (server-start))
