;; -*- lexical-binding: t -*-

(setq user-emacs-directory (file-name-directory load-file-name))

(defvar user-cache-directory (concat user-emacs-directory "cache/"))
(defvar user-data-directory (concat user-emacs-directory "data/"))

(defconst on-linux (string-equal system-type "gnu/linux"))
(defconst on-windows (string-equal system-type "windows-nt"))
(defconst on-mac (string-equal system-type "darwin"))

;; at_home/at_work are bash functions defined in ~/.bashrc
(defconst at-home (= 0 (call-process "bash" nil nil nil "-c" "source ~/.bashrc && at_home")))
(defconst at-work (= 0 (call-process "bash" nil nil nil "-c" "source ~/.bashrc && at_work")))

(defun on-host (hostname)
  "Return non-nil when the current system is `hostname'."
  (string-equal (system-name) hostname))

(defconst on-tablet (on-host "DESKTOP-F6Q3GN2"))
(defconst on-old-imac (on-host "Michaels-iMac.local"))
(defconst on-home-windows-desktop (and on-windows (on-host "Gladden")))
(defconst on-home-windows-desktop-wsl (and on-linux (on-host "Gladden")))

(setq use-package-always-demand t)

;; do we have native compilation available?
(defconst have-native-compilation
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)))

(when have-native-compilation
  ;; don't blast me with (mostly) useless warnings
  (setq native-comp-async-report-warnings-errors nil)
  ;; keep the eln cache tidy
  (setq native-comp-eln-load-path
        (list (concat user-cache-directory "eln-cache/")
              (car (last native-comp-eln-load-path))))
  ;; needed for emacs-29.1
  (setq native-comp-deferred-compilation-deny-list nil))

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

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(setq straight-profiles `((nil . ,(concat user-data-directory "straight-default.el"))))

(straight-use-package 'org)

(use-package async
  :ensure t)

(setq user-init-file (concat user-emacs-directory "init.el"))

(defvar ic--tangle-init-async-cookie nil
  "Used to keep track of the async org-tangle process for init.el")

(defun ic-maybe-tangle-init-file ()
  "Tangle/compile my emacs configuration."
  (let ((org-files (list
                    (expand-file-name "~/src/emacs/README.org")
                    (concat user-emacs-directory "README.org"))))
    (when (member (buffer-file-name) org-files)
      (when (process-live-p ic--tangle-init-async-cookie)
        (kill-process ic--tangle-init-async-cookie))
      (message "Async tangling %s..." (buffer-file-name))
      (setq
       ic--tangle-init-async-cookie
       (async-start
        `(lambda ()
           (require 'ob-tangle)
           (require 'subr-x)
           (let ((start-time (current-time))
                 ;; set to silence messages from `org-babel-tangle-file'.
                 ;; this screws up the async return value on windows :(
                 (inhibit-message t))
             (when-let (ret (org-babel-tangle-file ,(buffer-file-name)
                                                   ,user-init-file))
               ;; (byte-compile-file ,user-init-file)
               ;; (when ,have-native-compilation
               ;;   (native-compile ,user-init-file))
               (cons ret (float-time (time-since start-time))))))
        `(lambda (ret)
           (unless (consp ret)
             (error "error in org-babel-tangle for %S"
                    ,(buffer-file-name)))
           (message "Tangled %s in %.2f seconds"
                    (caar ret) (cdr ret))))))))

(add-hook 'after-save-hook 'ic-maybe-tangle-init-file)

(use-package meow
  ;; :straight (meow :type git :host github :repo "turbana/meow")
  :ensure t
  :demand t
  :config
  ;; emacs keybind to access local leader map
  (setq emacs-local-leader-prefix "C-?")
  ;; meow keybind alias for local leader map
  (setq meow-local-leader-prefix "/")
  (setq meow-local-leader-insert-prefix "C-/")
  ;; keep the expand hints around a while longer
  (setq meow-expand-hint-remove-delay 3.0)
  ;; start git commits in insert mode
  (add-hook 'git-commit-mode-hook 'meow-insert-mode)
  ;; turn it on, baby
  (meow-global-mode 1))

(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

(setq global-leader-map
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "c") 'meow-keypad-start)
        (define-key keymap (kbd "g") 'meow-keypad-start)
        (define-key keymap (kbd "h") 'meow-keypad-start)
        (define-key keymap (kbd "m") 'meow-keypad-start)
        (define-key keymap (kbd "x") 'meow-keypad-start)
        keymap))

(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 ;; global leader
 `("SPC" . ,global-leader-map)
 )

(meow-normal-define-key
 '("1" . meow-1)
 '("2" . meow-2)
 '("3" . meow-3)
 '("4" . meow-4)
 '("5" . meow-5)
 '("6" . meow-6)
 '("7" . meow-7)
 '("8" . meow-8)
 '("9" . meow-9)
 '("0" . meow-0)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 ;; global leader
 `("SPC" . ,global-leader-map)
 ;; local leader
 `(,meow-local-leader-prefix . ,emacs-local-leader-prefix)
 '("a" . meow-append)
 '("A" . meow-open-below)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 '("d" . meow-delete)
 '("D" . meow-backward-delete)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("I" . meow-open-above)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . meow-search)
 '("o" . meow-block)
 '("O" . meow-to-block)
 '("p" . meow-yank)
 '("q" . keyboard-quit)
 '("Q" . meow-goto-line)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("s" . meow-kill)
 '("t" . meow-till)
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-visit)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-line)
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("'" . repeat)
 '("<escape>" . keyboard-quit))

;; local leader in insert mode
(define-key meow-insert-state-keymap
  (kbd meow-local-leader-insert-prefix)
  (meow--parse-def emacs-local-leader-prefix))

(defmacro def-meow-digit-action (func digit)
  "Create function FUNC that when called will call `meow-expand-DIGIT' when
  expanding, and `meow-digit-argument' otherwise."
  (let ((meow-expand-digit (intern (format "meow-expand-%d" digit))))
    `(defun ,func ()
       (interactive)
       (if (meow-expanding-p)
           (,meow-expand-digit)
         (meow-digit-argument)))))

(defun meow-expanding-p ()
  "Return non-NIL when `meow' is either expanding or selecting text."
  (meow--selection-type))

(def-meow-digit-action meow-1 1)
(def-meow-digit-action meow-2 2)
(def-meow-digit-action meow-3 3)
(def-meow-digit-action meow-4 4)
(def-meow-digit-action meow-5 5)
(def-meow-digit-action meow-6 6)
(def-meow-digit-action meow-7 7)
(def-meow-digit-action meow-8 8)
(def-meow-digit-action meow-9 9)
(def-meow-digit-action meow-0 0)

;; don't need the macro anymore
(fmakunbound 'def-meow-digit-action)

(use-package themian-theme
  :straight (themian :type git :host github :repo "turbana/themian")
  :init
  (setq themian-org-mode-variable-pitch t)
  (setq themian--show-unknowns nil)
  (let ((fixed
         (cond (on-old-imac
                '("Iosevka Extended" 160 ultra-light))
               ;; (on-home-windows-desktop-wsl
               ;;  '("Noto Sans Mono" 110 normal))
               (t
                '("Iosevka Extended" 110 normal))))
        (variable
         (cond (on-old-imac
                '("DejaVu Sans ExtraLight" 150 ultra-light))
               ;; (on-home-windows-desktop-wsl
               ;;  '("Noto Sans" 120 normal))
               (t
                '("DejaVu Sans Condensed" 120 normal)))))
    (set-face-attribute 'default nil :family (nth 0 fixed)
                        :height (nth 1 fixed) :weight (nth 2 fixed))
    (set-face-attribute 'fixed-pitch nil :family (nth 0 fixed)
                        :height (nth 1 fixed) :weight (nth 2 fixed))
    (set-face-attribute 'variable-pitch nil :family (nth 0 variable)
                        :height (nth 1 variable) :weight (nth 2 variable)))

  (defun themian--reload-dark-theme ()
    (interactive)
    (load-file "~/src/themian/themian-theme.el")
    (themian-create-color-theme 'themian-dark 'dark)
    (load-theme 'themian-dark t))
  (defun themian--reload-light-theme ()
    (interactive)
    (load-file "~/src/themian/themian-theme.el")
    (themian-create-color-theme 'themian-light 'light)
    (load-theme 'themian-light t))

  :config
  (defun themian--load-theme-on-frame-create (frame)
    "Enable default theme on FRAME, but only when graphics are enabled and no
  other theme is loaded."
    (with-selected-frame frame
      (when (and (display-graphic-p)
                 (not custom-enabled-themes))
        (load-theme
         (if (or on-tablet on-old-imac) 'themian-light 'themian-dark)
         t))))

  ;; set initial theme on frame create
  (add-hook 'after-make-frame-functions 'themian--load-theme-on-frame-create)
  ;; try to set initial theme now
  (themian--load-theme-on-frame-create (selected-frame))

  ;; HACK
  ;; something is using quote as a face, not sure where that is, but define it
  ;; here to silence the warnings in *Messages*
  (defface quote nil "not sure what this is")

  ;; don't use `general' to bind keys as it's not loaded yet
  :bind (("M-<f5>" . 'themian--reload-dark-theme)
         ("M-<f6>" . 'themian--reload-light-theme)))

;; use when live editing
;; (setq lexical-binding t)

(defvar ic/watch-variable-message-fmt "%s(%s): %s %S"
  "Format to use when displaying variable changes. Equivalent to
`(format ic/watch-variable-message-fmt symbol buffer operation value)'.")

(defun ic/watch-variable (symbol &rest ops)
  "Watch for any OPS operations on SYMBOL and call `message'. OPS defaults to
'(set), see `add-variable-watcher' for all possible values."
  (defun watch-variable-call-message (sym value operation buffer)
    (message (ic//watch-variable-format sym buffer operation value))
  (ic/watch-variable-call-func symbol 'watch-variable-call-message ops)))

(defun ic/watch-variable-raise-debug (symbol &rest ops)
  "Watch for any changes to SYMBOL and raise on error."
  (defun watch-variable-raise-debug (sym value operation buffer)
    (let ((msg (ic//watch-variable-format sym buffer operation value)))
      (message msg)
      (debug nil msg)))
  (ic/watch-variable-call-func symbol 'watch-variable-raise-debug ops))

(defun ic/watch-variable-print-stack (symbol &rest ops)
  "Watch for any changes to SYMBOL and print a stack trace."
  (defun watch-variable-print-stack (sym value operation buffer)
    (message (ic//watch-variable-format sym buffer operation value))
    (if-let ((stack-trace (with-output-to-string (backtrace))))
        (progn
          (message ">>>>>>>>>>")
          (message "%S" stack-trace)
          (message "<<<<<<<<<<"))
      (message "(no stack trace found)")))
  (ic/watch-variable-call-func symbol 'watch-variable-print-stack ops))

(defvar ic//variable-watchers nil "a-list of variable watchers")

(defun ic/watch-variable-call-func (symbol func &rest ops)
  "Watch for any OPS on SYMBOL and call FUNC."
  (unless (assq symbol ic//variable-watchers)
    (when (equal ops (list nil))
      (setq ops '(set let unlet makunbound defvaralias)))
    (defun watch-change (sym value operation buffer)
      (when (member operation ops)
        (apply func (list sym value operation buffer))))
    (add-variable-watcher symbol 'watch-change)
    (push (cons symbol 'watch-change) ic//variable-watchers)))

(defun ic/unwatch-variable (symbol)
  "Remove any `ic/watch-variable-*' watchers from SYMBOL."
  (let ((watcher (alist-get symbol ic//variable-watchers)))
    (when watcher
      (remove-variable-watcher symbol watcher)
      (setq ic//variable-watchers
            (assq-delete-all symbol ic//variable-watchers))
      t)))

(defun ic//watch-variable-format (symbol value operation buffer)
  "Generate printable string."
  (format "%s(%s): %s %S" symbol buffer operation value))

(require 'profiler)

(defun ic/start-cpu-profiler ()
  "Start the CPU profiler."
  (interactive)
  (profiler-start 'cpu))

(defun ic/start-mem-profiler ()
  "Start the memory profiler."
  (interactive)
  (profiler-start 'mem))

(defun ic/start-cpu-mem-profiler ()
  "Start both CPU and memory profiling."
  (interactive)
  (profiler-start 'cpu+mem))

(use-package general
  :demand t
  :config
  ;; ensure `general-override-mode-map' is active
  (general-override-mode 1)

  ;; create leader (SPC) definer
  (general-create-definer leader-keys
    :keymaps 'global-leader-map
    ;; :prefix "SPC"
    )

  ;; create local leader (,) definer
  (general-create-definer local-leader-keys
    :keymaps 'general-override-mode-map
    :prefix emacs-local-leader-prefix))

(use-package which-key
    :config
    (which-key-mode 1))

(defvar ic/secret-cmd "secret")

(defun ic/get-secret (secret)
  "Retrieve `secret' using the `ic/secret-cmd' program."
  (nth 0 (process-lines ic/secret-cmd secret)))

(use-package all-the-icons)

(use-package auto-dim-other-buffers
    :config
    (setq auto-dim-other-buffers-dim-on-focus-out t)
    (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
    ;; ensure org-indent face is properly hidden
    (when (boundp 'auto-dim-other-buffers-affected-faces)
      (push '(org-indent . auto-dim-other-buffers-hide-face)
            auto-dim-other-buffers-affected-faces))
    (auto-dim-other-buffers-mode t))

(use-package company
  :demand t
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-abort-on-unique-match t)
  (setq company-tooltip-width-grow-only t)

  :general
  (general-define-key
   :keymaps 'company-active-map
   "C-l" 'company-show-location
   "C-h" 'company-show-doc-buffer
   "C-w" 'backward-kill-word
   "C-s" 'company-search-candidates
   "C-f" 'company-filter-candidates
   ))

(use-package compat)
(require 'compat)

(use-package flycheck
  )

(use-package haskell-mode
  :demand t)

(use-package helpful
  :demand t
  :bind (:map global-map
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-key] . #'helpful-key)))

(use-package json)

(use-package lsp-mode
  :demand t
  :commands lsp

  :init
  ;; keep session file tidy
  (setq lsp-session-file (concat user-cache-directory "lsp-session-v1"))
  ;; don't show the top breadcrumbs by default
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; keep more log data
  (setq lsp-log-max 10000)
  ;; setup c# lsp server
  (setq lsp-csharp-server-path
        (cond (on-home-windows-desktop-wsl
               (executable-find "omnisharp"))))

  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (csharp-mode . lsp))

  :general
  (local-leader-keys
    :keymaps 'prog-mode-map
    "l" lsp-command-map))

(use-package lsp-ui)

(when (or on-tablet on-old-imac)
  (use-package magit-section))

(use-package magit
  :init
  ;; don't use magit's default key bindings
  (setq magit-define-global-key-bindings nil)

  ;; I run emacs on my home windows desktop under MSYS2, but magit guesses that
  ;; it's running under CYGWIN, which screws up filename expansion
  (when on-home-windows-desktop
    (defun ic-magit-maybe-override-cygwin-paths (f &rest args)
      "Call `f' with correct binding of `magit-cygwin-mount-points'.

When running emacs on windows under MSYS2, we need `magit' to rewrite local file
    paths using `magit-cygwin-mount-points', but ignore it for remote paths."
      (let ((magit-cygwin-mount-points
             (and (not (file-remote-p default-directory))
                  magit-cygwin-mount-points)))
        (apply f args)))
    (advice-add 'magit-expand-git-file-name :around
                #'ic-magit-maybe-override-cygwin-paths)
    (advice-add 'magit-convert-filename-for-git :around
                #'ic-magit-maybe-override-cygwin-paths))

  :general
  (general-define-key
   "C-x g" 'magit-status)
  (general-define-key
   :keymaps 'magit-section-mode-map
   "<up>" 'magit-section-backward
   "<down>" 'magit-section-forward)
  :bind (:map magit-status-mode-map
         ("x" . #'magit-discard)))

(use-package org
    :mode (("\\.org$" . org-mode))
    :ensure org-plus-contrib
    :after (all-the-icons yasnippet)
    :config
    (defvar org-home-file "~/org/home/home.org"
      "Default org file for home related items.")
    (defvar org-work-file "~/org/work.org"
      "Default org file for work related items.")
    (defvar org-default-file (if at-home org-home-file org-work-file)
      "Default org file.")
    (setq org-default-notes-file
          (if at-home "~/org/home/inbox.org" "~/org/inbox.org"))
    (setq org-agenda-files
          (append (list ;;org-default-file
                        org-default-notes-file)
                  (if at-home
                      '("~/org/home/mobile/inbox.org"
                        "~/org/home/jobs.org"))
                  ;; (directory-files "~/org" t "\\.org$")
                  ))
    (setq org-archive-location "%s_archive::")
    (add-hook 'org-mode-hook #'turn-on-auto-revert-mode)
    (setq org-refile-targets
          '((nil :maxlevel . 6)
            (org-agenda-files :maxlevel . 2)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (defun ic/org-jump ()
      "Jump to an `org-refile-targets' heading."
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'org-refile)))
    
    (defun ic/org-jump-in-buffer ()
      "Jump to a heading in the current buffer."
      (interactive)
      ;; `org-refile-targets' needs to be dynamically bound
      (defvar org-refile-targets)
      (let ((org-refile-targets '((nil :maxlevel . 999))))
        (ic/org-jump)))
    (defun ic/outline-current-heading-or-up ()
      "When point is on an org-mode heading: move to parent heading;
    otherwise: move to current heading."
      (interactive)
      (call-interactively (if (org-at-heading-p)
                              'outline-up-heading
                            'outline-previous-heading)))
    
    (defun ic/outline-current-heading-or-backward ()
      "when point is on an org-mode heading: move to previous heading;
    otherwise: move to previous heading"
      (interactive)
      (call-interactively (if (org-at-heading-p)
                              'org-backward-heading-same-level
                            'outline-previous-heading)))
    (setq org-startup-indented t)
    (setq org-startup-folded nil)
    (setq org-hide-leading-stars t)
    (setq org-odd-levels-only nil)
    
    (setq org-adapt-indentation nil)
    (setq org-indent-indentation-per-level 2)
    
    (setq org-fontify-whole-heading-line t)
    (setq org-fontify-done-headline nil)
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-pretty-entities t)
    ;; don't use super/sub-scripts as they mess with headings
    (setq org-pretty-entities-include-sub-superscripts nil)
    (add-hook 'org-mode-hook #'visual-line-mode)
    (add-hook 'org-mode-hook #'buffer-face-mode)
    ;; ⚫•⦾⦿—⬎⌄⌵↴⤵↘↓↷┅🅐🅑🅒
    ;; ⌵
    (setq org-ellipsis " …")
    
    ;; replace certain org-mode text with symbols
    (defun ic/org-mode-pretty-symbols ()
      "Enable `prettify-symbols-mode' and set `prettify-symbols-alist' for certain
    `org-mode' symbols."
      ;; don't show the prettified symbol around point
      (setq prettify-symbols-unprettify-at-point t)
      (setq prettify-symbols-alist
            `(("[#A]" . ?Ⓐ)
              ("[#B]" . ?Ⓑ)
              ("[#C]" . ?Ⓒ)
              ("[ ]" . ?)
              ("[X]" . ?)
              ("[-]" . ?)
              ("#+BEGIN_SRC" . ?λ)
              ("#+END_SRC" . ?ƛ)
              ("CLOSED:" . ?)
              ("SCHEDULED:" . ?)
              ("Scheduled:" . ?)
              ("Sched." . ?)
              ;; ("TODO" . ?)
              ("DEADLINE:" . ?)))
      ;; (print prettify-symbols-alist)
      (prettify-symbols-mode 1))
    (add-hook 'org-mode-hook #'ic/org-mode-pretty-symbols)
    (add-hook 'org-agenda-mode-hook #'ic/org-mode-pretty-symbols)
    (require 'org-element)
    
    (setq ic/org-list-icon (propertize "" 'face 'themian-subtle))
    
    (defun org+-match-item-marker (bound)
      "Match the bullet of itemizations."
      (and
       (re-search-forward "^ *\\(-\\) " bound t)
       (save-match-data
         (save-excursion
           (goto-char (match-end 1))
           (eq (org-element-type (org-element-at-point)) 'item)))))
    
    ;; change list dashes
    (font-lock-add-keywords
     'org-mode
     `((org+-match-item-marker
        (1
         '(face default display ,ic/org-list-icon)
         append)))
     t)
    (setq org-priority-faces '((?A . '(:inherit (themian-error org-priority)))
                               (?B . 'org-priority)
                               (?C . '(:inherit (themian-weak org-priority)))))
    (setq org-completion-use-ido t)
    (setq org-return-follows-link t)
    ;; (setq org-blank-before-new-entry nil)
    (setq org-hide-emphasis-markers t)
    (setq org-src-preserve-indentation nil)
    (setq org-edit-src-content-indentation 2)
    (setq org-src-window-setup 'current-window)
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (shell . t)))
    (setq org-persist-directory (concat user-cache-directory "org-persist/"))
    ;; custom agenda views
    (setq org-agenda-custom-commands
          '(
            ;; work agenda
            ("w" "Work Agenda"
             ((agenda "" nil)
              (tags "refile"
                    ((org-agenda-overriding-header "Refile tasks:")
                     (org-tags-match-list-sublevels nil))))
             ((org-agenda-tag-filter-preset '("-HOME" "-archive"))))
    
            ("W" "Work agenda today"
             ((agenda "" ((org-agenda-span 'week))))
             ((org-agenda-tag-filter-preset '("-HOME" "-archive"))))
    
            ;; home agenda
            ;; ("h" "Home agenda"
            ;;  (
            ;;   (tags "PRIORITY=\"A\""
            ;;         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
            ;;          (org-agenda-overriding-header
            ;;           (concat (ic/life-goals-formatted)
            ;;                   "\nToday's Tasks:"))))
            ;;   (agenda "" nil)
            ;;   (tags "refile"
            ;;         ((org-agenda-overriding-header "\nRefile Tasks:")
            ;;          (org-tags-match-list-sublevels nil)))
            ;;   )
            ;;  ((org-agenda-tag-filter-preset '("+HOME" "-archive")))
            ;;  ("~/test.html"))
    
            ("H" "Home agenda today"
             ((agenda "" ((org-agenda-span 'day))))
             ((org-agenda-tag-filter-preset '("+HOME" "-archive"))))
    
            ;; archives
            ("a" . "Archiving")
            ("ah" "Home archive"
             ((tags "+HOME-noarchive/DONE|CANCELLED|APPLIED"
                    ((org-agenda-overriding-header "Archive Tasks (*x$ to archive all):")))))
            ("aw" "Work archive"
             ((tags "-HOME-noarchive+TIMESTAMP_IA<=\"<-2w>\"/DONE|CANCELLED"
                    ((org-agenda-overriding-header "Archive Tasks (*x$ to archive all):")))))
    
            ;; test org-super-agenda
            ("h" "super test (home)"
             ((todo ""
                    ((org-agenda-overriding-header "")
                     (org-super-agenda-groups
                      '((:name "Refile"
                               :tag "refile")
                        (:name "Overdue"
                               :deadline past
                               :scheduled past)
                        (:name "Needs deadline"
                               :and (:priority "A" :deadline nil))
                        (:name "Needs scheduling"
                               :and (:priority "A" :scheduled nil))
                        (:name "Due Today"
                               :deadline today)
                        ;; (:name "Scheduled Today"
                        ;;        :date today
                        ;;        :scheduled today
                        ;;        :deadline today)
                        (:discard (:anything t))))))
              (agenda ""
                      ((org-agenda-overriding-header "")
                       (org-agenda-span 'week)))
              ;; (todo ""
              ;;       ((org-agenda-overriding-header "")
              ;;        (org-super-agenda-groups
              ;;         '(
              ;;           (:name "Scheduled Today"
              ;;                  :date today
              ;;                  :scheduled today
              ;;                  :deadline today)
              ;;           (:name "Upcoming"
              ;;                  :auto-planning t)
              ;;           (:discard (:anything t))))))
              )
             ((org-agenda-tag-filter-preset '("+HOME" "-archive"))))
            ))
    ;; don't show completed DEADLINE and SCHEDULED in agenda
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    
    ;; first day in agenda should be today
    (setq org-agenda-start-on-weekday nil)
    
    ;; show weekly agenda by default
    (setq org-agenda-span 'week)
    
    ;; don't have a separator between agenda sections
    (setq org-agenda-compact-blocks t)
    
    ;; setup format
    (setq org-agenda-prefix-format " %i %4e %?-12t")
    (setq org-super-agenda-date-format "%A, %e %B %Y")
    
    (setq org-agenda-sorting-strategy
          '((agenda habit-down time-up priority-down category-keep)
            (todo tag-up priority-down alpha-up)
            (tags priority-down category-keep)
            (search category-keep)))
    
    ;; highlight the current line
    (add-hook 'org-agenda-mode-hook 'hl-line-mode)
    
    ;; hide some tags I mostly use for filtering only
    (setq org-agenda-hide-tags-regexp "\\(HOME\\|WORK\\|agenda\\)")
    
    ;; show all future repeating entries
    (setq org-agenda-show-future-repeats nil)
    (defun ic-open-org-agenda ()
      "Open the correct org agenda based on location (home/work)."
      (interactive)
      (cond (at-home (org-agenda nil "h"))
            (at-work (org-agenda nil "w"))
            (t (org-agenda))))
    (defun ic/org-is-active-task-p (&optional state)
      "Returns `t' if the current task is a member of
    `org-not-done-keywords'. When specified, use STATE as the active
    state, defaulting to `org-not-done-keywords'."
      (member (org-get-todo-state)
              (if state (list state) org-not-done-keywords)))
    
    (defun ic/org-any-active-parent-p ()
      "Return `t' if any parent task is an active task."
      (save-excursion
        (widen)
        (let (active-parent)
          (while (and (not active-parent)
                      (org-up-heading-safe))
            (when (ic/org-is-active-task-p)
              (setq active-parent t)))
          active-parent)))
    
    (defun ic/org-any-active-children-p (&optional state)
      "Return `t' when any descendant is an active task. When
      specified, use STATE as the active state, defaulting to
      `org-not-done-keywords'."
      (save-excursion
        (let ((subtree-end (save-excursion
                             (org-end-of-subtree t)))
              (child-regex (format "^\\*\\{%d,\\} "
                                   (+ 1 (org-current-level))))
              active-child)
          (while (and (not active-child)
                      (re-search-forward child-regex
                                         subtree-end t))
            (when (ic/org-is-active-task-p state)
              (setq active-child t)))
          active-child)))
    
    (defun ic/org-is-project-p ()
      "Return `t' when the current task is considered a project."
      (and (ic/org-is-active-task-p)
           (not (ic/org-any-active-parent-p))
           (ic/org-any-active-children-p)))
    
    (defun ic/org-is-stuck-project-p ()
      "Return `t' when the current task is considered a stuck
    project."
      (and (ic/org-is-project-p)
           (not (ic/org-any-active-children-p "NEXT"))))
    
    (defun ic/org-skip-nonstuck-projects ()
      (let ((debug-on-error t))
        (save-excursion
          (widen)
          (unless (ic/org-is-stuck-project-p)
            (or (outline-next-heading) (point-max))))))
    (setq org-habit-preceding-days 7)
    (setq org-habit-following-days 4)
    (setq org-habit-graph-column 50)
    (setq org-habit-show-habits-only-for-today t)
    (setq org-habit-show-all-today nil)
    (setq org-habit-today-glyph ?@)
    (setq org-habit-completed-glyph ?*)
    
    ;; only show the first occurrence of a repeating task
    (setq org-agenda-show-future-repeats 'next)
    (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|"
                "CANCELLED(c@/!)" "PHONE" "MEETING" "HABIT(a)")))
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)
    ;; log status changes
    (setq org-log-done 'time)
    (setq org-log-redeadline 'time)
    (setq org-log-reschedule 'time)
    (setq org-log-into-drawer "LOGBOOK")
    (setq org-reverse-note-order nil)
    (setq ic/org-clock-in nil)
    (setq ic/org-clock-resume nil)
    
    (defun ic/org-capture-prompt (prompt symbol choices)
      "Call `(completing-read PROMPT CHOICES)' and save into SYMBOL.
    Inspired from: http://storax.github.io/blog/2016/05/02/org-capture-tricks/"
      (make-local-variable symbol)
      (set symbol (completing-read prompt choices)))
    
    (setq org-capture-templates
          `(("t" "todo" entry (file org-default-notes-file)
             "* TODO %?\n%u\n"
             ;; :clock-in ic/org-clock-in :clock-resume ic/org-clock-resume
             )
            ("T" "test org-protocol" entry (file org-default-notes-file)
             "* TODO %:description\n%U\n\n#+begin_quote\n%:initial\n#+end_quote\n/From: [[%:link][here]]./\n\n%?"
             :immediate-finish t
             )
            ("r" "reply" entry (file org-default-notes-file)
             "* TODO Reply to %:from on %:subject\nSCHEDULED: %t\n%u\n%a\n"
             :immediate-finish t
             ;; :clock-in ic/org-clock-in :clock-resume ic/org-clock-resume
             )
            ("n" "note" entry (file org-default-notes-file)
             "* %? :note:\n%u\n"
             ;; :clock-in ic/org-clock-in :clock-resume ic/org-clock-resume
             )
            ("m" "meeting" entry (file org-default-notes-file)
             "* MEETING with %? :meeting:\n%u"
             ;; :clock-in ic/org-clock-in :clock-resume ic/org-clock-resume
             )
            ("p" "phone call" entry (file org-default-notes-file)
             "* PHONE %? :phone:\n%u"
             ;; :clock-in ic/org-clock-in :clock-resume ic/org-clock-resume
             )
            ("b" "books")
            ("bp" "pleasure" entry (file+olp org-home-file "Books" "Pleasure")
             (function ic/org-capture-book)
             :immediate-finish t
             :empty-lines-after 1)
            ("bP" "pleasure (already read)" entry (file+olp org-home-file "Books" "Pleasure")
             (function ic/org-capture-book-read)
             :immediate-finish t
             :empty-lines-after 1)
            ("be" "enrichment" entry (file+olp org-home-file "Books" "Enrichment")
             (function ic/org-capture-book)
             :immediate-finish t
             :empty-lines-after 1)
            ("bE" "enrichment (already read)" entry (file+olp org-home-file "Books" "Enrichment")
             (function ic/org-capture-book-read)
             :immediate-finish t
             :empty-lines-after 1)
            ("bt" "technical" entry (file+olp org-home-file "Books" "Technical")
             (function ic/org-capture-book)
             :immediate-finish t
             :empty-lines-after 1)
            ("bT" "technical (already read)" entry (file+olp org-home-file "Books" "Technical")
             (function ic/org-capture-book-read)
             :immediate-finish t
             :empty-lines-after 1)
            ("H" "habit" entry (file org-default-notes-file)
             "* TODO %?\n%u\nscheduled: %(format-time-string \"<%y-%m-%d %a .+1d/3d>\")\n:properties:\n:style: habit\n:repeat_to_state: next\n:end:\n")
            ("R" "recipe" entry (file org-default-notes-file)
             "* %^{name}\n:PROPERTIES:\n:SOURCE: %^{source}\n:SERVINGS: %^{servings}\n:END:\n%U\n** Ingredients\n- %?\n** Steps\n-\n** Notes")
            ("J" "job" entry (file org-default-notes-file)
             "* TOAPPLY %^{company} - %^{title}\n%U\n[[%^{url}][Submission]]\n%?\n** Description\n%^{description}\n** Contact Info\n** Log\n")))
    (defconst ic/openlibrary-base-endpoint
      "http://openlibrary.org")
    
    (defvar ic/openlibrary-max-results 100
      "Maximum results requested from OpenLibrary.")
    
    (defvar ic/openlibrary--use-cached-data nil
      "Store results from OpenLibrary in cache?")
    
    (defvar ic/openlibrary--query-string ""
      "Query string to search OpenLibrary for. Internal use only.")
    (defun ic/openlibrary--request (url &optional args)
      (let (result)
        (request
          url
          :params args
          :parser 'json-read
          :sync t
          :timeout 10
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (error "error fetching (%s): %s"
                           url
                           (cdr error-thrown))))
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (setq result data))))
        result))
    (defun ic/openlibrary--parse-search-results (data)
      "Take DATA in OpenLibrary search.json format and parse it into a list of
    ALISTs."
      (mapcar
       (lambda (doc)
         (cl-flet*
             ((to-list (vec) (append vec nil))
              (get (attr &optional alist) (assoc-default attr (or alist doc)))
              (get-list (attr &optional alist) (to-list (get attr (or alist doc))))
              (get-first (attr &optional alist) (car (get-list attr (or alist doc)))))
           `((title . ,(get 'title))
             (query-title . ,(s-replace-regexp " " "+" (get 'title)))
             (ol-work-id . ,(get 'key))
             (ol-author-ids . ,(get-list 'author_key))
             (edition-count . ,(get 'edition_count))
             (first-year-published . ,(get 'first_publish_year))
             (pages-median . ,(get 'number_of_pages_median))
             (public-scan . ,(not (eq (get 'public_scan_b) ':json-false)))
             (authors . ,(get-list 'author_name))
             (goodreads-id . ,(get-first 'id_goodreads))
             (librarything-id . ,(get-first 'id_librarything)))))
       (assoc-default 'docs data)))
    (defun ic/openlibrary--add-work-data (doc)
      "Query OpenLibrary for the work contained in DOC and add in any needed data."
      (let* ((data (ic/openlibrary--request
                    (concat "http://openlibrary.org"
                            (assoc-default 'ol-work-id doc)
                            ".json")))
             (desc-raw (assoc-default 'description data))
             (desc-value (if (eq (type-of desc-raw) 'cons)
                             (assoc-default 'value desc-raw)
                           desc-raw))
             (desc-clean (if desc-value
                             (replace-regexp-in-string
                              ;; OL links in [title](link format)
                              "\\[\\(.*\\)](\\(.*\\))"
                              ;; replace with org-mode format
                              "[[\\2][\\1]]"
                              ;; remove windows new lines
                              (replace-regexp-in-string "\u000d" "" desc-value))
                           nil)))
        (cons (cons 'description desc-clean)
              doc)))
    (defun ic/openlibrary-search (query)
      "Run QUERY against OpenLibrary and return results as a list of association
    lists."
      (ic/openlibrary--parse-search-results
       (ic/openlibrary--request
        (concat ic/openlibrary-base-endpoint "/search.json")
        `(("q" . ,(split-string query " " t))
          ("limit" . ,ic/openlibrary-max-results)
          ("mode" . "everything")))))
    (defun ic/openlibrary-helm-find-book ()
      "Prompt user for query string to search OpenLibrary, then display helm buffer
    to select book. Returns an ALIST with OpenLibrary data."
      (interactive)
      (error "helm not installed")
      (setq ic/openlibrary--query-string
            (read-from-minibuffer "Search OpenLibrary: "))
      (helm :sources
            (helm-build-sync-source
                (format "OpenLibrary (%s)" ic/openlibrary--query-string)
              :candidates 'ic/openlibrary--helm-candidates
              :candidate-number-limit ic/openlibrary-max-results
              :fuzzy-match t
              :coerce 'ic/openlibrary--add-work-data)
            :buffer "*helm openlibrary*"))
    (defun ic/openlibrary--helm-candidates ()
      "Searches OpenLibrary for `ic/openlibrary--query-string', returning data in
    `((DISPLAY . DATA) ...)' format for use as a helm source."
      (mapcar 'ic/openlibrary--parse-helm-format
              (ic/openlibrary--parse-search-results
               (ic/openlibrary--request
                (concat ic/openlibrary-base-endpoint "/search.json")
                `(("q" . ,(split-string ic/openlibrary--query-string " " t))
                  ("limit" . ,ic/openlibrary-max-results)
                  ("mode" . "everything"))))))
    (defun ic/openlibrary--parse-helm-format (doc)
      "Take data in OpenLibrary format and return in `(DISPLAY . DATA)' format
    needed for helm."
      (cons
       (concat (propertize (assoc-default 'title doc) 'face 'helm-ol-title)
               (propertize " :: " 'face 'helm-ol-separator)
               (propertize (string-join (assoc-default 'authors doc) ", ")
                           'face 'helm-ol-author)
               (propertize " [" 'face 'helm-ol-separator)
               (format "%s" (assoc-default 'first-year-published doc))
               (propertize "] -- " 'face 'helm-ol-separator)
               (propertize (format "%s editions" (assoc-default 'edition-count doc))
                           'face 'helm-ol-editions))
       doc))
    (defun ic/openlibrary--lookup-work (works olid)
      "Given a list of association lists in WORKS, return the alist with 'ol-word-id
    equal to OLID."
      (when works
        (if (equal (cdr (assoc 'ol-work-id (car works)))
                   olid)
            (car works)
          (ic/openlibrary--lookup-work (cdr works) olid))))
    
    (defun ic/openlibrary-find-book (&optional query results)
      (interactive)
      (let* ((query (or query (read-from-minibuffer "Search OpenLibrary: ")))
             (results (or results (ic/openlibrary-search query)))
             (titles (mapcar (lambda (work) (alist-get 'title work))
                             results)))
        (ic/openlibrary--lookup-work
         results
         (completing-read
          (format "Search OpenLibrary (%s): " query)
          (lambda (str pred action)
            (if (eq action 'metadata)
                `(metadata
                  (annotation-function
                   . ,(lambda (cand)
                        (let ((work
                               (ic/openlibrary--lookup-work
                                results cand)))
                          (format " by %s [%d] %s {%s %s %s}"
                                  (alist-get 'authors work)
                                  (alist-get 'first-year-published work)
                                  (if-let ((editions (alist-get 'edition-count
                                                                work)))
                                      (format "%d editions" editions)
                                    "")
                                  (alist-get 'ol-work-id work)
                                  (alist-get 'goodreads-id work)
                                  (alist-get 'librarything-id work))))))
              (complete-with-action action titles str pred)))))))
    
    (defun ic/openlibrary-find-book (&optional query results)
      (interactive)
      (let* ((query (or query (read-from-minibuffer "Search OpenLibrary: ")))
             (results (or results (ic/openlibrary-search query)))
             (collection
              (mapcar
               (lambda (work)
                 (format "%s by %s [%s] %s"
                         (alist-get 'title work)
                         (or (and (alist-get 'authors work)
                                  (string-join (alist-get 'authors work) " & "))
                             "?")
                         (or (alist-get 'first-year-published work) "?")
                         (alist-get 'ol-work-id work)))
               results))
             (selected (completing-read
                        (format "Search OpenLibrary (%s): " query)
                        collection))
             (olid (car (last (split-string selected " ")))))
        (ic/openlibrary--lookup-work results olid)))
    (defun ic/org-capture-book (&optional already-read)
      "`org-capture' task for a new book. Prompts the user for a query string to
    search OpenLibrary against and generate an `org-mode' element from the resulting
      data."
      (let* ((result "")
             (doc (ic/openlibrary-find-book)))
        (cl-flet* ((add (fmt &rest args)
                        (when (-all? 'identity args)
                          (setq result (concat result
                                               (apply 'format fmt args))))))
          (add "* %s %s :: %s [%s]\n"
               (if already-read "DONE" "TODO")
               (assoc-default 'title doc)
               (string-join (assoc-default 'authors doc) ", ")
               (assoc-default 'first-year-published doc))
          (add ":PROPERTIES:\n")
          (add ":TITLE: %s\n" (assoc-default 'title doc))
          (add ":AUTHORS: %S\n" (assoc-default 'authors doc))
          (add ":PAGES_MEDIAN: %s\n" (assoc-default 'pages-median doc))
          (add ":FIRST_YEAR_PUBLISHED: %s\n" (assoc-default 'first-year-published doc))
          (add ":PUBLIC_SCAN: %s\n" (assoc-default 'public-scan doc))
          (add ":OL_AUTHOR_IDS: %S\n" (assoc-default 'ol-author-ids doc))
          (add ":OL_WORK_ID: %s\n" (assoc-default 'ol-work-id doc))
          (add ":GOODREADS_ID: %s\n" (assoc-default 'goodreads-id doc))
          (add ":LIBRARYTHING_ID: %s\n" (assoc-default 'librarything-id doc))
          (add ":END:\n")
          (add "%%u\n\n")
          (add "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n\n" (assoc-default 'description doc))
          (add "** Links\n")
          (add "- [[http://openlibrary.org%s][OpenLibrary]]\n"
               (assoc-default 'ol-work-id doc))
          (dolist (tuple (-zip-pair (assoc-default 'authors doc)
                                    (assoc-default 'ol-author-ids doc)))
            (add "- [[http://openlibrary.org/authors/%s][OpenLibrary - %s]]\n"
                 (cdr tuple) (car tuple)))
          (add (concat "- [[https://librarything.com"
                       (if-let (id (assoc-default 'librarything-id doc))
                           (format "/work/%s" id)
                         (format "/search.php?search=%s"
                                 (assoc-default 'query-title doc)))
                       "][LibraryThing]]\n"))
          (when (assoc-default 'public-scan doc)
            (add (concat "- [[https://www.gutenberg.org/ebooks/search/?query=%s]"
                       "[Project Gutenberg]]\n")
               (assoc-default 'query-title doc)))
          (add "- [[https://www.overdrive.com/search?q=%s][Overdrive]]\n"
               (assoc-default 'query-title doc))
          (add (concat "- [[https://goodreads.com"
                       (if-let (id (assoc-default 'goodreads-id doc))
                           (format "/book/show/%s" id)
                         (format "/search?q=%s&search_type=books"
                                 (assoc-default 'query-title doc)))
                       "][Goodreads]]\n"))
          (add "- [[https://amazon.com/s?k=%s][Amazon]]\n"
               (assoc-default 'query-title doc))
          (add "\n")
          (add "** Notes\n\n"))
        result))
    
    
    (defun ic/org-capture-book-read ()
      "Same as `ic/org-capture-book', but mark entry as DONE instead."
      (ic/org-capture-book t))
    (defface helm-ol-title nil
      "Face used for OpenLibrary titles in a helm buffer.")
    
    (defface helm-ol-author nil
      "Face used for OpenLibrary authors in a helm buffer.")
    
    (defface helm-ol-editions nil
      "Face used for OpenLibrary edition counts in a helm buffer.")
    
    (defface helm-ol-seperator nil
      "Face used for OpenLibrary seperators in a helm buffer.")
    (defun ic/maybe-org-capture-delete-other-windows (buf)
      "Maximize frame when starting an external org-capture"
      (let* ((buffer-name (if (bufferp buf) (buffer-name buf) buf))
             (is-capture-frame (equal "OrgCapture" (frame-parameter nil 'name)))
             (is-capture-buffer (or (equal "*Org Select*" buffer-name)
                                    (string-match "^CAPTURE-" buffer-name))))
        (when (and is-capture-frame is-capture-buffer)
          (delete-other-windows))))
    (advice-add 'org-switch-to-buffer-other-window :after #'ic/maybe-org-capture-delete-other-windows)
    
    (defun ic/maybe-org-capture-delete-frame ()
      "Close the frame when finalizing an external org-capture"
      (when (equal "OrgCapture" (frame-parameter nil 'name))
        (delete-frame)))
    (add-hook 'org-capture-after-finalize-hook #'ic/maybe-org-capture-delete-frame)
    (mapc
     (lambda (struct)
       (push struct org-structure-template-alist))
     (list
      '("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
      '("y" "#+BEGIN_SRC yaml\n?\n#+END_SRC")
      ))
    ;; don't modify the task state when archiving
    (setq org-archive-mark-done nil)
    ;; catch modifying collapsed text
    (setq org-catch-invisible-edits 'error)
    (defun ic-parse-org-date-string (time-str &optional next-day-when-no-time)
      "Translate TIME-STR into an emacs encoded time based on `org-read-date-*'
    syntax rules such as: +1d, -3w, etc. When NEXT-DAY-WHEN-NO-TIME is non-nil and
      TIME-STR does not contain a time component then use the next day. Returns nil
      on invalid time string."
      (setq org-time-was-given nil)
      (let* ((org-time (org-read-date-analyze
                        time-str
                        ;; encoded default time
                        (list 0 0 0 0)
                        ;; decoded default time
                        (list 0 0 0 0 0 0 0 0 0)))
             ;; `org-read-date-analyze' will return the default date if it failed to
             ;; parse a valid time string. We check for year 2000 because of the
             ;; following line in `org-read-date-analyze':
             ;;     (when (< year 100) (setq year (+ 2000 year)))
             (bad-time (list 0 0 0 0 0 2000))
             (encoded-time (apply #'encode-time org-time)))
        (cond ((equal org-time bad-time)
               (error "invalid time string: %s" time-str))
              ((or org-time-was-given (not next-day-when-no-time))
               ;; we either specified a time or we don't want to use the following day
               encoded-time)
              (t
               ;; we didn't specify a time and we want to use the *following* day
               (time-add encoded-time (* 24 60 60))))))
    ;; save id locations in a nicer place
    (setq org-id-locations-file (concat user-cache-directory "org-id-locations"))
    
    (setq org-id-files (append (directory-files "~/org" t "\\.org$")
                               (directory-files "~/org/home" t "\\.org$")
                               (and (boundp 'org-id-files) org-id-files)))
    (defvar ic/allow-babel-evaluate-directories
          '("~/org" "~/.etc"))
    
    (defun ic/org-confirm-babel-evaluate (_lang _body)
      (let ((filename (buffer-file-name))
            match)
        (dolist (path ic/allow-babel-evaluate-directories (not match))
          (when (string-prefix-p (expand-file-name path) filename)
            (setq match t)))))
    
    (setq org-confirm-babel-evaluate #'ic/org-confirm-babel-evaluate)
    (setq org-export-with-toc nil)
    (setq org-export-with-timestamps t)
    (setq org-babel-default-header-args
          '((:session . "none")
            (:results . "replace")
            (:exports . "both")
            (:cache   . "no")
            (:noweb   . "no")))
    (setq org-publish-project-alist
          '(("recipes"
             :base-directory "~/org/home/food/"
             :base-extension "org"
             :publishing-directory "~/food/"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4
             :auto-preamble t
             )))
    (add-hook 'org-export-before-parsing-hook 'ic/org-export-filter-recipes)
    (defun ic/yas-expand-> ()
      (interactive)
      (insert ">")
      (yas-expand))
    (defun ic/set-org-mode-tab-key ()
      (add-to-list 'org-tab-first-hook 'yas-expand)
      ;; XXX is this needed?
      ;; (general-define-key yas-keymap
      ;;   "<tab>" 'yas-next-field)
      )
    
    (add-hook 'org-mode-hook 'ic/set-org-mode-tab-key)
    (defun ic/org-insert-item-dwim ()
      "Insert an org item, with optional checkbox, below current item."
      (interactive)
      (when-let (item-pos (org-in-item-p))
        (goto-char item-pos)
        (org-insert-item (org-at-item-checkbox-p))
        (org-metadown)))
    
    (defun ic/org-insert-item-dwim-go-insert-mode ()
      "Call `ic/org-insert-item-dwim' and enter insert mode."
      (interactive)
      (when (ic/org-insert-item-dwim)
        (cond ((fboundp 'meow-insert-mode)
               (meow-insert-mode))
              ((fboundp 'evil-insert)
               (evil-insert 0)))))
    (add-hook 'org-todo-repeat-hook #'org-reset-checkbox-state-subtree)
    (defvar ic/weekly-reviews-directory "~/org/home/weekly-reviews")
    
    (defun ic/org-completed-date (heading)
      "Return the date of completion for HEADING."
      (org-timestamp-format (org-element-property :closed heading)
                            "%s"))
    
    (defun ic/org-completed-date-< (heading-left heading-right)
      "Return non-nil when the completed date of HEADING-LEFT is earlier than the
    completed date of HEADING-RIGHT."
      (let ((left-ts (ic/org-completed-date heading-left))
            (right-ts (ic/org-completed-date heading-right)))
        (cond ((and left-ts right-ts)
               (string< left-ts right-ts))
              (left-ts t)
              (right-ts nil))))
    
    (defun ic/org-element-get-link (elem &optional link-title)
      "Get a link (as a string) to the org-element ELEM."
      (let* ((marker (org-element-property :org-marker elem))
             (level (org-element-property :level elem))
             (title (org-element-property :raw-value elem))
             (search (org-with-point-at marker
                       (org-link-heading-search-string)))
             (file (buffer-file-name (marker-buffer marker)))
             (link 
              (format "[[%s][%s]]"
                      (format "file:%s::%s" file search)
                      (or link-title title))))
        (set-text-properties 0 (length link) nil link)
        link))
    
    (defun ic/weekly-review-items ()
      "Return a string containing all items needed for the weekly review in a form
    for direct insertion into a `yasnippit'."
      (let ((org-files (seq-filter #'file-exists-p
                                   (mapcar #'expand-file-name org-agenda-files)))
            current-date)
        (mapconcat (lambda (heading)
                     (let* ((ts (org-element-property :closed heading))
                            (date (format "%s-%s-%s"
                                          (org-element-property :year-start ts)
                                          (org-element-property :month-start ts)
                                          (org-element-property :day-start ts)))
                            (link (ic/org-element-get-link heading)))
                       (concat
                        (when (not (equal current-date date))
                          (setq current-date date)
                          (format-time-string "%A, %d %B %Y\n"
                                              (org-timestamp-to-time ts)))
                        "- " link)))
                   (org-ql-select org-files
                     '(and (tags "HOME")
                           (not (tags "noarchive"))
                           (todo "DONE" "APPLIED"))
                     :action 'element-with-markers
                     :sort #'ic/org-completed-date-<)
                   "\n")))
    
    (defun ic/weekly-review ()
      "Create a weekly review `org-mode' buffer with all completed tasks."
      (interactive)
      (let* ((year (format-time-string "%Y"))
             (week (format-time-string "%U"))
             (directory (format "%s/%s" ic/weekly-reviews-directory year))
             (filename (format "%s/weekly-review-%s.org" directory week)))
        (unless (file-directory-p directory)
          (make-directory directory t))
        (when (or (not (file-exists-p filename))
                  (yes-or-no-p (format "Weekly review for week %s of %s already exists. Delete and re-create? "
                                       week year)))
          (find-file filename)
          (erase-buffer)
          (org-mode)
          (yas-expand-snippet (yas-lookup-snippet "weekly review"))
          (save-buffer))))
    ;;; functions taken from http://doc.norang.ca/org-mode.html
    
    (defun bh/skip-non-archivable-tasks ()
      "Skip trees that are not available for archiving"
      (save-restriction
        (widen)
        ;; Consider only tasks with done todo headings as archivable candidates
        (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
              (subtree-end (save-excursion (org-end-of-subtree t))))
          (if (member (org-get-todo-state) org-todo-keywords-1)
              (if (member (org-get-todo-state) org-done-keywords)
                  (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                         (a-month-ago (* 60 60 24 (+ daynr 1)))
                         (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                         (this-month (format-time-string "%Y-%m-" (current-time)))
                         (subtree-is-current (save-excursion
                                               (forward-line 1)
                                               (and (< (point) subtree-end)
                                                    (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                    (if subtree-is-current
                        subtree-end ; Has a date in this month or last month, skip it
                      nil))  ; available to archive
                (or subtree-end (point-max)))
            next-headline))))
    
    
    (defun ic/skip-non-archivable-tasks ()
      "Skip tasks that I don't want to archive"
      (or (ic/skip-old-meal-plans)
          (bh/skip-non-archivable-tasks)))
    
    
    (defun ic/skip-old-meal-plans ()
      "Skip over a task if it's 1) a meal plan and 2) scheduled before today."
      (save-restriction
        (widen)
        (let* ((tags (org-get-tags))
               (subtree-end (save-excursion (org-end-of-subtree t)))
               (scheduled (org-get-scheduled-time (point)))
               (yesterday (* 60 60 24 1))
               ;; HACK we check 24 hours ago rather than previous midnight
               (now (time-subtract (current-time) (seconds-to-time yesterday))))
          (if (and (member "cooking" tags)
                   (member "plan" tags)
                   scheduled
                   (time-less-p scheduled now))
              subtree-end
            nil))))
    
    
    (defun ic/org-recipe-publish-to-html (_plist org-filename target-dir)
      "Export ORG-FILENAME as html. Similar to `org-html-publish-to-html', but only exporting certain sections"
      (interactive)
      (let* ((base-name (file-name-sans-extension (file-name-nondirectory org-filename)))
             (target-filename (concat target-dir base-name ".html")))
        (with-temp-file target-filename
          (insert-file-contents org-filename)
          (goto-char (point-min))
          )
        )
      )
    
    
    (defun ic/org-export-filter-recipes (backend)
      "Filter out unwanted elements from org-mode recipes, but only when export as HTML."
      (when (equal backend 'html)
        (org-map-entries
         (lambda ()
           )
         ;; (lambda () (delete-region (point) (progn (forward-line) (point)))))
        )
      ))
    :general
    (local-leader-keys org-mode-map
      "'" 'org-edit-special
      ":" 'org-set-tags-command
      "^" 'org-sort
      "A" 'org-archive-subtree
      "D" 'org-insert-drawer
      "d" 'org-deadline
      "e" 'org-set-effort
      "H" 'org-shiftleft
      "J" 'org-shiftdown
      "K" 'org-shiftup
      "L" 'org-shiftright
      "l" 'org-open-at-point
      "N" 'widen
      "n" 'org-narrow-to-subtree
      "p" 'org-priority
      "P" 'org-set-property
      "r" 'org-refile
      "s" 'org-schedule
      "t" 'org-todo
      "w" 'org-save-all-org-buffers
      )
    (local-leader-keys org-mode-map
      :infix "C"
      "" '(nil :which-key "clocks")
      "i" 'org-clock-in
      "j" 'ic/org-clock-jump-to-current-clock
      "o" 'org-clock-out
      "q" 'org-clock-cancel)
    (local-leader-keys org-mode-map
      :infix "E"
      "" '(nil :which-key "export")
      "e" 'org-export-dispatch
      "T" 'org-babel-tangle-file
      "t" 'org-babel-tangle)
    (local-leader-keys org-mode-map
      :infix "i"
      "" '(nil :which-key "insert")
      "a" 'org-attach
      "f" 'org-footnote-new
      "l" 'org-insert-link
      "t" 'org-time-stamp
      "T" 'org-time-stamp-inactive
      "s" 'org-download-screenshot
      "y" 'org-download-yank
      )
    
    (general-define-key
     :keymaps '(org-agenda-mode-map org-columns-map)
     "e" 'org-agenda-set-effort
     "p" 'org-agenda-priority
     "?" 'org-agenda-filter
     ;; why is is the local leader getting overridden?
     meow-local-leader-prefix (lookup-key org-agenda-mode-map (kbd emacs-local-leader-prefix))
     )
    
    (local-leader-keys org-agenda-mode-map
      "d" 'org-agenda-deadline
      "e" 'org-agenda-set-effort
      "p" 'org-agenda-priority
      "P" 'org-agenda-set-property
      "r" 'org-agenda-refile
      "s" 'org-agenda-schedule
      "t" 'org-agenda-todo
      )
    (local-leader-keys
      :keymaps 'org-src-mode-map
      "c" '(org-edit-src-exit :which-key "org-edit-src-exit")
      "k" 'org-edit-src-abort
      )
    (leader-keys
      :infix "o"
      "" '(nil :which-key "org-mode")
      "a" 'org-agenda
      "c" 'org-capture
      "j" 'ic/org-jump-in-buffer
      "J" 'ic/org-jump
      )
    
    (leader-keys
      :infix "oC"
      "" '(nil :which-key "clocks")
      "f" 'org-clock-modify-effort-estimate
      "i" 'org-clock-in
      "g" 'org-clock-goto
      "o" 'org-clock-out
      "l" 'org-clock-in-last
      "r" 'org-clock-report
      )
    (general-define-key
     "<f12>" 'ic-open-org-agenda
     "M-<f12>" 'org-clock-goto
     )
    (general-define-key
      :keymaps 'org-mode-map
    
      "<left>"      'ic/outline-current-heading-or-up
      "<up>"        'ic/outline-current-heading-or-backward
      "<down>"      'org-forward-heading-same-level
      "<right>"     'outline-next-visible-heading
    
      "S-<left>"    'org-promote-subtree
      "S-<up>"      'org-move-subtree-up
      "S-<down>"    'org-move-subtree-down
      "S-<right>"   'org-demote-subtree
    
      "C-S-<left>"  'org-do-promote
      "C-S-<right>" 'org-do-demote
    
      "M-<return>" 'ic/org-insert-item-dwim-go-insert-mode
      )
    (general-define-key
        :keymaps 'org-mode-map
        ">" 'ic/yas-expand->
        "M-<return>" 'ic/org-insert-item-dwim
        )
    )

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("•"))
  :hook (org-mode . org-bullets-mode))

(require 'org-protocol)

(use-package org-roam
  :demand t
  :requires org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (expand-file-name "~/src/roam/"))
  (setq org-roam-db-location
        (concat user-cache-directory "org-roam.db"))
  (setq org-roam-completion-everywhere t)

  ;; org-id doesn't play well with symlinks, it resolves them by calling
  ;; `file-truename'. This breaks org-roam since files can now live in two
  ;; places: the symlink'd location and the true location. So I want to disable
  ;; following the directory symlinks in `org-id-update-id-locations'. It will
  ;; still resolve file symlinks, but ignore directory symlinks.
  (defun ic/dont-follow-directory-symlinks (fn &rest args)
    "Don't call `file-truename' to resolve symlinks."
    (cl-letf (((symbol-function 'file-truename) #'file-chase-links))
      (apply fn args)))
  (advice-add 'org-id-update-id-locations
              :around #'ic/dont-follow-directory-symlinks)

  :config
  ;; the org-element cache messes with correct category lookup and seems to
  ;; break org-roam sometimes, so disable it
  (setq org-element-use-cache nil)
  (org-roam-setup)
  (org-roam-db-autosync-mode 1)
  (require 'org-roam-protocol)
  (setq org-id-files
        (seq-uniq (append (org-roam-list-files)
                          (and (boundp 'org-id-files) org-id-files))))

  :bind (("<f2>" . org-roam-capture)
         :map global-leader-map
         :prefix "z"
         :prefix-map zetian
         ("b" . org-roam-buffer-toggle)
         ("F" . zetian-select-find)
         ("u" . org-roam-ui-open)
         ("U" . zetian-update-git)
         :prefix "zc"
         :prefix-map zetian-capture
         ("c" . org-roam-capture)
         :prefix "zcb"
         :prefix-map zetian-capture-books
         ("b" . zetian-capture-book)
         ("p" . zetian-capture-book-pleasure-toread)
         ("P" . zetian-capture-book-pleasure-completed)
         ("e" . zetian-capture-book-enrichment-toread)
         ("E" . zetian-capture-book-enrichment-completed)
         :prefix "zd"
         :prefix-map zetian-database
         ("c" . org-roam-db-clear-all)
         ("d" . org-roam-db-diagnose-node)
         ("l" . zetian-lint)
         ("s" . org-roam-db-sync)
         ("t" . zetian-report-tasks)
         :prefix "zf"
         :prefix-map zetian-find
         ("f" . zetian-find-all)
         ("a" . zetian-find-area)
         ("A" . zetian-find-author)
         ("b" . zetian-find-book)
         ("c" . zetian-find-category)
         ("e" . zetian-find-open-event)
         ("g" . zetian-find-random-review)
         ("G" . zetian-find-random-permanent)
         ("l" . zetian-find-link-to)
         ("L" . zetian-find-link-from)
         ;; ("o" . zetian-find-task-open-orphaned)
         ("p" . zetian-find-project)
         ("P" . zetian-find-stuck-project)
         ("r" . zetian-find-review)
         ("s" . zetian-find-state)
         ;; ("S" . zetian-find-strange)
         ("t" . zetian-find-next-task)
         :prefix "zfT"
         :prefix-map zetian-find-tasks
         ("a" . zetian-find-task)
         ;; ("l" . zetian-find-task-open-link)
         ("n" . zetian-find-next-task)
         ("o" . zetian-find-open-task)
         ("p" . zetian-find-next-project-task)
         ("P" . zetian-find-open-project-task)
         ("T" . zetian-find-task)
         :prefix "zi"
         :prefix-map zetian-import
         ("m" . zetian-moonreader-import)
         :prefix "zj"
         :prefix-map zetian-journal
         ("j" . zetian-journal-goto-today)
         ("m" . zetian-journal-goto-month)
         ("t" . zetian-journal-goto-today)
         ("w" . zetian-journal-goto-week)
         ("y" . zetian-journal-goto-year)
         ))

(local-leader-keys 'org-mode-map
  :infix "z"
  "" '(nil :which-key "zetian")
  "c" #'zetian-set-context
  "i" #'zetian-link-insert-immediate
  "I" #'zetian-link-insert-search-immediate
  "l" #'zetian-find-link-from-current
  "L" #'zetian-find-link-to-current
  "R" #'zetian-rename-current
  "s" #'zetian-set-state
  "t" #'zetian-set-tag
  "p" #'zetian-set-project
  )

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-ql)

(use-package org-transclusion
  :config
  (setq org-transclusion-exclude-elements '(keyword property-drawer))

  ;; (defun ic/org-transclusion-add-background-face (beg end)
  ;;   (put-text-property beg end 'face 'org-block))

  ;; (add-hook 'org-transclusion-after-add-functions
  ;;           #'ic/org-transclusion-add-background-face)
  ;; (remove-hook 'org-transclusion-after-add-functions
  ;;              #'ic/org-transclusion-add-background-face)
  )

(use-package projectile
  :demand t
  :init
  ;; ensure projectile saves its files in a nice location
  (setq projectile-cache-file
        (concat user-cache-directory "projectile.cache"))
  (setq projectile-known-projects-file
        (concat user-cache-directory "projectile-bookmarks.eld"))

  :config
  (projectile-mode 1)
  (setq projectile-globally-ignored-file-suffixes
        '(
          ;; unity stuff
          ".meta" ".unity" ".asset" ".mat" ".cginc" ".prefab"
          ".renderTexture" ".lighting" ".shadergraph" ".shadersubgraph"
          ".shader" ".sceneWithBuildSettings" ".hlsl" ".vfx"
          ;; images
          ".png" ".xcf" ".jpg" ".jpeg" ".tif"
          ;; fonts
          ".ttf"
          ;; misc
          ".pdf"
          ))
  (setq projectile-indexing-method 'hybrid)

  :general
  (leader-keys
    "p" 'projectile-command-map))

(use-package request)

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode 1)
  ;; don't run smartparens in the minibuffer
  (add-hook 'minibuffer-mode-hook 'turn-off-smartparens-strict-mode)

  ;; XXX this is shadowing the insert mode binding for some reason
  ;; :general
  ;; (general-define-key
  ;;  :keymaps 'emacs-lisp-mode-map
  ;;  "^" #'sp-backward-up-sexp)
  )

(use-package spaceline
    :config
    ;; disable separators
    (setq powerline-default-separator nil)

    ;; change face based on evil state
    ;; TODO add meow state function
    ;; (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

    ;; ensure the mode-line is re-compiled when switching themes
    (advice-add 'load-theme :after
                (lambda (&rest args)
                  (spaceline-compile))))

(use-package spaceline-all-the-icons
    :after (all-the-icons spaceline)
    :config
    (defun ic/icon (name &optional group)
      (setf group (or group 'alltheicon))
      (let* ((icon (funcall (intern (format "all-the-icons-%s" group))
                            name))
             (family (plist-get (plist-get (text-properties-at 0 icon)
                                           'face)
                                :family))
             (face `(:family ,family :inherit)))
        (propertize icon 'face face 'font-lock-face face 'display '(raise 0.1))))
    (defun ic//vc-icon-git ()
      (let* ((branch (cadr (split-string vc-mode "Git[:-]")))
             ;; (project-name (second (nreverse (split-string (projectile-project-root) "/"))))
             (vc-state-icon (pcase (vc-state (buffer-file-name))
                              ('up-to-date "-")
                              ('edited "*")
                              ('added "^")
                              ('removed' "v")
                              ('conflict "!")
                              ('missing "m")
                              ('ignored "i")
                              ('unregistered "x")
                              (_ "?")))
             (branch-icon (if (string= branch "master")
                              (ic/icon "git")
                            (concat
                             (ic/icon "git-branch" 'octicon)
                             " "
                             branch))))
        (concat vc-state-icon " " branch-icon)))
    
    (spaceline-define-segment
        ic-vc-icon "A segment for the Version Control icon"
        (when vc-mode
          (cond ((string-match "Git[:-]" vc-mode) (ic//vc-icon-git))
                (t (propertize (format "%s" vc-mode))))))
    (spaceline-define-segment
        ic-project-name "A segment for the active projectile project."
        (let ((project (projectile-project-name))
              (persp (safe-persp-name (get-frame-persp))))
          (if (equal project persp)
              project
            (concat project "|" persp))))
    (spaceline-define-segment
        ic-tablet-fix "A segment to hack around the screen flickering on the Surface
        Pro 4."
        (let ((seconds (% (floor (time-to-seconds)) 2)))
          (if (= seconds 0) "☳" "☷")))
    
    (when on-tablet
      (setq ic//tablet-fix-timer (run-at-time nil 1 #'force-mode-line-update))
      ;; (cancel-timer ic//tablet-fix-timer)
      )
    ;; taken from: https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-core.el
    (defun doom-modeline--create-bar-image (face width height)
      "Create the bar image.
    Use FACE1 for the bar, FACE2 for the background.
    WIDTH and HEIGHT are the image size in pixels."
      (when (and ;;(display-graphic-p)
                 (image-type-available-p 'pbm))
        (propertize
         " " 'display
         (let ((color (or (face-background face nil t) "None")))
           (ignore-errors
             (create-image
              (concat (format "P1\n%i %i\n" width height)
                      (make-string (* width height) ?1)
                      "\n")
              'pbm t :foreground color :ascent 'center))))))
    ;; bring in bundled segments
    (require 'spaceline-segments)
    
    ;; and define our custom mode-line
    (spaceline-compile
      'main
      ;; left side
      '((window-number
         :priority 100
         ;; :fallback evil-state
         :face highlight-face)
        ((buffer-modified
          all-the-icons-mode-icon
          remote-host)
         :priority 95)
        ((buffer-id)
         :priority 96)
        ((anzu
          selection-info)
         :priority 90)
        ((flycheck-error
          flycheck-warning
          flycheck-info)
         :priority 85)
        process)
    
      ;; right side
      '(((org-clock) :when active)
        ((ic-vc-icon)
         :priority 87)
        all-the-icons-git-status
        ;; XXX (projectile-root) is really slow outside a project
        ;; (projectile-root)
        ;; ((buffer-encoding-abbrev buffer-size
        ;;   input-method)
        ;;  :priority 5)
        (((ic-tablet-fix :when (and active on-tablet))
          buffer-position)
         :priority 91)))
    
    ;; and enable it
    (let ((spacing-image (doom-modeline--create-bar-image
                          'powerline-active1 1 30)))
      (setq-default mode-line-format `(,(concat spacing-image "%e")
                                       (:eval (spaceline-ml-main)))))
    
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (setq mode-line-format (default-toplevel-value 'mode-line-format)))))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(zetian-mode all-the-icons-material "local_florist"
                               :height 1.0 :v-adjust -0.1))
    )

(use-package org-super-agenda
  :config
  (org-super-agenda-mode 1))

(use-package visual-fill-column
  :init (setq-default fill-column 80)
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package wgrep
  ;; :init
  ;; (add-hook 'helm-occur-mode-hook #'wgrep-change-to-wgrep-mode)
  )

(use-package winum
    :demand t
    :config
    ;; I want frames numbered only within a frame, not globally
    (setq winum-scope 'frame-local)
    ;; spaceline handles window numbers
    (setq winum-auto-setup-mode-line nil)
    (winum-mode)

    (defun ic/winum-delete-window-0-or-10 (&optional _arg)
      "Delete window 0 if assigned or 10 if exists."
      (interactive "P")
      (winum-select-window-0-or-10 '(4)))

    (defmacro defun-ic--winum-delete-window-num (n)
      "Create function for calling `winum-select-window-by-number'
so that it kills window N."
      `(defun ,(intern (format "ic/winum-delete-window-%d" n)) (&optional _arg)
         ,(format "Delete window %d." n)
         (interactive "P")
         (winum-select-window-by-number ,(- n))))

    (defun-ic--winum-delete-window-num 1)
    (defun-ic--winum-delete-window-num 2)
    (defun-ic--winum-delete-window-num 3)
    (defun-ic--winum-delete-window-num 4)
    (defun-ic--winum-delete-window-num 5)
    (defun-ic--winum-delete-window-num 6)
    (defun-ic--winum-delete-window-num 7)
    (defun-ic--winum-delete-window-num 8)
    (defun-ic--winum-delete-window-num 9)

    :general
    (leader-keys
     "0" '(winum-select-window-0-or-10 :which-key ("0..9" . "winum-window 0..9"))
     "1" '(winum-select-window-1 :which-key t)
     "2" '(winum-select-window-2 :which-key t)
     "3" '(winum-select-window-3 :which-key t)
     "4" '(winum-select-window-4 :which-key t)
     "5" '(winum-select-window-5 :which-key t)
     "6" '(winum-select-window-6 :which-key t)
     "7" '(winum-select-window-7 :which-key t)
     "8" '(winum-select-window-8 :which-key t)
     "9" '(winum-select-window-9 :which-key t)
     "C-0" '(ic/winum-delete-window-0-or-10 :which-key ("C-0..9" . "ic/winum-delete-window 0..9"))
     "C-1" '(ic/winum-delete-window-1 :which-key t)
     "C-2" '(ic/winum-delete-window-2 :which-key t)
     "C-3" '(ic/winum-delete-window-3 :which-key t)
     "C-4" '(ic/winum-delete-window-4 :which-key t)
     "C-5" '(ic/winum-delete-window-5 :which-key t)
     "C-6" '(ic/winum-delete-window-6 :which-key t)
     "C-7" '(ic/winum-delete-window-7 :which-key t)
     "C-8" '(ic/winum-delete-window-8 :which-key t)
     "C-9" '(ic/winum-delete-window-9 :which-key t)
     )
    )

(use-package yaml-mode
  :demand t
  )

(use-package yasnippet
    :init
    (setq yas-snippet-dirs (list (concat user-data-directory "snippets")))
    (yas-global-mode 1))

(defun ic/csharp-authoring-base-filename ()
  "Return base part of '*.cs' filename."
  (string-remove-suffix
   ".cs"
   (file-name-nondirectory (buffer-file-name))))

(defun ic/csharp-authoring-base ()
  "Return base part of '*Authoring.cs' filename"
  (string-remove-suffix "Authoring" (ic/csharp-authoring-base-filename)))

(defun ic/csharp-authoring-component-menu ()
  "Return a string suitable for C# `AddComponentMenu'"
  (let ((case-fold-search nil))
    (concat (s-replace "." "/" (or (yas-field-value 1) ""))
            "/"
            (string-trim-left
             (s-replace-regexp (rx upper)
                               (lambda (m) (concat " " m))
                               (concat (ic/csharp-authoring-base)
                                       (yas-field-value 2)))
             " "))))

(defun ic/csharp-authoring-variables ()
  "Return a list of defined variable names"
  (when (yas-text)
    (mapcar (lambda (s)
              (car (reverse (split-string s " "))))
            (split-string (yas-text) ";" t " \n"))))

(defun ic/csharp-authoring-assign-variables ()
  "Return a string where each variable is assigned from the authoring class"
  (let ((indent (s-repeat 24 " ")))
    (string-join (mapcar (lambda (v)
                           (format "%s = auth.%s" v v))
                         (ic/csharp-authoring-variables))
                 (concat ",\n" indent))))

(add-hook 'Info-mode-hook 'variable-pitch-mode)

(defun ic/lisp-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq tab-width 8)
  (setq lisp-indent-function 'common-lisp-indent-function))

(add-hook 'lisp-mode-hook #'ic/lisp-mode-setup)

(defun ic/eval-outer-sexp-dwim ()
  "Evaluate the outermost sexp at POINT, stopping at any (def*) found."
  (interactive)
  (save-excursion
    (condition-case nil
        ;; move up as much as possible
        (while (not (looking-at "(def"))
          (up-list -1 t))
      (error nil))
    ;; are we on a left paren?
    (if (eq ?\( (following-char))
        (progn
          ;; move to the end
          (forward-sexp)
          ;; and evaluate
          (call-interactively 'eval-last-sexp))
      (message "Cannot evaluate: not inside an sexp"))))

(defun ic/eval-enclosing-sexp ()
  "Evaluate the sexp enclosing POINT."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn
          (up-list -1 t)
          (forward-sexp)
          (backward-char))
      (error
       (message "Cannot evaluate: not inside an sexp"))
      (:success
       (call-interactively 'eval-last-sexp)))))

(local-leader-keys
  :keymaps 'emacs-lisp-mode-map
  "e" '(ic/eval-enclosing-sexp :which-key "ic/eval-enclosing-sexp")
  "E" 'ic/eval-outer-sexp-dwim
  "B" 'eval-buffer
  "M" 'emacs-lisp-macroexpand
  )

(defun ic/prog-mode-setup ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (company-mode 1)
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'prog-mode-hook #'ic/prog-mode-setup)

(eval-when-compile
  (require 'python))

(defun ic/python-mode-setup ()
  (setq python-indent-offset tab-width)
  (lsp))

(add-hook 'python-mode-hook #'ic/python-mode-setup)

(add-hook 'text-mode-hook 'flyspell-mode)

(winner-mode 1)

;; disable unwanted UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; static cursor
(blink-cursor-mode -1)

;; don't show the initial emacs screen
(setq inhibit-startup-screen t)
;; start in the *Messages* buffer
(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
;; let me use y/n in prompts
(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8-unix)

;; don't open ediff controls in a new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; use a more comfortable window split
(setq ediff-split-window-function 'split-window-horizontally)

;; restore old window configuration on exit
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(defvar ic//ediff-buffer-A)
(defvar ic//ediff-buffer-B)

(defun ic/ediff-current-buffer-with-disk ()
  "Run `ediff' against the current buffer and the file on disk."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer not visiting a file."))
  (let* ((fn (buffer-file-name))
         (disk-buf-name (format " *disk version of %s*"
                                (file-name-nondirectory fn)))
         (disk-buf (with-current-buffer (get-buffer-create disk-buf-name)
                     (erase-buffer)
                     (insert-file-contents-literally fn)
                     (current-buffer)))
         (emacs-contents (save-restriction
                           (widen)
                           (buffer-substring-no-properties (point-min) (point-max))))
         (emacs-buf-name (format " *emacs version of %s*"
                                 (file-name-nondirectory fn)))
         (emacs-buf (with-current-buffer (get-buffer-create emacs-buf-name)
                      (erase-buffer)
                      (insert emacs-contents)
                      (current-buffer))))
    (setq ic//ediff-buffer-A disk-buf)
    (setq ic//ediff-buffer-B emacs-buf)
    (add-hook 'ediff-quit-hook #'ic//ediff-cleanup)
    (ediff-buffers disk-buf emacs-buf)))

(defun ic//ediff-cleanup ()
  "Clean up the old buffers after calling `ic/ediff-current-buffer-with-disk'."
  (dolist (buf (list ic//ediff-buffer-A ic//ediff-buffer-B))
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (remove-hook 'ediff-quit-hook #'ic//ediff-cleanup))

;; cache/ directory
(setq recentf-save-file (concat user-cache-directory "recentf"))
(setq eshell-history-file-name (concat user-cache-directory "eshell-history"))
(setq savehist-file (concat user-cache-directory "savehist-history"))
(setq request-storage-directory (concat user-cache-directory "request"))
(setq transient-history-file (concat user-cache-directory "transient-history.el"))
(setq transient-values-file (concat user-cache-directory "transient-values.el"))
(setq transient-levels-file (concat user-cache-directory "transient-levels.el"))
(setq bookmark-default-file (concat user-cache-directory "bookmarks"))
(setq tramp-persistency-file-name (concat user-cache-directory "tramp"))

;; cache/{backups,auto-saves}
(let ((backup-dir (concat user-cache-directory "backups/"))
      (autosave-dir (concat user-cache-directory "auto-saves/")))
  ;; create directories if needed
  (dolist (path (list backup-dir autosave-dir))
    (unless (file-directory-p path)
      (make-directory path t)))
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
  (setq auto-save-list-file-prefix (concat autosave-dir "saves-"))
  (setq tramp-backup-directory-alist `((".*" . ,backup-dir)))
  (setq tramp-auto-save-directory autosave-dir))

;; data/ directory
(setq abbrev-file-name (concat user-data-directory "abbrev"))

(defun ic/yank-buffer-to-clipboard ()
  "Yank contents of current buffer to the clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun ic/paste-clipboard-to-buffer ()
  "Overwrite the current buffer with the clipboard contents."
  (interactive)
  (when (yes-or-no-p "Overwrite current buffer with clipboard?")
    (widen)
    (erase-buffer)
    (clipboard-yank)))

(defun ic/yank-current-filename ()
  "Yank the full filename of the currently active buffer. Return
`nil' if no underlying file."
  (interactive)
  (let ((filename (or (buffer-file-name) "nil")))
    (kill-new filename)
    (message filename)))

(defvar ic//current-scratch nil
  "The most recently created *scratch* buffer.")

(defun ic/switch-to-scratch-buffer (arg)
  "Switch to (or create) the `*scratch*' buffer."
  (interactive "P")
  (when (or (equal arg '(4))
            (not (buffer-live-p ic//current-scratch)))
    (setq ic//current-scratch (ic//create-scratch-buffer)))
  (pop-to-buffer-same-window (buffer-name ic//current-scratch)))

(defun ic//create-scratch-buffer ()
  "Create a new *scratch* buffer."
  (let* ((buffers (seq-filter
                   (lambda (buf)
                     (string-prefix-p "*scratch"
                                      (buffer-name buf)))
                   (buffer-list)))
         (number (+ 1 (length buffers)))
         (name (format "*scratch-%d*" number)))
    (with-current-buffer (get-buffer-create name)
      (org-mode)
      (insert (format "#+TITLE: Scratch Buffer %d\n\n" number))
      (current-buffer))))

(setq ibuffer-saved-filter-groups
      '(("default"
         ("zetian" (or
                    (mode . zetian-mode)
                    (name . "^zetian:")
                    (name . "^\\*zetian")
                    (filename . "org/home/roam")))
         ("org" (or
                 (mode . org-mode)
                 (name . "^\\*Org Src")
                 (name . "^\\*Org Agenda\\*$")))
         ("tramp" (name . "^\\*tramp.*"))
         ("magit" (or
                   (mode . magit-mode)
                   (mode . magit-diff-mode)
                   (mode . magit-process-mode)
                   (name . "^magit.*")))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Warnings\\*$")
                   (name . "^\\*Shell Command Output\\*$")
                   (name . "^\\*Async-native-compile-log\\*$")
                   (name . "^\\*straight-")))
         ("ediff" (or
                   (name . "^\\*ediff.*")
                   (name . "^\\*Ediff.*")))
         ("dired" (mode . dired-mode))
         ("terminal" (or
                       (mode . term-mode)
                       (mode . shell-mode)
                       (mode . eshell-mode)))
         ("help" (or
                  (name . "^\\*Help\\*$")
                  (name . "^\\*info\\*$")
                  (name . "^\\*helpful"))))))

;; use our custom groupings by default
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show empty groups
(setq ibuffer-show-empty-filter-groups nil)

(defun ic/edit-init-file ()
  "Open the `init. l' file for editing."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun ic/edit-early-init-file ()
  "Open the `early-init.el' file for editing."
  (interactive)
  (find-file (concat user-emacs-directory "early-init.el")))

(defun ic/edit-org-config-file ()
  "Open the `README.org' file for editing."
  (interactive)
  (find-file (concat user-emacs-directory "README.org")))

(defun ic/delete-current-buffer-file ()
  "Delete the currently visited file."
  (interactive)
  (if-let (filename (buffer-file-name))
      (when (yes-or-no-p (format "Delete file %S? " filename))
        (delete-file filename)
        (kill-buffer)
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p))
          (delete-file-projectile-remove-from-cache filename))
        (message (format "Deleted file %S." filename)))))


(defun ic/rename-current-buffer-file ()
  "Rename the currently visited file."
  (interactive)
  (if-let (cur-filename (buffer-file-name))
      (let ((new-filename (read-file-name
                           (format "Rename %S to: "
                                   (file-name-nondirectory cur-filename))))
            (cur-buffer (buffer-name)))
        (rename-file cur-filename new-filename)
        (find-file new-filename)
        (kill-buffer cur-buffer)
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p))
          (delete-file-projectile-remove-from-cache cur-filename)
          (call-interactively 'projectile-cache-current-file))
        (message (format "Renamed %S to %S." cur-filename new-filename)))))

(setq display-buffer-alist
      '(
        ;; display help windows on the right
        ("^\\*\\(Help\\|info\\)"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 80)
         (reusable-frames . nil))

        ;; display compile errors on the bottom
        ("^\\*\\(Compile\\|Backtrace\\)"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.5)
         (window-height . 15)
         (reusable-frames . nil))

        ;; display magit windows on the left
        ("^magit:"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . left)
         (slot . 1)
         (window-width . 80)
         (window-height . 1.0)
         (reusable-frames . nil))
        ))

(setq custom-file (concat (temporary-file-directory) "emacs-customize-settings"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun ic/save-and-kill-buffer-and-window ()
  "Save current buffer and kill it along with current window."
  (interactive)
  (save-buffer)
  (ic/kill-buffer-and-window))

(defun ic/maybe-kill-emacs ()
  "Prompt to quit emacs, then save and quit."
  (interactive)
  (when (yes-or-no-p "Really quit emacs?")
    (save-buffers-kill-emacs)))

(defun ic/kill-buffer-and-window ()
  "Destroy the current buffer and window, if it was the sole window also delete
the frame."
  (interactive)
  (condition-case-unless-debug nil
      (kill-buffer-and-window)
    (error
     (spacemacs/frame-killer))))

;; from spacemacs
(defun spacemacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(leader-keys
 :infix "q"
 "" '(nil :which-key "quit")
 "q" 'spacemacs/frame-killer
 "Q" 'ic/maybe-kill-emacs)

(defun ic/short-persp-name ()
  "Return a shortened name for currently active perspective. Returns `nil'
when `persp-mode' not enabled."
  ;; we guard calls to `get-frame-persp' and `safe-persp-name' behind a
  ;; check for `\'persp-mode' so shutup the compiler about unknown functions.
  (eval-when-compile
    (unless (fboundp 'get-frame-persp)
      (defun get-frame-persp ()))
    (unless (fboundp 'safe-persp-name)
      (defun safe-persp-name (_))))
  (when (and (boundp 'persp-mode) (get-frame-persp))
    (let ((name (safe-persp-name (get-frame-persp))))
      (concat " <"
              (if (file-directory-p name)
                  (file-name-nondirectory (directory-file-name name))
                name)
              "> "))))

(setq frame-title-format
      '((:eval (buffer-name (window-buffer (minibuffer-selected-window))))
        (:eval (ic/short-persp-name))))

(general-define-key
 "C-e" #'flyspell-auto-correct-previous-word)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; use lines displayed on screen rather than new line characters; so folds work
(setq-default display-line-numbers-type 'visual)
;; show the current line as absolute
(setq-default display-line-numbers-current-absolute t)
;; ignore folding when determining relative lines
(setq-default display-line-numbers-widen t)
;; start with a width of 3 ...
(setq-default display-line-numbers-width 3)
;; ... and allow it to shrink :(
(setq-default display-line-numbers-grow-only nil)

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program
      (cond
       (on-windows
        "c:/Program Files/Mozilla Firefox/firefox.exe")
       (on-old-imac
        "/Applications/Firefox.app/Contents/MacOS/firefox")
       (t
        "firefox")))

(use-package selectrum
  :init
  ;; always show a certain amount of candidates
  (setq selectrum-max-window-height 15)
  (setq selectrum-fix-vertical-window-height t)
  ;; selection face should extend to the screen edge
  (setq selectrum-extend-current-candidate-highlight t)

  :config
  (selectrum-mode 1)

  :general
  (leader-keys
    "R" 'selectrum-repeat))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  ;; Optional performance optimization
  ;; by highlighting only the visible candidates.
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

  :config
  ;; persist history
  (savehist-mode)
  )

(use-package selectrum-prescient
  :demand t
  :init
  (setq selectrum-prescient-enable-filtering nil)
  (setq prescient-save-file (concat user-cache-directory "prescient-save.el"))
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package embark-consult
  :demand t
  :config
  ;; always use completing-read for selecting actions, rather than keybinds
  (setq embark-prompter 'embark-completing-read-prompter)

  ;; setup initial action keybind
  (meow-normal-define-key
   '("C-." . embark-act))

  (defun consult-outline-or-org-heading (&optional match scope)
    "Call `consult-org-heading' when in an `org-mode' buffer, otherwise call
  `consult-outline'."
    (interactive)
    (if (eq major-mode 'org-mode)
        (consult-org-heading match scope)
      (consult-outline)))

  (defun ic-consult-bookmark ()
    "Call `consult-bookmark' without automatic buffer preview.
This is to prevent possible `tramp' calls."
    (interactive)
    (let ((consult-preview-key 'nil))
      (call-interactively #'consult-bookmark)))

  :bind (:map minibuffer-local-map
         ("C-." . embark-act)))

(use-package consult-lsp
  :bind (:map lsp-mode-map
         ([remap consult-imenu-multi] . #'consult-lsp-symbols)
         :map lsp-command-map
         ("GD" . #'consult-lsp-diagnostics)))

(defun ic/move-to-outline-dwim ()
  "Prompt for, and move to, an outline heading."
  (interactive)
  (cond ((eq major-mode 'org-mode)
         (ic/org-jump-in-buffer))
        (t
         (message (format "No outline command found for mode `%s'" major-mode)))))

(setq gc-cons-threshold (* 100 (expt 2 20)))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages nil)

(defun ic/shell-command-on-region-replace ()
  "Call `shell-command-on-region' with prefix arg set."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'shell-command-on-region)))

(when on-windows
  (set-fontset-font "fontset-default" '(#xf000 . #xf2b4) "FontAwesome"))

(when on-home-windows-desktop
  (setq null-device "/dev/null"))

(when on-old-imac
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin"))

(when on-mac
  (setq frame-resize-pixelwise t))

(when (and on-mac
           (executable-find "gls"))
  (setq insert-directory-program "gls")
  (setq dired-listing-switches "-alU"))

(unless on-home-windows-desktop-wsl
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; taken from: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(setq zetian-special-dir (concat org-roam-directory "special/"))
(setq zetian-journal-dir (concat org-roam-directory "journal/"))
(setq zetian-refnote-dir (concat org-roam-directory "ref/"))
(setq zetian-archive-dir (concat org-roam-directory "archive/"))

(setq zetian-tasks-file (concat zetian-special-dir "zetian-tasks.org"))
(setq zetian-people-file (concat zetian-special-dir "zetian-people.org"))
(setq zetian-tasks-archive-file (concat zetian-special-dir "zetian-tasks-archive.org"))

(defvar-local zetian-mode--header-overlay nil)
(defvar-local zetian-mode-inhibit-header-update nil)

(defun zetian-mode--update-header ()
  "Update the overlay `zetian-mode--header-overlay' with header information."
  (unless zetian-mode-inhibit-header-update
    (unless zetian-mode--header-overlay
      (zetian-mode--create-header))
    (org-roam-db-update-file)
    (org-set-regexps-and-options)
    (zetian-mode--update-header-1)))

(defun zetian-mode--update-header-1 ()
  (let*
      ((category (zetian-get-category))
       (states (zetian-get-states))
       (title (zetian-get-title))
       (modified (or (zetian-get-last-modified) "?"))
       (num-outbound-links
        (length (zetian-query `(links-from ,(org-roam-node-at-point)))))
       (num-inbound-links
        (length (zetian-query `(links-to ,(org-roam-node-at-point)))))
       (header
        (concat
         (propertize title 'face 'org-document-title)
         (when category
           (concat (propertize " [" 'face 'org-drawer)
                   category
                   (propertize "]" 'face 'org-drawer)))
         (when states
           (concat
            (propertize " {" 'face 'org-drawer)
            (s-join (propertize ", " 'face 'org-drawer)
                    (mapcar (lambda (s)
                              (propertize s 'face 'org-special-keyword))
                            states))
            (propertize "}" 'face 'org-drawer)))
         "\n"
         (propertize "Mod:" 'face 'org-drawer)
         (propertize modified 'face 'org-date)
         (propertize " Out:" 'face 'org-drawer)
         (propertize (format "%d" num-outbound-links)
                     'face 'org-special-keyword)
         (propertize " In:" 'face 'org-drawer)
         (propertize (format "%d" num-inbound-links)
                     'face 'org-special-keyword)
         "\n"
         (propertize "\n" 'face '(:inherit default :extend t)))))
    (overlay-put zetian-mode--header-overlay 'face 'header-line)
    (overlay-put zetian-mode--header-overlay 'display header)
    (overlay-put zetian-mode--header-overlay 'cursor (length header))))

(defvar-local zetian-mode--modification-hook-save nil)
(defvar-local zetian-mode--modification-hook-save-state nil)
(defun zetian-mode--modification-hook
    (ov after beg end &optional replaced-length)
  ;; (message "%s: (%d %d) %S %S"
  ;;          (if after "after" "begin") beg end replaced-length
  ;;          zetian-mode--modification-hook-save-pos)
  (if after
      (progn
        (if (not (equal end (overlay-end ov)))
            (when (get-buffer-window (current-buffer))
              (add-hook 'post-command-hook #'zetian--post-command-hook))
          (insert zetian-mode--modification-hook-save)
          (move-overlay ov
                        (overlay-start zetian-mode--header-overlay)
                        (+ end (length zetian-mode--modification-hook-save)))
          (set-buffer-modified-p zetian-mode--modification-hook-save-state)))
    (setq zetian-mode--modification-hook-save (buffer-substring beg end))
    (setq zetian-mode--modification-hook-save-state (buffer-modified-p))))

(defun zetian--post-command-hook ()
  "update zetian header once after a command completes"
  (remove-hook 'post-command-hook #'zetian--post-command-hook)
  (zetian-mode--update-header))

(defun zetian-mode--move-cursor-out-of-header (window old-pos action)
  (when (eq action 'entered)
    (goto-char (overlay-end zetian-mode--header-overlay))))

(defun zetian-mode--create-header ()
  "Create the overlay `zetian-mode' uses to display information."
  (unless zetian-mode--header-overlay
    (goto-char (point-min))
    (let* ((beg (point))
           (end (re-search-forward "^$"))
           (ov (make-overlay beg (+ 1 end))))
      (overlay-put ov 'cursor-sensor-functions
                   (list #'zetian-mode--move-cursor-out-of-header))
      (overlay-put ov 'modification-hooks
                   (list #'zetian-mode--modification-hook))
      (setq zetian-mode--header-overlay ov))))

(defun zetian-mode--remove-header ()
  "Removes the overlay and any text properties created."
  (when-let ((ov zetian-mode--header-overlay))
    (delete-overlay ov)))

(defun zetian-mode--before-save-h ()
  (let ((zetian-mode-inhibit-header-update t))
    (zetian-set-last-modified)))

(defun zetian-mode--after-save-h ()
  (zetian-mode--update-header))

(defun zetian-mode--cmmh ()
  "Destroy any changes `zetian-mode' makes to a buffer. Should be run in
`change-major-mode-hook'."
  (zetian-mode--remove-header)
  (remove-hook 'after-save-hook #'zetian-mode--after-save-h)
  (remove-hook 'before-save-hook #'zetian-mode--before-save-h)
  (remove-hook 'change-major-mode-hook #'zetian-mode--cmmh))

(define-derived-mode zetian-mode org-mode "zetian-mode"
  "A variant of org-mode designed to be used with zetian."
  (add-hook 'after-save-hook #'zetian-mode--after-save-h 0 t)
  (add-hook 'before-save-hook #'zetian-mode--before-save-h 0 t)
  ;; (add-hook 'kill-buffer-hook #'zetian-mode--remove-header)
  (add-hook 'change-major-mode-hook #'zetian-mode--cmmh 0 t)
  (cursor-sensor-mode 1)
  (zetian-mode--update-header)
  (goto-char (overlay-end zetian-mode--header-overlay)))

(add-to-list 'org-global-properties
             (cons "Effort_ALL" "0 0:10 0:30 1:00 2:00 4:00 8:00"))

(setq org-columns-default-format-for-agenda
      "%TODO %1Priority %4Effort(Efft){:} %40ITEM(Task) %20TAGS %33DEADLINE %33SCHEDULED")

(defvar zetian--node-marker nil
  "Internal value to mark a node's position in a (changing) buffer.")

(defmacro with-zetian-node (node &rest body)
  "Evaluate BODY with NODE open and the view restricted to the note."
  (declare (indent 1))
  (let ((sym (gensym)))
    `(let ((,sym ,node))
       (if (not (org-roam-node-p ,sym))
           (error "Wrong type argument: org-roam-node, %S" ,sym)
         (save-excursion
           (with-current-buffer
               (find-file-noselect (org-roam-node-file ,sym))
             (save-restriction
               (widen)
               (goto-char (or zetian--node-marker
                              (org-roam-node-point ,sym)))
               (when (org-at-heading-p)
                 (org-narrow-to-subtree))
               ,@body)))))))

(defun map-zetian (func nodes)
  "Call FUNC for each zetian node in NODES.
The node is opened, and narrowed before FUNC is called."
  (mapcar (lambda (x)
            (let ((zetian--node-marker (car x)))
              (with-zetian-node (cdr x)
                (funcall func (cdr x)))))
          (mapcar (lambda (n)
                    (cons (org-roam-node-marker n) n))
                  nodes)))

(setq zetian-capture-default-filename "%<%Y%m%d%H%M%S>-${id}.org")

(cl-defun zetian--capture-template
    (&key file-name title file-tags file-cat prop-cat prop-id other is-element
          todo suppress-prop-drawer tail)
  (let ((body
         (concat
          (when is-element
            (concat "* "
                    (when todo (concat todo " "))
                    (or title "${title}")
                    "\n"))
          (unless suppress-prop-drawer
            (concat
             ":PROPERTIES:\n"
             ":CREATED: %U\n"
             ":LAST_MODIFIED: %U\n"
             ;; XXX for some reason org-roam is inserting the file's id for
             ;; "${id}" when capturing an element (task, event, etc) rather than
             ;; creating a new id.
             (format ":ID: %s\n" (or prop-id
                                     (if is-element
                                         "%(org-id-new)"
                                       "${id}")))
             (when prop-cat (format ":CATEGORY: %s\n" prop-cat))
             ":END:\n"))
          (when file-cat (format "#+category: %s\n" file-cat))
          (when file-tags (format "#+filetags: %s\n" file-tags))
          (when other (concat other "\n"))
          (unless is-element
            (format "#+title: %s" (or title "${title}")))
          tail)))
    (if is-element
        body
      `(file+head ,(or file-name zetian-capture-default-filename)
                  ,body))))

(let* ((zetian-tasks-file "special/zetian-tasks.org")
       (zetian-people-file "special/zetian-people.org")
       (default-template "%U\n\n%?")
       (task-target
        (zetian--capture-template
         :file-name zetian-tasks-file :title "All My Tasks"
         :file-cat "task" :other "#+startup: contents"
         :suppress-prop-drawer t
         :file-tags ":%(if at-home \"HOME\" \"WORK\"):agenda:"))
       (people-target
        (zetian--capture-template
         :file-name zetian-people-file :title "All My Contacts"
         :file-cat "person" :other "#+startup: contents"
         :suppress-prop-drawer t
         :file-tags ":%(if at-home \"HOME\" \"WORK\"):agenda:")))
  (setq org-roam-capture-templates
      `(("p" "permanent note" plain ,default-template
         :target ,(zetian--capture-template
                   :file-cat "perm" :file-tags "review")
         :unnarrowed t)

        ("i" "idea" plain ,default-template
         :target ,(zetian--capture-template
                   :file-cat "idea" :file-tags "review")
         :unnarrowed t)

        ("t" "task" entry
         ,(concat (zetian--capture-template
                   :is-element t :todo "TODO" :prop-cat "task")
                  default-template)
         :target ,task-target
         :empty-lines-before 1)

        ("e" "event" entry
         ,(concat (zetian--capture-template
                   :is-element t :todo "TODO" :prop-cat "event")
                  default-template)
         :target ,task-target
         :empty-lines-before 1)

        ("P" "person" entry
         ,(zetian--capture-template
           :is-element t :prop-cat "person"
           :other (concat
                   "%U\n\n"
                   "- organization ::\n"
                   "- birthday ::\n"
                   "%% FIXME(org-anniversary yyyy mm dd) "
                   "${title}'s %d%s birthday\n"
                   "- phone ::\n"
                   "- address ::\n"
                   "\n%?"))
         :target ,people-target
         :empty-lines-before 1)
        )))

(setq org-roam-capture-ref-templates
      `(("W" "web zettel (org-protocol)" plain
         ,(concat
           "* %<%Y-%m-%d %H:%M:%S>\n"
           "#+begin_quote\n${body}\n#+end_quote\n\n%?")
         :target ,(zetian--capture-template
                   :file-name (concat zetian-refnote-dir
                                      "%(secure-hash 'sha1 \"${ref}\").org")
                   :file-cat "ref" :title "refnote ${title}"
                   :tail "\n\nsource: [[%:link][${title}]]\n\n")
         :unnarrowed t)))

(defun zetian-node-find (&optional node)
  "Act like `org-roam-node-find', but create a narrowed indirect buffer when
  visiting an element (as opposed to a file)."
  (interactive)
  (when-let (node (or node (zetian-search nil "Find or create note" t)))
    (if-let ((guid (org-roam-node-id node))
             (title (org-roam-node-title node))
             (buf-name (format "zetian: %s" title)))
        (let ((buf (get-buffer buf-name))
              (loc (org-id-find guid)))
          (cond
           ((buffer-live-p buf)
            (pop-to-buffer-same-window buf))
           (loc
            (find-file (car loc))
            (widen)
            (goto-char (cdr loc))
            (if (not (org-at-heading-p))
                ;; we're in a file based node
                (progn
                  (rename-buffer buf-name)
                  (zetian-mode))
              ;; or we're in a heading based node
              (pop-to-buffer-same-window
               (make-indirect-buffer (current-buffer) buf-name t))
              ;; jit-lock-mode isn't supported in indirect buffers, so disable
              ;; it.
              (make-local-variable 'font-lock-support-mode)
              (setq font-lock-support-mode nil)
              (goto-char (cdr loc))
              (org-narrow-to-subtree)))
           (t
            (find-file (org-roam-node-file node))
            (rename-buffer buf-name)
            (zetian-mode))))
      (error "could not lookup zetian node: %s" (org-id-find (org-roam-node-id node))))))

(setq zetian-sql-query-all
      "SELECT
  id,
  file,
  filetitle,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    filetitle,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      files.title as filetitle,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")

(defun zetian--bulk-node-query (&optional ids)
  (cond
   ;; no ids: run bare query
   ((null ids)
    zetian-sql-query-all)
   ;; list of ids: filter based on bare ids
   ((listp ids)
    (emacsql-format
     (emacsql-prepare
      (format "SELECT * FROM (%s) WHERE id IN $v1"
              zetian-sql-query-all))
     (apply #'vector ids)))
   ;; vector found: filter based on sub-query
   ((vectorp ids)
    (emacsql-format
     (emacsql-prepare
      (format "SELECT * FROM (%s) WHERE id IN (%s)"
              zetian-sql-query-all
              (replace-regexp-in-string
               "%" "%%"
               (emacsql-format
                (emacsql-prepare ids)))))))))

(defun zetian-bulk-node-populate (&optional ids)
  "Return a list of org-roam-nodes that match IDS.
IDS can either be an `emacsql' query selecting a list of ids or a vector of
  string ids or NIL to return the entire database."
  (let ((rows (org-roam-db-query (zetian--bulk-node-query ids))))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-title file-title
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :aliases aliases
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))

(defvar zetian--readline-histories nil
  "An ALIST of histories to use with `completing-read'.")

(defun zetian-search-find (&optional query prompt create)
  "Search zetian for QUERY and navigate to chosen buffer. When CREATE is
non-nil allow user to create a new node."
  (interactive)
  (when-let ((node (zetian-search query create prompt)))
    (zetian-node-find node)))

(defun ic/org-roam-node-body (node)
  "Return the text contents of NODE"
  (save-excursion
    (with-temp-buffer
      (insert-file-contents-literally (org-roam-node-file node))
      (when (< 0 (org-roam-node-level node))
        (org-mode)
        (widen)
        (goto-char (org-roam-node-point node))
        (org-narrow-to-subtree))
      (buffer-string))))

(defun zetian-query (&optional query)
  "Run QUERY against `org-roam' database."
  (zetian-bulk-node-populate
   (and query (zetian--unroll-query query))))

(defun zetian-query-1 (query)
  "Run QUERY against zetian and return the first result. Signal an error when
  receiving zero or more than one result."
  (let* ((results (zetian-query query))
         (count (length results)))
    (if (not (= count 1))
        (error "zetian-query-1 received %s results from: %S"
               count query)
      (car results))))

(defun zetian--unroll-query (query)
  "Translate QUERY into a suitable `emacsql' query."
  `[:select [nodes:id]
    :from nodes
    :where ,(zetian--unroll-query-impl query)])

(defun zetian--unroll-query-impl (query)
  (cl-flet ((parse #'zetian--unroll-query-impl)
            (parse-like #'zetian--query-quote-like)
            (parse-date #'zetian--query-parse-date)
            (parse-id #'zetian--query-parse-id))
    (let ((atom (and (consp query) (car query)))
          (args (and (consp query) (cdr query)))
          (first (and (consp query) (cadr query)))
          (second (and (consp query) (caddr query))))
      (pcase (type-of query)
        ('string
         query)
        ('org-roam-node
         (org-roam-node-id query))
        ('cons
         (pcase atom
           ('and
            `(and ,@(mapcar #'parse args)))
           ('or
            `(or ,@(mapcar #'parse args)))
           ('not
            `(not ,(parse first)))
           ('tag
            `(in nodes:id [:select [tags:node-id]
                           :from tags
                           :where (= tags:node-id nodes:id)
                           :and (= tags:tag ,(parse first))]))
           ('title
            `(like title ,(parse-like t (parse first))))
           ('title-like
            `(like title ,(parse-like nil (parse first))))
           ('todo
            `(like todo ,(parse-like t (parse first))))
           ('priority
            `(= priority ,(string-to-char (upcase (parse first)))))
           ((or 'scheduled 'deadline)
            (let ((col (make-symbol (format "nodes:%s" atom))))
              (pcase first
                ('after
                 `(> ,col ,(parse-date (parse second))))
                ('before
                 `(< ,col ,(parse-date (parse second))))
                ;; ('on-or-after
                ;;  `(>= ,col ,(zetian--query-parse-date (caddr query))))
                ;; ('on-or-before
                ;;  `(<= ,col ,(zetian--query-parse-date (caddr query))))
                ('on
                 (let ((date (parse-date (parse second))))
                   `(<= ,date ,col ,date)))
                ('between
                 `(<= ,(parse-date (parse second))
                      ,col
                      ,(parse-date (parse (cadddr query)) t)))
                ('none
                 `(is ,col nil))
                (_
                 (error "invalid operation for %S: %S" atom first)))))
           ('level
            ;; '(level < 42) becomes '(< "level" 42)
            `(,first nodes:level ,second))
           ('prop
            `(like
              nodes:properties
              ,(parse-like nil (cons (parse first) (parse second)))))
           ('category
            (parse `(prop "CATEGORY" ,first)))
           ('created-on
            `(like
              nodes:properties
              ,(parse-like nil (cons "DATE_CREATED"
                                     (zetian--query-parse-date-like
                                      (parse first))))))
           ('last-modified-on
            `(like
              nodes:properties
              ,(parse-like nil (cons "LAST_MODIFIED"
                                     (zetian--query-parse-date-like
                                      (parse first))))))
           ((or 'links-to 'links-from)
            (let ((col1 (if (eq 'links-to atom) 'links:source 'links:dest))
                  (col2 (if (eq 'links-to atom) 'links:dest 'links:source)))
              `(in nodes:id [:select [,col1]
                             :from links
                             :where (= ,col1 nodes:id)
                             :and (= links:type "id")
                             :and (= ,col2 ,(parse-id (parse first)))])))
           ('has
            (let* ((col (cond ((eq 'links-to first)
                               'links:dest)
                              ((eq 'links-from first)
                               'links:source)
                              (_
                               (error "invalid option for 'has: %S" first)))))
              `(in nodes:id [:select [,col]
                             :from links
                             :where (= links:type "id")])))
           ('eval
            (parse (eval first)))
           (_
            (error "invalid search key: %S" atom))))
        (_
         (error "invalid query: %S" query))))))

(defun zetian--query-quote-like (exact obj)
  "Return quoted OBJ in a form for `emacsql' query: (like column
,(zetian--query-quote-like (cons 'a 'b)))"
  (let ((double-quote "\"")
        (maybe-percent (unless exact "%%")))
    `(quote ,(if (stringp obj)
                 (concat double-quote maybe-percent
                         obj
                         maybe-percent double-quote)
               (concat maybe-percent
                       (prin1-to-string obj)
                       maybe-percent)))))

(defun zetian--query-parse-date (date &optional next-day-when-no-time)
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                      (ic-parse-org-date-string
                       date
                       next-day-when-no-time)))

(defun zetian--query-parse-date-like (date &optional next-day-when-no-time)
  (format-time-string "[%Y-%m-%d%%%%]"
                      (ic-parse-org-date-string
                       date
                       next-day-when-no-time)))

(defun zetian--query-parse-id (obj)
  (cond
   ((org-roam-node-p obj)
    (org-roam-node-id obj))
   ((stringp obj)
    (org-roam-node-id
     (or (org-roam-node-from-id obj)
         (org-roam-node-from-title-or-alias obj))))
   (t
    (error "invalid zetian id: %S" obj))))

(defmacro def-zetian-query (sym query &optional form skip-completing-read)
  (declare (indent 1))
  (let* ((name (symbol-name sym))
         (pretty-name (replace-regexp-in-string "-" " " name))
         (q-sym (intern (format "zetian-query-%s" name)))
         (f-sym (intern (format "zetian-find-%s" name)))
         (s-sym (intern (format "zetian-search-%s" name))))
    `(progn
       ,(when query
          `(setq ,q-sym ,query))
       (defun ,q-sym (&optional include-archive)
         ,(format
           (concat "Return a list of all %s zetian nodes.\nIgnore archive notes "
                   "unless INCLUDE-ARCHIVE is non-nil.")
           pretty-name)
         ,(if query
              `(zetian-query (if include-archive
                                 ,q-sym
                               `(and ,,q-sym (not ,zetian-query-archive))))
            form))
       (defun ,s-sym (&optional arg)
         ,(format
           (concat "Search zetian for any %s note, return user selected node.\n"
                   "Ignore archive notes unless universal argument given.")
           pretty-name)
         (interactive "P")
         ,(if skip-completing-read
              `(,q-sym (equal arg '(4)))
            `(zetian-completing-read (,q-sym (equal arg '(4)))
                                     (concat ,pretty-name " notes"
                                             (when (equal arg '(4))
                                               " (include archive)")))))
       (defun ,f-sym (&optional arg)
         ,(format
           (concat "Search zetian for any %s note, prompt user for selection and "
                   "navigate to the buffer.\nIgnore archive tags unless"
                   "universal argument given.")
           pretty-name)
         (interactive "P")
         (when-let ((node (,s-sym arg)))
           (zetian-node-find node))))))

(defmacro def-zetian-query-function (sym &rest body)
  (declare (indent 1))
  `(def-zetian-query ,sym nil ,@body))

;; needs to come first as the macros use it
(def-zetian-query archive
  '(tag "archive"))

(defun zetian--fix-archive-query (func &rest _)
  "Fix `zetian-query-archive' filtering out the notes we want to see.
Zetian filters out archive notes by default, which fails when we want to view
only archive notes."
  (funcall func t))
(advice-add 'zetian-query-archive :around #'zetian--fix-archive-query)

;; all notes
(def-zetian-query all
  '(title-like ""))

;; things
(def-zetian-query category
  '(category (eval (completing-read "Category: " (zetian-all-categories)))))

(def-zetian-query project
  '(and (level = 0)
        (category "project")))

(def-zetian-query area
  '(category "area"))

(def-zetian-query book
  '(category "book"))

(def-zetian-query author
  '(category "author"))

(def-zetian-query task
  '(category "task"))

(def-zetian-query event
  '(category "event"))

(def-zetian-query permanent
  '(category "perm"))

(def-zetian-query refnote
  '(category "ref"))

(def-zetian-query import
  '(category "import"))

;; tasks
(def-zetian-query open
  '(or (todo "NEXT") (todo "TODO")))

(def-zetian-query next-task
  `(and ,zetian-query-task
        (todo "NEXT")))

(def-zetian-query open-task
  `(and ,zetian-query-task
        ,zetian-query-open))

(def-zetian-query next-project-task
  `(and ,zetian-query-next-task
        (links-to (eval (zetian-search-project)))))

(def-zetian-query open-project-task
  `(and ,zetian-query-open-task
        (links-to (eval (zetian-search-project)))))

(def-zetian-query nodes-with-project
  '(eval `(or ,@(mapcar (lambda (p) `(links-to ,(org-roam-node-id p)))
                        (zetian-query-project)))))

(def-zetian-query tasks-without-project
  `(and ,zetian-query-task
        (not ,zetian-query-nodes-with-project)))

(def-zetian-query open-tasks-without-project
  `(and ,zetian-query-open-task
        (not ,zetian-query-nodes-with-project)))

;; links
(def-zetian-query link-to
  '(links-to (eval (zetian-search))))

(def-zetian-query link-to-current
  '(links-to (eval (org-roam-node-at-point))))

(def-zetian-query link-from
  '(links-from (eval (zetian-search))))

(def-zetian-query link-from-current
  '(links-from (eval (org-roam-node-at-point))))

;; misc
(def-zetian-query open-event
  `(and ,zetian-query-event
        ,zetian-query-open))

(def-zetian-query open-task-or-event
  `(and (or ,zetian-query-task
            ,zetian-query-event)
        ,zetian-query-open))

(def-zetian-query closed-task-or-event
  `(and (or ,zetian-query-task
            ,zetian-query-event)
        (not ,zetian-query-open)))

(def-zetian-query unscheduled-open-task-or-event
  `(and ,zetian-query-open-task-or-event
        (scheduled none)))

(def-zetian-query state
  '(tag (eval (completing-read "State: " (zetian-all-states)))))

(def-zetian-query review
  '(tag "review"))

(def-zetian-query-function random-review
  (seq-random-elt (zetian-query-review))
  'skip-completing-read)

(def-zetian-query-function random-permanent
  (seq-random-elt (zetian-query-permanent))
  'skip-completing-read)

;; linting searches
(def-zetian-query-function stuck-project
  (seq-filter
   (lambda (project)
     (not (zetian-query
           `(and ,zetian-query-open-task
                 (links-to ,project)))))
   (zetian-query-project)))

(def-zetian-query-function stuck-area
  (seq-filter
   (lambda (area)
     (not (zetian-query
           `(and ,zetian-query-project
                 (links-to ,area)))))
   (zetian-query-area)))

(def-zetian-query missing-outbound-links
  `(and ,zetian-query-permanent
        (not ,zetian-query-review)
        (not (has links-from))))

(def-zetian-query missing-inbound-links
  `(and ,zetian-query-permanent
        (not ,zetian-query-review)
        (not (has links-to))))

(def-zetian-query isolated
  `(and ,zetian-query-missing-inbound-links
        ,zetian-query-missing-outbound-links))

(def-zetian-query permanent-created-today
  `(and ,zetian-query-permanent
        (created-on ".")))

(def-zetian-query permanent-modified-today
  `(and ,zetian-query-permanent
        (last-modified-on ".")))

;; chores
;; I consider a chore to be any task or event that links to a project that is
;; under a specific list of areas.
(setq zetian--project-chore-ids
      (mapcar 'org-roam-node-id
              (mapcan (lambda (area)
                        (zetian-query `(and (category "project")
                                            (links-to ,area))))
                      (zetian-query '(and (category "area")
                                          (or (title "Health")
                                              (title "Chores")
                                              (title "Finances")
                                              (title "social life")))))))

(def-zetian-query chores
  `(or ,@(mapcar (lambda (proj)
                   `(links-to ,proj))
                 zetian--project-chore-ids)))

(def-zetian-query open-chores
  `(and ,zetian-query-open-task-or-event
        ,zetian-query-chores))

(def-zetian-query unscheduled-chores
  `(and ,zetian-query-open-chores
        (scheduled none)))

(defun zetian--select-call-command (arg regex prompt &rest args)
  "Select and call any defined `zetian-find-*' function."
  (interactive "P")
  (let ((symbols))
    (mapatoms (lambda (sym)
                (when (and (string-match-p regex (symbol-name sym))
                           (commandp sym))
                  (setq symbols (cons sym symbols)))))
    (when-let ((func-name (completing-read prompt symbols)))
      (apply (symbol-function (intern func-name)) arg args))))

(defun zetian-select-find (&optional arg)
  (interactive "P")
  (zetian--select-call-command arg "^zetian-find-" "function: "))

(defun zetian-select-search (&optional arg)
  (interactive "P")
  (zetian--select-call-command arg "^zetian-search-" "function: "))

(defconst zetian-new-note-template-key "p"
  "Template key used when creating a new note from elisp.")

(defun zetian-node-link (node)
  "Return an `org-mode' link to NODE as a string."
  (org-make-link-string (concat "id:" (org-roam-node-id node))
                        (org-roam-node-title node)))

(defun zetian-link (&optional immediate)
  "Query Zetian for a node and return an `org-mode' link as a string, creating
the node if IMMEDIATE is non-nil."
  (interactive)
  (when-let ((node (zetian-search nil
                                  (concat "node to link"
                                          (when immediate
                                            " (create if needed)"))
                                  immediate)))
      (zetian-node-link node)))

(defun zetian-link-immediate ()
  "Query Zetian for a node and return an `org-mode' link as a string, creating
the node if needed."
  (interactive)
  (zetian-link t))

(defun zetian-link-insert ()
  "Query Zetian for a node and insert a link at point."
  (interactive)
  (if-let ((link (zetian-link)))
      (insert link)
    (message "Error: specified zetian does not exist")))

(defun zetian-link-insert-immediate ()
  "Query Zetian for a node and insert a link at point, creating the node if
  needed."
  (interactive)
  (insert (zetian-link-immediate)))

(defun zetian-link-insert-search-immediate ()
  "Query zetian for a search function, call that function: insert a link to
  chosen node at point."
  (interactive)
  (when-let ((node (zetian-select-search)))
    (insert (zetian-node-link node))))

(defun zetian-node-create (&optional title template-key)
  "Return zetian node for TITLE, creating it if necessary. Uses TEMPLATE-KEY to
lookup template from `org-roam-capture-templates', when NIL use
  `zetian-new-note-template-key'."
  (if title
      (if-let (node (org-roam-node-from-title-or-alias title))
          ;; we have a title and an existing node
          node
        ;; we have a title, but the node needs to be created
        (let ((template (append
                         (assoc (or template-key zetian-new-note-template-key)
                                org-roam-capture-templates)
                         '(:immediate-finish t)))
              (node (org-roam-node-create :title title)))
          (when (org-roam-capture-
                 :node node
                 :templates (list template))
            (or (org-roam-node-from-title-or-alias (org-roam-node-title node))
                (org-roam-node-from-id (org-roam-node-id node))))))
    ;; we don't have a title, so prompt for one and retry
    (zetian-node-create (org-roam-node-title
                         (zetian-search nil "Zetian note: "))
                        template-key)))

(defun zetian-set-category (&optional category)
  "Set the category of the node at point. If it's on a file, set the #+category:
  property, otherwise use the property drawer."
  (interactive)
  (let ((node (org-roam-node-at-point 'assert))
        (cat (or category (completing-read "Category: " nil))))
    (save-excursion
      (org-set-property "CATEGORY" cat))))

(defun zetian-get-category ()
  "Return the category of node at point."
  (org-entry-get nil "CATEGORY"))

(defun zetian-org-get-tags-local-only (func &optional pos-or-element _)
  "Wrapper for `org-get-tags' to force LOCAL argument to t."
  (funcall func pos-or-element t))

(defun zetian-add-state (state)
  "Add STATE to current zetian node."
  (advice-add #'org-get-tags :around #'zetian-org-get-tags-local-only)
  (unwind-protect
      (org-roam-tag-add (list state))      
    (advice-remove #'org-get-tags #'zetian-org-get-tags-local-only)))

(defun zetian-remove-state (state)
  "Remove STATE from current zetian node."
  (advice-add #'org-get-tags :around #'zetian-org-get-tags-local-only)
  (unwind-protect
      (org-roam-tag-remove (list state))      
    (advice-remove #'org-get-tags #'zetian-org-get-tags-local-only)))

(defun zetian-set-state (&optional arg)
  "Prompt user for a state to set, when given a universal-argument: remove
instead."
  (interactive "P")
  (let ((remove (equal arg '(4))))
    (funcall (if remove 'zetian-remove-state 'zetian-add-state)
             (completing-read "state: "
                              (if remove (zetian-get-states) (zetian-all-states))))))

(defun zetian-get-states (&optional include-hidden)
  "Return a list of states of node at point."
  (seq-filter
   (lambda (tag)
     (or (not (equal (string-to-char tag) ?_))
         include-hidden))
   (org-get-tags)))

(defun zetian-has-state (state)
  "Return true if STATE is a current state of node at point."
  (member state (zetian-get-states)))

(defun zetian-get-last-modified ()
  "Return the `org-mode' timestamp of the last time this node was modified."
  (org-entry-get nil "LAST_MODIFIED"))

(defun zetian-set-last-modified ()
  "Set the `org-mode' timestamp of the last time this node was modified to now."
  (save-excursion
    (goto-char (point-min))
    (org-entry-put nil
                   "LAST_MODIFIED"
                   (with-temp-buffer
                     (org-time-stamp-inactive '(16))
                     (buffer-string)))))

(defun zetian-refresh-agenda-list ()
  "Search zetian for any files with an 'agenda' tag and add them to
`org-agenda-files'."
  (interactive)
  (mapc (lambda (node)
          (add-to-list 'org-agenda-files
                       (org-roam-node-file node)))
        (zetian-query '(tag "agenda"))))

(defun zetian--update-agenda-files-on-load ()
  "Call `zetian-refresh-agenda-list' from `after-init-hook'."
  (remove-hook 'after-init-hook #'zetian--update-agenda-files-on-load)
  (zetian-refresh-agenda-list))
(add-hook 'after-init-hook #'zetian--update-agenda-files-on-load)

(defvar zetian-header-formats '((context . "- context :: ")
                                (project . "- project :: ")
                                (tags . "- tags :: ")))
(defvar zetian-header-seperator ", ")


(defun zetian-set-header-links (type links)
  (setq links (seq-uniq links))
  (sort links (lambda (n1 n2)
                (string< (org-roam-node-title n1)
                         (org-roam-node-title n2))))
  (let* ((other-types (seq-filter (lambda (s) (not (eq s type)))
                                  (mapcar 'car zetian-header-formats)))
         (other-links (mapcar (lambda (type)
                                (cons type (zetian-get-header-links type)))
                              other-types))
         (all-links (cons (cons type links) other-links)))
    (zetian--overwrite-header all-links)))

(defun zetian--overwrite-header (all-links)
  "Set the entire header with the alist ALL-LINKS."
  (with-zetian-node (org-roam-node-at-point)
    (zetian--remove-header)
    (let (inserted-header)
      (dolist (type (mapcar 'car zetian-header-formats))
        (when-let ((links (assoc-default type all-links)))
          (setq inserted-header t)
          (insert
           (assoc-default type zetian-header-formats)
           (mapconcat #'zetian-node-link links zetian-header-seperator)
           "\n")))
      (when inserted-header
        (insert "\n"))))
  ;; when overwriting a header and the point is on the initial content character
  ;; the point is moved to the start of the header, so move back to the content.
  (when (zetian--looking-at-header)
    (while (and (not (= (point) (point-max)))
                (or (zetian--looking-at-header)
                    (looking-at "^$")))
      (forward-line 1))))

(defun zetian--remove-header ()
  "Remove the header from the current note.
Leave point where the header would be."
  (goto-char (point-min))
  ;; move to start of potential header
  (re-search-forward "^$")
  (forward-char 1)
  ;; save starting point
  (let ((beg (point)))
    ;; move over any found headers
    (while (zetian--looking-at-header)
      (forward-line 1))
    ;; move over any consecutive new lines
    (while (and (not (= (point) (point-max)))
                (looking-at "^$"))
      (forward-char 1))
    ;; delete and leave point
    (delete-region beg (point))))

(defun zetian-get-header-links (type)
  "Return list of nodes contained in zetian header TYPE."
  (with-zetian-node (org-roam-node-at-point)
    (let ((header (assoc-default type zetian-header-formats)))
      (goto-char (zetian--header-pos type))
      (when (looking-at header)
        (forward-char (length header))
        (let ((start (point))
              links done)
          (while (not done)
            ;; is the point on an org link?
            (if-let ((elem (org-element-link-parser)))
                (let* ((link (cadr elem))
                       (id (plist-get link :path))
                       (tgt (plist-get link :raw-link))
                       (desc (buffer-substring-no-properties
                              (plist-get link :contents-begin)
                              (plist-get link :contents-end)))
                       (end (plist-get link :end)))
                  ;; add the found node
                  (setq links (cons (org-roam-node-from-id id) links))
                  ;; move to the end of the link
                  (goto-char end)
                  ;; try and move over `zetian-header-seperator'
                  (re-search-forward
                   (regexp-quote zetian-header-seperator)
                   (+ (point) (length zetian-header-seperator))
                   t)
                  ;; keep going if we're looking at another org link
                  (setq done (not (looking-at (regexp-quote "[[")))))
              ;; point was not on an org link, so we're done
              (setq done t)))
          links)))))

(defun zetian--header-pos (type)
  "Return start position of header TYPE.
If header TYPE doesn't exist: return the position it would have if it existed."
  (save-excursion
    (goto-char (point-min))
    (condition-case err
        (progn
          (re-search-forward
           (regexp-quote (assoc-default type zetian-header-formats)))
          (match-beginning 0))
      (search-failed
       (+ 1 (re-search-forward "^$"))))))

(defun zetian--looking-at-header (&optional type)
  "Return non-nil when POINT is on header TYPE.
When TYPE is nil check for any type."
  (let ((types (if type
                   (list type)
                 (mapcar #'car zetian-header-formats))))
    (-any? (lambda (type)
             (looking-at
              (regexp-quote (assoc-default type zetian-header-formats)))) 
           types)))

(defun zetian-get-tags ()
  "Return the tags of the node at point."
  (save-excursion (zetian-get-header-links 'tags)))

(defun zetian-set-tag (&optional arg)
  "Set a tag on the current node.
Removes a tag when the universal argument is set."
  (interactive "P")
  (let ((tags (zetian-get-header-links 'tags))
        (removing (equal arg '(4))))
    (when-let ((node (if removing
                         (zetian-completing-read tags)
                       (zetian-search zetian-query-permanent
                                      "add tag" t))))
      (setq tags (if removing
                     (delete node tags)
                   (cons node tags)))
      (zetian-set-header-links 'tags tags))))

(defun zetian-get-project ()
  "Return the project associated with the node at point."
  (car (zetian-get-header-links 'project)))

(defun zetian-set-project (&optional arg)
  "Set the project for the current node."
  (interactive "P")
  (let* ((remove (equal arg '(4)))
         (old-project (zetian-get-project))
         (new-project (unless remove
                        (zetian-search-project))))
    (zetian-set-header-links 'project (unless remove (list new-project)))
    (when old-project
      (zetian-remove-state (zetian-node-to-state old-project)))
    (when new-project
      (zetian-add-state (zetian-node-to-state new-project)))))

(defun zetian-node-to-state (&optional node)
  "Return a string suitable for an org-mode tag from the NODE's title."
  (concat "_"
          (replace-regexp-in-string (rx (not (any alnum "@_")))
                                    "_"
                                    (org-roam-node-title
                                     (or node (org-roam-node-at-point t))))))

(defun zetian-node-from-state (state)
  "Return a zetian node given a project STATE."
  (unless (= (string-to-char state) ?_)
    (error "Not a valid project state: %s" state))
  (zetian-query-1 `(title-like ,(substring state 1))))

(defun zetian-get-context ()
  "Return the context links of the node at point."
  (zetian-get-header-links 'context))

(defun zetian-set-context (&optional arg)
  "Set a context on the current node.
Removes a tag when the universal argument is set."
  (interactive "P")
  (let ((links (zetian-get-header-links 'context))
        (removing (equal arg '(4))))
    (when-let ((node (if removing
                         (zetian-completing-read links)
                       (zetian-search nil "add context" t))))
      (setq links (if removing
                      (delete node links)
                    (cons node links)))
      (zetian-set-header-links 'context links))))

(defun zetian-get-title ()
  "Return the title from the current zetian node."
  (when-let ((node (org-roam-node-at-point)))
    (org-roam-node-title node)))

(defun zetian-all-states (&optional include-hidden)
  "Return a list of all states (org tags) in the zetian database."
  (let ((results (org-roam-db-query
                  `[:select :distinct tag
                    :from tags
                    ,@(unless include-hidden
                        '(:where tag :not :like '"\"\\_%\""
                          :escape '"\\"))])))
    (sort (mapcar #'car results)
          #'string-collate-lessp)))

(defun zetian-all-categories ()
  "Returns a list of all categories in the zetian database. Note: does a full
table search."
  (let ((results
         (seq-reduce
          (lambda (cats node)
            (let ((cat (assoc-default "CATEGORY"
                                      (org-roam-node-properties node))))
              (if (member cat cats)
                  cats
                (cons cat cats))))
          (org-roam-node-list)
          '())))
    (sort results #'string-collate-lessp)))

(defun zetian-kill-id ()
  "Add the current node's id to the kill ring and system clipboard."
  (interactive)
  (when-let ((node (org-roam-node-at-point)))
    (with-temp-buffer
      (insert (org-roam-node-id node))
      (clipboard-kill-ring-save 1 (point))
      (message "%S" (buffer-string)))))

(defun zetian--format-template (book filename)
  "Load the `yas-snippet' found in FILENAME with the environment from BOOK"
  (with-temp-buffer
    (unless (yas-minor-mode 1)
      (error "failed to load yas-minor-mode"))
    (let ((snippet-env
           (append
            (mapcar (lambda (c)
                      (list (car c)
                            (if (listp (cdr c))
                                `(quote ,(cdr c))
                              (cdr c))))
                    book)
            `((authors-pretty
               ,(string-join (assoc-default 'authors book)
                             ", and "))
              (authors-pretty-links
               ,(string-join (mapcar
                              (lambda (author)
                                (zetian-node-link
                                 (zetian-node-create author "P")))
                              (assoc-default 'authors book))
                             ", and "))
              (authors-emacs-list
               ,(format "%S" (assoc-default 'authors book)))
              (author-ol-links
               ,(string-join
                 (mapcar
                  (lambda (tuple)
                    (format "- [[https://openlibrary.org/authors/%s][OpenLibrary - %s]]"
                            (cdr tuple)
                            (car tuple)))
                  (-zip-pair (assoc-default 'authors book)
                             (assoc-default 'ol-author-ids book)))
                 "\n"))
              (maybe-quote
               ,(when-let ((desc (assoc-default 'description book)))
                  (format "#+begin_quote\n%s\n#+end_quote\n\n" desc)))
              (lt-uri
               ,(if-let (id (assoc-default 'librarything-id book))
                    (format "/work/%s" id)
                  (format "/search.php?search=%s"
                          (assoc-default 'query-title book))))
              (gr-uri
               ,(if-let (id (assoc-default 'goodreads-id book))
                    (format "/book/show/%s" id)
                  (format "/search?q=%s&search_type=books"
                          (assoc-default 'query-title book))))))))
      (yas-expand-snippet
       (with-temp-buffer
         (insert-file-contents
          (locate-user-emacs-file filename))
         (buffer-string))       
       nil
       nil
       snippet-env))
    (buffer-string)))

(defun zetian-capture-book (&optional tag)
  "Import a book from OpenLibrary, optionally applying zetian TAG."
  (interactive)
  (let* ((book (ic/openlibrary-find-book))
         (title (assoc-default 'title book))
         (template `("p" "capture book template" plain
                     ,(zetian--format-template
                       book "data/templates/capture-book-body.org")
                     :target (file+head
                              "%<%Y%m%d%H%M%S>-${id}.org"
                              ,(zetian--format-template
                                book "data/templates/capture-book-header.org")))))
    (cl-letf ((org-roam-capture-templates (list template)))
      (when-let ((node (zetian-node-create title)))
        (when tag
          (if-let ((tag-node (org-roam-node-from-title-or-alias tag)))
              (save-excursion
                (zetian-node-find tag-node)
                (goto-char (point-max))
                (insert "- " (zetian-node-link node) "\n")
                (save-buffer))
            (error "Invalid zetian tag: %s" tag)))
        ;; end visiting the captured book's node
        (zetian-node-find node)))))

(defun zetian-capture-book-pleasure-toread ()
  "Import a book from OpenLibrary, and apply the 'Read for Pleasure' tag."
  (interactive)
  (zetian-capture-book "Read for Pleasure"))

(defun zetian-capture-book-pleasure-completed ()
  "Import a book from OpenLibrary, and apply the 'Completed Pleasure Books' tag."
  (interactive)
  (zetian-capture-book "Completed Pleasure Books"))

(defun zetian-capture-book-enrichment-toread ()
  "Import a book from OpenLibrary, and apply the 'Read for Enrichment' tag."
  (interactive)
  (zetian-capture-book "Read for Enrichment"))

(defun zetian-capture-book-enrichment-completed ()
  "Import a book from OpenLibrary, and apply the 'Completed Enrichment Books' tag."
  (interactive)
  (zetian-capture-book "Completed Enrichment Books"))

(defun zetian-completing-read (&optional nodes query-name create)
  (interactive)
  (let*
      ((candidates
        (mapcar
         (lambda (node)
           ;; add 'zetian-node to the title's properties so we can reference
           ;; it later.
           (let ((title (org-roam-node-title node)))
             (add-text-properties 0 1 `(zetian-node ,node) title)
             (cons title node)))
         nodes))
       (completing-func
        (lambda (input predicate action)
          (if (eq action 'metadata)
              '(metadata (category . zetian))
            (complete-with-action action candidates input predicate))))
       (prompt (if query-name
                   (format "[%s]: " query-name)
                 "Zetian Node: "))
       (selection (completing-read prompt candidates)))
    (if-let ((node (assoc-default selection candidates)))
        (prog1 node
          ;; remove the 'zetian-node property added to the title
          (remove-text-properties
           0 1 '(zetian-node) (org-roam-node-title node)))
      (when create
        (zetian-node-create selection)))))

(defun zetian-search (&optional query prompt create)
  (interactive)
  (zetian-completing-read (zetian-query query) prompt create))

(defvar zetian--org-done-keywords nil
  "Saved `org-done-keywords' so we can properly style todo items.")
(with-eval-after-load 'org
  (setq zetian--org-done-keywords
        (with-temp-buffer
          (org-mode)
          org-done-keywords)))

(defun zetian--completion-annotator (cand)
  "An annotator function for zetian nodes to be called from
`completing-read'."
  (when-let ((node (get-text-property 0 'zetian-node cand)))
    (let* ((todo (org-roam-node-todo node))
           (todo-face (if (member todo zetian--org-done-keywords)
                          'org-done
                        'org-todo))
           (todo-str (if todo
                         (if (< 4 (length todo))
                             (concat (propertize (substring todo 0 4)
                                                 'face todo-face)
                                     ".")
                           (propertize todo 'face todo-face))
                       ""))
           (tags (seq-filter
                  (lambda (tag)
                    (not (string-match-p
                          org-agenda-hide-tags-regexp tag)))
                  (org-roam-node-tags node)))
           (tags-str (string-join tags ":"))
           (cat (assoc-default "CATEGORY" (org-roam-node-properties node)))
           (pri (org-roam-node-priority node))
           (pri-str (if pri
                        (format "[#%s]" (char-to-string pri))
                      ""))
           ;; (contents (zetian--completion-get-contents node))
           )
      ;; ensure todo string is full width, otherwise the 'org-todo face extends past the tag
      (setq todo-str (concat todo-str
                             (make-string (- 5 (length todo-str)) ? )))
      (marginalia--fields
       (cat :face 'org-document-info
            ;; :width 8
            :truncate 8
            )
       (pri-str :face (org-get-priority-face pri)
                :truncate 4
                ;; :width 4
                )
       (todo-str :truncate 5)
       (tags-str :face 'org-tag
                 ;; :width 10
                 :truncate 10
                 )
       ;; ("|" :width 1 :truncate 1)
       ;; (contents :face 'org-footnote
       ;;           :truncate 200)
       ))))

(defun zetian--completion-get-contents (node)
  "Return the formatted contents of NODE as a string."
  (with-temp-buffer
    (insert (zetian-get-contents node))
    (goto-char (point-min))
    (re-search-forward "^$")
    (s-replace-regexp
     "\\[\\[.*\\]\\[\\(.*\\)\\]\\]"
     "\\1"
     (replace-regexp-in-string
      "\n" " "
      (buffer-substring-no-properties (point) (point-max))))))

;; add our annotator to marginalia
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(zetian zetian--completion-annotator builtin none)))

(defvar zetian-ebook-mapping-file
  (concat org-roam-directory "/zetian-ebook-mapping.el")
  "The file to store ebook filename -> zetian id mappings.")

(setq zetian--ebook-mapping
      (when (file-exists-p zetian-ebook-mapping-file)
        (with-temp-buffer
          (insert-file zetian-ebook-mapping-file)
          (read (buffer-string)))))

(defun zetian--add-ebook-mapping (ebook)
  "Prompt the user for the zetian note that EBOOK should be associated with."
  (interactive)
  (when-let ((node (zetian-search '(category "book")
                                  (format "associate (%s) with book" ebook))))
    (push (cons ebook (org-roam-node-id node)) zetian--ebook-mapping)
    (with-temp-file zetian-ebook-mapping-file
      (insert (format "%S" zetian--ebook-mapping)))
    (org-roam-node-id node)))

(defun zetian-node-from-ebook (filename)
  "Return the zetian node that the filename FILENAME is associated with, prompting
  the user to add a mapping if needed."
  (interactive)
  (let ((ebook (file-name-nondirectory filename)))
    (or (org-roam-node-from-id (file-name-base ebook))
        (org-roam-node-from-id (assoc-default ebook zetian--ebook-mapping))
        (org-roam-node-from-id (zetian--add-ebook-mapping ebook)))))

(defvar zetian-moonreader-backup-dir
  "~/sync/moonreader/"
  "The directory to search for moonreader backup files.")

(defun zetian--moonreader-backup-file ()
  "Return the filename of the moonreader backup file to process."
  (interactive)
  (when-let ((files (file-expand-wildcards
                     (concat zetian-moonreader-backup-dir "*.mrpro"))))
    (if (= 1 (length files))
        (car files)
      (completing-read "moonreader backup to process: " files))))

(defun zetian--moonreader-extract (file)
  "Extract the moonreader zip backup FILE to a temporary directory, return the
directory."
  (let ((tmpdir (concat temporary-file-directory "moonreader-extract/")))
    (when (file-exists-p tmpdir)
      (delete-directory tmpdir t))
    (make-directory tmpdir)
    (let ((ret (call-process "unzip" nil nil nil
                             "-q" (expand-file-name file)
                             "-d" (expand-file-name tmpdir))))
      (unless (= ret 0)
        (error "received exit %d from unzip" ret)))
    tmpdir))

(defun zetian--moonreader-database-file (dir)
  "Return the sqlite database file under the extracted backup DIR."
  (let* ((base (concat dir "com.flyersoft.moonreaderp/"))
         (metadata
          (with-temp-buffer
            (insert-file-contents (concat base "_names.list"))
            (split-string (buffer-string) "\n")))
         (idx (-elem-index
               "com.flyersoft.moonreaderp/databases/mrbooks.db"
               metadata))
         (file (format "%s%d.tag" base (+ idx 1))))
    (if (file-exists-p file)
        file
      (error "Could not find database file under %s" dir))))

(defun zetian--moonreader-highlights (db-file)
  "Given a sqlite db at DB-FILE: return a list of moonreader highlights in the
form: ((ebook quoted-text note) ...)."
  (with-temp-buffer
    (call-process "sqlite3" nil t nil "-line" db-file
                  "select filename, original, note, time from notes")
    (goto-char (point-min))
    (let (result filename quoted note time)
      (while (re-search-forward "^filename = " nil t)
        (setq filename (buffer-substring-no-properties
                        (point) (line-end-position)))
        (re-search-forward "^original = ")
        (setq quoted (buffer-substring-no-properties
                      (point) (line-end-position)))
        (re-search-forward "^    note = ")
        (setq note (buffer-substring-no-properties
                    (point) (line-end-position)))
        (re-search-forward "^    time = ")
        (setq time (buffer-substring-no-properties
                    (point) (line-end-position)))
        (setq timestamp
              (format-time-string "[%Y-%m-%d %a %H:%M]"
                                  (seconds-to-time
                                   (/ (string-to-number time) 1000))))
        (push `((ebook . ,filename)
                (quoted . ,quoted)
                (note . ,note)
                (timestamp . ,timestamp)
                (checksum . ,(md5 (concat quoted note))))
              result))
      result)))

(defun zetian--format-template-ebook (highlight filename)
  "Load the `yas-snippet' found in FILENAME with the environment from
HIGHLIGHT."
  (with-temp-buffer
    (unless (yas-minor-mode 1)
      (error "failed to load yas-minor-mode"))
    (yas-expand-snippet
     (with-temp-buffer
       (insert-file-contents
        (locate-user-emacs-file filename))
       (buffer-string))       
     nil
     nil
     (mapcar (lambda (c)
               (list (car c)
                     (if (listp (cdr c))
                         `(quote ,(cdr c))
                       (cdr c))))
             highlight))
    (buffer-string)))

(defun zetian--moonreader-import-highlight (highlight)
  "Import HIGHLIGHT into zetian if it's not already imported."
  (interactive)
  (unless (org-roam-node-from-title-or-alias
           (format "refnote %s" (assoc-default 'checksum highlight)))
    (when-let ((node (zetian-node-from-ebook (assoc-default 'ebook highlight))))
      (push `(book-id . ,(org-roam-node-id node)) highlight)
      (push `(book-title . ,(org-roam-node-title node)) highlight)
      (let ((template `("l" "literature note" plain
                        ,(zetian--format-template-ebook
                          highlight "data/templates/capture-refnote-body.org")
                        :target (file+head
                                 ,(concat zetian-refnote-dir
                                         zetian-capture-default-filename)
                                 ,(zetian--format-template-ebook
                                   highlight "data/templates/capture-refnote-header.org"))))
            (title (format "refnote %s" (assoc-default 'checksum highlight))))
        (cl-letf ((org-roam-capture-templates (list template)))
          (zetian-node-create title "l"))))))

(defun zetian--moonreader-import-summary (imported)
  "Generate a summary note from highlights in the list IMPORTED"
  (interactive)
  (when imported
    (let* ((parent-node (org-roam-node-from-title-or-alias
                         "automated moonreader import"))
           (body
            (concat "%U\n\n%?"
                    (format "[[id:%s][%s]] "
                            (org-roam-node-id parent-node)
                            (org-roam-node-title parent-node))
                    (format "added %d literature notes:\n\n"
                            (length imported))
                    (mapconcat
                     (lambda (node)
                       (let ((link (format "[[id:%s][%s]]"
                                           (org-roam-node-id node)
                                           (org-roam-node-title node))))
                         (format "* %s\n#+transclude: %s\n\n" link link)))
                     imported)))
           (header-template
            "data/templates/capture-moonreader-import-summary-header.org")
           (title (format "refnote import for %s"
                          (format-time-string "[%Y-%m-%d %a %H:%M]")))
           (template `("s" "moonreader import summary" plain
                       ,body
                       :target (file+head
                                "%<%Y%m%d%H%M%S>-${id}.org"
                                ,(zetian--format-template-ebook
                                  `((title . ,title))
                                  header-template)))))
      (cl-letf ((org-roam-capture-templates (list template)))
        (message "creating with %s" title)
        (zetian-node-create title "s")))))

(defun zetian-moonreader-import ()
  "Import any highlights from a moonreader backup in
`zetian-moonreader-backup-dir'."
  (interactive)
  (if-let ((backup (zetian--moonreader-backup-file)))
      (let* ((tmpdir (zetian--moonreader-extract backup))
             (db-file (zetian--moonreader-database-file tmpdir))
             (highlights (zetian--moonreader-highlights db-file))
             imported)
        (dolist (highlight highlights)
          (when-let ((node (zetian--moonreader-import-highlight highlight)))
            (push node imported)))
        (prog1
            (when imported
              (zetian-node-find
               (zetian--moonreader-import-summary imported)))
          (message "imported %d new notes (%d total moonreader notes)"
                   (length imported) (length highlights))
          (delete-directory tmpdir t)
          (delete-file backup)))
    (message "no moonreader backup file found")))

(defun zetian-get-contents (&optional node)
  "Return the content of NODE as a string. If NODE is nil: use the node at
point."
  (let ((node (or node (org-roam-node-at-point))))
    (with-temp-buffer
      (insert-file-contents
       (org-roam-node-file node))
      (goto-char (org-roam-node-point node))
      (org-mode)
      (when (org-at-heading-p)
        (org-narrow-to-element))
      (buffer-substring-no-properties (point) (point-max)))))

(defun zetian-rename-current ()
  "Rename the current note, updating any linking notes"
  (interactive)
  (when-let ((old-title (zetian-get-title)))
    (let ((new-title (read-from-minibuffer "new name: " old-title))
          (cur (org-roam-node-at-point))
          (inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^#\\+title: ")
        (delete-region (point)
                       (progn (end-of-line) (point)))
        (insert new-title))
      (rename-buffer (format "zetian: %s" new-title))
      (save-buffer)
      (dolist (node (zetian-query `(links-to ,new-title)))
        (save-excursion
          (with-current-buffer
              (find-file-noselect (org-roam-node-file node))
            (zetian--update-link-to (org-roam-node-id cur) new-title)
            (save-buffer)))))))

(defun zetian--update-link-to (id desc)
  "In the current buffer: update all `org-mode' links to ID with the description
  DESC."
  (when id
    (goto-char (point-min))
    (let ((regex (format "\\[\\[id:%s\\]\\[\\(.*\\)\\]\\]" id)))
      (while (re-search-forward regex nil t)
        (save-restriction
          (narrow-to-region (match-beginning 0) (match-end 0))
          (replace-match desc t t nil 1))))))

(defun zetian-archive-completed-tasks ()
  "Move all completed zetian tasks and events into the archive."
  (interactive)
  ;; because we're modifying one file mostly: we want to work back-to-front or
  ;; end-to-beginning.
  (when-let ((nodes (sort (zetian-query-closed-task-or-event)
                     (lambda (n1 n2)
                       (< (org-roam-node-point n2)
                          (org-roam-node-point n1))))))
    (mapc #'zetian-archive-task nodes)
    (with-current-buffer (find-file-noselect zetian-tasks-file)
      (save-buffer))
    (with-current-buffer (find-file-noselect zetian-tasks-archive-file)
      (save-buffer))
    (message "archived %d tasks" (length nodes))))

(defun zetian-archive-task (node)
  "Move the zetian note NODE into the archive."
  (save-excursion
    (with-current-buffer
        (find-file-noselect (org-roam-node-file node))
      (goto-char (org-roam-node-point node))
      (let ((org-log-refile nil))
        (org-refile nil nil (list "" zetian-tasks-archive-file nil 1)))
      (save-window-excursion
        (org-refile-goto-last-stored)
        (org-entry-put
         nil "ARCHIVED_ON" (with-temp-buffer
                             (org-time-stamp-inactive '(16))
                             (buffer-string)))))))

(defun zetian--lint-duplicates ()
  "Return a list of nodes that are duplicates of one another in the form
  ((NODE-A1 . NODE-A2) ...)"
  (let* ((results (org-roam-db-query
                   [:select [file title] :from files
                    :where (in title [:select title :from files
                                      :group :by title
                                      :having (> (funcall count *) 1)])
                    :order :by title])))
    (mapcar (lambda (x)
              (cons (car x)
                    (mapcar (lambda (n) (zetian--lint-node-from-file (car n)))
                            (cdr x))))
            (-group-by 'cadr results))))

(defun zetian--lint-node-from-file (filename)
  (with-current-buffer (find-file-noselect filename)
    (org-roam-node-at-point)))

(defun zetian--lint-broken-links ()
  "Return a list of broken links in the form ((NODE1 POS1 POS2) ...)."
  (let ((results (org-roam-db-query
                  [:select [source pos]
                   :from links
                   :where (and (= type "id")
                               (not (in dest
                                        [:select id :from nodes])))])))
    (mapcar (lambda (x)
              (cons (org-roam-node-from-id (car x))
                    (mapcar 'cadr (cdr x))))
            (-group-by 'car results))))

(defun zetian--lint-read-link-at-pos (node pos)
  "Return the link's description for NODE at POS within buffer."
  (with-current-buffer
      (find-file-noselect (org-roam-node-file node))
    (save-restriction
      (save-excursion
        (widen)
        (goto-char pos)
        (re-search-forward "\\[\\[.*\\]\\[\\(.*\\)\\]\\]")
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1))))))


(defun zetian--lint-show-buffer (dups links stuck-projects stuck-areas
                                      tasks-without-project isolated)
  "Build and show a buffer with any errors found."
  (let ((buf (get-buffer-create "*zetian database lint*"))
        (now (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (pop-to-buffer-same-window buf)
    (read-only-mode -1)
    (erase-buffer)
    (insert "#+title: zetian database lint\n\n")
    (insert (format "linting was run at %s\n" now))
    (when stuck-areas
      (insert (format "\n* %d stuck areas found\n" (length stuck-areas)))
      (dolist (area stuck-areas)
        (insert (format "- %s\n" (zetian-node-link area)))))
    (when stuck-projects
      (insert (format "\n* %d stuck projects found\n" (length stuck-projects)))
      (dolist (project stuck-projects)
        (insert (format "- %s\n" (zetian-node-link project)))))
    (when dups
      (insert (format "\n* %d duplicate titles found" (length dups)))
      (dolist (dup dups)
        (insert (format "\n** %s\n" (car dup)))
        (dolist (bad (cdr dup))
          (insert (format "- [[id:%s][%s]]\n"
                          (org-roam-node-id bad)
                          (file-relative-name
                           (org-roam-node-file bad)
                           org-roam-directory)))
          ;; (insert (format "in-links=%d out-links=%d\n"
          ;;                 (length (zetian-links-to bad))
          ;;                 (length (zetian-links-from bad))))
          )))
    (when links
      (insert (format "\n* %d nodes with broken links found" (length links)))
      (dolist (link links)
        (insert (format "\n** %s\n" (zetian-node-link (car link))))
        (dolist (pos (cdr link))
          (insert (format "- \"%s\" (pos:%d)\n"
                          (zetian--lint-read-link-at-pos (car link) pos)
                          pos)))))
    (when tasks-without-project
      (insert (format "\n* %d open tasks with missing projects\n"
                      (length tasks-without-project)))
      (dolist (task tasks-without-project)
        (insert (format "- %s\n" (zetian-node-link task)))))
    (when isolated
      (insert (format "\n* %d isolated tasks (no in-bound or out-bound links)"
                      (length isolated)))
      (dolist (task isolated)
        (insert (format "\n- %s" (zetian-node-link task)))))
    (org-mode)
    (read-only-mode 1)
    (goto-char (point-min))))

(defun zetian-lint ()
  "Check zetian for any inconsistencies and open a buffer containing the
faults."
  (interactive)
  (org-roam-db-sync)
  (let ((found (list
                (zetian--lint-duplicates)
                (zetian--lint-broken-links)
                (zetian-query-stuck-project)
                (zetian-query-stuck-area)
                (zetian-query-open-tasks-without-project)
                (zetian-query-isolated))))
    (if (-any 'identity found)
        (apply #'zetian--lint-show-buffer found)
      (message "no errors found in zetian"))))

;; strange categories
;; orphaned nodes

(defmacro zetian--with-report-buffer (name &rest body)
  "Open a list of all open tasks grouped by project."
  (declare (indent 1))
  (let ((sym-name (gensym))
        (sym-buf (gensym))
        (sym-now (gensym)))
    `(let* ((,sym-name ,name)
            (,sym-buf (get-buffer-create (concat "*" ,sym-name "*")))
            (,sym-now (format-time-string "[%Y-%m-%d %a %H:%M]")))
       (pop-to-buffer-same-window ,sym-buf)
       (read-only-mode -1)
       (erase-buffer)
       (insert (concat "#+title: " ,sym-name "\n\n"))
       (insert (format "report was run at %s\n\n" ,sym-now))
       ,@body
       (org-mode)
       (read-only-mode 1)
       (goto-char (point-min)))))

(defun zetian-report-tasks ()
  (interactive)
  (zetian--with-report-buffer "zetian task list"
    ;; for each area
    (mapc
     (lambda (area)
       (insert (concat "* " (zetian--report-format area) "\n"))
       (when-let ((tasks (zetian-query `(and ,zetian-query-open-task-or-event
                                             (links-to ,area)))))
         ;; for each task linked to this area
         (mapc
           (lambda (task)
             (insert (concat "- " (zetian--report-format task) "\n")))
           tasks))
       ;; for each project linked to the area
       (mapc
        (lambda (proj)
          (insert (concat "** " (zetian--report-format proj) "\n"))
          ;; for each task linked to the project
          (mapc
           (lambda (task)
             (insert (concat "- " (zetian--report-format task) "\n")))
           (zetian-query `(and ,zetian-query-open-task-or-event
                               (links-to ,proj)))))
        (zetian-query `(and ,zetian-query-project
                            (links-to ,area)
                            (not ,zetian-query-archive))))
       (insert "\n"))
     (zetian-query-area))))

(defun zetian--report-generate (name elements)
  "Generate a report of `NAME', using the alist `ELEMENTS'."
  (zetian--with-report-buffer name
    (mapc (lambda (elem)
            (let ((title (car elem))
                  (items (cdr elem)))
              (insert (concat "\n* " (zetian--report-format title) "\n"))
              (mapc (lambda (it) (insert (concat "- " (zetian--report-format it)"\n")))
                    items)))
         elements)))

(defun zetian--report-format (thing)
  "Format `THING' as a string suitable for inserting into an org document."
  (cond ((stringp thing) thing)
        (t
         ;; assume an org-roam node
         (format "[[id:%s][%s]]"
                 (org-roam-node-id thing)
                 (org-roam-node-title thing)))))

(defun zetian-show-links-buffer ()
  (interactive)
  (when-let ((node (org-roam-node-at-point)))
    (pop-to-buffer-same-window
     (get-buffer-create "*zetian links buffer*"))
    (org-transclusion-mode -1)
    (read-only-mode -1)
    (erase-buffer)
    (insert (format "#+title: Links for %s\n\n" (org-roam-node-title node)))
    (insert "* In Bound Links\n")
    (dolist (link (zetian-query `(links-to ,node)))
      (insert (format "** %1$s\n#+transclude: %1$s\n\n"
                      (zetian-node-link link))))
    (insert "\n* Out Bound Links\n")
    (dolist (link (zetian-query `(links-from ,node)))
      (insert (format "** %1$s\n#+transclude: %1$s\n"
                      (zetian-node-link link))))
    (org-mode)
    (org-transclusion-mode 1)
    (read-only-mode 1)
    (goto-char (point-min))
    (re-search-forward "^$")
    (forward-char)))

(defun zetian-show-list (nodes &optional no-sort)
  "Turn `NODES' into a string of ORG-MODE links suitable for inclusion in an org
  document."
  (mapconcat (lambda (node) (format "- [[id:%s][%s]]\n"
                                    (org-roam-node-id node)
                                    (org-roam-node-title node)))
             (if no-sort
                 nodes
               (sort nodes
                     (lambda (n1 n2) (string< (org-roam-node-title n1)
                                              (org-roam-node-title n2)))))))

(defun zetian--journal-buffer (date-format template-name &optional time)
  "Return the buffer for today's journal."
  (let* ((journal
          (concat zetian-journal-dir
                  (format-time-string date-format time)
                  ".org"))
         (existing (file-exists-p journal)))
    (if (file-exists-p journal)
        (find-file-noselect journal)
      (with-current-buffer (find-file-noselect journal)
        ;; (find-file journal)
        (yas-expand-snippet
         (yas-lookup-snippet template-name 'org-mode))
        (goto-char (point-min))
        (org-id-get-create)
        (save-buffer)
        (zetian-mode)
        ;; (re-search-forward "^$")
        (current-buffer)
        ))))

(defun zetian-journal-today ()
  "Return the buffer for today's journal"
  (zetian--journal-buffer "%Y-%m-%d" "journal-day"))

(defun zetian-journal-week ()
  "Return the buffer for week's journal"
  (zetian--journal-buffer "%Y-w%W" "journal-week"))

(defun zetian-journal-month ()
  "Return the buffer for month's journal"
  (zetian--journal-buffer "%Y-%m" "journal-month"))

(defun zetian-journal-year ()
  "Return the buffer for year's journal"
  (zetian--journal-buffer "%Y" "journal-year"))

(defun zetian-journal-goto-today ()
  "Open today's journal."
  (interactive)
  (pop-to-buffer-same-window
   (zetian-journal-today)))

(defun zetian-journal-goto-week ()
  "Open this week's journal."
  (interactive)
  (pop-to-buffer-same-window
   (zetian-journal-week)))

(defun zetian-journal-goto-month ()
  "Open this month's journal."
  (interactive)
  (pop-to-buffer-same-window
   (zetian-journal-month)))

(defun zetian-journal-goto-year ()
  "Open this year's journal."
  (interactive)
  (pop-to-buffer-same-window
   (zetian-journal-year)))

(defun zetian--journal-link (buffer title)
  "Return and org-mode link to BUFFER with TITLE"
  (format "[[id:%s][%s]]"
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (org-id-get)))
          title))

(defun zetian-journal-today-link ()
  "Return an org-mode link to today's journal."
  (zetian--journal-link
   (zetian-journal-today)
   (format-time-string "%Y-%m-%d")))

(defun zetian-journal-week-link ()
  "Return an org-mode link to this week's journal."
  (zetian--journal-link
   (zetian-journal-week)
   (format-time-string "Week %W")))

(defun zetian-journal-month-link ()
  "Return an org-mode link to this month's journal."
  (zetian--journal-link
   (zetian-journal-month)
   (format-time-string "%B")))

(defun zetian-journal-year-link ()
  "Return an org-mode link to this year's journal."
  (zetian--journal-link
   (zetian-journal-year)
   (format-time-string "%Y")))

(defun zetian--add-completed-task-to-journal ()
  "This hook should run under `org-after-todo-state-change-hook' and will add
  any completed task to the current day's journal."
  (when (org-entry-is-done-p)
    (let ((link (format "[[id:%s][%s]]"
                        (org-id-get)
                        (org-get-heading t t t t))))
      (with-current-buffer (zetian-journal-today)
        (save-excursion
          (save-restriction
            (goto-char (marker-position
                        (org-find-olp '("Completed Tasks")
                                      (current-buffer))))
            (org-narrow-to-subtree)
            (goto-char (point-max))
            (insert (format " - [X] %s" link))
            (save-buffer)))))))
(add-hook 'org-after-todo-state-change-hook
          #'zetian--add-completed-task-to-journal)

(defconst zetian-update-git-seconds 600
  "Automatically update zetian's git repo every count seconds.")

(defun zetian-update-git ()
  "Update the git repo under `org-roam-directory'.
Automatically pull any updates, commit our changes, and push back up."
  (interactive)
  (let ((default-directory org-roam-directory)
        (command (concat user-data-directory "zetian-update-git.sh"))
        exit-status)
    (with-current-buffer (get-buffer-create "*zetian-git-output*")
      (read-only-mode -1)
      (goto-char (point-max))
      (setq exit-status (call-process command nil t nil))
      (read-only-mode 1)
      (unless (= exit-status 0)
        (error "zetian-update-git.sh return non-zero exit status, check *zetian-git-output*")))))

;; ensure we run `zetian-update-git' every `zetian-update-git-seconds'.
(run-with-idle-timer zetian-update-git-seconds t #'zetian-update-git)

(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; only use $PATH on the remote host
(connection-local-set-profile-variables
 'use-remote-path-envvar
 '((tramp-remote-path . (tramp-own-remote-path))))

(connection-local-set-profiles
 '(:application tramp :machine "tornado")
 'use-remote-path-envvar)

;; setup directory local variables and make them as safe
(let* ((ignore-dirs '("Build" "Library" "Logs" "obj" "Packages"
                      "ProjectSettings" "Temp" "UserSettings"))
       (ignore-regexes (append (mapcar (lambda (d) (concat "[/\\\\]" d "\\'"))
                                       ignore-dirs)
                               lsp-file-watch-ignored-directories))
       (vars `((lsp-file-watch-ignored-directories . ,ignore-regexes))))
  (dir-locals-set-class-variables
   'unity-project
   `((nil . ,vars)))
  (dolist (var vars)
    (add-to-list 'safe-local-variable-values var)))

(when on-home-windows-desktop
  (dir-locals-set-directory-class
   "c:/Code/Icarus" 'unity-project))

(defvar ic/random-list-file (concat org-directory
                                    "/home/lists-of-names.org"))

(defun ic/random-name ()
  "Return a random name from the lists in `ic/random-list-file'."
  (interactive)
  (save-excursion
    (save-restriction
      (with-current-buffer (find-file-noselect ic/random-list-file)
        (widen)
        (let* (;; find tag
               (tag (completing-read "Tag for random name:"
                                     (org-get-buffer-tags)))
               ;; find random heading matching tag
               (headings (org-ql-select (current-buffer) `(tags ,tag)))
               (heading (seq-random-elt headings))
               ;; data for heading
               (data (org-element-parse-secondary-string
                      (buffer-substring
                       (plist-get (cadr heading) :contents-begin)
                       (plist-get (cadr heading) :contents-end))
                      '(item)))
               (text (substring-no-properties (car data)))
               ;; find random line
               (lines (split-string text "\n" t))
               (line (seq-random-elt lines))
               (result (ic//random-name-cleanup (substring line 2))))
          ;; add to kill ring and return
          (kill-new result)
          (message result))))))

(defun ic//random-name-cleanup (name)
  (let ((regexes (list ;; (rx (or " in" " and" " or" " is" "," ":" " -")
                       ;;     word-end (* nonl) eol)
                       (rx " in" word-end (* nonl) eol)
                       (rx " and" word-end (* nonl) eol)
                       (rx " or" word-end (* nonl) eol)
                       (rx " is" word-end (* nonl) eol)
                       (rx " - " (* nonl) eol)
                       (rx "," (* nonl) eol)
                       (rx ":" (* nonl) eol)
                       (rx ";" (* nonl) eol)
                       (rx (optional space) "(" (* (not ")")) ")")
                       (rx (optional space) "[" (* (not "]")) "]")
                       )))
    (mapc
     (lambda (re)
       ;; (message re)
       (setq name (replace-regexp-in-string re "" name)))
     regexes)
    name))

(leader-keys
  "|" 'ic/shell-command-on-region-replace
  "C-|" 'shell-command-on-region
  "TAB" 'mode-line-other-buffer
  "u" 'universal-argument)

;; (general-define-key
;;  :states '(normal)
;;  :keymaps 'general-override-mode-map
;;  "go" 'ic/move-to-outline-dwim
;;  "gl" 'helm-occur)

(general-define-key
 :keymaps  'meow-insert-state-keymap
 "<left>"  'meow-left
 "<up>"    'meow-prev
 "<down>"  'meow-next
 "<right>" 'meow-right
 )

(general-define-key
    :keymaps '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map)
    "<escape>" 'keyboard-escape-quit)

(leader-keys
 :infix "a"
 "" '(nil :which-key "applications")
 "d" 'dired
 "s" 'ansi-term
 "i" 'ielm
 )

(leader-keys
 :infix "b"
 "" '(nil :which-key "buffers")
 "b" 'consult-buffer
 "B" 'consult-buffer-other-window
 "d" 'kill-this-buffer
 "D" 'ic/ediff-current-buffer-with-disk
 "i" 'ibuffer
 "P" 'ic/paste-clipboard-to-buffer
 "R" 'revert-buffer
 "s" 'ic/switch-to-scratch-buffer
 "w" 'read-only-mode
 "Y" 'ic/yank-buffer-to-clipboard)

(leader-keys
  :infix "E"
  "" '(nil :which-key "emacs")
  "d" 'toggle-debug-on-error
  "f" 'toggle-frame-fullscreen
  "F" 'make-frame
  "m" 'toggle-frame-maximized)

(leader-keys
 :infix "Ep"
 "" '(nil :which-key "emacs profiler")
 "c" #'ic/start-cpu-profiler
 "m" #'ic/start-mem-profiler
 "p" #'ic/start-cpu-mem-profiler
 "s" #'profiler-stop
 "r" #'profiler-report)

(leader-keys
 :infix "f"
 "" '(nil :which-key "files")
 "b" 'ic-consult-bookmark
 "c" 'copy-file
 "D" 'ic/delete-current-buffer-file
 "E" 'sudo-edit
 "f" 'find-file
 "F" 'find-file-other-window
 "L" 'locate-library
 "M" 'ic/rename-current-buffer-file
 ;; "r" 'ic/helm-recentf
 "y" 'ic/yank-current-filename)

(leader-keys
 :infix "fC"
 "" '(nil :which-key "convert")
 "d" 'unix-to-dos
 "u" 'dos-to-unix)

(leader-keys
 :infix "fe"
 "" '(nil :which-key "emacs")
 "i" 'ic/edit-init-file
 "I" 'ic/edit-early-init-file
 "o" 'ic/edit-org-config-file)

(define-key help-map (kbd "a") 'consult-apropos)

(leader-keys
 :infix "i"
 "" '(nil :which-key "insert")
 ;; "8" 'helm-ucs
 "c" 'insert-char
 )

(leader-keys
  :infix "j"
  "" '(nil :which-key "jump")
  "i" 'consult-imenu
  "I" 'consult-imenu-multi
  "l" 'consult-line
  "m" 'consult-mark
  "M" 'consult-global-mark
  "o" 'consult-outline-or-org-heading
  )

(leader-keys
  :infix "r"
  "" '(nil :which-key "registers")
  "l" 'consult-register-load
  "k" 'consult-yank-from-kill-ring
  "r" 'consult-register
  "s" 'consult-register-store
  )

(leader-keys
  :infix "s"
  "" '(nil :which-key "search")
  "f" 'consult-find
  "g" 'consult-git-grep
  "G" 'consult-grep
  "r" 'consult-ripgrep
  )

(leader-keys
  :infix "S"
  "" '(nil :which-key "snippets")
  "e" #'yas-visit-snippet-file
  "i" #'yas-insert-snippet
  "n" #'yas-new-snippet)

(leader-keys
 :infix "t"
 "" '(nil :which-key "toggle")
 "C" 'flycheck-mode
 "S" 'flyspell-mode
 "T" 'load-theme
 "m" 'consult-minor-mode-menu
 "w" 'whitespace-mode
 )

(leader-keys
 :infix "w"
 "" '(nil :which-key "windows")
 "=" 'balance-windows
 "d" 'delete-window
 "f" 'follow-mode
 ;; "H" 'evil-window-move-far-left
 ;; "h" 'evil-window-left
 ;; "J" 'evil-window-move-very-bottom
 ;; "j" 'evil-window-down
 ;; "K" 'evil-window-move-very-top
 ;; "k" 'evil-window-up
 ;; "L" 'evil-window-move-far-right
 ;; "l" 'evil-window-right
 "h" 'windmove-left
 "j" 'windmove-down
 "k" 'windmove-up
 "l" 'windmove-right
 "m" 'delete-other-windows
 "S" 'split-window-below-and-focus
 "s" 'split-window-below
 "U" 'winner-redo
 "u" 'winner-undo
 "V" 'split-window-right-and-focus
 "v" 'split-window-right
 )
