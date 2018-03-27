;;; init-languages.el --- Set up programming languages
;;; Commentary:
;; Basic programming languages

;;; Code:

;; --------------------------------------------------------------------
;; generate the tag files
;; --------------------------------------------------------------------
(require 'gtags)
(use-package bpr :ensure t)

;; Bind some useful keys in the gtags select buffer that evil overrides.
(add-hook 'gtags-select-mode-hook
          (lambda ()
            (evil-define-key 'normal gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
            (evil-define-key 'normal gtags-select-mode-map (kbd "q") 'kill-buffer-and-window)))

(defun gtags-reindex ()
  "Kick off gtags reindexing."
  (interactive)
  (let* ((root-path (expand-file-name (vc-git-root (buffer-file-name))))
         (gtags-filename (expand-file-name "GTAGS" root-path)))
    (if (file-exists-p gtags-filename)
      (gtags-index-update root-path)
      (gtags-index-initial root-path))))

(defun gtags-index-initial (path)
  "Generate initial GTAGS files for PATH."
  (let ((bpr-process-directory path))
    (bpr-spawn "gtags")))

(defun gtags-index-update (path)
  "Update GTAGS in PATH."
  (let ((bpr-process-directory path))
    (bpr-spawn "global -uv")))

;;--------------------------------------------------------------
;; sh-mode
;;--------------------------------------------------------------
(use-package sh-script
             :defer t
             :config (setq sh-basic-offset 4))

(use-package eldoc
             :diminish eldoc-mode
             :init  (setq eldoc-idle-delay 0.1))

(use-package cmake-mode 
             :defer t)

;;--------------------------------------------------------------
;; cc-mode
;;--------------------------------------------------------------
(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "bsd")))
    (setq tab-width 4)
    (setq default-tab-width 4)
    (setq c-default-style "linux")
    (setq-default indent-tabs-mode nil)
    (setq c-basic-offset 4)))

(require 'company-c-headers)
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.4.0/"))

;;---------------------------------------------------------------
;; Erlang
;;---------------------------------------------------------------
(let* ((emacs-version "2.11.1")
       (tools-path
        (concat "/usr/local/lib/erlang/lib/tools-" emacs-version "/emacs")))
  (when (file-exists-p tools-path)
    (setq load-path (cons tools-path load-path))
    (setq erlang-root-dir "/usr/local/lib/erlang")
    (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
    (require 'erlang-start)
    (defvar inferior-erlang-prompt-timeout t)))

;; get erlang man page
(defun get-erl-man ()
  (interactive)
  (let* ((man-path "/usr/local/lib/erlang/man")
         (man-args (format "-M %s %s" man-path (current-word))))
    (man man-args)))

(add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("rebar.config.script" . erlang-mode))
(add-to-list 'auto-mode-alist '("app.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.src$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.erlang$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))


;;;----------------------------------------------------------------------------
;;; cmake
;;;----------------------------------------------------------------------------
(use-package cmake-mode
             :defer t)

;;;----------------------------------------------------------------------------
;;; lua
;;;----------------------------------------------------------------------------
(use-package lua-mode
             :ensure t
             :defer t
             :mode "\\.lua\\'"
             :init
             (add-hook 'lua-mode-hook
                       (lambda ()
                         (setq lua-indent-level 4)
                         (auto-complete-mode)
                         (hs-minor-mode)
                         (turn-on-font-lock)
                         )
                       )
             )

;;----------------------------------------------------------------------------
;; lisp: C-x C-e 执行光标下lisp
;;----------------------------------------------------------------------------
(use-package slime
             :defer t
             :config
             (progn
               (use-package ac-slime :ensure t)
               (setq tab-width 4)
               (setq inferior-lisp-program "sbcl")
               (slime-setup)
               (add-hook 'slime-mode-hook 'set-up-slime-ac)
               (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
               (add-hook 'slime-mode-hook
                         (lambda ()
                           (unless (slime-connected-p)
                             (save-excursion (slime)))))
               (slime-setup '(slime-fancy slime-asdf))
               (slime-setup '(slime-repl slime-fancy slime-banner))
               (setq slime-protocol-version 'ignore
                     slime-net-coding-system 'utf-8-unix
                     ;;slime-complete-symbol*-fancy t
                     slime-complete-symbol-function 'slime-fuzzy-complete-symbol))
             :ensure t)

(use-package color-identifiers-mode
             :ensure t
             :init
             (add-hook 'emacs-lisp-mode-hook 'color-identifiers-mode)
             :diminish color-identifiers-mode)

;; refer https://www.emacswiki.org/emacs/PareditCheatsheet
(use-package paredit
             :ensure t
             :init
             (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;;----------------------------------------------------------------------------
;; Golang
;; go get github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/nsf/gocode
;;----------------------------------------------------------------------------
(use-package company-go
             :ensure t
             :defer t
             :init
             (with-eval-after-load 'company
                                   (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc
             :ensure t
             :defer
             :init
             (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
 :config
 (bind-keys :map go-mode-map
  ("M-." . godef-jump)
  ("M-," . pop-tag-mark)
  )
 (add-hook 'go-mode-hook '(lambda () (setq tab-width 2)))
 (setq gofmt-command "goimports")
 (add-hook 'before-save-hook 'gofmt-before-save))

;;----------------------------------------------------------------------------
;; es6
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(use-package js2-mode
             :config (setq js2-basic-offset 2))

;; force web-mode’s content type as jsx for .js and .jsx files
(use-package web-mode :ensure t)

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;;----------------------------------------------------------------------------
;; other programming languages
;;----------------------------------------------------------------------------
;; (use-package crontab-mode
;;              :defer t
;;              :mode "\\.?cron\\(tab\\)?\\'")
(use-package markdown-mode
             :defer t
             :config
             (progn
               (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
               (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
               (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
             (add-hook 'markdown-mode-hook
                       (lambda ()
                         (set-fill-column 80)
                         (turn-on-auto-fill)
                         (flyspell-mode t)
                         (visual-line-mode t)))
             (setq markdown-command "pandoc --smart -f markdown -t html")
             ;(setq markdown-css-paths `(,(expand-file-name "markdown.css" vendor-dir)))
             )

(use-package yaml-mode
             :defer t
             :mode ("\\.yml$" . yaml-mode))

(use-package ansible
             :defer t
             :init (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))


;;----------------------------------------------------------------------------
;; racket
;;----------------------------------------------------------------------------
(use-package racket-mode
             :mode ("\\.rkt\\'" . racket-mode)
             :interpreter ("racket" . racket-mode)
             :config
             (define-abbrev-table 'racket-mode-abbrev-table
                                  '(("lambda" "λ" nil 1)))
             (setq default-abbrev-mode t)
             :ensure t)

(mapc (lambda (pr) (put (car pr) 'racket-indent-function (cdr pr)))
      '((conde . 0)
        (fresh . 1)
        (run . 1)
        (run* . 1)
        (run . 2)))

(use-package geiser
             :config
             (add-hook 'racket-mode-hook (lambda () (geiser-mode t)))
             :ensure t)
(setq geiser-active-implementations '(racket))
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)


;;----------------------------------------------------------------------------
;; docker
;;----------------------------------------------------------------------------
;; docker-mode
(use-package docker
             :ensure t
             :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
(use-package dockerfile-mode
             :ensure t
             :mode "Dockerfile.*\\'")

(add-hook 'prog-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

;;----------------------------------------------------------------------------
;; auto insert
;;----------------------------------------------------------------------------
;; https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-auto-insert.el
(load "autoinsert")
(auto-insert-mode)
(setq auto-insert t)
(setq auto-insert-query t)
(add-hook 'find-file-hooks 'auto-insert)
;(setq auto-insert-directory "~/.emacs.d/vendor/auto-insert/")
(setq auto-insert-alist
      (append
       '(
         (("\\\\.el\\\\'" . "Emacs Lisp header")
          "Short description: "
          ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "
;; Copyright (C) " (substring (current-time-string) -4) " by Xuancong Lee " "
;; Author: Xuancong Lee"
'(end-of-line 1) " <" (user-login-name) ?@ "congleetea@gmail.com>
(defconst "
(substring (file-name-nondirectory (buffer-file-name)) 0 -3)
"-version \\"$Id: "
(file-name-nondirectory (buffer-file-name))
",v 1.1 "
'(require 'time-stamp)
(concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss))
" matsu Exp matsu $\\")" "
;; Keywords: "
 '(require 'finder)
 ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
 '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
                   finder-known-keywords)
        v2 (mapconcat (lambda (x) (format "%10.0s:  %s" (car x) (cdr x)))
           finder-known-keywords
           "\\n"))
 ((let ((minibuffer-help-form v2))
    (completing-read "Keyword, C-h: " v1 nil t))
    str ", ") & -2 "
;;
;; This program is free software; you can redistribute it and/or modify
 (中略)
;;; Commentary:
;; " _ "
;;; Code:
;;; " (file-name-nondirectory (buffer-file-name)) " ends here"))
       auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.py$" . "python template")
                 nil
                 "#!/usr/bin/env python\n"
                 "\n"
                 "import sys, os, math\n"
                 "# import numpy as np\n"
                 "# import scipy as sp\n"
                 "# import ROOT\n"
                 "# import pyfits as pf\n"
                 "\n"
                 _
                 )) auto-insert-alist))
(setq auto-insert-alist
      (append '(
                (("\\.sh$" . "shell script template")
                 nil
                 "#!/bin/bash\n"
                 "\n"
                 _
                 )) auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.erl$" . "erlang header")
                 nil
                 "%%%--------------------------------------------------------------------\n"
                 "%%% @Copyright (c) 2016-2017 MOLMC Enterprise, Inc. (http://intoyun.com)\n"
                 "%%% @Author: Xuancong Lee[congleetea] <congleetea@gmail.com>\n"
                 "%%%\n"
                 "%%% @date "(format-time-string "%A, %B %e %Y" (current-time)) "\n"
                 "%%%--------------------------------------------------------------------\n"
                 "\n"
                 "-module("(file-name-sans-extension (file-name-nondirectory buffer-file-name))").\n"
                 "\n"
                 _
                 ))
              auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.go$" . "golang header")
                 nil
                 "///--------------------------------------------------------------------\n"
                 "/// Copyright (c) 2016-2017 MOLMC Enterprise, Inc. (http://intoyun.com)\n"
                 "/// Author: Xuancong Lee[congleetea] <congleetea@gmail.com>\n"
                 "///\n"
                 "/// Date "(format-time-string "%A, %B %e %Y" (current-time)) "\n"
                 "///--------------------------------------------------------------------\n"
                 "package "
                 _
                 ))
              auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.h\\'" . "C/C++ header")
                 nil
                 '(c++-mode)
                 '(setq my:skeleton-author (identity user-full-name))
                 '(setq my:skeleton-mail-address (identity user-mail-address))
                 '(setq my:skeleton-namespace (read-string "Namespace: " ""))
                 '(setq my:skeleton-description (read-string "Short Description: " ""))
                 '(setq my:skeleton-inherit (read-string "Inherits from (space separate for multiple inheritance): " ""))
                 '(setq my:skeleton-inherit-list (split-string my:skeleton-inherit " " t))
                 '(setq my:skeleton-inheritance (cond ((null my:skeleton-inherit-list)
                                                       "")
                                                      (t
                                                        (setq my:skeleton-inheritance-concat "")
                                                        (dolist (element my:skeleton-inherit-list)
                                                          (setq my:skeleton-inheritance-concat
                                                                (concat my:skeleton-inheritance-concat
                                                                        "public " element ", ")))
                                                        (setq my:skeleton-inheritance-concat
                                                              (concat " : "
                                                                      my:skeleton-inheritance-concat))
                                                        (eval (replace-regexp-in-string ", \\'" "" my:skeleton-inheritance-concat)))))
                 '(setq my:skeleton-include (cond ((null my:skeleton-inherit-list)
                                                   "")
                                                  (t
                                                    (setq my:skeleton-include "\n")
                                                    (dolist (element my:skeleton-inherit-list)
                                                      (setq my:skeleton-include
                                                            (concat my:skeleton-include
                                                                    "#include \"" element ".h\"\n")))
                                                    (eval my:skeleton-include))))
                 '(setq my:skeleton-namespace-list (split-string my:skeleton-namespace "::"))
                 '(setq my:skeleton-file-name (file-name-nondirectory (buffer-file-name)))
                 '(setq my:skeleton-class-name (file-name-sans-extension my:skeleton-file-name))
                 '(setq my:skeleton-namespace-class
                        (cond ((string= my:skeleton-namespace "")
                               my:skeleton-class-name)
                              (t
                                (concat my:skeleton-namespace "::" my:skeleton-class-name)
                                )))
                 '(setq my:skeleton-namespace-decl
                        (cond ((string= my:skeleton-namespace "")
                               ""
                               )
                              (t
                                (setq my:skeleton-namespace-decl-pre "")
                                (setq my:skeleton-namespace-decl-post "")
                                (setq my:skeleton-namespace-decl-indent "")
                                (dolist (namespace-element my:skeleton-namespace-list)
                                  (setq my:skeleton-namespace-decl-pre
                                        (concat my:skeleton-namespace-decl-pre
                                                my:skeleton-namespace-decl-indent
                                                "namespace " namespace-element " {\n"))
                                  (setq my:skeleton-namespace-decl-post
                                        (concat "\n"
                                                my:skeleton-namespace-decl-indent
                                                "}"
                                                my:skeleton-namespace-decl-post))
                                  (setq my:skeleton-namespace-decl-indent
                                        (concat my:skeleton-namespace-decl-indent "   "))
                                  )
                                (eval (concat my:skeleton-namespace-decl-pre
                                              my:skeleton-namespace-decl-indent
                                              "class " my:skeleton-class-name ";"
                                              my:skeleton-namespace-decl-post))
                                )))
                 '(random t)
                 '(setq my:skeleton-include-guard
                        (upcase
                          (format "INCLUDE_GUARD_UUID_%04x%04x_%04x_4%03x_%04x_%06x%06x"
                                  (random (expt 16 4))
                                  (random (expt 16 4))
                                  (random (expt 16 4))
                                  (random (expt 16 3))
                                  (+ (random (expt 2 14)) (expt 2 5))
                                  (random (expt 16 6))
                                  (random (expt 16 6)))))
                 "/**" n
                 "* @file   " my:skeleton-file-name > n
                 "* @brief  " my:skeleton-description > n
                 "*" > n
                 "* @date   Created       : " (format-time-string "%Y-%m-%d %H:%M:%S") > n
                 "*         Last Modified :" > n
                 "* @author " my:skeleton-author " <" my:skeleton-mail-address ">" > n
                 "*" > n
                 "*    (C) " (format-time-string "%Y") " " my:skeleton-author > n
                 "*/" > n
                 n
                 "#ifndef " my:skeleton-include-guard n
                 "#define " my:skeleton-include-guard n
                 my:skeleton-include n
                 my:skeleton-namespace-decl n
                 n
                 "class " my:skeleton-namespace-class my:skeleton-inheritance " {" n
                 "public:" > n
                 my:skeleton-class-name "();" n
                 "virtual ~" my:skeleton-class-name "();" n
                 n
                 my:skeleton-class-name "(const " my:skeleton-class-name "& rhs);" n
                 my:skeleton-class-name "& operator=(const " my:skeleton-class-name "& rhs);" n
                 n
                 "protected:" > n
                 n
                 "private:" > n
                 n
                 "ClassDef(" my:skeleton-class-name ",1) // " my:skeleton-description n
                 "};" > n
                 n
                 "#endif // " my:skeleton-include-guard n
                 '(delete-trailing-whitespace)
                 )
                )
              auto-insert-alist))

(provide 'init-languages)
;;; init-languages.el ends here
