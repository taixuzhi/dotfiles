;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)

;;; set custom variables
(setq default-directory "~/gitlab")
(setq user-full-name "Xuancong Lee")
(setq user-mail-address "congleetea@gmail.com")

;;; set system directory and load them.
(defvar vendor-dir (expand-file-name "vendor" user-emacs-directory))
(defvar backup-dir "~/.emacs.d/backups/")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path vendor-dir)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


;;; install packages
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))

;; use proxy if repository is not access.
;; (setq url-proxy-services '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;                            ("http" . "127.0.0.1:8123")
;;                            ("https" . "127.0.0.1:8123")
;;                            ))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile (require 'use-package))

;;; Require my custom configs. 
(require 'init-bootstrap)
(require 'init-utils)
(require 'init-platform)
(require 'init-pkgs)
(require 'init-evil)
(require 'init-org)
(require 'init-languages)
(require 'init-maps)


(provide 'init)
