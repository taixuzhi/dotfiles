;;; init-org.el --- Set up Org Mode
;;; Commentary:
;; reffer to: http://www.zmonster.me/2015/07/15/org-mode-planning.html
;; reffer to: https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/

(use-package org-evil :ensure t)
(setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))

;;; org-page
(require 'org-page)
(setq op/repository-directory "~/myin/congleetea.github.io")
(setq op/site-domain "https://congleetea.github.io")
(setq op/personal-github-link "https://github.com/congleetea")
(setq op/site-main-title "qingyuan")
(setq op/site-sub-title "SubTitle")
(setq op/personal-disqus-shortname "congleetea")
(setq op/theme-root-directory "~/.emacs.d/themes/")
(setq op/theme 'org-page-theme-weidaotong)
(setq op/highlight-render 'js)

(provide 'init-org)
