;;; init-org.el --- Set up Org Mode
;;; Commentary:
;; reffer to: http://www.zmonster.me/2015/07/15/org-mode-planning.html
;; reffer to: https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/

(use-package org-evil :ensure t)
(setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))

;;; org-page
(use-package uimage
             :ensure t
             :defer t
             )
(setq org-plantuml-jar-path "~/.emacs.d/vendor/plantuml.jar")	
(setq org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")	
(setq org-confirm-babel-evaluate nil)	
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)	
	
(require 'ob)	
(require 'ob-plantuml)	
	
(org-babel-do-load-languages	
 'org-babel-load-languages	
 '(	
   (emacs-lisp . t)	
   (org . t)	
   (sh . t)	
   (C . t)	
   (python . t)	
   (awk . t)	
   (plantuml . t)	
   (ditaa . t)	
   ))	
	
;; Highlight and indent source code blocks	
(setq org-src-fontify-natively t)	
(setq org-src-tab-acts-natively t)	
	
;; Prevent confirmation	
(setq org-confirm-babel-evaluate nil)

(use-package htmlize
             :ensure t
             :defer t
             :commands (htmlize-buffer
                        htmlize-file
                        htmlize-many-files
                        htmlize-many-files-dired
                        htmlize-region))

(require 'org-page)
(setq op/repository-directory "~/gitlab/congleetea.github.io")
(setq op/site-domain "https://congleetea.github.io")
(setq op/personal-github-link "https://github.com/congleetea")
(setq op/site-main-title "qingyuan")
(setq op/site-sub-title "SubTitle")
(setq op/personal-disqus-shortname "congleetea")
(setq op/theme-root-directory "~/.emacs.d/themes/")
(setq op/theme 'org-page-theme-weidaotong)
(setq op/highlight-render 'js)

(provide 'init-org)
