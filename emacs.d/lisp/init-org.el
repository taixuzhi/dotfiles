;;; init-org.el --- Set up Org Mode
;;; Commentary:
;; reffer to: http://www.zmonster.me/2015/07/15/org-mode-planning.html

;; reffer to: https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/

(setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
(setq org-agenda-files (list "~/Dropbox/org/personal.org"
                             "~/Dropbox/org/groupon.org"))

(setq debug-on-error t)

;; reffer to https://thraxys.wordpress.com/2016/01/14/pimp-up-your-org-agenda/
(use-package org-evil :ensure t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

;; ==================== Code blocks =====================
;; cd .emacs.d/elpa/org-20161102
;; rm *.elc
;(setq org-plantuml-jar-path (expand-file-name "plantuml.jar" vendor-dir))
;(setq org-ditaa-jar-path (expand-file-name "ditaa0_9.jar" vendor-dir))
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

;; Export org-mode to Google I/O HTML5 slide.
(use-package ox-ioslide
             :ensure t
             :defer t
             :config
             (require 'ox-ioslide-helper))


;; ---------------------------------------------------
;; for blog
;; ---------------------------------------------------
;; support display the online image in org-mode of emacs
;; (require 'uimage)
(use-package uimage
             :ensure t
             :defer t
             )

(use-package htmlize
             :ensure t
             :defer t
             :commands (htmlize-buffer
                         htmlize-file
                         htmlize-many-files
                         htmlize-many-files-dired
                         htmlize-region))
(require 'org-page)
(use-package org-page
             :ensure t
             :defer t
             )
(setq op/repository-directory "~/myin/congleetea.github.io")
(setq op/site-domain "https://congleetea.github.io")
(setq op/personal-github-link "https://github.com/congleetea")
(setq op/site-main-title "qingyuan")
(setq op/site-sub-title "SubTitle")
(setq op/personal-disqus-shortname "congleetea")
(setq op/theme-root-directory "~/.emacs.d/themes/")
(setq op/theme 'org-page-theme-weidaotong)
(setq op/highlight-render 'js)
;; (setq op/personal-avatar "https://avatars2.githubusercontent.com/u/9694398?v=3&s=460")
;; (setq op/personal-google-analytics-id "userid_of_google_analytics")

; cd .emacs.d ; cd elpa ; cd org-20161102 ; rm *.elc
; https://www.websequencediagrams.com/examples.html
(use-package wsd-mode
             :ensure t
             :commands (wsd-mode)
             :config
             (add-hook 'wsd-mode-hook 'company-mode))


(provide 'init-org)
;;; init-org.el ends here
