;;; code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                     General Settings                                  ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use bash as the shell
(setq shell-file-name "/bin/bash")

;; turn off the bell
(setq ring-bell-function 'ignore)

;;dont litter the init file with custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; dont minimize on control-z
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z") 

;;save between sessions
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")


;; Don't show the start up screen/other messages  when Emacs opens
(setq inhibit-startup-message t
      inhibit-default-init t
      inhibit-startup-echo-area-message "Bilal"
      initial-scratch-message "")

;;show line numbers on left margin
(add-hook 'prog-mode-hook 'linum-mode)


;;highlight matching parens
(show-paren-mode t)

;; automatically refresh files if there are outside changes. Do this for remote files too
(global-auto-revert-mode 1)
(setq auto-revert-remote-files 1)
(setq revert-without-query '(".*"))


;; Ignore modification-time-only changes in files, i.e. ones that
;; don't really change the contents. 
(defun update-buffer-modtime-if-byte-identical ()
  (let* ((size      (buffer-size))
         (byte-size (position-bytes size))
         (filename  buffer-file-name))
    (when (and byte-size (<= size 1000000))
      (let* ((attributes (file-attributes filename))
             (file-size  (nth 7 attributes)))
        (when (and file-size
                   (= file-size byte-size)
                   (string= (buffer-substring-no-properties 1 (1+ size))
                            (with-temp-buffer
                              (insert-file-contents filename)
                              (buffer-string))))
          (set-visited-file-modtime (nth 5 attributes))
          t)))))

(defun verify-visited-file-modtime--ignore-byte-identical (original &optional buffer)
  (or (funcall original buffer)
      (with-current-buffer buffer
        (update-buffer-modtime-if-byte-identical))))
(advice-add 'verify-visited-file-modtime :around #'verify-visited-file-modtime--ignore-byte-identical)

(defun ask-user-about-supersession-threat--ignore-byte-identical (original &rest arguments)
  (unless (update-buffer-modtime-if-byte-identical)
    (apply original arguments)))
(advice-add 'ask-user-about-supersession-threat :around #'ask-user-about-supers)




(fset 'yes-or-no-p 'y-or-n-p) ;; accept y/n for yes/no prompts

;;Backup and auto-save settings
(setq backup-by-copying t  ;;dont overwrite symlinks
      backup-directory-alist '(("." . "~/.emacs.d/auto-saves/")) ;; don't litter FS with save files
      delete-old-versions t ;;don't keep too many old files
      kept-new-versions 10
      kept-old-versions 5
      version-control t) ;;use versioning for backups

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-saves/" t)))

;; tramp auto save settings
'( tramp-auto-save-directory "~/.emacs.d/auto-saves/")

;;use the ibuffer buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;switch between frames easily
(global-set-key (kbd "C-x M-o") 'next-multiframe-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                        Packages                                       ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;load the package system. Add the melpa, marmalade, gnu archives
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)


;;get the use-package system
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;Install try, which lets you run a package without installing it
(use-package try
  :ensure t
  :defer t)

;; Install, which key, which brings up help on key combinations
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :bind
  ("C-x w". which-key-show-top-level))

;;Install Magit, a git porcelain. Set key for common command.
(use-package magit
  :ensure t
  :defer t
 :bind
  ("C-x g" . magit-status))

;;Install Python tools

(use-package elpy
  :ensure t
  :defer t
  :bind
  (:map elpy-mode-map ("C-c C-z" . 'elpy-shell-switch-to-shell)))


(use-package py-autopep8
  :ensure t
  :defer t)

(use-package ein
  :ensure t
  :defer t)
(setenv "WORKON_HOME" "~/env/miniconda3/envs/")
(pyvenv-mode 1)


;;install fly-check and fly-check-tip, which do syntax checking

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :defer t)

;;company mode, which does auto completion of syntax along with additional mode pacakges
(use-package company
  :ensure t
  :defer t)

(use-package company-auctex
  :ensure t
  :defer t)

;;smart parens, which provides IDE like paren management
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  '(sp-base-key-binding 'paraedit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings))

;;AUCTeX for latex tools
(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;;(setq-default TeX-master nil) ;;AUCTeX will prompt for master file when creating new file
  (setq global-font-lock-mode t)
  (company-auctex-init))

;;Evil to provide VIM keybindings
(use-package evil
  :ensure t
  :config
  '(evil-set-initial-state 'magit-popup-mode 'emacs)
  '(evil-set-initial-state 'org-mode 'emacs)
  (setq evil-default-state 'emacs))
  

;;docker-tramp mode which extends tramp to work within docker containers
(use-package docker-tramp
  :ensure t
  :defer t)

;; dockerfile mode which provides syntactical highlighting for dockerfiles
(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package ascii-art-to-unicode
  :ensure t
  :defer t)

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page))
(use-package org-pdfview
  :ensure t
  :defer t
  :config
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                      Org Mode                                         ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Load org and set some key bindings and enable encryption
(use-package org
  :ensure t
  :defer t
  :init
  (require 'org-crypt)
  (require 'ox)
  (require 'ox-org)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)
  (require 'org-habit)
  :config
  (add-to-list 'org-file-apps' ("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (setq org-agenda-files (quote("~/Org/")))
  (setq org-todo-keywords'((sequence "TODO(t)" "IN-PROGRESS(p)"  "WAIT(w@/!)"  "|" "DONE(d!)" "CANCELED(c@)")))

  :hook  (org-mode . visual-line-mode)
  :bind(
	("C-c l" . org-store-link)
	("C-c a" . org-agenda)
	("C-c c" . org-capture))
  :bind ( :map org-mode-map
	       ("C-c d" . org-decrypt-entries)))

(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode))

;; org-gcal to sync agenda to google calendar
(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "8240918350-f32o6lnqmbfuvcledi75ptbf7aia2iv0.apps.googleusercontent.com"
	org-gcal-client-secret "KryFDAztv4ysgsm2Cr_NyMMq"
	org-gcal-file-alist '(("bill2507733@gmail.com" .  "~/Org/gcal.org")))
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) )))


;; Org-brain
(use-package org-brain
  :ensure t
  :init
  (setq org-brain-path "~/Org/")
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  :bind
   ("C-c v" . org-brain-visualize)
  :bind( :map org-mode-map
  ("C-c i" . org-id-get-create)))


;;used to turn on ascii-art-to-unicode package
(defun aa2u-buffer ()
  (aa2u (point-min) (point-max)))
  (add-hook 'org-brain-after-visualize-hook #'aa2u-buffer)


;; org-mindmap

(use-package org-mind-map
  :ensure t
  :config
    (setq org-mind-map-engine "dot")  ; default; Directed Graph
    ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph"
    ;; (setq org-mind-map-engine "twopi")  ; Radial Layout"
    ;; (setq org-mind-map-engine "circo")  ; Circular Layout"
    ;; (setq org-mind-map-engine "fdp")  ; Undirected Spring Force-Directed"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                      Editing Settings                                 ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spell check options
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;evil mode
(add-hook 'text-mode-hook 'evil-mode)
(add-hook 'prog-mode-hook 'evil-mode)

;;syntax checking
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'latex-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode)

;;auto completion
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'latex-mode-hook 'company-mode)


;;HS mode bindings when not already in use
;; Call this function as needed through hooks
(defun hs-minor-mode-keys ()
  (local-set-key "\C-ch" 'hs-hide-block)
  (local-set-key "\C-cs" 'hs-show-block))


;;Python Editing
(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'hs-minor-mode)
(add-hook 'elpy-mode-hook 'hs-minor-mode-keys)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                               Theme                                   ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package solarized-theme
  :ensure t
  :defer t)
;;(load-theme 'solarized-dark t)

(use-package monokai-theme
  :ensure t
  :defer t)
;;(load-theme 'monokai t)

(use-package spacemacs-theme
  :ensure t
  :defer t)
(load-theme 'spacemacs-dark)

;;Font Settings
'(default ((t (:family "Consolas" :foundry "PfEd" :slant normal :weight normal :height 116 :width normal))))

(provide '.emacs)
;;; .emacs ends here
