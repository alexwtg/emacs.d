;;; System

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "127.0.0.1:2081")
     ("https" . "127.0.0.1:2081")))
;; bars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;;
;; Put backup files neatly away
(let ((backup-dir "~/.emacs.d/local/backups")
      (auto-saves-dir "~/.emacs.d/local/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
	auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
	auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
	tramp-backup-directory-alist `((".*" . ,backup-dir))
	tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; neat scrolling
(setq scroll-margin 999
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
	      scroll-down-aggressively 0.01)
(setq c-default-style "linux"
      c-basic-offset 4)

;; Set up package.el to work with MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun pangpang/use-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;; neotree
(pangpang/use-package 'neotree)
(global-set-key (kbd "M-1") 'neotree-toggle)

;; rg.el
(pangpang/use-package 'rg)
(rg-enable-default-bindings)

;;; Evil Mode
;; Download Evil
(pangpang/use-package 'evil)
(evil-mode 1)

;; Terminal Emulator
(pangpang/use-package 'multi-term)
(setq multi-term-program "/bin/bash")

;;;; Programming Languages
;;; SLIME
(pangpang/use-package 'slime)
(setq inferior-lisp-program "sbcl")
;;; emmet
(pangpang/use-package 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; company
(pangpang/use-package 'company)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay 0)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)


;; yasnippet
(pangpang/use-package 'yasnippet)
(yas-global-mode 1)
(pangpang/use-package 'yasnippet-snippets)

;; golang
(pangpang/use-package 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook #'linum-mode)

;; eglot-mode
(pangpang/use-package 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'objc-mode-hook 'eglot-ensure)

(defun eglot-go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))
(add-hook 'go-mode-hook #'eglot-go-install-save-hooks)

;; rainbow parens
(pangpang/use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; electric pair
(electric-pair-mode 1)
;;; Simple clip
(pangpang/use-package 'simpleclip)
(simpleclip-mode 1)
;;; all the icons
(pangpang/use-package 'all-the-icons)

;;; helm
(pangpang/use-package 'helm)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)
(defun helm-buffer-face-mode ()
  "Helm buffer face"
  (interactive)
  (with-helm-buffer
    (setq line-spacing 2)
    (buffer-face-set '(:family "monospace" :height 200))))

(add-hook 'helm-after-initialize-hook 'helm-buffer-face-mode)

;;; which-key
(pangpang/use-package 'which-key)
(setq which-key-idle-delay .5)
(which-key-mode)

;;; general
(pangpang/use-package 'general)
(general-define-key
 :states 'normal
 ;; switch buffers
 "J" 'previous-buffer
 "K" 'next-buffer)


(general-create-definer pangpang/leader-def
  :prefix "SPC")
(pangpang/leader-def
 :keymaps 'normal
 "p" 'package-refresh-contents
 "b" 'helm-buffers-list
 "f" 'helm-find-files
 "x" 'helm-M-x
 "k" 'kill-buffer
 )
;;; Programming Languages
;; python
(pangpang/use-package 'pyvenv)
;;; Theming
;; font size
(set-face-attribute 'mode-line nil :height 200)
(setq default-frame-alist '((font . "Source Code Pro-20")))
(pangpang/use-package 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-city-lights t)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;;; mode line
(pangpang/use-package 'doom-modeline)
(doom-modeline-mode 1)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
(doom-themes-treemacs-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
