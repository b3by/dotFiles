;;; .emacs --- Summary
;;; Commentary:
;; Some useful configurations for emacs

;;; Code:

;; marmalade repo
(require 'package)
(add-to-list 'package-archives
             '("marmalade" ."http://marmalade-repo.org/packages/") t)

;; melpa repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; elpa repo
(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)

;; init packages
(package-initialize)

;; minimum package required
(setq package-list '(auto-complete
                     autopair
                     color-theme-solarized
                     elscreen
                     flymake
                     pretty-lambdada
                     pretty-symbols
                     pretty-mode
                     ))

;; fetch list
(when (not package-archive-contents)
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; plugins path
;;(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/emms/")

(global-set-key (kbd "<f8>")
                (lambda ()
                  (interactive)
                  (emms-play-directory-tree "./Music/")
                  (emms-random)))

(global-set-key (kbd "<f9>") 'emms-next)

(global-set-key (kbd "<f7>")
                (lambda ()
                  (interactive)
                  (emms-previous)))

;; emms stuff
(require 'emms-setup)
(emms-devel)
(emms-default-players)

;; customization from emacs, basically just trusted themes
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "fee4bfb5cfc4912d443840127d10b3ef64918bf510c181d7cafff4fb1510a1a2" "2b7d55fde75d5636ed0d5bf33d1b06ac75162f06816abe00522692e7daddc9ec" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; no startup screen
(setq-default inhibit-startup-screen 1)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; visible bell for errors
(setq visible-bell t)

;; for me just 2, thanks!
(setq-default tab-width 2)

;; easy to read
(setq-default fill-column 80)

;; never ever!
(setq-default indent-tabs-mode nil)

;; seek and destroy
;;(hc-toggle-highlight-tabs)
;;(hc-toggle-highlight-trailing-whitespace)

;; standard indent value
(setq-default standard-indent 2)

;; autosaves and backups out of sight, please!
(setq-default auto-save-file-name-transforms
              (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
(setq-default backup-directory-alist
              (quote ((".*" . "~/.emacs.d/backups/"))))

;; add tab when go to new line
(setq indent-line-function 'insert-tab)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; load login shell
(defvar explicit-bash-args '("--login"))

;; auto-completion
(require 'auto-complete-config)
(ac-config-default)

;; auto-completion for ielm mode
(defun ielm-auto-complete () "Enables `auto-complete' support in \\[ielm]."
  (defvar ac-sources '(ac-source-functions
                       ac-source-variables
                       ac-source-features
                       ac-source-symbols
                       ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))

(add-hook 'ielm-mode'hook 'ielm-auto-complete)

;; theme
(load-theme 'solarized-dark 1)

;; change font
(set-frame-font "Anonymous Pro")

;; font size
(set-face-attribute 'default nil :height 134)

;; show column number
(column-number-mode 1)

;; show matching parens
(show-paren-mode 1)

;; don't show that useless tool-bar
(tool-bar-mode 0)

;; show always line number
(global-linum-mode 1)

;; completion for commands
(icomplete-mode 99)

;; braces completion
;;(autopair-global-mode t)
(electric-pair-mode 1)

;; disable backup
(setq backup-inhibited t)

;; disable auto save
(setq auto-save-default nil)

;; highlight current line
(global-hl-line-mode 1)

;; some autocompletion
(global-auto-complete-mode 1)

;; dat lambda
(global-pretty-lambda-mode 1)

;; lazyness remedy
(turn-on-auto-fill)

;; what time is it?
(display-time)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)


;; dat mode!
;;(global-set-key (kbd "<f7>")
;;                '(lambda()
;;                   (interactive)
;;                   (auto-fill-mode 'toogle)
;;                   (auto-complete-mode 'toogle)
;;                   (hs-minor-mode 'toogle)
;;                   (message "dat mode enabled")))

(define-minor-mode dat-mode
  "Mmm...dat mode!"
  :lighter " dat-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f7>") '(lambda () (message "dat mode enabled")))
              map)
  (auto-fill-mode)
  (auto-complete-mode 1)
  (hs-minor-mode))

;; kill everything left to cursor, no mercy
(global-set-key (kbd "ESC k")
                (lambda ()
                  (interactive)
                  (kill-line 0)))

;; hide / show block
(global-set-key (kbd "ESC <down>")
                (lambda ()
                  (interactive)
                  (hs-show-block)))

(global-set-key (kbd "ESC <up>")
                (lambda ()
                  (interactive)
                  (hs-hide-block)))

;; elscreen bindings
(load "elscreen" "ElScreen" t)
(global-set-key (kbd "C-x t")
                (lambda ()
                  (interactive)
                  (elscreen-start)
                  (elscreen-create)))
(global-set-key (kbd "\s S-<right>")
                (lambda ()
                  (interactive)
                  (elscreen-next)))
(global-set-key (kbd "\s S-<left>")
                (lambda ()
                  (interactive)
                  (elscreen-previous)))
(global-set-key (kbd "\s S-<down>")
                (lambda ()
                  (interactive)
                  (elscreen-kill)))

;; default window size
(if (window-system)
    (progn
      (set-frame-width (selected-frame) 100)
      (set-frame-height (selected-frame) 40)))

(provide '.emacs)

;;; .emacs ends here
