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
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages"))

;; elpa repo
;; (require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)

;; init packages
(package-initialize)

;; minimum package required
(setq package-list '(auto-complete
                     autopair
                     fill-column-indicator
                     color-theme-solarized
                     smart-mode-line
                     pretty-lambdada
                     pretty-symbols
                     pretty-mode
                     blank-mode
                     ))

(elpy-enable)

(require 'cl)
;; (require 'ess-site)
(require 'fill-column-indicator)

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq powerline-arrow-shape 'rounded)
(setq powerline-default-separator-dir '(right . left))
(setq sml/theme 'respectful)
(setq sml/mode-width 0)
(setq sml/name-width 20)
(sml/setup)

(setq-default py-split-windows-on-execute-function 'split-window-horizontally)

;; fetch list
(when (not package-archives)
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; plugins path
;; (add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/plugins/")

; ;; customization from emacs, basically just trusted themes
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" default)))
 '(global-hl-line-mode t)
 '(package-selected-packages
   (quote
    (virtualenvwrapper elpy virtualenv py-autopep8 nyan-mode molokai-theme moe-theme focus jedi-direx jedi helm-spotify zenburn-theme zenburn yasnippet web-mode vala-mode tangotango-theme solarized-theme smart-mode-line-powerline-theme s pyvenv python-mode pretty-symbols pretty-mode pretty-lambdada php-mode material-theme markdown-preview-mode markdown-mode+ julia-mode ipython highlight-indentation gitignore-mode flatland-theme find-file-in-project fill-column-indicator evil elscreen ein dracula-theme company color-theme-solarized color-theme-sanityinc-tomorrow blank-mode autopair auto-complete ample-zen-theme alect-themes)))
 '(py-split-window-on-execute (quote just-two))
 '(python-shell-exec-path (quote ("/usr/bin/python3.6")))
 '(python-shell-interpreter "python3.6"))

(setq python-python-command "/usr/bin/python3.6")
(setq python-shell-interpeter "/usr/bin/python3.6")
(setq py-python-command "/usr/bin/python3.6")

;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)

;; (setq jedi:complete-on-dot t)

;; no startup screen
(setq-default inhibit-startup-screen t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

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
(load-theme 'zenburn 1)

;; set color for highlighted lines
(set-face-background 'hl-line "#333333")

;; show column indicator
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; ruler color
(setq fci-rule-color "white")

;; auto fill mode everywhere
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; change font
(set-frame-font "Inconsolata")

;; font size
(set-face-attribute 'default nil :height 134)

;; show column number
(column-number-mode 1)

;; no scrollbar
(toggle-scroll-bar -1)

;; show matching parens
(show-paren-mode 1)

;; don't show that useless tool-bar
(tool-bar-mode -1)

;; menu bar, gone
(menu-bar-mode -1)

;; show always line number
(global-linum-mode 1)
(setq linum-format " %d ")

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

;; split horizontally by default
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; javascript indentation level
(setq js-indent-level 2)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scrolls one line at a time

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
(define-minor-mode dat-mode
  "Mmm...dat mode!"
  :lighter " dat-mode"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<f7>") '(lambda () (message "dat mode enabled")))
	    map)
  (auto-fill-mode)
  (pretty-mode)
  (auto-complete-mode 1)
  (fci-mode)
  (hs-minor-mode))

;; comment current line
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-x /")
                (lambda ()
                  (interactive)
                  (toggle-comment-on-line)))

;; copy current line to ring
(defun copy-current-line ()
  "copy line current line"
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-x c")
                (lambda ()
                  (interactive)
                  (copy-current-line)))

;; kill everything left to cursor, no mercy
(global-set-key (kbd "ESC k")
                (lambda ()
                  (interactive)
                  (kill-line 0)))

;; split window vertically, tmux style
(global-set-key (kbd "C-x _")
                (lambda ()
                  (interactive)
                  (split-window-vertically)))

;; split window horizontally, tmux style
(global-set-key (kbd "C-x |")
                (lambda ()
                  (interactive)
                  (split-window-horizontally)))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-o") "\C-e\C-m")

;; hide / show block
(global-set-key (kbd "ESC <down>")
		(lambda ()
		  (interactive)
		  (hs-show-block)))

(global-set-key (kbd "ESC <up>")
		(lambda ()
		  (interactive)
		  (hs-hide-block)))

;; default window size
(if (window-system)
    (progn
      (set-frame-width (selected-frame) 100)
      (set-frame-height (selected-frame) 40)))

;; just 2 spaces for java mode
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2)))

;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide '.emacs)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
