;;; * General settings
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(display-time-mode t)
(when (not window-system)
  (menu-bar-mode -1))
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(show-paren-mode t)
(blink-cursor-mode -1)
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/etc")

;; emacs -nw:  emacs in terminal
;; map RET to [return] in terminal mode, fix `ret` in cscope jumping
(let ((map (if (boundp 'input-decode-map)
	       input-decode-map function-key-map)))
  (define-key map (kbd "RET") [return]))

;;; * Miscellaneous

;; metrics
(setq initial-frame-alist
      `((height . 60)
	(width . 150)))
;; (setq-default line-spacing 2)

;; scrolling
(setq scroll-margin 10)
(setq scroll-step 1)

;; Set find-file to be case insensitive
(setq read-file-name-completion-ignore-case t)

;; font
;; (add-to-list 'default-frame-alist
;; 	     '(font . "PragmataPro-14"))
(add-to-list 'default-frame-alist
	     '(font . "Monaco-12"))

;; backup
(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; whole line or region
(defun my-kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end) nil)
		 (list (line-beginning-position)
		       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
	  (goto-char end)
	(goto-char beg))
      (sit-for blink-matching-delay))))
(global-set-key [remap kill-ring-save] 'my-kill-ring-save)

(put 'kill-region 'interactive-form      
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; open large files
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

;; controls
(windmove-default-keybindings 'meta)

;; global key rebinding
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-k") '(lambda ()
				   ;; kill current active buffer
				   (interactive)
				   (kill-buffer (current-buffer))))
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-=") 'cssh-term-remote-open)

(global-set-key (kbd "s-p") 'previous-line)
(global-set-key (kbd "s-n") 'next-line)
(global-set-key (kbd "s-b") 'backward-char)
(global-set-key (kbd "s-f") 'forward-char)
(global-set-key (kbd "s-o") 'find-file)

;; desktop
(desktop-save-mode t)

;;; * Packages

(package-initialize)

;;; ** ag
(setq ag-highlight-search t)
(setq ag-reuse-window 't)
(global-set-key [f2] 'ag-project)
;;; ** ansi-term
(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;;; ** perl

;; append perlbrew path to environment variable
(setenv "PATH"
	(concat "~/.pb/perls/perl-5.16-thread/bin:~/zion/go/bin:~/zion/letsgo/bin:/usr/local/bin:" 
		(getenv "PATH")))
(setenv "PATH" (concat "~/zion/racket/racket/bin:" (getenv "PATH")))
(setq exec-path (split-string (getenv "PATH") ":"))

;;; ** ELPA

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

;;; ** Git

;; (add-to-list 'load-path "/usr/local/share/git-core/contrib/emacs")
;; (require 'git)
(global-set-key (kbd "C-c i") 'magit-status) ; 'i' for info (of git repo)

;; ;; bookmark+
;; (add-to-list 'load-path "~/.emacs.d/elpa/bookmark+-20130317.1522")
;; (require 'bookmark+)


;;; ** gnus Gmail

;; (require 'gnus)
;; (setq gnus-select-method '(nnimap "gmail"
;; 				  (nnimap-address "imap.gmail.com")   ; it could also be imap.googlemail.com if that's your server.
;; 				  (nnimap-server-port 993)
;; 				  (nnimap-stream ssl)))

;;; ** gocode

;; (eval-after-load "auto-complete-config"
;;   '(progn
;;      (require 'go-autocomplete)
;;      ))
;; (require 'auto-complete-config)

;; (eval-after-load "company-mode"
;;   '(progn
;;      ))
;; (setq company-tooltip-limit 20)                      ; bigger popup window
;; (setq company-minimum-prefix-length 0)               ; autocomplete right after '.'
;; (setq company-idle-delay .3)                         ; shorter delay before autocompletion popup
;; (setq company-echo-delay 0)                          ; removes annoying blinking
;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; (add-hook 'after-init-hook (lambda ()
;; 			     (progn
;; 			       (require 'company-clang)
;; 			       (add-hook 'go-mode-hook (lambda ()
;; 							 (progn
;; 							   (require 'company) ; load company mode
;; 							   (require 'company-go) ; load company mode go backend
;; 							   (set (make-local-variable 'company-backends) '(company-go))
;; 							   (company-mode))))
;; 			       (add-hook 'c-mode-hook (lambda ()
;; 							(company-mode)
;; 							)))))
;; (setq geiser-company--completions t)

;; (custom-set-faces
;;  '(company-preview
;;    ((t (:foreground "darkgray" :underline t))))
;;  '(company-preview-common
;;    ((t (:inherit company-preview))))
;;  '(company-tooltip
;;    ((t (:background "lightgray" :foreground "black"))))
;;  '(company-tooltip-selection
;;    ((t (:background "steelblue" :foreground "white"))))
;;  '(company-tooltip-common
;;    ((((type x)) (:inherit company-tooltip :weight bold))
;;     (t (:inherit company-tooltip))))
;;  '(company-tooltip-common-selection
;;    ((((type x)) (:inherit company-tooltip-selection :weight bold))
;;     (t (:inherit company-tooltip-selection)))))

;;; ** go-mode

;; change default indent tab width from 8 to 4
(add-hook 'go-mode-hook (lambda ()
			  (setq tab-width 4)))

;;; ** lua-mode
;; use an indentation width of two spaces
(setq lua-indent-level 2)
;;; ** powerline

;; (add-to-list 'load-path "~/.emacs.d/powerline")
;; (add-hook 'after-init-hook (lambda ()
;; 			     (progn
;; 			       (require 'powerline)
;; 			       (when window-system
;; 				 (set-face-attribute 'mode-line nil
;; 				 		     ;; :background "OliveDrab3"
;; 				 		     :foreground "black")
;; 				 (powerline-default-theme))
;; 				)))

;; hl-line-mode
;; (require 'hl-line)
;; (when window-system
;;   (global-hl-line-mode t))

;;; ** quack
;; The binary of your interpreter
(setq scheme-program-name "racket")
 
;; This hook lets you use your theme colours instead of quack's ones.
;; (defun scheme-mode-quack-hook ()
;;   (require 'quack)
;;   ;; (setq quack-fontify-style 'plt)
;;   )
;; (add-hook 'scheme-mode-hook 'scheme-mode-quack-hook)

;;; ** ido

(require 'ido)
(ido-mode t)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))) ; Display ido results vertically, rather than horizontally
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
;; (global-set-key (kbd "C-x b") 'list-buffers)
;; (global-set-key (kbd "C-x C-b") 'list-buffers)
;; (global-set-key (kbd "C-x b") 'ido-switch-buffer)

;; From EmacsWiki
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
	  (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  ;; N.B. `let' syntax, symbol solely will be bould to nil
	  name-and-pos symbol-names position)
      (unless ido-mode
	      (ido-mode 1)
	      (setq ido-enable-flex-matching t))
      (while (progn
	       ;; this is "repeat...until" mode of while loop
	       ;; the last statement will act as the end-test
	      (imenu--cleanup)
	      (setq imenu--index-alist nil)
	      (ido-goto-symbol (imenu--make-index-alist)) ; generate the symbols
	      (setq selected-symbol
		    (ido-completing-read "Symbol: " symbol-names))
	      (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	      (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
	    (let (name position)
	      (cond
	       ((and (listp symbol) (imenu--subalist-p symbol))
		(ido-goto-symbol symbol))
	       ((listp symbol)
		(setq name (car symbol))
		(setq position (cdr symbol)))
	       ((stringp symbol)
		(setq name symbol)
		(setq position
		      (get-text-property 1 'org-imenu-marker symbol))))
	      (unless (or (null position) (null name) ;; test if variable is still nil
			  (string= (car imenu--rescan-item) name))
		      (add-to-list 'symbol-names name)
		      (add-to-list 'name-and-pos (cons name position))))))))

(defvar smart-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended
syntax in finding matches, if such matches exist.")

(defvar smart-last-symbol-name ""
  "Contains the current symbol name.

This is only refreshed when `last-command' does not contain
either `smart-symbol-go-forward' or `smart-symbol-go-backward'")

(make-local-variable 'smart-use-extended-syntax)

(defvar smart-symbol-old-pt nil
  "Contains the location of the old point")

(defun smart-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smart-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smart-symbol-go-forward
                               smart-symbol-go-backward))
    (setq smart-last-symbol-name name))
  (setq smart-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smart-last-symbol-name))
  (unless (catch 'done
            (while (funcall (cond
                             ((eq direction 'forward) ; forward
                              'search-forward)
                             ((eq direction 'backward) ; backward
                              'search-backward)
                             (t (error "Invalid direction"))) ; all others
                            smart-last-symbol-name nil t)
              (unless (memq (syntax-ppss-context
                             (syntax-ppss (point))) '(string comment))
                (throw 'done t))))
    (goto-char smart-symbol-old-pt)))

(defun smart-symbol-go-forward ()
  "Jumps forward to the next symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'end) 'forward))

(defun smart-symbol-go-backward ()
  "Jumps backward to the previous symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'beginning) 'backward))

(defun smart-symbol-at-pt (&optional dir)
  "Returns the symbol at point and moves point to DIR (either `beginning' or `end') of the symbol.

If `smart-use-extended-syntax' is t then that symbol is returned
instead."
  (with-syntax-table (make-syntax-table)
    (if smart-use-extended-syntax
        (modify-syntax-entry ?. "w"))
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    ;; grab the word and return it
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (cond
             ((eq dir 'beginning) (goto-char (car bounds)))
             ((eq dir 'end) (goto-char (cdr bounds)))
             (t (error "Invalid direction")))
            word)
        (error "No symbol found")))))

(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)

;;; ** speedbar

;; (when window-system
;;   (require 'sr-speedbar))
;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)
;; (setq speedbar-show-unknown-files t)
;; (setq sr-speedbar-right-side nil)
;; ;; (custom-set-variables '(sr-speedbar-right-side nil) '(sr-speedbar-skip-other-window-p t) '(sr-speedbar-max-width 30) '(sr-speedbar-width-x 30))
;; (setq sr-speedbar-width 16)
;; (setq sr-speedbar-width-x 16)
;; (setq sr-speedbar-skip-other-window-p t)
;; (setq speedbar-frame-parameters (quote
;; 				 ((minibuffer)
;; 				  (width . 20)
;; 				  (border-width . 0)
;; 				  (menu-bar-lines . 0)
;; 				  (tool-bar-lines . 0)
;; 				  (unsplittable . t)
;; 				  (left-fringe . 0))))
;; (sr-speedbar-open);


;;; ** tramp
(setq tramp-ssh-controlmaster-options (concat "-o ControlPath=~/.ssh/cm_socket/%r@%h:%p "
					      "-o ControlMaster=auto -o ControlPersist=no"))
(setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>›\n]*#?[]#$%>›] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")

;;; ** yasnippet

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)

;; first unbind the tab from auto expand
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "C-c ; u") 'yas-expand)
(defadvice yas/insert-snippet (around use-completing-prompt activate)
  "Use `yas/completing-prompt' for `yas/prompt-functions' but only here..."
  (let ((yas/prompt-functions '(yas/completing-prompt)))
    ad-do-it))
(add-hook 'c-mode-hook
	  '(lambda ()
	     (yas-minor-mode)))
(add-hook 'org-mode-hook
	  '(lambda ()
	     (yas-minor-mode)))
;; (yas-global-mode 1)
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand)
(yas-reload-all)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/yas-go")

;; (font-lock-add-keywords 'c-mode '(("\\(\\w+\\)\\s-*\(" . font-lock-function-name-face)))
;; (font-lock-add-keywords 'c-mode '(("if" . font-lock-keyword-face)))
;; (font-lock-add-keywords 'c-mode '(("for" . font-lock-keyword-face)))
;; (font-lock-add-keywords 'c-mode '(("while" . font-lock-keyword-face)))
(global-font-lock-mode t)
;; (setq font-lock-defaults t)

;; (when (and (fboundp 'semantic-mode)
;;            (not (locate-library "semantic-ctxt"))) ; can't found offical cedet
;;       (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;; 					global-semanticdb-minor-mode
;; 					global-semantic-idle-summary-mode
;; 					global-semantic-mru-bookmark-mode)))
;; (semantic-mode 1)
;; (require 'semantic/ctxt)
;; (add-to-list 'load-path "~/.emacs.d/elpa/highlight-21.0")
;; (require 'zjl-hl)
;; ;
;;(zjl-hl-enable-global-all-modes)

;; (zjl-hl-enable-global 'c-mode);; (zjl-hl-disable-global 'c-mode)

;; (zjl-hl-enable-global 'emacs-lisp-mode);; (zjl-hl-disable-global 'emacs-lisp-mode)


;;; ** auto-complete

(add-hook 'after-init-hook (lambda ()
			     (require 'auto-complete-config)
			     (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
			     (ac-config-default)
			     (setq-default ac-sources '(
							ac-source-yasnippet
							ac-source-abbrev
							ac-source-dictionary
							ac-source-words-in-same-mode-buffers
							))
))

(add-hook 'after-init-hook (lambda ()
			     (progn
			       (defun ac-cc-mode-setup ()
				 (setq ac-clang-complete-executable "~/.emacs.d/emacs-clang-complete-async/clang-complete")
				 (setq ac-sources '(ac-source-clang-async))
				 (ac-clang-launch-completion-process)
				 )
			       (defun ac-go-mode-setup ()
				 (require 'go-autocomplete)
				 (require 'auto-complete-config)
				 (add-hook 'before-save-hook #'gofmt-before-save))

			       (defun my-ac-config ()
				 (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
				 (add-hook 'auto-complete-mode-hook 'ac-common-setup)
				 (add-hook 'go-mode-hook 'ac-go-mode-setup)
				 )

			       (my-ac-config))))

;;; cursor
;; (setq-default cursor-type 'hbar)
;; (require 'cursor-chg)
;; (change-cursor-mode t)
;; (toggle-cursor-type-when-idle t)

;;; ** xcscope/ascope

;; (add-to-list 'load-path "~/.emacs.d/xcscope")
;; (require 'xcscope)
(require 'ascope)
(add-hook 'after-init-hook
	  (lambda ()
	    (progn
	      (defun ascope-c-mode-setup ()
		(progn
		  (define-key c-mode-base-map (kbd "C-c s a") 'ascope-init)
		  (define-key c-mode-base-map (kbd "C-c s g") 'ascope-find-global-definition)
		  (define-key c-mode-base-map (kbd "C-c s c") 'ascope-find-functions-calling-this-function)
		  (define-key c-mode-base-map (kbd "C-c s s") 'ascope-find-this-symbol)
		  (define-key c-mode-base-map (kbd "C-c s u") 'ascope-pop-mark)))
	      (defun ascope-asm-mode-setup ()
		(progn
		  (define-key asm-mode-map (kbd "C-c s a") 'ascope-init)
		  (define-key asm-mode-map (kbd "C-c s g") 'ascope-find-global-definition)
		  (define-key asm-mode-map (kbd "C-c s c") 'ascope-find-functions-calling-this-function)
		  (define-key asm-mode-map (kbd "C-c s s") 'ascope-find-this-symbol)
		  (define-key asm-mode-map (kbd "C-c s u") 'ascope-pop-mark)))
	      (defun my-ascope-config ()
		(add-hook 'c-mode-common-hook 'ascope-c-mode-setup)
		(add-hook 'asm-mode-hook 'ascope-asm-mode-setup))
	      (my-ascope-config))))

;; eshell
(setq eshell-cmpl-ignore-case t)
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map (kbd "C-g") 'delete-window)))


;; Key bindings
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)
;; (global-set-key [f1] 'term) ;; instead of shell
(global-set-key [f1] 'eshell)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f8] 'sr-speedbar-toggle)
(global-set-key [f11] 'ns-toggle-fullscreen)
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)


;;; ** markdown-mode

(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          (function
           (lambda ()
             (setq tab-width 4
		   indent-tabs-mode nil)
	     )))
(add-hook 'markdown-mode-hook
          (function
           (lambda ()
	     (local-set-key (kbd "<tab>") 'markdown-insert-pre)
	     )))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;; ** org
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" default)))
 '(fci-rule-color "#49483E")
 '(fill-column 80)
 '(helm-split-window-in-side-p t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(ido-max-window-height 15)
 '(magit-diff-use-overlays nil)
 '(org-capture-templates
   (quote
    (("m" "Miscellaneous pieces of thoughts" entry
      (file+headline
       (concat org-directory "/misc.org")
       ""))
     ("t" "Task" entry
      (file+headline
       (concat org-directory "/todo.org")
       "Tasks")
      "* TODO %?
  %u
  %a"))))
 '(org-default-notes-file "~/Dropbox/notes/todo.org")
 '(org-directory "~/Dropbox/notes")
 '(split-height-threshold 120)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(global-set-key "\C-cn" 'org-capture)

(setq org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-todo-done 'note)

;; organize structure of .emacs configuraton
(add-hook 'emacs-lisp-mode-hook 'turn-on-orgstruct)
(setq orgstruct-heading-prefix-regexp "^;;; *")
(setq org-startup-truncated nil)

(setq org-src-fontify-natively t)

;; Someone says it is not necessary to do the `require' and also shouldn't
;; (require 'org-latex)
(require 'ox-latex)

(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf -quiet %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex='xelatex -8bit --shell-escape' -pdf -quiet %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))
(add-hook 'org-latex-after-initial-vars-hook 'my-auto-tex-cmd)

(defun my-auto-tex-parameters ()
  "Automatically select the tex packages to include."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-latex-default-packages-alist
	'(("AUTO" "inputenc" t)
	  ("T1"   "fontenc"   t)
	  (""     "fixltx2e"  nil)
	  (""     "wrapfig"   nil)
	  (""     "soul"      t)
	  (""     "textcomp"  t)
	  (""     "marvosym"  t)
	  (""     "wasysym"   t)
	  (""     "latexsym"  t)
	  (""     "amssymb"   t)
	  (""     "hyperref"  nil)))
  
  ;; Packages to include when xelatex is used
  ;; (see https://github.com/kjhealy/latex-custom-kjh for the 
  ;; non-standard ones.)
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-latex-default-packages-alist
	    '(("" "fontspec" t)
	      ("" "xunicode" t)
	      ("" "url" t)
	      ("" "rotating" t)
	      ("" "memoir-article-styles" t)
	      ("american" "babel" t)
	      ("babel" "csquotes" t)
	      ("" "listings" nil)
	      ("" "listings-sweave-xelatex" nil)
	      ("svgnames" "xcolor" t)
	      ("" "soul" t)
	      ("xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels, bookmarksnumbered" "hyperref" nil)
	      )))
  
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-latex-classes
	    (cons '("article"
		    "\\documentclass[11pt,article,oneside]{memoir}
  \\input{vc}
  \\usepackage[style=authoryear-comp-ajs, abbreviate=true]{biblatex}
  \\bibliography{socbib}"
		    ("\\section{%s}" . "\\section*{%s}")
		    ("\\subsection{%s}" . "\\subsection*{%s}")
		    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		    ("\\paragraph{%s}" . "\\paragraph*{%s}")
		    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
		  org-latex-classes))))  

(add-hook 'org-latex-after-initial-vars-hook 'my-auto-tex-parameters)

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

;; reftex settings
(defun org-mode-reftex-setup ()
  ;; (load-library "reftex")
  (require 'reftex)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
	 ;; add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?p . "[[papers:%l][%l-paper]]")
	    (?t . "%t")
	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;;; ** org-jekyll
;; (require 'org-publish)
(setq org-export-async-debug t)
;; (setq org-html-html5-fancy t)

(defadvice org-html-paragraph (before org-html-paragraph-advice
				      (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let* ((fixed-contents)
	 (origin-contents (ad-get-arg 1))
	 (fix-regexp "[[:multibyte:]]"))
    (setq fixed-contents
	  (replace-regexp-in-string
	   (concat
	    "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2"
	    origin-contents))
    (ad-set-arg 1 fixed-contents)))

(defadvice org-publish-org-sitemap
  (after remove-index-from-sitemap (project &optional sitemap-filename) activate)
  ;; search backward because the default position is at the end
  ;; because the buffer is closed by `org-publish-org-sitemap'
  ;; so I have to open it again
  (with-current-buffer
      (let* ((project-plist (cdr project))
	     (dir (file-name-as-directory
		   (plist-get project-plist :base-directory)))
	     (sitemap-filename (concat dir (or sitemap-filename "sitemap.org"))))
      	(setq sitemap-buffer
	      (find-file sitemap-filename)))
    ;; move cursor to beginning of buffer
    (beginning-of-buffer)
    ;; search for index and delete it
    (while (search-forward-regexp "^.*\\(index\\|about\\|header\\).*\n" nil t)
      (replace-match "" t t))

    ;; move date to a span outside anchor
    (beginning-of-buffer)
    (replace-regexp " \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\]\\]" "]]
     #+BEGIN_HTML
       <span class=\"timestamp\">\\1</span>
     #+END_HTML")
    (save-buffer)))

(defadvice org-html-template
  (after org-html-disable-title-in-content (contents info) activate)
  (if (string-equal (file-name-nondirectory buffer-file-name) "index.org")
      (setq ad-return-value
	    (replace-regexp-in-string "<h1 class=\"title\">.*\n" "" ad-return-value))))

;; (defadvice org-publish-org-sitemap
;;   (around remove-index-from-sitemap (project &optional sitemap-filename) activate)
;;   (let ((project (ad-get-arg 0)))
;;     (ad-set-arg 0 (cons (car project) (plist-put (cdr project) :exclude "index.org")))
;;     (ad-do-it)
;;     (plist-put (cdr project) :exclude nil)))

;; (defun my-org-html-publish-to-html (plist filename pub-dir)
;;   ;; (message "---------")
;;   ;; (message filename)
;;   (if (string-equal (file-name-nondirectory filename) "sitemap.org")
;;       (org-html-publish-to-html (plist-put plist :body-only t)
;; 				filename pub-dir)
;;     (org-html-publish-to-html plist filename pub-dir)))

(setq org-publish-project-alist
      '(
	("org-blog"
	 :base-directory "~/Documents/Dropbox/notes/org-blog/"
	 :base-extension "org"
	 :publishing-directory "~/p/org-blog/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
         :headline-levels 4               ; Just the default for this project.
         :auto-preamble nil
         :auto-sitemap t                  ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title ""                ; ... with title 'Sitemap'.
	 :sitemap-file-entry-format "%t %d"
	 :sitemap-sort-files anti-chronologically
         :export-creator-info nil    ; Disable the inclusion of "Created by Org" in the postamble.
         :export-author-info nil     ; Disable the inclusion of "Author: Your Name" in the postamble.
         :auto-postamble nil         ; Disable auto postamble 
         :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
         :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
         ;; :html-postamble "    <p class=\"postamble\">Last Updated %d.</p> " ; your personal postamble

	 :html-preamble "<div class=\"header\">
<div class=\"header-menu\">
<ul class=\"org-ul\">
<li><a href=\"about.html\">ABOUT</a>
</li>
</ul>
</div>
<section class=\"header-name\"><a href=\"/\"><b>Y</b>ANG<b>H</b>ONG</a></section>
</div>
"
	 :html-postamble "<div id=\"disqus_thread\"></div>
    <script type=\"text/javascript\">
        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
        var disqus_shortname = 'yanghong'; // required: replace example with your forum shortname

        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
            dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        })();
    </script>
    <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
    <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>"
         :html-head-include-default-style nil  ;Disable the default css style
	 :html-head-include-scripts nil
	 :html-html5-fancy t
	 :html-doctype "html5"

	 :with-toc nil 			; Disable table of contents
)))

;;; ** linum-mode
(global-linum-mode 1)
;; seperate line numbers from text
(setq linum-format
      (lambda (line)
	(propertize (format
		     (let ((w (length (number-to-string
				       (count-lines (point-min) (point-max))))))
		       (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))
(column-number-mode 1)
(add-hook 'linum-mode-hook (lambda ()
			     (progn
			       (require 'auto-complete-config)
			       (ac-linum-workaround))))

;;; ** slime & scheme

;; (setq inferior-lisp-program "/opt/sbcl/bin/sbcl") ; your Lisp system
;; (require 'slime)
;; (slime-setup)

;; gambit - emacs interface
;; (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;; (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;; (add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "/usr/local/bin/scheme")
(require 'xscheme)
(setq geiser-active-implementations '(racket))

;; (require 'gambit)

;;; ** zencoding

(add-to-list 'load-path "~/.emacs.d/zencoding")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;; ** c-mode and c-common-mode

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(defun my-c-style-hook ()
  (let ((filename (buffer-file-name))
	(google-c-lineup-expression-plus-8
	 (lambda (langelem)
	   (save-excursion
	     (back-to-indentation)
	     ;; Go to beginning of *previous* line:
	     (c-backward-syntactic-ws)
	     (back-to-indentation)
	     ;; We are making a reasonable assumption that if there is a control
	     ;; structure to indent past, it has to be at the beginning of the line.
	     (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
		 (goto-char (match-end 1)))
	     (vector (+ 8 (current-column))))))
	(google-c-lineup-expression-plus-0
	 (lambda (langelem)
	   (save-excursion
	     (back-to-indentation)
	     ;; Go to beginning of *previous* line:
	     (c-backward-syntactic-ws)
	     (back-to-indentation)
	     ;; We are making a reasonable assumption that if there is a control
	     ;; structure to indent past, it has to be at the beginning of the line.
	     (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
		 (goto-char (match-end 1)))
	     (vector (current-column))))))
    ;; Enable kernel mode for the appropriate files
    (when (and filename
	       (string-match "drreplay"
			     (expand-file-name filename)))
      (setq comment-start "//"
	    comment-end   "")
      
      (c-set-offset 'arglist-intro
		    'google-c-lineup-expression-plus-8)
      (c-set-offset 'arglist-cont-nonempty
		    '(c-lineup-gcc-asm-reg (c-lineup-argcont 8))))
    (when (and filename
	       (or (string-match "cse-lab"
				 (expand-file-name filename))
		   (string-match "yparse"
				 (expand-file-name filename))))
      (setq c-basic-offset 4)
      (c-set-offset 'arglist-intro
		    'google-c-lineup-expression-plus-8)
      (c-set-offset 'arglist-cont-nonempty
		    '(c-lineup-gcc-asm-reg (c-lineup-argcont 8))))
    (when (and filename
	       (or ;; (string-match "linux"
		   ;; 	     (expand-file-name filename))
      (setq indent-tabs-mode t)
      (setq c-indentation-style "linux")
      (setq c-basic-offset 8)
      ;; (c-set-offset 'arglist-intro
      ;; 		    'google-c-lineup-expression-plus-4)
      (c-set-offset 'arglist-cont-nonempty 8)
      		    ;; 'google-c-lineup-expression-plus-4)
      ;; 		    '(c-lineup-gcc-asm-reg (c-lineup-argcont 0)))
      ;; (c-set-offset 'statement-cont 8)
      )))))

(add-hook 'c-mode-hook 'my-c-style-hook)
(add-hook 'c++-mode-hook 'my-c-style-hook)

;; Linux Kernel Coding Style
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
			 (or
			  (string-match "linux"
					(expand-file-name filename))
			  (string-match "lxch"
					(expand-file-name filename))
			  (string-match "dpdk"
					(expand-file-name filename))
			  (string-match "pkt-chan"
					(expand-file-name filename))))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))

;; highlight fixme
(add-hook 'tex-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (add-hook 'c-mode-hook
;; 	  '(lambda()
;; 	     (c-set-style "k&r")
;; 	     (setq c-basic-offset 4)
;; 	     (setq indent-tabs-mode nil)))

;; (defun c-lineup-arglist-tabs-only (ignored)
;;   "Line up argument lists by tabs, not spaces"
;;   (let* ((anchor (c-langelem-pos c-syntactic-element))
;; 	 (column (c-langelem-2nd-pos c-syntactic-element))
;; 	 (offset (- (1+ column) anchor))
;; 	 (steps (floor offset c-basic-offset)))
;;     (* (max steps 1)
;;        c-basic-offset)))

;;; ** cperl-mode

(defalias 'perl-mode 'cperl-mode)


;;; ** evernote-mode

;;; ** daemon settings

(when window-system
  ;; (setq server-host "styx-mbp")
  ;; (setq server-use-tcp t)
  (unless
      (and (boundp 'server-process)
	   (memq (process-status server-process) '(connect listen open run)))
    (server-start)))

(add-hook 'server-switch-hook
        (lambda ()
          (when (current-local-map)
            (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x j") 'server-edit))))

;; test existence and state of server
;; (and (boundp 'server-process)
;;      (memq (process-status server-process) '(connect listen open run)))

;;; ** Literature
(defun dired-literature-create-directory-from-pdf ()
  (interactive)
  (save-window-excursion
    (dired-do-async-shell-command
     "~/Documents/research_library/scripts/litcreatedir.sh" current-prefix-arg
     (dired-get-marked-files t current-prefix-arg))))
(require 'dired)
(define-key dired-mode-map (kbd "C-c l") 'dired-literature-create-directory-from-pdf)

(defun literature-update ()
  (interactive)
  (shell-command "~/Documents/research_library/scripts/litupdate.sh"))
(global-set-key (kbd "C-c u") 'literature-update)

;;; * Themes
;; (if window-system
;;     (load-theme 'monokai t)
;;   (load-theme 'cyberpunk t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/yamonokai-theme")
;; (load-theme 'zenburn t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/molokai-theme")
;; (setq molokai-theme-kit t)
;; (load-theme 'molokai t)
;; (setq linum-format "%4d ")
;; (toggle-indicate-empty-lines nil)
;; (setq-default mode-line-format
;; 	      (list
;; 	       ;; the buffer name; the file name as a tool tip
;; 	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face 'help-echo (buffer-file-name)))

;; 	       ;; line and column
;; 	       "(" (propertize "%4l" 'face 'font-lock-type-face) "," (propertize "%4c" 'face 'font-lock-type-face) ") "
;; 	       '(:eval (propertize "%4l" 'face 'font-lock-type-face) (-3 "%4l"))

;; 	       ;; relative position, size of file
;; 	       "[" (propertize "%p" 'face 'font-lock-constant-face) "/" (propertize "%I" 'face 'font-lock-constant-face) "] "

;; 	       ;; the current major mode for the buffer.
;; 	       "[" '(:eval (propertize "%m" 'face 'font-lock-string-face 'help-echo buffer-file-coding-system)) "] "


;; 	       "[" ;; insert vs overwrite mode, input-method in a tooltip
;; 	       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;; 				   'face 'font-lock-preprocessor-face
;; 				   'help-echo (concat "Buffer is in "
;; 						      (if overwrite-mode "overwrite" "insert") " mode")))

;; 	       ;; was this buffer modified since the last save?
;; 	       '(:eval (when (buffer-modified-p)
;; 			 (concat ","  (propertize "Mod"
;; 						  'face 'font-lock-warning-face
;; 						  'help-echo "Buffer has been modified"))))

;; 	       ;; is this buffer read-only?
;; 	       '(:eval (when buffer-read-only
;; 			 (concat ","  (propertize "RO"
;; 						  'face 'font-lock-type-face
;; 						  'help-echo "Buffer is read-only"))))
;; 	       "] "

;; 	       ;; add the time, with the date and the emacs uptime in the tooltip
;; 	       '(:eval (propertize (format-time-string "%H:%M")
;; 				   'help-echo
;; 				   (concat (format-time-string "%c; ")
;; 					   (emacs-uptime "Uptime:%hh"))))
;; 	       " --"
;; 	       ;; i don't want to see minor-modes; but if you want, uncomment this:
;; 	       ;; minor-mode-alist  ;; list of minor modes
;; 	       "%-" ;; fill with '-'
;; 	       ))
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
;; (when window-system
;;   (load-theme 'yamonokai t))
(load-theme 'monokai t)
;; (Load-theme 'solarized-dark t)
