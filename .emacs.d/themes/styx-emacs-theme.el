(deftheme styx-emacs
  "Dark Emacs is designed to give the same look and feel as
  `emacs --no-window-system'. It is based on a theme made by the
  emacswiki user, ZWZ. The original can be found at
  http://www.emacswiki.org/emacs/zwz.

  It also tries to not interfere with the faces when emacs is
  running in a tty (since that is the goal in the first place).
  There are some `org-mode' related faces defined for TODO
  keywords and special tags as well.

  Color theme by Suvayu Ali. Created 2012-04-28.")

(custom-theme-set-variables
 'styx-emacs
 '(org-todo-keyword-faces
   '(("DBUG" . (:background "gold" :foreground "indianred3" :weight bold))
     ("LEAK" . (:background "gold" :foreground "indianred3" :weight bold))
     ("SEGF" . (:background "gold" :foreground "indianred3" :weight bold))
     ("CNCL" . (:background "snow3" :foreground "black" :weight bold))
     ))
 '(org-tag-faces
   '(("PROJ" . (:background "indianred3" :foreground "cornsilk2" :weight bold))
     ))
 )


;; Customise using DISPLAY types if you wish to use both dark and
;; light backgrounds alternatively.  This also takes values like tty
;; and nt to describe text terminals and windows machines.  For more
;; details see:
;; - (info "(elisp)Defining Faces")
;; - (describe-function "custom-theme-set-faces")

(custom-theme-set-faces
 'styx-emacs
 '(bold ((t (:weight bold :inherit (default)))))
 '(bold-italic ((t (:weight bold :inherit (italic)))))
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:weight bold :foreground "salmon"))))
 '(cursor ((((type graphic)) (:background "red")))
	  nil "All other attributes are ignored; in -nw mode depends on tty")
 '(default ((((type graphic)) (:foreground "ivory2" :background "black"))))
 '(font-lock-builtin-face ((((min-colors 88)) (:foreground "gold" :inherit (default)))))
 '(font-lock-comment-face ((((type graphic)) (:foreground "cyan3" :inherit (italic)))
			   (((type tty) (class color) (min-colors 88)) (:foreground "#9e9e9e"))))
 '(font-lock-constant-face ((((type graphic)) (:foreground "LightGoldenrod2" :inherit (default)))))
 '(font-lock-function-name-face ((((type graphic)) (:weight bold :foreground "white" :inherit (default)))
				 (((type tty)) (:foreground "brightblue" :weight bold))))
 '(font-lock-keyword-face ((((type graphic)) (:foreground "firebrick1" :inherit (italic)))
			   (((type tty) (class color) (min-colors 88)) (:foreground "#ff0000"))))
 '(font-lock-preprocessor-face ((((type graphic)) (:foreground "IndianRed3" :inherit (font-lock-builtin-face)))))
 '(font-lock-string-face ((((type graphic)) (:foreground "lawn green" :inherit (default)))
			  (((type tty)) (:foreground "green"))))
 '(font-lock-type-face ((((type graphic)) (:foreground "orchid" :inherit (italic)))))
 ;; (((type tty) (class color) (min-colors 256)) (:foreground "orchid"))
 '(font-lock-variable-name-face ((((type graphic)) (:foreground "orange" :inherit (bold)))
				 (((type tty)) (:inherit default :foreground "DarkOrange1"))))
 '(font-lock-warning-face ((((type graphic)) (:weight bold :foreground "Red1" :inherit (bold)))
			   (((type tty)) (:weight bold :inherit error))))
 '(highlight ((((type graphic)) (:foreground "yellow" :background "blue" :inherit (default)))))
 '(info-menu-header ((t (:bold t :family "Sans Serif" :foreground "tomato" :weight bold))))
 '(info-node ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))))
 '(info-xref ((t (:inherit link :foreground "powder blue" :weight bold))))
 '(info-xref-visited ((t (:foreground "violet" :underline t :weight bold))))
 '(italic ((((supports :slant italic)) (:slant italic :foreground "blanched almond" :inherit (default)))))
 '(link ((t (:foreground "cyan" :underline t :weight extra-bold))))
 '(link-visited ((t (:foreground "violet" :inherit link))))
 '(match ((((type graphic)) (:foreground "RoyalBlue3"))
	  (((type tty)) (:background "brightyellow" :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 '(mode-line-buffer-id ((((type graphic)) (:weight bold :foreground "red" :background "ghost white"))))
 '(mode-line ((((type graphic)) (:box (:line-width -1 :color nil :style released-button) :foreground "blue" :background "azure"))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :background "snow" :foreground "DodgerBlue4" :weight bold))))
 '(org-document-info ((default (:foreground "pale turquoise"))
		      (((type tty) (class color) (min-colors 256))
		       (:foreground "#0000ff"))))
 '(org-document-title ((default (:foreground "pale turquoise" :weight bold))
		       (((type tty) (class color) (min-colors 256))
			(:weight bold :foreground "#0000ff"))))
 '(org-done ((t (:background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
 '(org-inlinetask ((t (:inherit org-level-8 :slant oblique))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "sandy brown" :weight bold))))
 '(org-table ((default (:foreground "LightSkyBlue"))
	      (((type tty) (class color) (min-colors 256)) (:foreground "#0000ff"))))
 '(org-todo ((t (:background "royalblue4" :foreground "thistle" :weight bold))))
 '(region ((((type tty) (class color) (min-colors 256)) (:background "#0000af"))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))))
 '(secondary-selection ((((type graphic)) (:background "blue")))
		       nil "Depends on tty in -nw mode")
 '(show-paren-mismatch ((((type graphic)) (:foreground "white" :background "magenta"))))
 '(show-paren-match ((((type graphic)) (:background "SlateBlue1"))))
 '(underline ((((supports :underline t)) (:underline t :foreground "green" :inherit (default)))))
 '(woman-bold ((t (:weight bold :foreground "forest green"))))
 '(woman-italic ((t (:slant italic :foreground "salmon"))))
 '(font-lock-global-todo-face ((t (:background "royalblue4" :foreground "thistle" :weight bold)))
			      t "Face for the TODO keyword globally.")
 )

(provide-theme 'styx-emacs)
