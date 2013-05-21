;;; zencoding-mode.el --- Unfold CSS-selector-like expressions to markup

;; Copyright (C) 2009, Chris Done

;; Version: 0.5.1
;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/rooney/zencoding
;; Last-Updated: 2011-12-31 Sat
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Unfold CSS-selector-like expressions to markup. Intended to be used
;; with sgml-like languages; xml, html, xhtml, xsl, etc.
;;
;; See `zencoding-mode' for more information.
;;
;; Copy zencoding-mode.el to your load-path and add to your .emacs:
;;
;;    (require 'zencoding-mode)
;;
;; Example setup:
;;
;;    (add-to-list 'load-path "~/Emacs/zencoding/")
;;    (require 'zencoding-mode)
;;    (add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
;;    (add-hook 'html-mode-hook 'zencoding-mode)
;;    (add-hook 'css-mode-hook  'zencoding-mode)
;;
;; Enable the minor mode with M-x zencoding-mode.
;;
;; See ``Test cases'' section for a complete set of expression types.
;;
;; If you are hacking on this project, eval (zencoding-test-cases) to
;; ensure that your changes have not broken anything. Feel free to add
;; new test cases if you add new features.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; Modified by Lennart Borgman.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst zencoding-mode:version "0.5.1")

;; Include the trie data structure for caching
;(require 'zencoding-trie)

(require 'cl)

(defmacro zencoding-defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

(defun zencoding-join-string (lis joiner)
  (mapconcat 'identity lis joiner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic parsing macros and utilities

(defmacro zencoding-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-pif (test-form then-form &rest else-forms)
  "Parser anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if (not (eq 'error (car it))) ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-parse (regex nums label &rest body)
  "Parse according to a regex and update the `input' variable."
  `(zencoding-aif (zencoding-regex ,regex input ',(number-sequence 0 nums))
                  (let ((input (elt it ,nums)))
                    ,@body)
                  `,`(error ,(concat "expected " ,label))))

(defmacro zencoding-run (parser then-form &rest else-forms)
  "Run a parser and update the input properly, extract the parsed
   expression."
  `(zencoding-pif (,parser input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  ,@(or else-forms '(it))))

(defmacro zencoding-por (parser1 parser2 then-form &rest else-forms)
  "OR two parsers. Try one parser, if it fails try the next."
  `(zencoding-pif (,parser1 input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  (zencoding-pif (,parser2 input)
                                 (let ((input (cdr it))
                                       (expr (car it)))
                                   ,then-form)
                                 ,@else-forms)))

(defun zencoding-regex (regexp string refs)
  "Return a list of (`ref') matches for a `regex' on a `string' or nil."
  (if (string-match (concat "^" regexp "\\([^\n]*\\)$") string)
      (mapcar (lambda (ref) (match-string ref string))
              (if (sequencep refs) refs (list refs)))
    nil))
;; src/snippets.el
;; This file is generated from conf/snippets.json
;; Don't edit.
(zencoding-defparameter zencoding-snippets
(let ((tbl (make-hash-table :test 'equal)))
(puthash "xml" (let ((tbl (make-hash-table :test 'equal)))
(puthash "profile" "xml" tbl)
(puthash "extends" "html" tbl)
(puthash "filters" "html" tbl)
tbl) tbl)
(puthash "scss" (let ((tbl (make-hash-table :test 'equal)))
(puthash "extends" "css" tbl)
tbl) tbl)
(puthash "sass" (let ((tbl (make-hash-table :test 'equal)))
(puthash "extends" "css" tbl)
tbl) tbl)
(puthash "less" (let ((tbl (make-hash-table :test 'equal)))
(puthash "extends" "css" tbl)
tbl) tbl)
(puthash "variables" (let ((tbl (make-hash-table :test 'equal)))
(puthash "lang" "en" tbl)
(puthash "locale" "en-US" tbl)
(puthash "charset" "UTF-8" tbl)
(puthash "indentation" "\t" tbl)
(puthash "newline" "\n" tbl)
tbl) tbl)
(puthash "stylus" (let ((tbl (make-hash-table :test 'equal)))
(puthash "extends" "css" tbl)
tbl) tbl)
(puthash "html" (let ((tbl (make-hash-table :test 'equal)))
(puthash "profile" "html" tbl)
(puthash "abbreviations" (let ((tbl (make-hash-table :test 'equal)))
(puthash "a:link" "<a href=\"http://|\">" tbl)
(puthash "doc4" "html>(head>meta[http-equiv=\"Content-Type\" content=\"text/html;charset=${charset}\"]+title{${1:Document}})" tbl)
(puthash "input:datetime-local" "inp[type=datetime-local]" tbl)
(puthash "input:reset" "input:button[type=reset]" tbl)
(puthash "meta:vp" "<meta name=\"viewport\" content=\"width=${1:device-width}, user-scalable=${2:no}, initial-scale=${3:1.0}, maximum-scale=${4:1.0}, minimum-scale=${5:1.0}\" />" tbl)
(puthash "colg" "colgroup" tbl)
(puthash "figc" "figcaption" tbl)
(puthash "btn:s" "button[type=submit]" tbl)
(puthash "btn:r" "button[type=reset]" tbl)
(puthash "style" "<style>" tbl)
(puthash "adr" "address" tbl)
(puthash "img" "<img src=\"\" alt=\"\" />" tbl)
(puthash "bdo:l" "<bdo dir=\"ltr\">" tbl)
(puthash "param" "<param name=\"\" value=\"\" />" tbl)
(puthash "colgroup+" "colgroup>col" tbl)
(puthash "btn:b" "button[type=button]" tbl)
(puthash "form:post" "<form action=\"\" method=\"post\">" tbl)
(puthash "bdo:r" "<bdo dir=\"rtl\">" tbl)
(puthash "fig" "figure" tbl)
(puthash "input:radio" "inp[type=radio]" tbl)
(puthash "link:print" "<link rel=\"stylesheet\" href=\"${1:print}.css\" media=\"print\" />" tbl)
(puthash "opt" "option" tbl)
(puthash "input:i" "input:image" tbl)
(puthash "input:h" "input:hidden" tbl)
(puthash "input:f" "input:file" tbl)
(puthash "input:c" "input:checkbox" tbl)
(puthash "input:b" "input:button" tbl)
(puthash "abbr" "<abbr title=\"\">" tbl)
(puthash "colg+" "colgroup>col" tbl)
(puthash "input:t" "inp" tbl)
(puthash "input:p" "input:password" tbl)
(puthash "input:s" "input:submit" tbl)
(puthash "input:r" "input:radio" tbl)
(puthash "ifr" "iframe" tbl)
(puthash "emb" "embed" tbl)
(puthash "optg+" "optgroup>option" tbl)
(puthash "isindex" "<isindex/>" tbl)
(puthash "html:5" "!!!+doc[lang=${lang}]" tbl)
(puthash "link:atom" "<link rel=\"alternate\" type=\"application/atom+xml\" title=\"Atom\" href=\"${1:atom.xml}\" />" tbl)
(puthash "table+" "table>tr>td" tbl)
(puthash "cmd" "command" tbl)
(puthash "art" "article" tbl)
(puthash "frame" "<frame/>" tbl)
(puthash "area:r" "<area shape=\"rect\" coords=\"\" href=\"\" alt=\"\" />" tbl)
(puthash "area:p" "<area shape=\"poly\" coords=\"\" href=\"\" alt=\"\" />" tbl)
(puthash "input:date" "inp[type=date]" tbl)
(puthash "meta" "<meta/>" tbl)
(puthash "video" "<video src=\"\">" tbl)
(puthash "input:button" "<input type=\"button\" value=\"\" />" tbl)
(puthash "area:d" "<area shape=\"default\" href=\"\" alt=\"\" />" tbl)
(puthash "area:c" "<area shape=\"circle\" coords=\"\" href=\"\" alt=\"\" />" tbl)
(puthash "out" "output" tbl)
(puthash "ftr" "footer" tbl)
(puthash "dlg" "dialog" tbl)
(puthash "script:src" "<script src=\"\">" tbl)
(puthash "form:get" "<form action=\"\" method=\"get\">" tbl)
(puthash "meta:utf" "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\" />" tbl)
(puthash "label" "<label for=\"\">" tbl)
(puthash "basefont" "<basefont/>" tbl)
(puthash "input:time" "inp[type=time]" tbl)
(puthash "link:favicon" "<link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"${1:favicon.ico}\" />" tbl)
(puthash "menu:toolbar" "menu[type=toolbar]>" tbl)
(puthash "prog" "progress" tbl)
(puthash "input:email" "inp[type=email]" tbl)
(puthash "str" "strong" tbl)
(puthash "leg" "legend" tbl)
(puthash "acronym" "<acronym title=\"\">" tbl)
(puthash "ol+" "ol>li" tbl)
(puthash "tr+" "tr>td" tbl)
(puthash "optgroup+" "optgroup>option" tbl)
(puthash "base" "<base href=\"\" />" tbl)
(puthash "bq" "blockquote" tbl)
(puthash "br" "<br/>" tbl)
(puthash "src" "source" tbl)
(puthash "obj" "object" tbl)
(puthash "dl+" "dl>dt+dd" tbl)
(puthash "script" "<script>" tbl)
(puthash "acr" "acronym" tbl)
(puthash "input:password" "inp[type=password]" tbl)
(puthash "col" "<col/>" tbl)
(puthash "html:4t" "!!!4t+doc4[lang=${lang}]" tbl)
(puthash "input:file" "inp[type=file]" tbl)
(puthash "html:4s" "!!!4s+doc4[lang=${lang}]" tbl)
(puthash "tarea" "textarea" tbl)
(puthash "select" "<select name=\"\" id=\"\">" tbl)
(puthash "input:number" "inp[type=number]" tbl)
(puthash "input:range" "inp[type=range]" tbl)
(puthash "area" "<area shape=\"\" coords=\"\" href=\"\" alt=\"\" />" tbl)
(puthash "input:image" "<input type=\"image\" src=\"\" alt=\"\" />" tbl)
(puthash "ul+" "ul>li" tbl)
(puthash "input:search" "inp[type=search]" tbl)
(puthash "html:xxs" "!!!xxs+doc4[xmlns=http://www.w3.org/1999/xhtml xml:lang=${lang}]" tbl)
(puthash "input:month" "inp[type=month]" tbl)
(puthash "fset" "fieldset" tbl)
(puthash "meta:win" "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=windows-1251\" />" tbl)
(puthash "option" "<option value=\"\">" tbl)
(puthash "form" "<form action=\"\">" tbl)
(puthash "hr" "<hr/>" tbl)
(puthash "menu:c" "menu:context" tbl)
(puthash "link" "<link rel=\"stylesheet\" href=\"\" />" tbl)
(puthash "input" "<input type=\"${1:text}\" />" tbl)
(puthash "link:rss" "<link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS\" href=\"${1:rss.xml}\" />" tbl)
(puthash "select+" "select>option" tbl)
(puthash "hdr" "header" tbl)
(puthash "cap" "caption" tbl)
(puthash "det" "details" tbl)
(puthash "keygen" "<keygen/>" tbl)
(puthash "link:touch" "<link rel=\"apple-touch-icon\" href=\"${1:favicon.png}\" />" tbl)
(puthash "iframe" "<iframe src=\"\" frameborder=\"0\">" tbl)
(puthash "link:css" "<link rel=\"stylesheet\" href=\"${1:style}.css\" />" tbl)
(puthash "input:week" "inp[type=week]" tbl)
(puthash "embed" "<embed src=\"\" type=\"\" />" tbl)
(puthash "optg" "optgroup" tbl)
(puthash "input:datetime" "inp[type=datetime]" tbl)
(puthash "inp" "<input type=\"${1:text}\" name=\"\" id=\"\" />" tbl)
(puthash "datag" "datagrid" tbl)
(puthash "menu:t" "menu:toolbar" tbl)
(puthash "!" "html:5" tbl)
(puthash "html:xml" "<html xmlns=\"http://www.w3.org/1999/xhtml\">" tbl)
(puthash "btn" "button" tbl)
(puthash "input:url" "inp[type=url]" tbl)
(puthash "menu:context" "menu[type=context]>" tbl)
(puthash "fst" "fieldset" tbl)
(puthash "map" "<map name=\"\">" tbl)
(puthash "input:color" "inp[type=color]" tbl)
(puthash "meta:compat" "<meta http-equiv=\"X-UA-Compatible\" content=\"${1:IE=7}\" />" tbl)
(puthash "input:hidden" "input[type=hidden name]" tbl)
(puthash "object" "<object data=\"\" type=\"\">" tbl)
(puthash "a:mail" "<a href=\"mailto:|\">" tbl)
(puthash "html:xs" "!!!xs+doc4[xmlns=http://www.w3.org/1999/xhtml xml:lang=${lang}]" tbl)
(puthash "html:xt" "!!!xt+doc4[xmlns=http://www.w3.org/1999/xhtml xml:lang=${lang}]" tbl)
(puthash "a" "<a href=\"\">" tbl)
(puthash "datal" "datalist" tbl)
(puthash "map+" "map>area" tbl)
(puthash "kg" "keygen" tbl)
(puthash "textarea" "<textarea name=\"\" id=\"\" cols=\"${1:30}\" rows=\"${2:10}\">" tbl)
(puthash "doc" "html>(head>meta[charset=UTF-8]+title{${1:Document}})+body" tbl)
(puthash "input:submit" "<input type=\"submit\" value=\"\" />" tbl)
(puthash "input:text" "inp" tbl)
(puthash "input:checkbox" "inp[type=checkbox]" tbl)
(puthash "command" "<command/>" tbl)
(puthash "sect" "section" tbl)
(puthash "audio" "<audio src=\"\">" tbl)
(puthash "bdo" "<bdo dir=\"\">" tbl)
tbl) tbl)
(puthash "snippets" (let ((tbl (make-hash-table :test 'equal)))
(puthash "c" "<!-- |${child} -->" tbl)
(puthash "!!!" "<!doctype html>" tbl)
(puthash "!!!xxs" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" tbl)
(puthash "cc:ie6" "<!--[if lte IE 6]>\n\t${child}|\n<![endif]-->" tbl)
(puthash "cc:ie" "<!--[if IE]>\n\t${child}|\n<![endif]-->" tbl)
(puthash "!!!xs" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" tbl)
(puthash "!!!4t" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" tbl)
(puthash "cc:noie" "<!--[if !IE]><!-->\n\t${child}|\n<!--<![endif]-->" tbl)
(puthash "!!!4s" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" tbl)
(puthash "!!!xt" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" tbl)
tbl) tbl)
(puthash "filters" "html" tbl)
tbl) tbl)
(puthash "css" (let ((tbl (make-hash-table :test 'equal)))
(puthash "snippets" (let ((tbl (make-hash-table :test 'equal)))
(puthash "bdls" "border-left-style:|;" tbl)
(puthash "bdlw" "border-left-width:|;" tbl)
(puthash "bdli" "border-left-image:url(|);" tbl)
(puthash "bdlc" "border-left-color:#${1:000};" tbl)
(puthash "whsc" "white-space-collapse:|;" tbl)
(puthash "bdtlrs" "border-top-left-radius:|;" tbl)
(puthash "bdblrs" "border-bottom-left-radius:|;" tbl)
(puthash "d:tbc" "display:table-cell;" tbl)
(puthash "wow:n" "word-wrap:none;" tbl)
(puthash "tw" "text-wrap:|;" tbl)
(puthash "tt" "text-transform:${1:uppercase};" tbl)
(puthash "tr" "text-replace:|;" tbl)
(puthash "to" "text-outline:|;" tbl)
(puthash "wow:u" "word-wrap:unrestricted;" tbl)
(puthash "tj" "text-justify:|;" tbl)
(puthash "wow:s" "word-wrap:suppress;" tbl)
(puthash "th" "text-height:|;" tbl)
(puthash "d:tbr" "display:table-row;" tbl)
(puthash "td" "text-decoration:${1:none};" tbl)
(puthash "te" "text-emphasis:|;" tbl)
(puthash "bgc:t" "background-color:transparent;" tbl)
(puthash "list:lr" "list-style-type:lower-roman;" tbl)
(puthash "bdl+" "border-left:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "bxz" "box-sizing:${1:border-box};" tbl)
(puthash "bdbk:c" "border-break:close;" tbl)
(puthash "bgbk:bb" "background-break:bounding-box;" tbl)
(puthash "f" "font:|;" tbl)
(puthash "list" "list-style-type:|;" tbl)
(puthash "trf" "transform:|;" tbl)
(puthash "p" "padding:|;" tbl)
(puthash "bdbi:n" "border-bottom-image:none;" tbl)
(puthash "bdf:r" "border-fit:repeat;" tbl)
(puthash "trs" "transition:${1:prop} ${2:time};" tbl)
(puthash "bdrst" "border-right-style:|;" tbl)
(puthash "zm" "zoom:1;" tbl)
(puthash "to:n" "text-outline:none;" tbl)
(puthash "trf:t" "transform: translate(${1:x}, ${2:y});" tbl)
(puthash "animfm:bt" "animation-fill-mode:both;" tbl)
(puthash "animfm:bh" "animation-fill-mode:both;" tbl)
(puthash "bgcp:cb" "background-clip:content-box;" tbl)
(puthash "lisp:o" "list-style-position:outside;" tbl)
(puthash "lisp:i" "list-style-position:inside;" tbl)
(puthash "d:tbclg" "display:table-column-group;" tbl)
(puthash "bdf" "border-fit:${1:repeat};" tbl)
(puthash "@f" "@font-face {\n\tfont-family:|;\n\tsrc:url(|);\n}" tbl)
(puthash "bdc" "border-color:#${1:000};" tbl)
(puthash "d:rbt" "display:ruby-text;" tbl)
(puthash "bdl" "border-left:|;" tbl)
(puthash "@i" "@import url(|);" tbl)
(puthash "bdi" "border-image:url(|);" tbl)
(puthash "bgsz" "background-size:|;" tbl)
(puthash "@m" "@media ${1:screen} {\n\t|\n}" tbl)
(puthash "fef:eb" "font-effect:emboss;" tbl)
(puthash "bdt" "border-top:|;" tbl)
(puthash "bdw" "border-width:|;" tbl)
(puthash "pgbb" "page-break-before:|;" tbl)
(puthash "fef:eg" "font-effect:engrave;" tbl)
(puthash "bds" "border-style:|;" tbl)
(puthash "bdr" "border-right:|;" tbl)
(puthash "pgbi" "page-break-inside:|;" tbl)
(puthash "bgi" "background-image:url(|);" tbl)
(puthash "mr" "margin-right:|;" tbl)
(puthash "ta-lst" "text-align-last:|;" tbl)
(puthash "te:c" "text-emphasis:circle;" tbl)
(puthash "te:b" "text-emphasis:before;" tbl)
(puthash "q:n" "quotes:none;" tbl)
(puthash "te:n" "text-emphasis:none;" tbl)
(puthash "ta:l" "text-align:left;" tbl)
(puthash "bdbs:n" "border-bottom-style:none;" tbl)
(puthash "bt" "border-top:|;" tbl)
(puthash "bg:ie" "filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='${1:x}.png',sizingMethod='${2:crop}');" tbl)
(puthash "d:li" "display:list-item;" tbl)
(puthash "tj:k" "text-justify:kashida;" tbl)
(puthash "bd+" "border:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "fems:ac" "font-emphasize-style:accent;" tbl)
(puthash "fst:n" "font-stretch:normal;" tbl)
(puthash "fst:c" "font-stretch:condensed;" tbl)
(puthash "fst:e" "font-stretch:expanded;" tbl)
(puthash "wid" "widows:|;" tbl)
(puthash "bdts:n" "border-top-style:none;" tbl)
(puthash "fef" "font-effect:|;" tbl)
(puthash "cur:t" "cursor:text;" tbl)
(puthash "bdsp" "border-spacing:|;" tbl)
(puthash "cur:p" "cursor:pointer;" tbl)
(puthash "animps:r" "animation-play-state:running;" tbl)
(puthash "animps:p" "animation-play-state:paused;" tbl)
(puthash "maw:n" "max-width:none;" tbl)
(puthash "fw:n" "font-weight:normal;" tbl)
(puthash "bdtri:n" "border-top-right-image:none;" tbl)
(puthash "fw:b" "font-weight:bold;" tbl)
(puthash "bdtri:c" "border-top-right-image:continue;" tbl)
(puthash "cur:a" "cursor:auto;" tbl)
(puthash "cur:c" "cursor:crosshair;" tbl)
(puthash "bdls:n" "border-left-style:none;" tbl)
(puthash "tw:u" "text-wrap:unrestricted;" tbl)
(puthash "lts" "letter-spacing:|;" tbl)
(puthash "c:ra" "color:rgba(${1:0}, ${2:0}, ${3:0}, .${4:5});" tbl)
(puthash "va:sub" "vertical-align:sub;" tbl)
(puthash "tw:s" "text-wrap:suppress;" tbl)
(puthash "ti" "text-indent:|;" tbl)
(puthash "pgbi:av" "page-break-inside:avoid;" tbl)
(puthash "tj:t" "text-justify:tibetan;" tbl)
(puthash "bgc" "background-color:#${1:fff};" tbl)
(puthash "trf:tx" "transform: translateX(${1:x});" tbl)
(puthash "trf:ty" "transform: translateY(${1:y});" tbl)
(puthash "va:sup" "vertical-align:super;" tbl)
(puthash "va:bl" "vertical-align:baseline;" tbl)
(puthash "tw:n" "text-wrap:normal;" tbl)
(puthash "mb" "margin-bottom:|;" tbl)
(puthash "ml" "margin-left:|;" tbl)
(puthash "bdtrrs" "border-top-right-radius:|;" tbl)
(puthash "va:m" "vertical-align:middle;" tbl)
(puthash "mt" "margin-top:|;" tbl)
(puthash "bds:dt" "border-style:dotted;" tbl)
(puthash "v" "visibility:${1:hidden};" tbl)
(puthash "@import" "@import url(|);" tbl)
(puthash "ta" "text-align:${1:left};" tbl)
(puthash "pos:r" "position:relative;" tbl)
(puthash "va:b" "vertical-align:bottom;" tbl)
(puthash "tsh+" "text-shadow:${1:0} ${2:0} ${3:0} ${4:#000};" tbl)
(puthash "cps:t" "caption-side:top;" tbl)
(puthash "cps:b" "caption-side:bottom;" tbl)
(puthash "list:dclz" "list-style-type:decimal-leading-zero;" tbl)
(puthash "f+" "font:${1:1em} ${2:Arial,sans-serif};" tbl)
(puthash "animic:i" "animation-iteration-count:infinite;" tbl)
(puthash "bd:n" "border:none;" tbl)
(puthash "!" "!important" tbl)
(puthash "bdl:n" "border-left:none;" tbl)
(puthash "bgsz:a" "background-size:auto;" tbl)
(puthash "bdlen:a" "border-length:auto;" tbl)
(puthash "tw:no" "text-wrap:none;" tbl)
(puthash "bdtri" "border-top-right-image:url(|);" tbl)
(puthash "ovs" "overflow-style:${1:scrollbar};" tbl)
(puthash "mar" "max-resolution:${1:res};" tbl)
(puthash "w" "width:|;" tbl)
(puthash "maw" "max-width:|;" tbl)
(puthash "fw" "font-weight:|;" tbl)
(puthash "ovx" "overflow-x:${1:hidden};" tbl)
(puthash "ovy" "overflow-y:${1:hidden};" tbl)
(puthash "ff" "font-family:|;" tbl)
(puthash "ov" "overflow:${1:hidden};" tbl)
(puthash "wfsm:sa" "-webkit-font-smoothing:subpixel-antialiased;" tbl)
(puthash "mah" "max-height:|;" tbl)
(puthash "cnt:oq" "content:open-quote;" tbl)
(puthash "fl" "float:${1:left};" tbl)
(puthash "trsp" "transition-property:${1:prop};" tbl)
(puthash "va:tb" "vertical-align:text-bottom;" tbl)
(puthash "h:a" "height:auto;" tbl)
(puthash "d:ib" "display:inline-block;" tbl)
(puthash "bgo:cb" "background-origin:content-box;" tbl)
(puthash "q" "quotes:|;" tbl)
(puthash "tal:l" "text-align-last:left;" tbl)
(puthash "va:tt" "vertical-align:text-top;" tbl)
(puthash "fef:o" "font-effect:outline;" tbl)
(puthash "ct:ncq" "content:no-close-quote;" tbl)
(puthash "wow:nm" "word-wrap:normal;" tbl)
(puthash "ct:noq" "content:no-open-quote;" tbl)
(puthash "tov:c" "text-overflow:clip;" tbl)
(puthash "colm" "columns:|;" tbl)
(puthash "cur:d" "cursor:default;" tbl)
(puthash "bdli:n" "border-left-image:none;" tbl)
(puthash "d:tbhg" "display:table-header-group;" tbl)
(puthash "bga:s" "background-attachment:scroll;" tbl)
(puthash "tsh" "text-shadow:${1:hoff} ${2:voff} ${3:blur} ${4:#000};" tbl)
(puthash "td:n" "text-decoration:none;" tbl)
(puthash "lh" "line-height:|;" tbl)
(puthash "fst:sc" "font-stretch:semi-condensed;" tbl)
(puthash "fst:ec" "font-stretch:extra-condensed;" tbl)
(puthash "fsm:a" "font-smooth:auto;" tbl)
(puthash "bdlc:t" "border-left-color:transparent;" tbl)
(puthash "bga:f" "background-attachment:fixed;" tbl)
(puthash "fsm:n" "font-smooth:never;" tbl)
(puthash "anim" "animation:|;" tbl)
(puthash "tbl:f" "table-layout:fixed;" tbl)
(puthash "tbl:a" "table-layout:auto;" tbl)
(puthash "bdci:n" "border-corner-image:none;" tbl)
(puthash "bxsh:ra" "box-shadow:${1:inset }${2:h} ${3:v} ${4:blur} ${5:spread }rgba(${6:0}, ${7:0}, ${8:0}, .${9:5});" tbl)
(puthash "whsc:k" "white-space-collapse:keep-all;" tbl)
(puthash "bdci:c" "border-corner-image:continue;" tbl)
(puthash "va:t" "vertical-align:top;" tbl)
(puthash "colmc" "column-count:|;" tbl)
(puthash "colmf" "column-fill:|;" tbl)
(puthash "colmg" "column-gap:|;" tbl)
(puthash "bdb:n" "border-bottom:none;" tbl)
(puthash "l" "left:|;" tbl)
(puthash "lisp" "list-style-position:|;" tbl)
(puthash "colmr" "column-rule:|;" tbl)
(puthash "colms" "column-span:|;" tbl)
(puthash "colmw" "column-width:|;" tbl)
(puthash "bdbi" "border-bottom-image:url(|);" tbl)
(puthash "bdbk" "border-break:${1:close};" tbl)
(puthash "pgba:r" "page-break-after:right;" tbl)
(puthash "wfsm" "-webkit-font-smoothing:${antialiased};" tbl)
(puthash "bdbc" "border-bottom-color:#${1:000};" tbl)
(puthash "ec" "empty-cells:|;" tbl)
(puthash "te:ac" "text-emphasis:accent;" tbl)
(puthash "fs" "font-style:${italic};" tbl)
(puthash "l:a" "left:auto;" tbl)
(puthash "bdr:n" "border-right:none;" tbl)
(puthash "bdrst:n" "border-right-style:none;" tbl)
(puthash "bdbs" "border-bottom-style:|;" tbl)
(puthash "bdbw" "border-bottom-width:|;" tbl)
(puthash "whsc:ba" "white-space-collapse:break-all;" tbl)
(puthash "@kf" "@-webkit-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@-o-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@-moz-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}" tbl)
(puthash "fv" "font-variant:|;" tbl)
(puthash "lisi" "list-style-image:|;" tbl)
(puthash "whsc:bs" "white-space-collapse:break-strict;" tbl)
(puthash "b:a" "bottom:auto;" tbl)
(puthash "bdt+" "border-top:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "fz" "font-size:|;" tbl)
(puthash "ta:c" "text-align:center;" tbl)
(puthash "bdf:sp" "border-fit:space;" tbl)
(puthash "ovy:a" "overflow-y:auto;" tbl)
(puthash "bdb+" "border-bottom:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "d:tb" "display:table;" tbl)
(puthash "bdf:st" "border-fit:stretch;" tbl)
(puthash "ti:-" "text-indent:-9999px;" tbl)
(puthash "c:r" "color:rgb(${1:0}, ${2:0}, ${3:0});" tbl)
(puthash "ta:j" "text-align:justify;" tbl)
(puthash "ovy:h" "overflow-y:hidden;" tbl)
(puthash "cnt:cs" "content:counters(|);" tbl)
(puthash "orp" "orphans:|;" tbl)
(puthash "cnt:cq" "content:close-quote;" tbl)
(puthash "ovy:s" "overflow-y:scroll;" tbl)
(puthash "ta:r" "text-align:right;" tbl)
(puthash "bdf:sc" "border-fit:scale;" tbl)
(puthash "ovy:v" "overflow-y:visible;" tbl)
(puthash "ori" "orientation:|;" tbl)
(puthash "fem" "font-emphasize:|;" tbl)
(puthash "cnt:n" "content:normal;" tbl)
(puthash "bdts" "border-top-style:|;" tbl)
(puthash "bdtw" "border-top-width:|;" tbl)
(puthash "cnt:c" "content:counter(|);" tbl)
(puthash "cnt:a" "content:attr(|);" tbl)
(puthash "bdtc" "border-top-color:#${1:000};" tbl)
(puthash "cnt:noq" "content:no-open-quote;" tbl)
(puthash "td:u" "text-decoration:underline;" tbl)
(puthash "bdti" "border-top-image:url(|);" tbl)
(puthash "bdtli" "border-top-left-image:url(|);" tbl)
(puthash "bdf:of" "border-fit:overflow;" tbl)
(puthash "fl:l" "float:left;" tbl)
(puthash "tt:n" "text-transform:none;" tbl)
(puthash "fl:n" "float:none;" tbl)
(puthash "bdbrrs" "border-bottom-right-radius:|;" tbl)
(puthash "bdf:ow" "border-fit:overwrite;" tbl)
(puthash "tt:l" "text-transform:lowercase;" tbl)
(puthash "fl:r" "float:right;" tbl)
(puthash "tt:c" "text-transform:capitalize;" tbl)
(puthash "tov" "text-overflow:${ellipsis};" tbl)
(puthash "ec:s" "empty-cells:show;" tbl)
(puthash "bgr:sp" "background-repeat:space;" tbl)
(puthash "ec:h" "empty-cells:hide;" tbl)
(puthash "tt:u" "text-transform:uppercase;" tbl)
(puthash "animdir:ar" "animation-direction:alternate-reverse;" tbl)
(puthash "bdti:n" "border-top-image:none;" tbl)
(puthash "trstf" "transition-timing-function:${1:tfunc};" tbl)
(puthash "colmrc" "column-rule-color:|;" tbl)
(puthash "fst:ee" "font-stretch:extra-expanded;" tbl)
(puthash "bdbri" "border-bottom-right-image:url(|);" tbl)
(puthash "trf:r" "transform: rotate(${1:angle});" tbl)
(puthash "colmrs" "column-rule-style:|;" tbl)
(puthash "colmrw" "column-rule-width:|;" tbl)
(puthash "rsz" "resize:|;" tbl)
(puthash "d:cp" "display:compact;" tbl)
(puthash "b" "bottom:|;" tbl)
(puthash "fef:n" "font-effect:none;" tbl)
(puthash "bdt:n" "border-top:none;" tbl)
(puthash "to+" "text-outline:${1:0} ${2:0} ${3:#000};" tbl)
(puthash "r" "right:|;" tbl)
(puthash "bdbri:n" "border-bottom-right-image:none;" tbl)
(puthash "tsh:r" "text-shadow:${1:h} ${2:v} ${3:blur} rgb(${4:0}, ${5:0}, ${6:0});" tbl)
(puthash "animdel" "animation-delay:${1:time};" tbl)
(puthash "pgbi:au" "page-break-inside:auto;" tbl)
(puthash "bdbri:c" "border-bottom-right-image:continue;" tbl)
(puthash "animic" "animation-iteration-count:${1:1};" tbl)
(puthash "tsh:n" "text-shadow:none;" tbl)
(puthash "fv:sc" "font-variant:small-caps;" tbl)
(puthash "bds:i" "border-style:inset;" tbl)
(puthash "mih" "min-height:|;" tbl)
(puthash "r:a" "right:auto;" tbl)
(puthash "bdrc:t" "border-right-color:transparent;" tbl)
(puthash "fems:ds" "font-emphasize-style:disc;" tbl)
(puthash "fems:dt" "font-emphasize-style:dot;" tbl)
(puthash "zoo" "zoom:1;" tbl)
(puthash "lis" "list-style:|;" tbl)
(puthash "mir" "min-resolution:${1:res};" tbl)
(puthash "miw" "min-width:|;" tbl)
(puthash "whs" "white-space:|;" tbl)
(puthash "bdtc:t" "border-top-color:transparent;" tbl)
(puthash "op:ms" "-ms-filter:'progid:DXImageTransform.Microsoft.Alpha(Opacity=100)';" tbl)
(puthash "bds:dtds" "border-style:dot-dash;" tbl)
(puthash "bdb" "border-bottom:|;" tbl)
(puthash "m" "margin:|;" tbl)
(puthash "bgcp:nc" "background-clip:no-clip;" tbl)
(puthash "wow" "word-wrap:|;" tbl)
(puthash "w:a" "width:auto;" tbl)
(puthash "bg:n" "background:none;" tbl)
(puthash "ml:a" "margin-left:auto;" tbl)
(puthash "bds:dtdtds" "border-style:dot-dot-dash;" tbl)
(puthash "animtf:cb" "animation-timing-function:cubic-bezier(${1:0.1}, ${2:0.7}, ${3:1.0}, ${3:0.1});" tbl)
(puthash "pgba" "page-break-after:|;" tbl)
(puthash "ff:s" "font-family:serif;" tbl)
(puthash "d:rbb" "display:ruby-base;" tbl)
(puthash "tj:d" "text-justify:distribute;" tbl)
(puthash "tj:a" "text-justify:auto;" tbl)
(puthash "trfo" "transform-origin:|;" tbl)
(puthash "ff:f" "font-family:fantasy;" tbl)
(puthash "trfs" "transform-style:${1:preserve-3d};" tbl)
(puthash "fems:c" "font-emphasize-style:circle;" tbl)
(puthash "ff:c" "font-family:cursive;" tbl)
(puthash "d:itb" "display:inline-table;" tbl)
(puthash "fems:n" "font-emphasize-style:none;" tbl)
(puthash "ff:m" "font-family:monospace;" tbl)
(puthash "pgba:au" "page-break-after:auto;" tbl)
(puthash "bdri:n" "border-right-image:none;" tbl)
(puthash "mt:a" "margin-top:auto;" tbl)
(puthash "tbl" "table-layout:|;" tbl)
(puthash "wob:l" "word-break:loose;" tbl)
(puthash "animfm:b" "animation-fill-mode:backwards;" tbl)
(puthash "animfm:f" "animation-fill-mode:forwards;" tbl)
(puthash "td:o" "text-decoration:overline;" tbl)
(puthash "td:l" "text-decoration:line-through;" tbl)
(puthash "bxz:bb" "box-sizing:border-box;" tbl)
(puthash "bxsh:n" "box-shadow:none;" tbl)
(puthash "h" "height:|;" tbl)
(puthash "pgba:al" "page-break-after:always;" tbl)
(puthash "animtf:e" "animation-timing-function:ease;" tbl)
(puthash "animtf:l" "animation-timing-function:linear;" tbl)
(puthash "bxsh:r" "box-shadow:${1:inset }${2:hoff} ${3:voff} ${4:blur} ${5:spread }rgb(${6:0}, ${7:0}, ${8:0});" tbl)
(puthash "fw:br" "font-weight:bolder;" tbl)
(puthash "ovs:p" "overflow-style:panner;" tbl)
(puthash "ovs:s" "overflow-style:scrollbar;" tbl)
(puthash "fv:n" "font-variant:normal;" tbl)
(puthash "ovs:a" "overflow-style:auto;" tbl)
(puthash "m:a" "margin:auto;" tbl)
(puthash "animdir" "animation-direction:${1:normal};" tbl)
(puthash "ovs:m" "overflow-style:move;" tbl)
(puthash "ol:n" "outline:none;" tbl)
(puthash "fsm:aw" "font-smooth:always;" tbl)
(puthash "whs:nw" "white-space:nowrap;" tbl)
(puthash "wos" "word-spacing:|;" tbl)
(puthash "wob" "word-break:|;" tbl)
(puthash "pgbb:au" "page-break-before:auto;" tbl)
(puthash "op:ie" "filter:progid:DXImageTransform.Microsoft.Alpha(Opacity=100);" tbl)
(puthash "rsz:h" "resize:horizontal;" tbl)
(puthash "bgcp" "background-clip:${1:padding-box};" tbl)
(puthash "fst:ue" "font-stretch:ultra-expanded;" tbl)
(puthash "fst:uc" "font-stretch:ultra-condensed;" tbl)
(puthash "pgbb:al" "page-break-before:always;" tbl)
(puthash "c" "color:#${1:000};" tbl)
(puthash "bdrc" "border-right-color:#${1:000};" tbl)
(puthash "bdtli:n" "border-top-left-image:none;" tbl)
(puthash "bdtli:c" "border-top-left-image:continue;" tbl)
(puthash "list:ur" "list-style-type:upper-roman;" tbl)
(puthash "fst:se" "font-stretch:semi-expanded;" tbl)
(puthash "coi" "counter-increment:|;" tbl)
(puthash "tr:n" "text-replace:none;" tbl)
(puthash "bgbk:c" "background-break:continuous;" tbl)
(puthash "ov:a" "overflow:auto;" tbl)
(puthash "te:a" "text-emphasis:after;" tbl)
(puthash "cm" "/* |${child} */" tbl)
(puthash "cl" "clear:${1:both};" tbl)
(puthash "femp" "font-emphasize-position:|;" tbl)
(puthash "ov:h" "overflow:hidden;" tbl)
(puthash "wob:bs" "word-break:break-strict;" tbl)
(puthash "ov:v" "overflow:visible;" tbl)
(puthash "ov:s" "overflow:scroll;" tbl)
(puthash "bxsh" "box-shadow:${1:inset }${2:hoff} ${3:voff} ${4:blur} ${5:color};" tbl)
(puthash "cp" "clip:|;" tbl)
(puthash "ct" "content:|;" tbl)
(puthash "pr" "padding-right:|;" tbl)
(puthash "cp:r" "clip:rect(${1:top} ${2:right} ${3:bottom} ${4:left});" tbl)
(puthash "pt" "padding-top:|;" tbl)
(puthash "ori:p" "orientation:portrait;" tbl)
(puthash "pb" "padding-bottom:|;" tbl)
(puthash "cp:a" "clip:auto;" tbl)
(puthash "bdf:c" "border-fit:clip;" tbl)
(puthash "ori:l" "orientation:landscape;" tbl)
(puthash "pl" "padding-left:|;" tbl)
(puthash "bdci" "border-corner-image:url(|);" tbl)
(puthash "bdcl" "border-collapse:|;" tbl)
(puthash "cor" "counter-reset:|;" tbl)
(puthash "bgsz:cv" "background-size:cover;" tbl)
(puthash "mb:a" "margin-bottom:auto;" tbl)
(puthash "trf:sky" "transform: skewY(${1:angle});" tbl)
(puthash "trf:skx" "transform: skewX(${1:angle});" tbl)
(puthash "d:rb" "display:ruby;" tbl)
(puthash "bdlen" "border-length:|;" tbl)
(puthash "d:ri" "display:run-in;" tbl)
(puthash "va" "vertical-align:${1:top};" tbl)
(puthash "bds:db" "border-style:double;" tbl)
(puthash "d:tbfg" "display:table-footer-group;" tbl)
(puthash "bds:ds" "border-style:dashed;" tbl)
(puthash "lis:n" "list-style:none;" tbl)
(puthash "d:b" "display:block;" tbl)
(puthash "tal:a" "text-align-last:auto;" tbl)
(puthash "tal:c" "text-align-last:center;" tbl)
(puthash "d:i" "display:inline;" tbl)
(puthash "pgba:l" "page-break-after:left;" tbl)
(puthash "bgbk:eb" "background-break:each-box;" tbl)
(puthash "d:n" "display:none;" tbl)
(puthash "tal:r" "text-align-last:right;" tbl)
(puthash "tj:iw" "text-justify:inter-word;" tbl)
(puthash "trf:sc" "transform: scale(${1:x}, ${2:y});" tbl)
(puthash "olc:i" "outline-color:invert;" tbl)
(puthash "bgr:x" "background-repeat:repeat-x;" tbl)
(puthash "bgr:y" "background-repeat:repeat-y;" tbl)
(puthash "bgr:n" "background-repeat:no-repeat;" tbl)
(puthash "cnt:ncq" "content:no-close-quote;" tbl)
(puthash "list:c" "list-style-type:circle;" tbl)
(puthash "list:d" "list-style-type:disc;" tbl)
(puthash "bdbli:c" "border-bottom-left-image:continue;" tbl)
(puthash "bdbli:n" "border-bottom-left-image:none;" tbl)
(puthash "list:n" "list-style-type:none;" tbl)
(puthash "list:s" "list-style-type:square;" tbl)
(puthash "mah:n" "max-height:none;" tbl)
(puthash "bgr:rd" "background-repeat:round;" tbl)
(puthash "th:t" "text-height:text-size;" tbl)
(puthash "d:rbtg" "display:ruby-text-group;" tbl)
(puthash "bdc:t" "border-color:transparent;" tbl)
(puthash "th:f" "text-height:font-size;" tbl)
(puthash "th:a" "text-height:auto;" tbl)
(puthash "ct:cq" "content:close-quote;" tbl)
(puthash "ct:cs" "content:counters(|);" tbl)
(puthash "th:m" "text-height:max-size;" tbl)
(puthash "animdir:a" "animation-direction:alternate;" tbl)
(puthash "bxz:cb" "box-sizing:content-box;" tbl)
(puthash "rsz:n" "resize:none;" tbl)
(puthash "cur" "cursor:${pointer};" tbl)
(puthash "whs:p" "white-space:pre;" tbl)
(puthash "rsz:b" "resize:both;" tbl)
(puthash "animdir:n" "animation-direction:normal;" tbl)
(puthash "fw:lr" "font-weight:lighter;" tbl)
(puthash "whsc:n" "white-space-collapse:normal;" tbl)
(puthash "animtf:eio" "animation-timing-function:ease-in-out;" tbl)
(puthash "animdir:r" "animation-direction:reverse;" tbl)
(puthash "te:ds" "text-emphasis:disc;" tbl)
(puthash "te:dt" "text-emphasis:dot;" tbl)
(puthash "whs:n" "white-space:normal;" tbl)
(puthash "whsc:l" "white-space-collapse:loose;" tbl)
(puthash "rsz:v" "resize:vertical;" tbl)
(puthash "wob:ba" "word-break:break-all;" tbl)
(puthash "list:dc" "list-style-type:decimal;" tbl)
(puthash "d:tbrg" "display:table-row-group;" tbl)
(puthash "tov:e" "text-overflow:ellipsis;" tbl)
(puthash "mr:a" "margin-right:auto;" tbl)
(puthash "pos" "position:${1:relative};" tbl)
(puthash "fems" "font-emphasize-style:|;" tbl)
(puthash "d" "display:${1:block};" tbl)
(puthash "@media" "@media ${1:screen} {\n\t|\n}" tbl)
(puthash "bgi:n" "background-image:none;" tbl)
(puthash "wfsm:s" "-webkit-font-smoothing:subpixel-antialiased;" tbl)
(puthash "q:en" "quotes:'\\201C' '\\201D' '\\2018' '\\2019';" tbl)
(puthash "wfsm:n" "-webkit-font-smoothing:none;" tbl)
(puthash "t" "top:|;" tbl)
(puthash "ovs:mq" "overflow-style:marquee;" tbl)
(puthash "wfsm:a" "-webkit-font-smoothing:antialiased;" tbl)
(puthash "animtf" "animation-timing-function:${1:linear};" tbl)
(puthash "bgbk" "background-break:|;" tbl)
(puthash "olo" "outline-offset:|;" tbl)
(puthash "ols" "outline-style:|;" tbl)
(puthash "olw" "outline-width:|;" tbl)
(puthash "cnt" "content:'|';" tbl)
(puthash "bg+" "background:${1:#fff} url(${2}) ${3:0} ${4:0} ${5:no-repeat};" tbl)
(puthash "bdcl:s" "border-collapse:separate;" tbl)
(puthash "bdcl:c" "border-collapse:collapse;" tbl)
(puthash "anim-" "animation:${1:name} ${2:duration} ${3:timing-function} ${4:delay} ${5:iteration-count} ${6:direction} ${7:fill-mode};" tbl)
(puthash "bgsz:ct" "background-size:contain;" tbl)
(puthash "bd" "border:|;" tbl)
(puthash "wob:n" "word-break:normal;" tbl)
(puthash "bg" "background:|;" tbl)
(puthash "wob:k" "word-break:keep-all;" tbl)
(puthash "tj:ic" "text-justify:inter-cluster;" tbl)
(puthash "bb" "border-bottom:|;" tbl)
(puthash "bl" "border-left:|;" tbl)
(puthash "tj:ii" "text-justify:inter-ideograph;" tbl)
(puthash "t:a" "top:auto;" tbl)
(puthash "bgcp:bb" "background-clip:border-box;" tbl)
(puthash "ff:ss" "font-family:sans-serif;" tbl)
(puthash "br" "border-right:|;" tbl)
(puthash "bga" "background-attachment:|;" tbl)
(puthash "ol" "outline:|;" tbl)
(puthash "cl:r" "clear:right;" tbl)
(puthash "q:ru" "quotes:'\\00AB' '\\00BB' '\\201E' '\\201C';" tbl)
(puthash "bgo" "background-origin:|;" tbl)
(puthash "bgp" "background-position:${1:0} ${2:0};" tbl)
(puthash "bgr" "background-repeat:|;" tbl)
(puthash "cl:n" "clear:none;" tbl)
(puthash "cl:l" "clear:left;" tbl)
(puthash "cl:b" "clear:both;" tbl)
(puthash "animn" "animation-name:${1:none};" tbl)
(puthash "op" "opacity:|;" tbl)
(puthash "fs:o" "font-style:oblique;" tbl)
(puthash "fs:n" "font-style:normal;" tbl)
(puthash "bds:h" "border-style:hidden;" tbl)
(puthash "bdrw" "border-right-width:|;" tbl)
(puthash "bds:n" "border-style:none;" tbl)
(puthash "bds:o" "border-style:outset;" tbl)
(puthash "fs:i" "font-style:italic;" tbl)
(puthash "ct:oq" "content:open-quote;" tbl)
(puthash "bds:g" "border-style:groove;" tbl)
(puthash "olc" "outline-color:#${1:000};" tbl)
(puthash "bds:r" "border-style:ridge;" tbl)
(puthash "bds:s" "border-style:solid;" tbl)
(puthash "bds:w" "border-style:wave;" tbl)
(puthash "z:a" "z-index:auto;" tbl)
(puthash "ct:c" "content:counter(|);" tbl)
(puthash "cur:m" "cursor:move;" tbl)
(puthash "ct:a" "content:attr(|);" tbl)
(puthash "pgbb:r" "page-break-before:right;" tbl)
(puthash "bgo:bb" "background-origin:border-box;" tbl)
(puthash "ct:n" "content:normal;" tbl)
(puthash "bdr+" "border-right:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "pgbb:l" "page-break-before:left;" tbl)
(puthash "fza" "font-size-adjust:|;" tbl)
(puthash "d:tbcl" "display:table-column;" tbl)
(puthash "ovx:a" "overflow-x:auto;" tbl)
(puthash "whs:pl" "white-space:pre-line;" tbl)
(puthash "trsde" "transition-delay:${1:time};" tbl)
(puthash "ovx:h" "overflow-x:hidden;" tbl)
(puthash "ovx:v" "overflow-x:visible;" tbl)
(puthash "ovx:s" "overflow-x:scroll;" tbl)
(puthash "trf:scy" "transform: scaleY(${1:y});" tbl)
(puthash "trf:scx" "transform: scaleX(${1:x});" tbl)
(puthash "@f+" "@font-face {\n\tfont-family: '${1:FontName}';\n\tsrc: url('${2:FileName}.eot');\n\tsrc: url('${2:FileName}.eot?#iefix') format('embedded-opentype'),\n\t\t url('${2:FileName}.woff') format('woff'),\n\t\t url('${2:FileName}.ttf') format('truetype'),\n\t\t url('${2:FileName}.svg#${1:FontName}') format('svg');\n\tfont-style: ${3:normal};\n\tfont-weight: ${4:normal};\n}" tbl)
(puthash "trsdu" "transition-duration:${1:time};" tbl)
(puthash "d:tbcp" "display:table-caption;" tbl)
(puthash "bdrs" "border-radius:|;" tbl)
(puthash "us" "user-select:${none};" tbl)
(puthash "bgcp:pb" "background-clip:padding-box;" tbl)
(puthash "bdri" "border-right-image:url(|);" tbl)
(puthash "z" "z-index:|;" tbl)
(puthash "d:rbbg" "display:ruby-base-group;" tbl)
(puthash "tsh:ra" "text-shadow:${1:h} ${2:v} ${3:blur} rgba(${4:0}, ${5:0}, ${6:0}, .${7:5});" tbl)
(puthash "femp:a" "font-emphasize-position:after;" tbl)
(puthash "femp:b" "font-emphasize-position:before;" tbl)
(puthash "animtf:eo" "animation-timing-function:ease-out;" tbl)
(puthash "whs:pw" "white-space:pre-wrap;" tbl)
(puthash "animtf:ei" "animation-timing-function:ease-in;" tbl)
(puthash "ap" "appearance:${none};" tbl)
(puthash "animps" "animation-play-state:${1:running};" tbl)
(puthash "lisi:n" "list-style-image:none;" tbl)
(puthash "bdbc:t" "border-bottom-color:transparent;" tbl)
(puthash "pos:s" "position:static;" tbl)
(puthash "animdur" "animation-duration:${1:0}s;" tbl)
(puthash "bdi:n" "border-image:none;" tbl)
(puthash "pos:f" "position:fixed;" tbl)
(puthash "pos:a" "position:absolute;" tbl)
(puthash "fza:n" "font-size-adjust:none;" tbl)
(puthash "v:h" "visibility:hidden;" tbl)
(puthash "bgpy" "background-position-y:|;" tbl)
(puthash "bgpx" "background-position-x:|;" tbl)
(puthash "v:c" "visibility:collapse;" tbl)
(puthash "bdbli" "border-bottom-left-image:url(|);" tbl)
(puthash "fsm" "font-smooth:|;" tbl)
(puthash "animfm" "animation-fill-mode:${1:both};" tbl)
(puthash "cur:he" "cursor:help;" tbl)
(puthash "fst" "font-stretch:|;" tbl)
(puthash "cur:ha" "cursor:hand;" tbl)
(puthash "bgo:pb" "background-origin:padding-box;" tbl)
(puthash "cps" "caption-side:|;" tbl)
(puthash "v:v" "visibility:visible;" tbl)
tbl) tbl)
(puthash "filters" "html" tbl)
tbl) tbl)
(puthash "haml" (let ((tbl (make-hash-table :test 'equal)))
(puthash "profile" "xml" tbl)
(puthash "extends" "html" tbl)
(puthash "filters" "haml" tbl)
tbl) tbl)
(puthash "xsl" (let ((tbl (make-hash-table :test 'equal)))
(puthash "profile" "xml" tbl)
(puthash "abbreviations" (let ((tbl (make-hash-table :test 'equal)))
(puthash "ch" "<xsl:choose>" tbl)
(puthash "co" "<xsl:copy-of select=\"\"/>" tbl)
(puthash "fall" "<xsl:fallback>" tbl)
(puthash "vare" "<xsl:variable name=\"\" select=\"\"/>" tbl)
(puthash "ap" "<xsl:apply-templates select=\"\" mode=\"\"/>" tbl)
(puthash "api" "<xsl:apply-imports/>" tbl)
(puthash "attrs" "<xsl:attribute-set name=\"\">" tbl)
(puthash "strip" "<xsl:strip-space elements=\"\"/>" tbl)
(puthash "cp" "<xsl:copy select=\"\"/>" tbl)
(puthash "if" "<xsl:if test=\"\">" tbl)
(puthash "par" "<xsl:param name=\"\">" tbl)
(puthash "val" "<xsl:value-of select=\"\"/>" tbl)
(puthash "for" "each" tbl)
(puthash "tn" "<xsl:template name=\"\">" tbl)
(puthash "imp" "<xsl:import href=\"\"/>" tbl)
(puthash "tm" "<xsl:template match=\"\" mode=\"\">" tbl)
(puthash "call" "<xsl:call-template name=\"\"/>" tbl)
(puthash "var" "<xsl:variable name=\"\">" tbl)
(puthash "inc" "<xsl:include href=\"\"/>" tbl)
(puthash "proc" "<xsl:processing-instruction name=\"\">" tbl)
(puthash "pres" "<xsl:preserve-space elements=\"\"/>" tbl)
(puthash "sort" "<xsl:sort select=\"\" order=\"\"/>" tbl)
(puthash "pare" "<xsl:param name=\"\" select=\"\"/>" tbl)
(puthash "nam" "<namespace-alias stylesheet-prefix=\"\" result-prefix=\"\"/>" tbl)
(puthash "xsl:when" "<xsl:when test=\"\">" tbl)
(puthash "wh" "xsl:when" tbl)
(puthash "tname" "tn" tbl)
(puthash "key" "<xsl:key name=\"\" match=\"\" use=\"\"/>" tbl)
(puthash "wp" "<xsl:with-param name=\"\" select=\"\"/>" tbl)
(puthash "msg" "<xsl:message terminate=\"no\">" tbl)
(puthash "tmatch" "tm" tbl)
(puthash "attr" "<xsl:attribute name=\"\">" tbl)
(puthash "tex" "<xsl:text></xsl:text>" tbl)
(puthash "elem" "<xsl:element name=\"\">" tbl)
(puthash "num" "<xsl:number value=\"\"/>" tbl)
(puthash "choose+" "xsl:choose>xsl:when+xsl:otherwise" tbl)
(puthash "each" "<xsl:for-each select=\"\">" tbl)
(puthash "ot" "<xsl:otherwise>" tbl)
(puthash "com" "<xsl:comment>" tbl)
(puthash "xsl" "!!!+xsl:stylesheet[version=1.0 xmlns:xsl=http://www.w3.org/1999/XSL/Transform]>{\n|}" tbl)
tbl) tbl)
(puthash "extends" "html" tbl)
(puthash "snippets" (let ((tbl (make-hash-table :test 'equal)))
(puthash "!!!" "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" tbl)
tbl) tbl)
(puthash "filters" "html, xsl" tbl)
tbl) tbl)
tbl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML abbrev

(defun zencoding-expr (input)
  "Parse a zen coding expression with optional filters."
  (zencoding-pif (zencoding-parse "\\(.*?\\)|" 2 "expr|filter" it)
                 (let ((input (elt it 1))
                       (filters (elt it 2)))
                   (zencoding-pif (zencoding-extract-filters filters)
                                  (zencoding-filter input it)
                                  it))
                 (zencoding-filter input (zencoding-default-filter))))

(defun zencoding-subexpr (input)
  "Parse a zen coding expression with no filter. This pretty much defines precedence."
  (zencoding-run zencoding-siblings
                 it
                 (zencoding-run zencoding-parent-child
                                it
                                (zencoding-run zencoding-multiplier
                                               it
                                               (zencoding-run zencoding-pexpr
                                                              it
                                                              (zencoding-run zencoding-text
                                                                             it
                                                                             (zencoding-run zencoding-tag
                                                                                            it
                                                                                            '(error "no match, expecting ( or a-zA-Z0-9"))))))))

(defun zencoding-extract-filters (input)
  "Extract filters from expression."
  (zencoding-pif (zencoding-parse "\\([^\\|]+?\\)|" 2 "" it)
                 (let ((filter-name (elt it 1))
                       (more-filters (elt it 2)))
                   (zencoding-pif (zencoding-extract-filters more-filters)
                                  (cons filter-name it)
                                  it))
                 (zencoding-parse "\\([^\\|]+\\)" 1 "filter name" `(,(elt it 1)))))

(defun zencoding-filter (input filters)
  "Construct AST with specified filters."
  (zencoding-pif (zencoding-subexpr input)
                 (let ((result (car it))
                       (rest (cdr it)))
                   `((filter ,filters ,result) . ,rest))
                 it))

(defun zencoding-default-filter ()
  "Default filter(s) to be used if none is specified."
  (let* ((file-ext (car (zencoding-regex ".*\\(\\..*\\)" (or (buffer-file-name) "") 1)))
         (defaults '(".html" ("html")
                     ".htm"  ("html")
                     ".haml" ("haml")
                     ".clj"  ("hic")))
         (default-else      '("html"))
         (selected-default (member file-ext defaults)))
    (if selected-default
        (cadr selected-default)
      default-else)))

(defun zencoding-numbering (input)
  (zencoding-parse
   "\\(\\$+\\)" 2 "numbering, $"
   (let ((doller (elt it 1)))
     (zencoding-pif (zencoding-parse
                     "@\\([0-9-][0-9]*\\)" 2 "numbering args"
                     (let* ((args (read (elt it 1)))
                            (direction  (not (or (eq '- args) (minusp args))))
                            (base       (if (eq '- args) 1 (abs args))))
                       `((n ,(length doller) ,direction ,base) . ,input)))
                    it
                    `((n ,(length doller) t 1) . ,input)))))

(defun zencoding-split-numbering-expressions (input)
  (labels
      ((iter (input)
             (zencoding-aif (zencoding-regex "\\([^$]*\\)\\(\\$.*\\)" input '(1 2))
                (let ((prefix (car it))
                      (input (cadr it)))
                  (if (and (< 0 (length prefix)) ; check if ..\\$... or ...$...
                           (string-equal (substring prefix -1) "\\"))
                      `(,(store-substring prefix (- (length prefix) 1) ?$)
                        ,@(iter (substring input 1)))
                    (let ((res (zencoding-numbering input)))
                      `(,prefix ,(car res) ,@(iter (cdr res))))))
                (list input))))
    (let ((res (iter input)))
      (if (every #'stringp res)
          (apply #'concat res)
        `(numberings ,@res)))))

(defun zencoding-instantiate-numbering-expression (i lim exp)
  (labels ((instantiate
            (i lim exps)
            (apply #'concat
                   (mapcar
                    (lambda (exp)
                      (if (listp exp)
                          (let ((digits (second exp))
                                (direction (third exp))
                                (base (fourth exp)))
                            (let ((num (if direction (+ base i)
                                         (- (+ lim (- base 1)) i))))
                              (format (concat "%0" (format "%d" digits) "d") num)))
                        exp)) exps)))
           (search
            (i lim exp)
            (if (listp exp)
                (if (eql (car exp) 'numberings)
                    (instantiate i lim (cdr exp))
                  ;; Should do like this for real searching.
                  ;; But stack overflow occurs.
                  ;; (cons (search-numberings i lim (car exp))
                  ;;       (search-numberings i lim (cdr exp)))
                  (mapcar (lambda (exp) (search i lim exp)) exp))
              exp)))
    (search i lim exp)))

(defun zencoding-multiply-expression (multiplicand exp)
  (loop for i to (- multiplicand 1) collect
        (zencoding-instantiate-numbering-expression i multiplicand exp)))

(defun zencoding-multiplier (input)
  (zencoding-pif (zencoding-run zencoding-pexpr
                                it
                                (zencoding-run zencoding-tag
                                               it
                                               (zencoding-run zencoding-text
                                                              it
                                                              '(error "expected *n multiplier"))))
                 (let* ((expr (car it)) (input (cdr it))
                        (multiplier expr))
                   (zencoding-parse "\\*\\([0-9]+\\)" 2 "*n where n is a number"
                                    (let ((multiplicand (read (elt it 1))))
                                      `((list ,(zencoding-multiply-expression
                                                multiplicand
                                                multiplier)) . ,input))))))

(defun zencoding-tag (input)
  "Parse a tag."
  (zencoding-run zencoding-tagname
                 (let ((tagname (cadr expr))
                       (has-body? (cddr expr)))
                   (zencoding-pif (zencoding-run zencoding-identifier
                                                 (zencoding-tag-classes
                                                  `(tag (,tagname ,has-body? ,(cddr expr))) input)
                                                 (zencoding-tag-classes
                                                  `(tag (,tagname ,has-body? nil)) input))
                                  (let ((tag-data (cadar it)) (input (cdr it)))
                                    (zencoding-pif (zencoding-run zencoding-props
                                                                  (let ((props (cdr expr)))
                                                                    `((tag ,(append tag-data (list props))) . ,input))
                                                                  `((tag ,(append tag-data '(nil))) . ,input))
                                                   (let ((expr (car it)) (input (cdr it)))
                                                     (zencoding-tag-text expr input))))))
                 (zencoding-default-tag input)))

(defun zencoding-default-tag (input)
  "Parse a #id or .class"
  (zencoding-parse "\\([#|\\.]\\)" 1 "tagname"
                   (zencoding-tag (concat "div" (elt it 0)))))

(defun zencoding-tag-text (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-text
                   (let ((txt (cadr expr)))
                     `((tag ,(append tag-data (list txt))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-tag-props (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-props
                   (let ((props (cdr expr)))
                     `((tag ,(append tag-data (list props))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-props (input)
  "Parse many props."
    (zencoding-run zencoding-prop
                   (zencoding-pif (zencoding-props input)
                                  `((props . ,(cons expr (cdar it))) . ,(cdr it))
                                  `((props . ,(list expr)) . ,input))))

(defun zencoding-prop (input)
  (zencoding-parse
   " " 1 "space"
   (zencoding-run
    zencoding-name
    (let ((name (cdr expr)))
      (zencoding-pif (zencoding-prop-value name input)
                     it
                     `((,(read name) "") . ,input))))))

(defun zencoding-prop-value (name input)
  (zencoding-pif (zencoding-parse "=\"\\(.*?\\)\"" 2
                                  "=\"property value\""
                                  (let ((value (elt it 1))
                                        (input (elt it 2)))
                                    `((,(read name) ,value) . ,input)))
                 it
                 (zencoding-parse "=\\([^\\,\\+\\>\\ )]*\\)" 2
                                  "=property value"
                                  (let ((value (elt it 1))
                                        (input (elt it 2)))
                                    `((,(read name) ,value) . ,input)))))

(defun zencoding-tag-classes (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-classes
                   (let ((classes (mapcar (lambda (cls) (cdadr cls))
                                          (cdr expr))))
                     `((tag ,(append tag-data (list classes))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-tagname (input)
  "Parse a tagname a-zA-Z0-9 tagname (e.g. html/head/xsl:if/br)."
  (zencoding-parse "\\([a-zA-Z][a-zA-Z0-9:$@-]*\/?\\)" 2 "tagname, a-zA-Z0-9"
                   (let* ((tag-spec (elt it 1))
                          (empty-tag (zencoding-regex "\\([^\/]*\\)\/" tag-spec 1))
                          (tag (zencoding-split-numbering-expressions
                                (if empty-tag (car empty-tag) tag-spec))))
                     `((tagname . (,tag . ,(not empty-tag))) . ,input))))

(defun zencoding-text (input)
  "A zen coding expression innertext."
  (zencoding-parse "{\\(.*?\\)}" 2 "inner text"
                   (let ((txt (zencoding-split-numbering-expressions (elt it 1))))
                     `((text ,txt) . ,input))))

(defun zencoding-pexpr (input)
  "A zen coding expression with parentheses around it."
  (zencoding-parse "(" 1 "("
                   (zencoding-run zencoding-subexpr
                                  (zencoding-aif (zencoding-regex ")" input '(0 1))
                                                 `(,expr . ,(elt it 1))
                                                 '(error "expecting `)'")))))

(defun zencoding-parent-child (input)
  "Parse an tag>e expression, where `n' is an tag and `e' is any
   expression."
  (defun listing (parents child input)
    (let ((len (length parents)))
      `((list ,(map 'list
                    (lambda (parent i)
                      `(parent-child ,parent
                                     ,(zencoding-instantiate-numbering-expression i len child)))
                    parents
                    (loop for i to (- len 1) collect i))) . ,input)))
  (zencoding-run zencoding-multiplier
                 (let* ((items (cadr expr))
                        (rest (zencoding-child-sans expr input)))
                   (if (not (eq (car rest) 'error))
                       (let ((child (car rest))
                             (input (cdr rest)))

                         (zencoding-aif (zencoding-regex "^" input '(0 1))
                                                   (let ((input (elt it 1)))
                                                     (zencoding-run zencoding-subexpr
                                                                    `((sibling ,(car (listing items child "")) ,expr) . ,input)
                                                                    (listing items child input)))
                                                   (listing items child input)))
                     '(error "expected child")))
                 (zencoding-run zencoding-tag
                                (zencoding-child expr input)
                                '(error "expected parent"))))

(defun zencoding-child-sans (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-subexpr
                                  it
                                  '(error "expected child"))))

(defun zencoding-child (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-subexpr
                                  (let ((child expr))
                                    (zencoding-aif (zencoding-regex "^" input '(0 1))
                                                   (let ((input (elt it 1)))
                                                     (zencoding-run zencoding-subexpr
                                                                    `((sibling (parent-child ,parent ,child) ,expr) . ,input)
                                                                    `((parent-child ,parent ,child) . ,input)))
                                                   `((parent-child ,parent ,child) . ,input)))
                                  '(error "expected child"))))

(defun zencoding-sibling (input)
  (zencoding-por zencoding-pexpr zencoding-multiplier
                 it
                 (zencoding-run zencoding-tag
                                it
                                (zencoding-run zencoding-text
                                               it
                                               '(error "expected sibling")))))

(defun zencoding-siblings (input)
  "Parse an e+e expression, where e is an tag or a pexpr."
  (zencoding-run zencoding-sibling
                 (let ((parent expr))
                   (zencoding-parse "\\+" 1 "+"
                                    (zencoding-run zencoding-subexpr
                                                   (let ((child expr))
                                                     `((sibling ,parent ,child) . ,input))
                                                   (zencoding-expand parent input))))
                 '(error "expected first sibling")))

(defvar zencoding-expandable-tags
  '("dl"    ">(dt+dd)"
    "ol"    ">li"
    "ul"    ">li"
    "table" ">tr>td"))

(defun zencoding-expand (parent input)
  "Parse an e+ expression, where e is an expandable tag"
  (let* ((parent-tag (car (elt parent 1)))
         (expandable (member parent-tag zencoding-expandable-tags)))
    (if expandable
        (let ((expansion (zencoding-child parent (concat (cadr expandable)))))
          (zencoding-pif (zencoding-parse "+\\(.*\\)" 1 "+expr"
                                          (zencoding-subexpr (elt it 1)))
                         `((sibling ,(car expansion) ,(car it)))
                         expansion))
      '(error "expected second sibling"))))

(defun zencoding-name (input)
  "Parse a class or identifier name, e.g. news, footer, mainimage"
  (zencoding-parse "\\([a-zA-Z$@][a-zA-Z0-9$@_:-]*\\)" 2 "class or identifer name"
                   `((name . ,(zencoding-split-numbering-expressions
                               (elt it 1))) . ,input)))

(defun zencoding-class (input)
  "Parse a classname expression, e.g. .foo"
  (zencoding-parse "\\." 1 "."
                   (zencoding-run zencoding-name
                                  `((class ,expr) . ,input)
                                  '(error "expected class name"))))
(defun zencoding-identifier (input)
  "Parse an identifier expression, e.g. #foo"
  (zencoding-parse "#" 1 "#"
                   (zencoding-run zencoding-name
                                  `((identifier . ,expr) . ,input))))

(defun zencoding-classes (input)
  "Parse many classes."
  (zencoding-run zencoding-class
                 (zencoding-pif (zencoding-classes input)
                                `((classes . ,(cons expr (cdar it))) . ,(cdr it))
                                `((classes . ,(list expr)) . ,input))
                 '(error "expected class")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding transformer from AST to string

(defvar zencoding-inline-tags
  '("a"
    "abbr"
    "acronym"
    "cite"
    "code"
    "dd"
    "dfn"
    "dt"
    "em"
    "h1" "h2" "h3" "h4" "h5" "h6"
    "kbd"
    "li"
    "q"
    "span"
    "strong"
    "var"
    "textarea"
    "small"
    "time" "del" "ins"
    "sub"
    "sup"
    "i" "s" "b"
    "ruby" "rt" "rp"
    "bdo"
    "iframe" "canvas"
    "audio" "video"
    "ovject" "embed"
    "map"))

(defvar zencoding-block-tags
  '("p"
    "article"
    "section"
    "aside"
    "nav"
    "figure"
    "address"
    "header"
    "footer"))

(defvar zencoding-self-closing-tags
  '("br"
    "img"
    "input"
    "wbr"
    "object"
    "source"
    "area"
    "param"
    "option"))

(defvar zencoding-leaf-function nil
  "Function to execute when expanding a leaf node in the
  Zencoding AST.")

(defvar zencoding-filters
  '("html" (zencoding-primary-filter zencoding-make-html-tag)
    "c"    (zencoding-primary-filter zencoding-make-commented-html-tag)
    "haml" (zencoding-primary-filter zencoding-make-haml-tag)
    "hic"  (zencoding-primary-filter zencoding-make-hiccup-tag)
    "e"    (zencoding-escape-xml)))

(defun zencoding-primary-filter (input proc)
  "Process filter that needs to be executed first, ie. not given output from other filter."
  (if (listp input)
      (let ((tag-maker (cadr proc)))
        (zencoding-transform-ast input tag-maker))
    nil))

(defun zencoding-process-filter (filters input)
  "Process filters, chain one filter output as the input of the next filter."
  (let ((filter-data (member (car filters) zencoding-filters))
        (more-filters (cdr filters)))
    (if filter-data
        (let* ((proc   (cadr filter-data))
               (fun    (car proc))
               (filter-output (funcall fun input proc)))
          (if more-filters
              (zencoding-process-filter more-filters filter-output)
            filter-output))
      nil)))

(defun zencoding-make-tag (tag-maker tag-info &optional content)
  "Extract tag info and pass them to tag-maker."
  (let* ((name      (pop tag-info))
         (has-body? (pop tag-info))
         (id        (pop tag-info))
         (classes   (pop tag-info))
         (props     (pop tag-info))
         (txt       (pop tag-info))
         (self-closing? (not (or txt content
                                 (and has-body?
                                      (not (member name zencoding-self-closing-tags)))))))
    (funcall tag-maker name id classes props txt self-closing?
             (if content content
               (if zencoding-leaf-function (funcall zencoding-leaf-function))))))

(defun zencoding-make-html-tag (tag-name tag-id tag-classes tag-props tag-txt self-closing? content)
  "Create HTML markup string"
  (let* ((id      (zencoding-concat-or-empty " id=\"" tag-id "\""))
         (classes (zencoding-mapconcat-or-empty " class=\"" tag-classes " " "\""))
         (props   (zencoding-mapconcat-or-empty " " tag-props " " nil
                                                (lambda (prop)
                                                  (concat (symbol-name (car prop)) "=\"" (cadr prop) "\""))))
         (content-multiline? (and content (string-match "\n" content)))
         (block-tag? (or (member tag-name zencoding-block-tags)
                         (and (> (length tag-name) 1)
                              (not (member tag-name zencoding-inline-tags)))))
         (lf (if (or content-multiline? block-tag?)
                 "\n")))
    (concat "<" tag-name id classes props (if self-closing?
                                              "/>"
                                            (concat ">"
                                                    (if tag-txt
                                                        (if (or content-multiline? block-tag?)
                                                            (zencoding-indent tag-txt)
                                                          tag-txt))
                                                    (if content
                                                        (if (or content-multiline? block-tag?)
                                                            (zencoding-indent content)
                                                          content))
                                                    lf
                                                    "</" tag-name ">")))))

(defun zencoding-make-commented-html-tag (tag-name tag-id tag-classes tag-props tag-txt self-closing? content)
  "Create HTML markup string with extra comments for elements with #id or .classes"
  (let ((body (zencoding-make-html-tag tag-name tag-id tag-classes tag-props tag-txt self-closing? content)))
    (if (or tag-id tag-classes)
        (let ((id      (zencoding-concat-or-empty "#" tag-id))
              (classes (zencoding-mapconcat-or-empty "." tag-classes ".")))
          (concat "<!-- " id classes " -->\n"
                  body
                  "\n<!-- /" id classes " -->"))
      body)))

(defun zencoding-make-haml-tag (tag-name tag-id tag-classes tag-props tag-txt self-closing? content)
  "Create HAML string"
  (let ((name    (if (and (equal tag-name "div")
                          (or tag-id tag-classes))
                     ""
                   (concat "%" tag-name)))
        (id      (zencoding-concat-or-empty "#" tag-id))
        (classes (zencoding-mapconcat-or-empty "." tag-classes "."))
        (props   (zencoding-mapconcat-or-empty "{" tag-props ", " "}"
                                               (lambda (prop)
                                                 (concat ":" (symbol-name (car prop)) " => \"" (cadr prop) "\"")))))
    (concat name id classes props
            (if tag-txt
                (zencoding-indent tag-txt))
            (if content
                (zencoding-indent content)))))

(defun zencoding-make-hiccup-tag (tag-name tag-id tag-classes tag-props tag-txt self-closing? content)
  "Create Hiccup string"
  (let* ((id      (zencoding-concat-or-empty "#" tag-id))
         (classes (zencoding-mapconcat-or-empty "." tag-classes "."))
         (props   (zencoding-mapconcat-or-empty " {" tag-props ", " "}"
                                                (lambda (prop)
                                                  (concat ":" (symbol-name (car prop)) " \"" (cadr prop) "\""))))
         (content-multiline? (and content (string-match "\n" content)))
         (block-tag? (or (member tag-name zencoding-block-tags)
                         (and (> (length tag-name) 1)
                              (not (member tag-name zencoding-inline-tags))))))
    (concat "[:" tag-name id classes props
            (if tag-txt
                (let ((tag-txt-quoted (concat "\"" tag-txt "\"")))
                  (if (or content-multiline? block-tag?)
                      (zencoding-indent tag-txt-quoted)
                    (concat " " tag-txt-quoted))))
            (if content
                (if (or content-multiline? block-tag?)
                    (zencoding-indent content)
                  (concat " " content)))
            "]")))

(defun zencoding-make-text (tag-maker text)
  (cond
   ((eq tag-maker 'zencoding-make-hiccup-tag)
    (concat "\"" text "\""))
   (t text)))

(defun zencoding-concat-or-empty (prefix body &optional suffix)
  "Return prefixed suffixed text or empty string."
  (if body
      (concat prefix body suffix)
    ""))

(defun zencoding-mapconcat-or-empty (prefix list-body delimiter &optional suffix map-fun)
  "Return prefixed suffixed mapconcated text or empty string."
  (if list-body
      (let* ((mapper (if map-fun map-fun 'identity))
             (body (mapconcat mapper list-body delimiter)))
        (concat prefix body suffix))
    ""))

(defun zencoding-escape-xml (input proc)
  "Escapes XML-unsafe characters: <, > and &."
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    ">" "&gt;"
    (replace-regexp-in-string
     "&" "&amp;"
     (if (stringp input)
         input
       (zencoding-process-filter (zencoding-default-filter) input))))))

(defun zencoding-html-transform (input)
  (let ((ast (car (zencoding-expr input))))
    (when (not (eq ast 'error))
      (zencoding-transform-ast-with-filters ast))))

(defun zencoding-transform-ast-with-filters (ast-with-filters)
  "Transform AST (containing filter data) into string."
  (let ((filters (cadr ast-with-filters))
        (ast (caddr ast-with-filters)))
    (zencoding-process-filter filters ast)))

(defun zencoding-transform-ast (ast tag-maker)
  "Transform AST (without filter data) into string."
  (let ((type (car ast)))
    (cond
     ((eq type 'list)
      (mapconcat (lexical-let ((make-tag-fun tag-maker))
                   #'(lambda (sub-ast)
                       (zencoding-transform-ast sub-ast make-tag-fun)))
                 (cadr ast)
                 "\n"))
     ((eq type 'tag)
      (zencoding-make-tag tag-maker (cadr ast)))
     ((eq type 'text)
      (zencoding-make-text tag-maker (cadr ast)))
     ((eq type 'parent-child)
      (let ((parent (cadadr ast))
            (children (zencoding-transform-ast (caddr ast) tag-maker)))
        (zencoding-make-tag tag-maker parent children)))
     ((eq type 'sibling)
      (let ((sib1 (zencoding-transform-ast (cadr ast) tag-maker))
            (sib2 (zencoding-transform-ast (caddr ast) tag-maker)))
        (concat sib1 "\n" sib2))))))

(defun zencoding-indent (text)
  "Indent the text"
  (if text
      (replace-regexp-in-string "\n" "\n    " (concat "\n" text))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; CSS abbrev:

(defun zencoding-css-split-args (exp)
  (zencoding-aif
   (string-match "[#0-9$-]" exp)
   (cons (substring exp 0 it) (substring exp it))
   (list exp)))

(defun zencoding-css-arg-number (input)
  (zencoding-parse
   "\\(\\(?:-\\|\\)[0-9.]+\\)\\(\\(?:-\\|e\\|p\\|x\\)\\|\\)" 3 "css number arguments"
   (cons (list (elt it 1)
               (let ((unit (string-to-char (elt it 2))))
                 (cond ((= unit ?-) "px")
                       ((= unit ?e) "em")
                       ((= unit ?p) "%")
                       ((= unit ?x) "ex")
                       (t "px"))))
         input)))

(defun zencoding-css-arg-color (input)
  (zencoding-parse
   "#\\([0-9a-fA-F]\\{1,6\\}\\)" 2 "css color argument"
   (cons (let* ((n (elt it 1))
                (l (length n)))
           (concat
            "#"
            (substring
             (cond ((= l 1) (concat (make-list 6 (string-to-char n))))
                   ((= l 2) (concat n n n))
                   ((= l 3) (concat
                             (loop for c in (string-to-list n)
                                   append (list c c))))
                   (t (concat n n)))
             0 6)))
         input)))

(defun zencoding-css-parse-arg (input)
  (zencoding-run zencoding-css-arg-number it
                 (zencoding-run zencoding-css-arg-color it
                                (if (equal input "")
                                    it
                                  (cons input "")))))

(defun zencoding-css-important-p (input)
  (let ((len (length input)))
    (and (< 0 len)
         (char-equal (aref input (1- len)) ?!))))

(defun zencoding-css-parse-args (args)
  (when args
    (let ((rt nil))
      (loop
       (zencoding-pif (zencoding-css-parse-arg args)
                      (progn (push (car it) rt)
                             (setf args (cdr it)))
                      (return (nreverse rt)))))))

(defun zencoding-css-subexpr (exp)
  (let* ((importantp (zencoding-css-important-p exp))
         (exp (zencoding-css-split-args
               (if importantp (subseq exp 0 -1) exp)))
         (args (cdr exp)))
    (setf (cdr exp) (cons importantp (zencoding-css-parse-args args)))
    exp))

(defun zencoding-css-toknize (str)
  (let* ((i (split-string str "+"))
         (rt nil))
    (loop
     (let ((f (first i))
           (s (second i)))
       (if f
           (if (and s (or (string= s "") (string-match "^[#0-9$-]" s)))
               (progn
                 (setf rt (cons (concat f "+" s) rt))
                 (setf i (cddr i)))
             (progn
               (setf rt (cons f rt))
               (setf i (cdr i))))
         (return (nreverse rt)))))))

(defun zencoding-css-expr (input)
  (mapcar #'zencoding-css-subexpr
          (zencoding-css-toknize input)))

(zencoding-defparameter
 zencoding-css-snippets
 (gethash "snippets" (gethash "css" zencoding-snippets)))

(zencoding-defparameter
 zencoding-css-unitless-properties
 '("z-index" "line-height" "opacity" "font-weight" "zoom"))

(zencoding-defparameter
 zencoding-css-unitless-properties-regex
 (concat "^\\(:?" (zencoding-join-string
                   zencoding-css-unitless-properties "\\|")
         "\\):.*$"))

(defun zencoding-css-instantiate-lambda (str)
  (flet ((split-string-to-body
          (str args-sym)
          (let ((rt '(concat)) (idx-max 0))
            (loop for i from 0 to 255 do
                  (zencoding-aif
                   (string-match "\\(?:|\\|${\\(?:\\([0-9]\\):\\|\\)\\(?:\\(.+?\\)\\|\\)}\\)" str)
                   (destructuring-bind (mat idx def)
                       (mapcar (lambda (ref) (match-string ref str)) '(0 1 2))
                     (setf rt
                           `((or
                              (nth ,(let ((cur-idx (if idx (1- (string-to-int idx)) i)))
                                      (setf idx-max (max cur-idx idx-max)))
                                   ,args-sym)
                              ,(or def ""))
                             ,(substring str 0 it) ;; ordered to reverse
                             ,@rt))
                     (setf str (substring str (+ it (length mat)))))
                   ;; don't use nreverse. cause bug in emacs-lisp.
                   (return (cons idx-max (reverse (cons str rt)))))))))
    (let ((args (gensym)))
      (destructuring-bind (idx-max . body) (split-string-to-body str args)
        (eval
         `(lambda (&rest ,args)
            (progn
              (when (nthcdr ,idx-max ,args)
                (setf (nthcdr ,idx-max ,args)
                      (list (zencoding-join-string
                             (nthcdr ,idx-max ,args) " "))))
              ,body)))))))

(defun zencoding-css-transform-exprs (exprs)
  (zencoding-join-string
   (mapcar
    #'(lambda (expr)
        (zencoding-aif
         (gethash (car expr) zencoding-css-snippets)
         (let ((set it) (fn nil) (unitlessp nil))
           (if (stringp set)
               (progn
                 ;; new pattern
                 ;; creating print function
                 (setf fn (zencoding-css-instantiate-lambda set))
                 ;; get unitless or no
                 (setf unitlessp
                       (not (null (string-match
                                   zencoding-css-unitless-properties-regex set))))
                 ;; caching
                 (puthash (car expr) (cons fn unitlessp) zencoding-css-snippets))
             (progn
               ;; cache hit.
               (setf fn (car set))
               (setf unitlessp (cdr set))))
           (let ((transformed
                  (apply fn
                         (mapcar
                          #'(lambda (arg)
                              (if (listp arg)
                                  (if unitlessp (car arg)
                                    (apply #'concat arg))
                                arg))
                          (cddr expr)))))
             (if (cadr expr)
                 (concat (subseq transformed 0 -1) " !important;")
               transformed)))
         (concat (car expr) ":"
                 (zencoding-join-string
                  (mapcar #'(lambda (arg)
                              (if (listp arg) (apply #'concat arg) arg))
                          (cdr expr)) " ")
                 ";")))
    exprs)
   "\n"))


(defun zencoding-css-transform (input)
  (zencoding-css-transform-exprs (zencoding-css-expr input)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding minor mode

(defgroup zencoding nil
  "Customization group for zencoding-mode."
  :group 'convenience)

(defun zencoding-expr-on-line ()
  "Extract a zencoding expression and the corresponding bounds
   for the current line."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties start end))
         (expr (zencoding-regex "\\([ \t]*\\)\\([^\n]+\\)" line 2)))
    (if (first expr)
        (list (first expr) start end))))

(defcustom zencoding-indentation 4
  "Number of spaces used for indentation."
  :type '(number :tag "Spaces")
  :group 'zencoding)

(defun zencoding-prettify (markup indent)
  (let ((first-col (format (format "%%%ds" indent) ""))
        (tab       (format (format "%%%ds" zencoding-indentation) "")))
    (concat first-col
            (replace-regexp-in-string "\n" (concat "\n" first-col)
                                      (replace-regexp-in-string "    " tab markup)))))

(defun zencoding-transform (input)
  (if (eql major-mode 'css-mode)
      (zencoding-css-transform input)
    (zencoding-html-transform input)))

;;;###autoload
(defun zencoding-expand-line (arg)
  "Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'."
  (interactive "P")
  (let* ((here (point))
         (preview (if zencoding-preview-default (not arg) arg))
         (beg (if preview
                  (progn
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (point))
                (when mark-active (region-beginning))))
         (end (if preview
                  (progn
                    (end-of-line)
                    (skip-chars-backward " \t")
                    (point))
                (when mark-active (region-end)))))
    (if beg
        (progn
          (goto-char here)
          (zencoding-preview beg end))
      (let ((expr (zencoding-expr-on-line)))
        (if expr
            (let ((markup (zencoding-transform (first expr))))
              (when markup
                (let ((pretty (zencoding-prettify markup (current-indentation))))
                  (save-excursion
                    (delete-region (second expr) (third expr))
                    (zencoding-insert-and-flash pretty))))))))))

(defvar zencoding-mode-keymap nil
  "Keymap for zencode minor mode.")

(if zencoding-mode-keymap
    nil
  (progn
    (setq zencoding-mode-keymap (make-sparse-keymap))
    (define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-line)
    (define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-line)))

;;;###autoload
(define-minor-mode zencoding-mode
  "Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'."
  :lighter " Zen"
  :keymap zencoding-mode-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding yasnippet integration

(defun zencoding-transform-yas (input)
  (let* ((leaf-count 0)
         (zencoding-leaf-function
          (lambda ()
            (format "$%d" (incf leaf-count)))))
    (zencoding-transform input)))

;;;###autoload
(defun zencoding-expand-yas ()
  (interactive)
  (let ((expr (zencoding-expr-on-line)))
    (if expr
        (let* ((markup (zencoding-transform-yas (first expr)))
               (filled (replace-regexp-in-string "><" ">\n<" markup)))
          (delete-region (second expr) (third expr))
          (insert filled)
          (indent-region (second expr) (point))
          (yas/expand-snippet
           (buffer-substring (second expr) (point))
           (second expr) (point))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Real-time preview
;;

;;;;;;;;;;
;; Lennart's version

(defvar zencoding-preview-input nil)
(make-local-variable 'zencoding-preview-input)
(defvar zencoding-preview-output nil)
(make-local-variable 'zencoding-preview-output)
(defvar zencoding-old-show-paren nil)
(make-local-variable 'zencoding-old-show-paren)

(defface zencoding-preview-input
  '((default :box t :inherit secondary-selection))
  "Face for preview input field."
  :group 'zencoding)

(defface zencoding-preview-output
  '((default :inherit highlight))
  "Face for preview output field."
  :group 'zencoding)

(defvar zencoding-preview-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'zencoding-preview-accept)
    (define-key map (kbd "<return>") 'zencoding-preview-accept)
    (define-key map [(control ?g)] 'zencoding-preview-abort)
    map))

(defun zencoding-preview-accept ()
  (interactive)
  (let ((ovli zencoding-preview-input))
    (if (not (and (overlayp ovli)
                  (bufferp (overlay-buffer ovli))))
        (message "Preview is not active")
      (let* ((indent (current-indentation))
             (markup (zencoding-preview-transformed indent)))
        (when markup
          (delete-region (line-beginning-position) (overlay-end ovli))
          (zencoding-insert-and-flash markup)))))
  (zencoding-preview-abort))

(defvar zencoding-flash-ovl nil)
(make-variable-buffer-local 'zencoding-flash-ovl)

(defun zencoding-remove-flash-ovl (buf)
  (with-current-buffer buf
    (when (overlayp zencoding-flash-ovl)
      (delete-overlay zencoding-flash-ovl))
    (setq zencoding-flash-ovl nil)))

(defcustom zencoding-preview-default t
  "If non-nil then preview is the default action.
This determines how `zencoding-expand-line' works by default."
  :type 'boolean
  :group 'zencoding)

(defcustom zencoding-insert-flash-time 0.5
  "Time to flash insertion.
Set this to a negative number if you do not want flashing the
expansion after insertion."
  :type '(number :tag "Seconds")
  :group 'zencoding)

(defun zencoding-insert-and-flash (markup)
  (zencoding-remove-flash-ovl (current-buffer))
  (let ((here (point)))
    (insert markup)
    (setq zencoding-flash-ovl (make-overlay here (point)))
    (overlay-put zencoding-flash-ovl 'face 'zencoding-preview-output)
    (when (< 0 zencoding-insert-flash-time)
      (run-with-idle-timer zencoding-insert-flash-time
                           nil 'zencoding-remove-flash-ovl (current-buffer)))))

;;;###autoload
(defun zencoding-preview (beg end)
  "Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it."
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (zencoding-preview-abort)
  (if (not beg)
      (message "Region not active")
    (setq zencoding-old-show-paren show-paren-mode)
    (show-paren-mode -1)
    (let ((here (point)))
      (goto-char beg)
      (forward-line 1)
      (unless (= 0 (current-column))
        (insert "\n"))
      (let* ((opos (point))
             (ovli (make-overlay beg end nil nil t))
             (ovlo (make-overlay opos opos))
             (info (propertize " Zen preview. Choose with RET. Cancel by stepping out. \n"
                               'face 'tooltip)))
        (overlay-put ovli 'face 'zencoding-preview-input)
        (overlay-put ovli 'keymap zencoding-preview-keymap)
        (overlay-put ovlo 'face 'zencoding-preview-output)
        (overlay-put ovlo 'before-string info)
        (setq zencoding-preview-input  ovli)
        (setq zencoding-preview-output ovlo)
        (add-hook 'before-change-functions 'zencoding-preview-before-change t t)
        (goto-char here)
        (add-hook 'post-command-hook 'zencoding-preview-post-command t t)))))

(defvar zencoding-preview-pending-abort nil)
(make-variable-buffer-local 'zencoding-preview-pending-abort)

(defun zencoding-preview-before-change (beg end)
  (when
      (or (> beg (overlay-end zencoding-preview-input))
          (< beg (overlay-start zencoding-preview-input))
          (> end (overlay-end zencoding-preview-input))
          (< end (overlay-start zencoding-preview-input)))
    (setq zencoding-preview-pending-abort t)))

(defun zencoding-preview-abort ()
  "Abort zen code preview."
  (interactive)
  (setq zencoding-preview-pending-abort nil)
  (remove-hook 'before-change-functions 'zencoding-preview-before-change t)
  (when (overlayp zencoding-preview-input)
    (delete-overlay zencoding-preview-input))
  (setq zencoding-preview-input nil)
  (when (overlayp zencoding-preview-output)
    (delete-overlay zencoding-preview-output))
  (setq zencoding-preview-output nil)
  (remove-hook 'post-command-hook 'zencoding-preview-post-command t)
  (when zencoding-old-show-paren (show-paren-mode 1)))

(defun zencoding-preview-post-command ()
  (condition-case err
      (zencoding-preview-post-command-1)
    (error (message "zencoding-preview-post: %s" err))))

(defun zencoding-preview-post-command-1 ()
  (if (and (not zencoding-preview-pending-abort)
           (<= (point) (overlay-end zencoding-preview-input))
           (>= (point) (overlay-start zencoding-preview-input)))
      (zencoding-update-preview (current-indentation))
    (zencoding-preview-abort)))

(defun zencoding-preview-transformed (indent)
  (let* ((string (buffer-substring-no-properties
		  (overlay-start zencoding-preview-input)
		  (overlay-end zencoding-preview-input))))
    (let ((output (zencoding-transform string)))
      (when output
        (zencoding-prettify output indent)))))

(defun zencoding-update-preview (indent)
  (let* ((pretty (zencoding-preview-transformed indent))
         (show (when pretty
                 (propertize pretty 'face 'highlight))))
    (when show
      (overlay-put zencoding-preview-output 'after-string
                   (concat show "\n")))))

(provide 'zencoding-mode)

;;; zencoding-mode.el ends here
