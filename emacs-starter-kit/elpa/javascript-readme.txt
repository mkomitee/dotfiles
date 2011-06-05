;;; Commentary:
;;
;; The main features of this JavaScript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments.
;;
;; This package has (only) been tested with GNU Emacs 21.4 (the latest
;; stable release).
;;
;; Installation:
;;
;; Put this file in a directory where Emacs can find it (`C-h v
;; load-path' for more info). Then add the following lines to your
;; Emacs initialization file:
;; 
;;    (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;;    (autoload 'javascript-mode "javascript" nil t)
;;    
;; General Remarks:
;; 
;; This mode assumes that block comments are not nested inside block
;; comments and that strings do not contain line breaks.
;; 
;; Exported names start with "javascript-" whereas private names start
;; with "js-".
;; 
;; Changes:
;;
;; See javascript.el.changelog.

