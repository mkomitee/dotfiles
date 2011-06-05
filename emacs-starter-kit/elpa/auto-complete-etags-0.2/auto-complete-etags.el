;;; auto-complete-etags.el --- Auto-complete etags

;; Copyright 2009 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee
;; Version: 0.2
;; $Id: auto-complete-etags.el,v 0.2 2009/04/23 00:38:01 coldnew Exp $
;; Keywords: 
;; X-URL: not distributed yet
;; Package-Requires: ((auto-complete "1.3"))


;; Change Log:
;; 08-Feb-2011    Matthew L. Fidler  
;;    Changed to match ELPA standards



;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-etags)

;;; Code:

(provide 'auto-complete-etags)
(require 'auto-complete)
(eval-when-compile
  (require 'cl))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(defvar ac-source-etags
  '((candidates . (lambda () 
         (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (requires . 3))
  "Source for etags.")

(provide 'auto-complete-etags)
;;; auto-complete-etags.el ends here
