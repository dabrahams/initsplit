;;; initsplit --- code to split customizations into different files

;; Copyright (C) 2000, 2001 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created:  8 Feb 2000
;; Version: 1.6
;; Keywords: lisp
;; X-URL: http://www.gci-net.com/users/j/johnw/emacs.html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file allows you to split Emacs customizations (set via M-x
;; customize) into different files, based on the names of the
;; variables.  It uses a regexp to match against each face and
;; variable name, and associates with a file that the variable should
;; be stored in.

;; To use it, just load the file in your .emacs:
;;
;;   (load "initsplit")
;;
;; If you want configuration files byte-compiled, add this after it:
;;
;;   (add-hook 'after-save-hook 'initsplit-byte-compile-files t)

;; Note that that you *must* load each file that contains your various
;; customizations from your .emacs.  Otherwise, the variables won't
;; all be set, and the next time you use the customize interface, it
;; will delete the settings in those other files.

;; Then, customize the variable `initsplit-customizations-alist', to
;; associate various configuration names with their respective
;; initialization files.

;; I find this module most useful for splitting up Gnus and Viper
;; customizations.

;;; History:

;;; Code:

(defconst initsplit-version "1.7"
  "This version of initsplit.")

(defgroup initsplit nil
  "Code to split customizations into different files."
  :group 'initialization)

;;; User Variables:

(defcustom initsplit-load-hook nil
  "*A hook that gets run after \"initsplit.el\" has been loaded."
  :type 'hook
  :group 'initsplit)

(defcustom initsplit-customizations-alist nil
  "*An alist that describes how to split up init file customizations."
  :type '(repeat
	  (list (regexp  :tag "Var regexp")
		(file    :tag "Custom file")
		(boolean :tag "Byte-compile")))
  :group 'initsplit)

;;; User Functions:

(defadvice custom-save-all (around initsplit-custom-save-all activate compile preactivate)
"Wrapper over custom-save-all that saves customizations into
multiple files per initsplit-customizations-alist"
  ;; Store up the saved-value properties of all symbols
  ;; and remember that we haven't saved them yet
  (mapatoms 
   (lambda (symbol) 
     (if (put symbol 'initsplit-saved-value (get symbol 'saved-value))
         (put symbol 'initsplit-unsaved-p t))))

  (unwind-protect

      ;; For each customization file, save appropriate symbols
      (dolist (s (append initsplit-customizations-alist 
                         `(("" ,(initsplit-custom-file)))))
        (let ((custom-file (cadr s)))

          ;; as-yet-unsaved symbols that match the regexp
          ;; get a saved-value property.  Others don't
          (mapatoms 
           (lambda (symbol)
             (put symbol 'saved-value 
                  (and (get symbol 'initsplit-unsaved-p)
                       (string-match (car s) (symbol-name symbol))
                       (progn (put symbol 'initsplit-unsaved-p nil)
                              (get symbol 'initsplit-saved-value))))))

          ad-do-it
          ))

    ;; Cleanup; restore the saved-value properties
    (mapatoms 
     (lambda (symbol) 
       (put symbol 'saved-value (get symbol 'initsplit-saved-value))
       (put symbol 'initsplit-saved-value nil)))))

(defun initsplit-current-file-truename ()
  (file-truename (buffer-file-name (current-buffer))))

(defun initsplit-custom-file ()
  (or custom-file user-init-file))

(defun initsplit-in-file-p (file)
  (string= (file-truename file) (initsplit-current-file-truename)))

(defun initsplit-in-custom-file-p ()
  (initsplit-in-file-p (initsplit-custom-file)))

(defun initsplit-byte-compile-current ()
  (byte-compile-file (initsplit-current-file-truename)))

(defun initsplit-byte-compile-files ()
  (if (initsplit-in-custom-file-p)
      (initsplit-byte-compile-current)
    (let ((cal initsplit-customizations-alist))
      (while cal
	(if (and (nth 2 (car cal))
		 (initsplit-in-file-p (nth 1 (car cal))))
	    (initsplit-byte-compile-current))
	(setq cal (cdr cal))))))

;;(add-hook 'after-save-hook 'initsplit-byte-compile-files t)

;;; Internal Functions:

(provide 'initsplit)

(run-hooks 'initsplit-load-hook)

;;; initsplit.el ends here
