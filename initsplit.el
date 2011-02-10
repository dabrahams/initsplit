;;; initsplit --- code to split customizations into different files

;; Copyright (C) 2000, 2001 John Wiegley
;; Copyright (C) 2010       Dave Abrahams

;; Author: John Wiegley <johnw@gnu.org>, Dave Abrahams <dave@boostpro.com>
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

(require 'cl)
(require 'find-func)

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
  "*Alist of (REGEXP FILE BYTECOMP PRE-LOAD)

Variables and faces matching REGEXP will be written to FILE.

If BYTECOMP is nil, `initsplit-byte-compile-files' will
not byte-compile FILE.

If PRE-LOAD is nil, initsplit will not try to ensure FILE is
loaded at startup."
  :type '(repeat
	  (list (regexp  :tag "Var regexp")
		(file    :tag "Custom file")
		(boolean :tag "Byte-compile")
		(boolean :tag "Pre-load" :value t)))
  :group 'initsplit)

(defvar initsplit-dynamic-customizations-alist nil
  "Alist that is appended to `initsplit-customizations-alist'
when customizations are saved.  

Used to programmatically add initsplit files that are not to be
saved as part of the customization of
`initsplit-customizations-alist' itself.  Note: Elements of
`initsplit-customizations-alist' take precedence.")

(defcustom initsplit-default-directory
  (file-name-as-directory user-emacs-directory)
  "*The directory for writing new customization files and the
first place initsplit will look when loading a customization file
specified with a non-absolute path"
  :group 'initsplit
  :type 'directory)


(defcustom initsplit-load-before-customizing 'ask
  "*Determines what is done with known customization files that
haven't yet been loaded."
  :group 'initsplit
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "Ask" ask)))

;;; User Functions:

;;; Helper Functions:

(defun initsplit-filter (list pred)
  "Return the subset of LIST that satisfies PRED"  
  (reduce (lambda (elt lst) (if (funcall pred elt) (cons elt lst) lst))
          list :from-end t :initial-value nil))

(defun initsplit-custom-alist ()
  "Return an alist of (PATTERN, FILE) pairs containing all
customization FILEs and the PATTERNs matching variable values
they store, accounting for initsplit-customizations-alist,
initsplit-dynamic-customizations-alist, and custom-file"
  (append initsplit-customizations-alist 
          initsplit-dynamic-customizations-alist
          `(("" ,(initsplit-custom-file)))))

(defun initsplit-customizations-subset (file-pred)
  "Return the subset of `(initsplit-custom-alist)' whose
FILE element satisfies FILE-PRED"
  (initsplit-filter (initsplit-custom-alist)
                    (lambda (s) (funcall file-pred (initsplit-filename s)))))

(defun initsplit-preload-p (filespec)
  "Return non-nil if the file given by filespec should be preloaded."
  (cadddr filespec))

(defun initsplit-filename (filespec)
  "Return the absolute path to the file associated with the
`(initsplit-custom-alist)' element FILESPEC"
  (let* ((file (cadr filespec))
         (default-directory initsplit-default-directory)
         (load-path (cons default-directory load-path)))
    (condition-case nil
        (find-library-name file)
        (error (file-truename file)))))

;;
;; Protection against overwriting valuable customizations
;;
(defun initsplit-writable-p (file)
  "Return non-nil iff the file named by FILE has been
loaded or does not exist"
  (or (not (file-exists-p file))
      (load-history-filename-element (load-history-regexp file))))

(defun initsplit-writable-alist ()
  "Return the subset of `(initsplit-custom-alist)' that we
can write safely (without lossage)"
  (initsplit-customizations-subset 'initsplit-writable-p))

(defun initsplit-non-writable-alist ()
  "Return the subset of `(initsplit-custom-alist)' that
can't be safely written"
  (set-difference
   (initsplit-custom-alist) (initsplit-writable-alist) :test 'equal))

(defun initsplit-pre-customize ()
  "Give the user a chance to load not-yet-loaded customization files."
    (map-y-or-n-p 
     (lambda (f) (if (eq initsplit-load-before-customizing 'ask)
                     (format "Load %S before customizing (answer `y' if you'll be changing the settings stored there)?
You can suppress this message by customizing \
`initsplit-load-before-customizing' or by setting the file to \
pre-load in `initsplit-customizations-alist'" f)
                   initsplit-load-before-customizing)) 
     'load
     (mapcar 'initsplit-filename (initsplit-non-writable-alist))
     '("customization file" "customization files" "load")))

(add-hook 'Custom-mode-hook 'initsplit-pre-customize)

(when nil
;;
;; initsplit customization file versioning
;;
(defun initsplit-written-by-version ()
  "Returns the version of initsplit that wrote customizations
into the current buffer, or `\"1.0\"' for versions predating 1.7"
  (or (bound-and-true-p initsplit-written-by-version) "1.0"))

(put 'initsplit-written-by-version 'safe-local-variable 'stringp)

(defadvice custom-save-faces (after initsplit-write-version
                                    activate compile preactivate)
  (add-file-local-variable-prop-line
   'initsplit-written-by-version initsplit-version))
)
  
;;
;; Where the hard work is done
;;
(defadvice custom-save-all (around initsplit-custom-save-all 
                                   activate compile preactivate)
  "Wrapper over custom-save-all that saves customizations into
multiple files per (initsplit-custom-alist)"

  ;; Store up the saved-value/face properties of all symbols
  ;; and remember that we haven't saved them yet
  (mapatoms 
   (lambda (symbol) 
     (when (or
            (put symbol 'initsplit-saved-value (get symbol 'saved-value))
            (put symbol 'initsplit-saved-face (get symbol 'saved-face)))
       (put symbol 'initsplit-unsaved-p t))))

  (unwind-protect

      ;; For each customization file, save appropriate symbols
      (dolist (s (initsplit-custom-alist))

        (let ((custom-file (initsplit-filename s)))

          ;; As-yet-unsaved symbols that match the regexp
          ;; get a saved-value/face property.  Others get nil.
          (mapatoms 

           (lambda (symbol)
             (if (and (get symbol 'initsplit-unsaved-p)
                      (string-match (car s) (symbol-name symbol)))
                 (progn
                   (put symbol 'saved-value (get symbol 'initsplit-saved-value))
                   (put symbol 'saved-face (get symbol 'initsplit-saved-face)))
               (put symbol 'saved-value nil)
               (put symbol 'saved-face nil))))

          ad-do-it))

    ;; Cleanup: restore the saved-value properties
    (mapatoms 
     (lambda (symbol) 
       (put symbol 'saved-value (get symbol 'initsplit-saved-value))
       (put symbol 'saved-face (get symbol 'initsplit-saved-face))
       (put symbol 'initsplit-saved-value nil)
       (put symbol 'initsplit-saved-face nil)))))

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
    (let ((cal (initsplit-custom-alist)))
      (while cal
	(if (and (nth 2 (car cal))
		 (initsplit-in-file-p (nth 1 (car cal))))
	    (initsplit-byte-compile-current))
	(setq cal (cdr cal))))))

;; (add-hook 'after-save-hook 'initsplit-byte-compile-files t)

;;; Internal Functions:

(defconst initsplit-load-suffix-regexp
  (concat (mapconcat 'regexp-quote (get-load-suffixes) "\\|") "\\'"))

(defun initsplit-strip-lisp-suffix (path)
  (replace-regexp-in-string initsplit-load-suffix-regexp "" path))

(provide 'initsplit)

;; Ensure customization files marked pre-load have been loaded
;; already.
(dolist (s (initsplit-non-writable-alist))
  (when (and (initsplit-preload-p s)
             (file-exists-p (initsplit-filename s)))
    (load (initsplit-strip-lisp-suffix (cadr s)))))
  
(run-hooks 'initsplit-load-hook)

;;; initsplit.el ends here
