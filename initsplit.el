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

(require 'cl)
(require 'cus-edit)

(defconst initsplit-version "1.7"
  "This version of initsplit.")

(defconst initsplit-loaded-files-introduced "1.7"
  "This version of initsplit where we introduced the recording of
  which customization files were loaded.")

(defgroup initsplit nil
  "Code to split customizations into different files."
  :group 'initialization)

;;; User Variables:

(defcustom initsplit-load-hook nil
  "*A hook that gets run after \"initsplit.el\" has been loaded."
  :type 'hook
  :group 'initsplit)

(defcustom initsplit-customizations-alist nil
  "*Alist of (REGEXP FILE BYTECOMP LOAD-STYLE)

REGEXP determines which variables and faces will be written to FILE.
BYTECOMP determines whether `initsplit-byte-compile-files' will
         byte-compile or skip FILE.
LOAD-STYLE determines how/when FILE will be loaded:

* `'eagerly': loaded by initsplit when it is loaded

* `'lazily': loaded by some other means (e.g. eval-after-load),
  but before any customizations are written out to disk,
  initsplit will load it automatically if it hasn't already been
  loaded, to avoid losing settings.

* `'manually': loaded by other means.  If it isn't /known/ to
  have been loaded by the time customizations are written to
  disk, its customizations will be backed up, and then *erased*.
  Legacy customization files should be transitioned to eager or
  lazy loading ASAP to avoid losing settings.
"
  :type '(repeat
	  (list (regexp  :tag "Var regexp")
		(file    :tag "Custom file")
		(boolean :tag "Byte-compile")
                (menu-choice :tag "Load this file:" 
                             :value 'eager
                             :notify (lambda (widget &rest ignore)
                                       (message "%S" (widget-value widget))
                                       (unless (widget-value widget)
                                         (warn "Manual loading is only here to support legacy configurations and not recommended; please choose one of the other settings.
`C-h v initsplit-customizations-alist RET' for more details
")))
                             (item :tag "eagerly" :value 'eager)
                             (item :tag "lazily" :value 'lazy)
                             (item :tag "manually (change me)" :value nil)
                             )))
  :group 'initsplit)

;;; User Functions:

(defun initsplit-load-behavior (filespec)
  "Return a list containing the load-behavior element of
customization tuple FILESPEC, or nil if FILESPEC has no
load-behavior set."
  (cadddr filespec))


;;
;; Keeping a record of already-loaded customization files
;;
(defvar initsplit-loaded-files nil
  "A list of known loaded customization files

If a legacy customization file is loaded before initsplit, it won't
appear in this list until it is loaded again.")

(defun initsplit-loaded-files-customize ()
  "Ensure the initialization form of `initsplit-loaded-files'
gets immediately evaluated upon loading custom-file.  Called just
before saving custom-file"

  (put 'initsplit-loaded-files 'initsplit-unsaved-p t)

  ;; Make initsplit-loaded-files look like a rogue customization.  When
  ;; the customization is loaded, customize will *IMMEDIATELY* evaluate
  ;; its customization expression, thereby recording that the
  ;; customization file was loaded.  See the cus-edit library for more
  ;; on rogue customizations.
  (dolist
      (p-val '((standard-value . nil)
              (custom-autoload . nil)
              (saved-value 
               . ((let ((l (bound-and-true-p initsplit-loaded-files)))
                    (add-to-list 'l (file-truename load-file-name)))))
              (saved-variable-comment "This variable was customized by \
initsplit. Do not try to tinker with it manually!")))
    (put 'initsplit-loaded-files (car p-val) (cdr p-val))))

;;
;; initsplit-written-by-version
;;
(defun initsplit-written-by-version ()
  "Returns the version of initsplit that wrote customizations
into the current buffer, or `\"1.0\"' for versions predating 1.7"
  (or (bound-and-true-p initsplit-written-by-version) "1.0"))

(put 'initsplit-written-by-version 'safe-local-variable 'stringp)

;;
;; Saving customizations
;;
(defun initsplit-back-up-legacy-file (f)
  (let* ((backup-directory-alist nil)
         (version-control t)
         (backup (car (find-backup-file-name f))))

    (warn "Backed up %S to %S.  If you choose not to load now, \
you can compare these two files to be sure you haven't lost any \
important settings." f backup)
    (copy-file f backup))
  t)

(defun initsplit-load (file)
  "Load FILE and remember that we did it."
  (load file)
  (add-to-list 'initsplit-loaded-files file))

(defun initsplit-load-all ()
  "Load any not-yet-loaded customization files to be sure their
customizations won't be lost when they are written.  Return a
list of newly-created buffers that can be killed after
customizations are saved.

\"Legacy\" customization files written prior to the introduction
of init file load detection are either loaded or backed up, based
on the user's interactive selection."
  ;; Anyone using custom-file already has to load it explicitly,
  ;; so nothing to worry about here.
;  (add-to-list 'initsplit-loaded-files (file-truename (initsplit-custom-file)))

  (let (buffers-to-kill legacy-files)

    (condition-case err
        (progn
          (dolist (s initsplit-customizations-alist)
            (let* ((f (file-truename (cadr s)))
                   (buffer-existed (get-file-buffer f)))

              (with-current-buffer (find-file-noselect f)
                ;; mark the buffer for later cleanup
                (unless buffer-existed
                  (push (current-buffer) buffers-to-kill))

                (when (and (file-exists-p f) 
                           (not (member f initsplit-loaded-files)))
                  
                  (if (version< (initsplit-written-by-version) 
                                initsplit-loaded-files-introduced)
                      (push f legacy-files)
                    (initsplit-load f))))))

          (map-y-or-n-p "Warning: I can't tell whether legacy \
customization file %S has been loaded yet!
* If it hasn't, saving customizations may overwrite its settings.
* On the other hand, it may not be safe to load it again

[Once the file has been saved, I won't have to ask you this anymore]
(Re-)Load it now? " 
                        'initsplit-load legacy-files 
                        '("customization file" "customization files" "load")
                        `((?b initsplit-back-up-legacy-file 
                              "make a backup instead of loading it"))))
          
      (error (mapc 'kill-buffer buffers-to-kill)
             (signal (car err) (cdr err))))        ;; re-raise err

    buffers-to-kill))

;; Make sure customizations are loaded before doing any customization!
(add-hook 'Custom-mode-hook 'initsplit-load-all)

(defadvice custom-save-faces (after initsplit-write-version
                                    activate compile preactivate)

  ;; Record the initsplit version in the file
  (add-file-local-variable-prop-line
   'initsplit-written-by-version initsplit-version))
  
(defadvice custom-save-all (around initsplit-custom-save-all 
                                   activate compile preactivate)
  "Wrapper over custom-save-all that saves customizations into
multiple files per initsplit-customizations-alist"

  ;; Store up the saved-value properties of all symbols
  ;; and remember that we haven't saved them yet
  (mapatoms 
   (lambda (symbol) 
     (if (put symbol 'initsplit-saved-value (get symbol 'saved-value))
         (put symbol 'initsplit-unsaved-p t))))

  (let ((buffers-to-kill (initsplit-load-all)))
    (unwind-protect

        ;; For each customization file, save appropriate symbols
        (dolist (s (append initsplit-customizations-alist 
                           `(("" ,(initsplit-custom-file)))))
          (let ((custom-file (cadr s)))

            ;; As-yet-unsaved symbols that match the regexp
            ;; get a saved-value property.  Others get nil.
            (mapatoms 
             (lambda (symbol)
               (put symbol 'saved-value 
                    (and (get symbol 'initsplit-unsaved-p)
                         (string-match (car s) (symbol-name symbol))
                         (progn (put symbol 'initsplit-unsaved-p nil)
                                (get symbol 'initsplit-saved-value))))))

            ;; Every file gets a special customization for this
            ;; variable
            (initsplit-loaded-files-customize)

            ad-do-it))

      ;; Cleanup: restore the saved-value properties
      (mapatoms 
       (lambda (symbol) 
         (put symbol 'saved-value (get symbol 'initsplit-saved-value))
         (put symbol 'initsplit-saved-value nil)))

      ;; Cleanup: kill extra buffers
      (mapc 'kill-buffer buffers-to-kill))))

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

(defconst initsplit-load-suffix-regexp
  (concat (mapconcat 'regexp-quote (get-load-suffixes) "\\|") "\\'"))

(defun initsplit-strip-lisp-suffix (path)
  (replace-regexp-in-string elhome-load-suffix-regexp "" path))

(provide 'initsplit)

;; Load eagerly-loaded customization files that haven't been loaded
;; already.
(dolist (s initsplit-customizations-alist)
  (when (and (eq (initsplit-load-behavior s) 'eager)
             (not (member (file-truename (cadr s)) initsplit-loaded-files)))
      (initsplit-load (initsplit-strip-lisp-suffix (cadr s)))))
  
(run-hooks 'initsplit-load-hook)

;;; initsplit.el ends here
