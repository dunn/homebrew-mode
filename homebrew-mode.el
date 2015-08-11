;;; homebrew-mode.el --- minor mode for editing Homebrew formulae

;; Copyright (C) 2015 Alex Dunn

;; Author: Alex Dunn <dunn.alex@gmail.com>
;; URL:
;; Version: 0.1.0
;; Package-Requires: ()
;; Keywords: homebrew brew ruby
;; Prefix: homebrew

;;; Commentary:

;; minor mode for editing Homebrew formula!

;;; Code:

(defconst homebrew-mode-version "0.1.0")

(defun homebrew-autotools ()
  "For HEAD builds."
  ;; TODO: search source dir for autogen.sh/bootstrap and check if
  ;; libtool is required or not.
  (interactive)
  (insert "depends_on \"automake\" => :build\n"
    "    depends_on \"autoconf\" => :build\n"
    "    depends_on \"libtool\" => :build"))

;;;###autoload
(defun homebrew-mode-start ()
  "Activate homebrew-mode."
  (interactive)
  (homebrew-mode 1))

;; TODO: make defcustom
(defconst homebrew-formula-file-patterns
  '( ".*\/homebrew-[^\/]*\/[^\/]*\.rb$"
     ".*\/Formula\/[^\/]*\.rb$"
     ".*\/HomebrewFormula\/[^\/]*\.rb$" ))

(defun homebrew--formula-file-p (buffer-or-string)
  "Return true if BUFFER-OR-STRING is:

1. A buffer visiting a formula file;
2. The filename (a string) of a formula file.

Otherwise return nil."
  ;; If thing is a buffer, convert it to a filename string
  (if (bufferp buffer-or-string)
    (setq buffer-or-string (buffer-file-name buffer-or-string)))
  ;; Now if thing isn't a string we can return nil
  (if (not (stringp buffer-or-string))
      nil
    (let (match)
      (dolist (elem homebrew-formula-file-patterns match)
        (if (string-match elem buffer-or-string)
          (setq match t))))))

;;;###autoload
(defun homebrew-mode-default-hooks ()
  "Register hooks for homebrew-mode."
  (add-hook 'find-file-hook
    (lambda ()
      (if (homebrew--formula-file-p (current-buffer))
        (homebrew-mode 1)))))

;;;###autoload
(define-minor-mode homebrew-mode
  "Helper functions for editing Homebrew formulae"
  :lighter "Brew")

(provide 'homebrew-mode)

;;; homebrew-mode.el ends here
