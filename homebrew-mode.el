;;; homebrew-mode.el --- minor mode for editing Homebrew formulae

;; Copyright (C) 2015 Alex Dunn

;; Author: Alex Dunn <dunn.alex@gmail.com>
;; URL: https://github.com/dunn/homebrew-mode
;; Version: 0.4.1
;; Package-Requires: ()
;; Keywords: homebrew brew ruby
;; Prefix: homebrew

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; # hombrew-mode

;; Emacs minor mode for editing [Homebrew](http://brew.sh) formulae.

;; ## setup

;; ```elisp
;; (add-to-list 'load-path "/where/is/homebrew-mode")
;; (require 'homebrew-mode)
;; (global-homebrew-mode)
;; ```

;; ## keys and commands

;; The command prefix is <kbd>C-c C-h</kbd>.  These are the commands currently mapped to it:

;; - <kbd>C-c C-h f</kbd>: Download the source file(s) for the formula
;;   in the current buffer.

;; - <kbd>C-c C-h u</kbd>: Download and unpack the source file(s) for the formula
;;   in the current buffer.

;; - <kbd>C-c C-h i</kbd>: Install the formula in the current buffer.

;; - <kbd>C-c C-h r</kbd>: Uninstall the formula in the current buffer.

;; - <kbd>C-c C-h t</kbd>: Run the test for the formula in the current buffer.

;; - <kbd>C-c C-h a</kbd>: Audit the formula in the current buffer.

;; - <kbd>C-c C-h c</kbd>: Open a dired buffer in the Homebrew cache
;;   (default `/Library/Caches/Homebrew`).

;; - <kbd>C-c C-h d</kbd>: Add `depends_on` lines for the specified
;;   formulae.  Call with one prefix (<kbd>C-u</kbd>) argument to make
;;   them build-time dependencies; call with two (<kbd>C-u C-u</kbd>) for
;;   run-time.

;; - <kbd>C-c C-h p</kbd>: Insert Python `resource` blocks (requires poet,
;;   installed with `pip install homebrew-pypi-poet`).

;; ## custom variables

;; - If you’re using Linuxbrew or a non-standard prefix on Mac OS, you’ll
;;   need to update `homebrew-prefix` to point at your `brew –-prefix`.

;; - If you’re using Linuxbrew or have your cache in a non-standard
;;   location on Mac OS, update `homebrew-cache-dir`.

;; - If you want to turn on whitespace-mode when editing formulae that
;;   have inline patches, set `homebrew-patch-whitespace-mode` to
;;   `t`. It’s off by default since it looks ugly.

;;; Requires:

(require 'dired)
(require 'diff-mode)
(require 'whitespace)

;;; Code:

(defconst homebrew-mode-version "0.4.1")

;; Customization

(defgroup homebrew-mode nil
  "Minor mode for editing Homebrew formulae."
  :group 'ruby)

;; Most of the keymap stuff discovered through studying flycheck.el,
;; so ty lunaryorn.
(defcustom homebrew-mode-keymap-prefix (kbd "C-c C-h")
  "Prefix for homebrew-mode key bindings."
  :group 'homebrew-mode
  :type 'string
  :risky t)

(defcustom homebrew-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a"     #'homebrew-brew-audit)
    (define-key map "c"     #'homebrew-pop-to-cache)
    (define-key map "d"     #'homebrew-add-deps)
    (define-key map "f"     #'homebrew-brew-fetch)
    (define-key map "i"     #'homebrew-brew-install)
    (define-key map "p"     #'homebrew-poet-insert)
    (define-key map "r"     #'homebrew-brew-uninstall)
    (define-key map "t"     #'homebrew-brew-test)
    (define-key map "u"     #'homebrew-brew-unpack)
    map)
  "Keymap for `homebrew-mode` commands prefixed by homebrew-mode-keymap-prefix."
  :group 'homebrew-mode
  :type 'string)

(defvar homebrew-mode-map
  ;; define-key doesn't return the map, so we need a `let`
  (let ((map (make-sparse-keymap)))
    (define-key map homebrew-mode-keymap-prefix homebrew-mode-command-map)
    map)
  "Keymap for `homebrew-mode`.")

(defcustom homebrew-prefix "/usr/local"
  "The base of your Homebrew installation.  May be different on your system."
  :group 'homebrew-mode
  :type 'string)

(defvar homebrew-executable (concat homebrew-prefix "/bin/brew"))

(defcustom homebrew-cache-dir "/Library/Caches/Homebrew/"
  "The cache directory for Homebrew."
  :group 'homebrew-mode
  :type 'string)

(defcustom homebrew-formula-file-patterns
  '( ".*\/homebrew-[^\/]*\/[^\/]*\.rb$"
     ".*\/Formula\/[^\/]*\.rb$"
     ".*\/HomebrewFormula\/[^\/]*\.rb$" )
  "Regular expressions matching Homebrew formulae files.

If you edit this variable, make sure the new value passes the formula-detection tests."
  :group 'homebrew-mode
  :type 'list
  :risky t)

(defcustom homebrew-patch-whitespace-mode nil
  "Turn on `whitespace-mode' when editing formulae with inline patches."
  :group 'homebrew-mode
  :type 'boolean)

(defcustom homebrew-poet-executable nil
  "Path to `poet` executable.  Install with `pip install homebrew-pypi-poet`."
  :group 'homebrew-mode
  :type 'string)

;;; Internal functions

;; Extracted from async.el
(defun homebrew--async-alert (process &rest change)
  "Simply displays a notification in the echo area when PROCESS succeeds.
Pop to the process buffer when it fails.
Ignore the CHANGE of state argument passed by `set-process-sentinel'."
  (when (eq 'exit (process-status process))
    (let ( (exit-code (process-exit-status process))
           (proc-name (process-name process)))
      (if (= 0 exit-code)
        (message "%s succeeded" proc-name)
        (progn
          (message "%s failed with %d" proc-name exit-code)
          (pop-to-buffer (concat "*Homebrew: " proc-name "*"))
          ;; if the same command has been run and failed recently, the
          ;; buffer will still be there with point at wherever it was
          ;; (probably at the previous failure), so move point to the
          ;; end in order to avoid confusion.
          (goto-char (point-max)))))))

(defun homebrew--async-unpack-and-jump (process &rest change)
  "Called when the `homebrew-unpack' PROCESS completes.
Unpack and enter the source dir.
Ignore the CHANGE of state argument passed by `set-process-sentinel'."
  (when (eq 'exit (process-status process))
    (let* ( (exit-code (process-exit-status process))
            (proc-name (process-name process))
            ;; is there no replace-in-string?
            (unpack-cmd (replace-regexp-in-string "fetch" "unpack" proc-name)))
      (if (= 0 exit-code)
        ;; * Temporarily set default-directory to the Homebrew cache,
        ;;   so we unpack in the right place.
        ;;
        ;; * Extract the formula name from the name of PROCESS.
        ;;
        ;; * We do the actual unpacking in the let* block, and
        ;;   'result' is the output of `brew unpack`.
        (let* ( (default-directory homebrew-cache-dir)
                (formula (replace-regexp-in-string ".*\\ " "" proc-name))
                (result (shell-command-to-string unpack-cmd))
                ;; * dest-dir will be the location of the unpacked source.
                (dest-dir))
          ;; capture the unpack directory whether it's already been created or not:
          (string-match "^Error: Destination \\(.*\\) already exists\\!*" result)
          (string-match "^==> Unpacking.*to: \\(.*\\)$" result)
          (setq dest-dir (match-string 1 result))
          ;; Add a slash to the end so dired enters the directory
          ;; instead of starting with it under point:
          (dired-jump t (concat dest-dir "/")))
        (message "%s failed with %d" proc-name exit-code)))))

(defun homebrew--start-formula-build-proc (command &optional formula build)
  "Start an instance of `brew COMMAND` \
with the specified FORMULA and BUILD type.  Return the process."

  ;; set 'build' to --stable if not given; it's harmless for commands
  ;; like `info` and `uninstall`, but passing nil or empty strings to
  ;; the args list of `start-process' causes unexpected behavior like
  ;; removing Caskroom ヽ(。_°)ノ
  (let* ( (build (or build "--stable"))
          (command-string (concat "brew " command " -v --build-from-source " build " " formula)) )
    (start-process
      ;; Process name:
      command-string
      ;; Buffer name:
      (concat "*Homebrew: " command-string "*")
      ;; the args passed to the program called by `start-process' have
      ;; to be multiple strings, rather than a list of strings
      homebrew-executable command "-v" "--build-from-source" build formula)))

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

(defun homebrew--formula-from-file (string)
  "Return the name of the formula located at the path specified by STRING.
Return nil if there definitely isn't one."
  (let ((f (and (homebrew--formula-file-p string) (file-exists-p string))))
    ;; f will be nil if STRING isn't a valid pathname
    (if f
      (progn
        (string-match ".*\/\\(.*\\)\\.rb" string)
        (setq f (match-string 1 string))))
    f))

(defun homebrew-add-deps (type &rest formulae)
  "Add `depends_on` lines of TYPE ('run', 'build', or nil) \
for the given FORMULAE.

One prefix argument makes them build-time dependencies.  Two makes them run-time."
  (interactive "P\nMAdd dependencies: ")
  (let ( (indentation (- 2 (current-column)))
         (padding "") )
    (dotimes (_ indentation) (setq padding (concat padding " ")))
    ;; We run `dolist' twice since each element in FORMULAE might itself
    ;; be a list of formulae
    (dolist (fgroup formulae)
      (setq fgroup (split-string fgroup))
      (dolist (formula fgroup)
        (insert padding "depends_on \"" formula "\"")
        (if type
          (progn
            (insert " => :")
            (if (> 5 (car type))
              (insert "build")
              (insert "run"))))
        (if (< 1 (length fgroup))
          (insert "\n"))))))

(defun homebrew-autotools ()
  "Insert autotool deps for HEAD builds."
  ;; TODO: check if libtool is required or not.
  (interactive)
  (let ( (indentation (- 4 (current-column)))
         (padding "") )
    (dotimes (_ indentation) (setq padding (concat padding " ")))
    (insert
      padding "depends_on \"automake\" => :build\n"
      "    depends_on \"autoconf\" => :build\n"
      "    depends_on \"libtool\" => :build")))

(defun homebrew-brew-audit (formula)
  "Run `brew audit --strict --online` on FORMULA."
  (interactive (list (homebrew--formula-from-file buffer-file-name)))
  (message "Auditing %s ..." formula)
  (set-process-sentinel
    (let* ((command-string (concat "brew audit --strict --online " formula)))
      (start-process
        ;; Process name:
        command-string
        ;; Buffer name:
        (concat "*Homebrew: " command-string "*")
        ;; the args passed to the program called by `start-process' have
        ;; to be multiple strings, rather than a list of strings
        homebrew-executable "audit" "--strict" "--online" formula))
    'homebrew--async-alert))

(defun homebrew-brew-fetch (formula build)
  "Download FORMULA to the Homebrew cache, and alert when done.
BUILD may be stable, devel or head."
  (interactive (list (homebrew--formula-from-file buffer-file-name)
                 (read-string "Build type (default stable) " nil nil "stable")))
  (if (not (or (equal build "stable") (equal build "devel") (equal build "HEAD")))
    (error "Allowed build types are \"stable\", \"devel\", and \"HEAD\"")    )
  (message "Downloading %s source of %s ..." build formula)
  (set-process-sentinel
    (homebrew--start-formula-build-proc "fetch" formula (concat "--" build))
    'homebrew--async-alert))

(defun homebrew-brew-install (formula build)
  "Start `brew install FORMULA` (of the specified BUILD) \
in a separate buffer and open a window to that buffer."
  (interactive (list (homebrew--formula-from-file buffer-file-name)
                 (read-string "Build type (default stable) " nil nil "stable")))
  (if (not (or (equal build "stable") (equal build "devel") (equal build "HEAD")))
    (error "Allowed build types are \"stable\", \"devel\", and \"HEAD\"")    )
  (set-process-sentinel
    (homebrew--start-formula-build-proc "install" formula (concat "--" build))
    'homebrew--async-alert)
  ;; This is instead of `pop-to-buffer' since we don't want the install buffer activated
  (let ((install-window (if (= 1 (length (window-list)))
                            (split-window-sensibly)
                          (next-window))))
    (with-selected-window install-window
      (switch-to-buffer (concat "*Homebrew: brew install -v --build-from-source --" build " " formula "*")))))

(defun homebrew-brew-test (formula build)
  "Test FORMULA and alert when done.  BUILD may be stable, devel or head."
  (interactive (list (homebrew--formula-from-file buffer-file-name)
                 (read-string "Build type (default stable) " nil nil "stable")))
  (if (not (or (equal build "stable") (equal build "devel") (equal build "HEAD")))
    (error "Allowed build types are \"stable\", \"devel\", and \"HEAD\"")    )
  (message "Testing %s build of %s ..." build formula)
  (set-process-sentinel
    (homebrew--start-formula-build-proc "test" formula (concat "--" build))
    'homebrew--async-alert))

(defun homebrew-brew-uninstall (formula)
  "Uninstall FORMULA , and alert when done."
  (interactive (list (homebrew--formula-from-file buffer-file-name)))
  (message "Uninstalling %s ..." formula)
  (set-process-sentinel
    (homebrew--start-formula-build-proc "uninstall" formula)
    'homebrew--async-alert))

(defun homebrew-brew-unpack (formula build)
  "Download FORMULA to the Homebrew cache, then unpack and open in a new window.
BUILD may be stable, devel or head."
  (interactive (list (homebrew--formula-from-file buffer-file-name)
                 (read-string "Build type (default stable) " nil nil "stable")))
  (if (not (or (equal build "stable") (equal build "devel") (equal build "HEAD")))
    (error "Allowed build types are \"stable\", \"devel\", and \"HEAD\"")    )
  (message "Unpacking %s source of %s ..." build formula)
  (set-process-sentinel
    (homebrew--start-formula-build-proc "fetch" formula (concat "--" build))
    'homebrew--async-unpack-and-jump))

(defun homebrew-poet-insert (packages)
  "Insert resource blocks for the specified Python PACKAGES."
  (interactive "MBuild stanzas for: ")
  (unless homebrew-poet-executable
    (error "Cannot find `poet` executable; set `homebrew-poet-executable'"))
  (dolist (package (split-string packages))
    (insert (shell-command-to-string
              (concat homebrew-poet-executable " " package " 2>/dev/null")))
    (insert "\n")))

(defun homebrew-pop-to-cache ()
  "Open the Homebrew cache in a new window."
  (interactive)
  (dired-jump t homebrew-cache-dir))

;;; Setup

;;;###autoload
(define-minor-mode homebrew-mode
  "Helper functions for editing Homebrew formulae"
  :lighter " Brew"
  :keymap homebrew-mode-map

  ;; Colorize inline patches
  (if (string-match "__END__" (buffer-string))
    (font-lock-add-keywords nil
      ;; ganked from `diff-font-lock-keywords'; why it can't be simpler idk
      '( ("\\(^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@\\)\\(.*\\)$"
           (1 diff-hunk-header-face) (6 diff-function-face))
         ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\([^\t\n]+?\\)\\(?:\t.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
           (0 diff-header-face)
           (2 (if (not (match-end 3)) diff-file-header-face) prepend))
         ("^\\([-<]\\)\\(.*\n\\)" (1 diff-indicator-removed-face) (2 diff-removed-face))
         ("^\\([+>]\\)\\(.*\n\\)" (1 diff-indicator-added-face) (2 diff-added-face))))
    (if homebrew-patch-whitespace-mode
      (whitespace-mode))))

;;;###autoload
(define-globalized-minor-mode global-homebrew-mode homebrew-mode
  (lambda ()
    (if (homebrew--formula-file-p (current-buffer))
      (homebrew-mode)))
  :init-value nil)

(provide 'homebrew-mode)

;;; homebrew-mode.el ends here
