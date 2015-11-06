;;; restart-emacs.el --- Restart emacs from within emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Iqbal Ansari

;; Author: Iqbal Ansari <iqbalansari02@yahoo.com>
;; Keywords: convenience
;; URL: https://github.com/iqbalansari/restart-emacs
;; Version: 0.1.1

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

;; This package provides a simple command to restart Emacs from within Emacs



;;; Code:

;; Making the byte compiler happy
(declare-function w32-shell-execute "w32fns.c")



;; Compatibility functions

(defun restart-emacs--string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR.

This function is available on Emacs v24.4 and higher, it has been
backported here for compatibility with older Emacsen."
  (if (fboundp 'string-join)
      (apply #'string-join (list strings separator))
    (mapconcat 'identity strings separator)))

(defun restart-emacs--user-error (format &rest args)
  "Signal a `user-error' if available otherwise signal a generic `error'.

FORMAT and ARGS correspond to STRING and OBJECTS arguments to `format'."
  (if (fboundp 'user-error)
      (apply #'user-error format args)
    (apply #'error format args)))



;; Core functions

(defvar restart-emacs--args nil
  "The arguments with which to restart Emacs is bound dynamically.")

(defun restart-emacs--get-emacs-binary ()
  "Get absolute path to binary of currently running Emacs."
  (expand-file-name invocation-name invocation-directory))

(defun restart-emacs--start-gui-using-sh (&optional args)
  "Start GUI version of Emacs using sh.

ARGS is the list arguments with which Emacs should be started"
  (call-process "sh" nil
                0 nil
                "-c" (format "%s %s &"
                             (shell-quote-argument (restart-emacs--get-emacs-binary))
                             (restart-emacs--string-join (mapcar #'shell-quote-argument
                                                                 args)
                                                         " "))))

(defun restart-emacs--start-gui-on-windows (&optional args)
  "Start GUI version of Emacs on windows.

ARGS is the list arguments with which Emacs should be started"
  (w32-shell-execute "open" (restart-emacs--get-emacs-binary) args))

(defun restart-emacs--start-emacs-in-terminal (&optional args)
  "Start Emacs in current terminal.

ARGS is the list arguments with which Emacs should be started.  This requires a
shell with `fg' command and `;' construct.  This has been tested to work with
sh, bash, zsh, fish, csh and tcsh shells"
  (suspend-emacs (format "fg ; %s %s -nw"
                         (shell-quote-argument (restart-emacs--get-emacs-binary))
                         (restart-emacs--string-join (mapcar #'shell-quote-argument
                                                             args)
                                                     " "))))

(defun restart-emacs--ensure-can-restart ()
  "Ensure we can restart Emacs on current platform."
  (when (and (not (display-graphic-p))
             (memq system-type '(windows-nt ms-dos)))
    (restart-emacs--user-error (format "Cannot restart emacs running in terminal on system of type `%s'" system-type))))

(defun restart-emacs--launch-other-emacs ()
  "Launch another Emacs session according to current platform."
  (apply (if (display-graphic-p)
             (if (memq system-type '(windows-nt ms-dos))
                 #'restart-emacs--start-gui-on-windows
               #'restart-emacs--start-gui-using-sh)
           (if (memq system-type '(windows-nt ms-dos))
               ;; This should not happen since we check this before triggering a restart
               (restart-emacs--user-error "Cannot restart Emacs running in a windows terminal")
             #'restart-emacs--start-emacs-in-terminal))
         ;; Since this function is called in `kill-emacs-hook' it cannot accept
         ;; direct arguments the arguments are let-bound instead
         (list restart-emacs--args)))

(defun restart-emacs--translate-prefix-to-args (prefix)
  "Translate the given PREFIX to arguments to be passed to Emacs.

It does the following translation
            `C-u' => --debug-init
      `C-u' `C-u' => -Q
`C-u' `C-u' `C-u' => Reads the argument from the user in raw form"
  (cond ((equal prefix '(4)) '("--debug-init"))
        ((equal prefix '(16)) '("-Q"))
        ((equal prefix '(64)) (split-string (read-string "Arguments to start Emacs with (separated by space): ")
                                            " "))))



;; User interface

;;;###autoload
(defun restart-emacs (&optional args)
  "Restart Emacs.

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is restarted
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is restarted with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be restarted."
  (interactive "P")
  ;; Do not trigger a restart unless we are sure, we can restart emacs
  (restart-emacs--ensure-can-restart)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list #'restart-emacs--launch-other-emacs)))
        (restart-emacs--args (if (called-interactively-p 'any)
                                        (restart-emacs--translate-prefix-to-args args)
                                      args)))
    (save-buffers-kill-emacs)))

(provide 'restart-emacs)
;;; restart-emacs.el ends here
