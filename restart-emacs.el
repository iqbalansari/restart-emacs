;;; restart-emacs.el --- Restart emacs from within emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017  Iqbal Ansari

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

(require 'server)
(require 'desktop)

;; Making the byte compiler happy
(declare-function w32-shell-execute "w32fns.c")



;; Customizations

(defgroup restart-emacs nil
  "Customization options for restart-emacs"
  :group 'tools
  :prefix "restart-emacs-")

(defcustom restart-emacs-daemon-with-tty-frames-p nil
  "Restart Emacs daemon even if it has tty frames.

Currently `restart-emacs' cannot restore such frames, it just
notifies the user once the daemon has restarted"
  :type 'boolean
  :group 'restart-emacs)



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
  "Get absolute path to binary of currently running Emacs.

On Windows get path to runemacs.exe if possible."
  (let ((emacs-binary-path (expand-file-name invocation-name invocation-directory))
        (runemacs-binary-path (when (memq system-type '(windows-nt ms-dos))
                                (expand-file-name "runemacs.exe" invocation-directory))))
    (if (and runemacs-binary-path (file-exists-p runemacs-binary-path))
        runemacs-binary-path
      emacs-binary-path)))

(defun restart-emacs--record-tty-buffer (current filtered parameters saving)
  (let ((window (frame-selected-window (process-get (cdr current) 'frame))))
    (cons 'restart-emacs-file
	  (buffer-file-name (window-buffer window)))))

(defun restart-emacs--notify-user (tty filename)
  (with-temp-file tty
    (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
           (server-file (expand-file-name server-name server-dir)))
      (insert (format "Emacs daemon restarted! Use 'emacsclient -nw -s %s %s' to reconnect to it"
                      server-file
                      filename)))))

(defun restart-emacs--frameset-tty-filter (tty filtered parameters saving)
  (when (cdr tty)
    (run-at-time 0.5
                 nil
                 (apply-partially 'restart-emacs--notify-user
                                  (cdr tty)
                                  (cdr (assoc 'restart-emacs-file filtered)))))
  (frameset-filter-tty-to-GUI tty filtered parameters saving))

(defun restart-emacs--restore-frames (file)
  (let* (desktop-file-modtime
         (desktop-dirname (file-name-directory file))
         (desktop-base-file-name (file-name-base file))
         (desktop-base-lock-name (concat desktop-base-file-name ".lock"))
         (desktop-restore-reuses-frames nil)
         ;; Add filter for tty frames, the filter simply logs a message on
         ;; the parent ttys of the frame
         (frameset-filter-alist (append '((tty . restart-emacs--frameset-tty-filter))
                                        frameset-filter-alist))
         ;; Disable prompts for safe variables during restart
         (enable-local-variables :safe)
         ;; We mock these two functions while restoring frames
         ;; Calls to `display-color-p' blocks Emacs in daemon mode (possibly)
         ;; because the call fails
         (display-color-p (symbol-function 'display-color-p))
         ;; We mock `display-graphic-p' since desktop mode has changed to
         ;; not restore frames when we are not on graphic display
         (display-graphic-p (symbol-function 'display-graphic-p)))
    (unwind-protect
        (progn
          (fset 'display-color-p (lambda (&rest ignored) t))
          (fset 'display-graphic-p (lambda (&rest ignored) t))
          (desktop-read desktop-dirname)
          (desktop-release-lock desktop-dirname))
      ;; Restore display-color-p's definition
      (fset 'display-color-p display-color-p)
      (fset 'display-graphic-p display-graphic-p)
      ;; Cleanup the files
      (ignore-errors (delete-file (desktop-full-file-name)))
      (ignore-errors (delete-file (desktop-full-lock-name))))))

(defun restart-emacs--save-frames-using-desktop ()
  "Save current frames to a file and return the full path to the file."
  (let* (desktop-file-modtime
         (desktop-base-file-name (make-temp-name "restart-emacs-desktop"))
         (desktop-dirname temporary-file-directory)
         (desktop-restore-eager t)
         (frameset-filter-alist (append '((client . restart-emacs--record-tty-buffer))
                                        frameset-filter-alist)))
    (desktop-save temporary-file-directory t t)
    (expand-file-name desktop-base-file-name desktop-dirname)))

(defun restart-emacs--frame-restore-args ()
  "Get the arguments for restoring frames."
  ;; frameset was not available on old versions
  (when (require 'frameset nil :no-error)
    (when (or (daemonp) ;; Restore frames unconditionally in daemon mode since desktop-mode does not
              (not (bound-and-true-p desktop-save-mode))) ;; If user has enabled desktop-save-mode leave him alone
      (list "--restart-emacs-desktop" (restart-emacs--save-frames-using-desktop)))))

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
  (w32-shell-execute "open"
                     (restart-emacs--get-emacs-binary)
                     (restart-emacs--string-join args " ")))

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

(defun restart-emacs--daemon-using-sh (&optional args)
  "Restart Emacs daemon with the provided ARGS.

This function makes sure the new Emacs instance uses the same server-name as the
current instance"
  (call-process "sh" nil
                0 nil
                "-c" (format "%s --daemon=%s %s &"
                             (shell-quote-argument (restart-emacs--get-emacs-binary))
                             server-name
                             (restart-emacs--string-join (mapcar #'shell-quote-argument args)
                                                         " "))))

(defun restart-emacs--daemon-on-windows (&optional args)
  "Restart Emacs daemon with the provided ARGS.

This function makes sure the new Emacs instance uses the same server-name as the
current instance

TODO: Not tested yet"
  (w32-shell-execute "open"
                     (restart-emacs--get-emacs-binary)
                     (append (list (concat "--daemon=" server-name)) args)))

(defun restart-emacs--ensure-can-restart ()
  "Ensure we can restart Emacs on current platform."
  (when (and (not (display-graphic-p))
             (memq system-type '(windows-nt ms-dos)))
    (restart-emacs--user-error (format "Cannot restart Emacs running in terminal on system of type `%s'" system-type)))

  (when (and (daemonp)
             (not (locate-library "frameset")))
    (restart-emacs--user-error "Cannot restart Emacs daemon on versions before 24.4"))

  (when (and (daemonp)
             (delq nil (mapcar (lambda (frame)
                                 (frame-parameter frame 'tty))
                               (frame-list)))
             (not restart-emacs-daemon-with-tty-frames-p)
             (not (y-or-n-p "Current Emacs daemon has tty frames, `restart-emacs' cannot restore them, continue anyway?")))
    (restart-emacs--user-error "Current Emacs daemon has tty frames, aborting `restart-emacs'.
Set `restart-emacs-with-tty-frames-p' to non-nil to restart Emacs irrespective of tty frames")))

(defun restart-emacs--launch-other-emacs ()
  "Launch another Emacs session according to current platform."
  (apply (cond ((daemonp) (if (memq system-type '(windows-nt ms-dos))
                              #'restart-emacs--daemon-on-windows
                            #'restart-emacs--daemon-using-sh))

               ((display-graphic-p) (if (memq system-type '(windows-nt ms-dos))
                                        #'restart-emacs--start-gui-on-windows
                                      #'restart-emacs--start-gui-using-sh))

               (t (if (memq system-type '(windows-nt ms-dos))
                      ;; This should not happen since we check this before triggering a restart
                      (restart-emacs--user-error "Cannot restart Emacs running in a windows terminal")
                    #'restart-emacs--start-emacs-in-terminal)))
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
(defun restart-emacs-handle-command-line-args (&rest ignored)
  (restart-emacs--restore-frames (pop command-line-args-left)))

;;;###autoload
(add-to-list 'command-switch-alist '("--restart-emacs-desktop" . restart-emacs-handle-command-line-args))

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
  (let* ((kill-emacs-hook (append kill-emacs-hook (list #'restart-emacs--launch-other-emacs)))
         (translated-args (if (called-interactively-p 'any)
                              (restart-emacs--translate-prefix-to-args args)
                            args))
         (restart-emacs--args (append translated-args
                                      (restart-emacs--frame-restore-args))))
    (save-buffers-kill-emacs)))

(provide 'restart-emacs)
;;; restart-emacs.el ends here
