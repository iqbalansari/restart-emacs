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

(defcustom restart-emacs-restore-frames nil
  "Attempt to restore frames on Emacs restart.

Please note this functionality works only on Emacs 24.4 and later, since the
earlier versions did not ship with the frameset library which is used to restore
the frames.  This variable is ignored while restarting daemon since frames are
restored unconditionally while restarting daemon mode."
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

(defun restart-emacs--record-tty-file (current &rest ignored)
  "Save the buffer which is being currently selected in the frame.

This function is used as a filter for tty frames in `frameset-filter-alist'.
See `frameset-filter-alist' for explanation of CURRENT and rest of the
parameters.  IGNORED are ignored."
  (when (processp (cdr current))
    (let ((window (frame-selected-window (process-get (cdr current) 'frame))))
      (cons 'restart-emacs-file (buffer-file-name (window-buffer window))))))

(defun restart-emacs--notify-connection-instructions (tty filename)
  "Print instructions on the given TTY about connecting to the daemon.

It prints the complete command line invocation that can be used connect to the
newly restarted daemon, FILENAME is the path to the the file that was selected
in the frame that was open on this tty before the daemon restarted."
  (with-temp-file tty
    (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
           (server-file (expand-file-name server-name server-dir))
           (emacsclient-path (expand-file-name "emacsclient" invocation-directory))
           (quoted-server-file (shell-quote-argument server-file))
           (quoted-emacsclient-path (shell-quote-argument emacsclient-path))
           (message (if filename
                        (format "Emacs daemon restarted! Use '%s -nw -s %s %s' to reconnect to it"
                                quoted-emacsclient-path
                                quoted-server-file
                                (shell-quote-argument filename))
                      (format "Emacs daemon restarted! Use '%s -nw -s %s' to reconnect to it"
                              quoted-emacsclient-path
                              quoted-server-file))))
      (insert message))))

(defun restart-emacs--frameset-tty-filter (tty filtered parameters saving)
  "Restore the TTY from saved frameset.

This does not actually restore anything rather it simply notifies the user on
tty the instructions to reconnect to the daemon and then invokes the default
filter for ttys (`frameset-filter-tty-to-GUI')

See the documentation for `frameset-filter-alist' to understand FILTERED,
PARAMETERS and SAVING."
  (when (cdr tty)
    (run-at-time 0.5
                 nil
                 (apply-partially 'restart-emacs--notify-connection-instructions
                                  (cdr tty)
                                  (cdr (assoc 'restart-emacs-file filtered)))))
  (frameset-filter-tty-to-GUI tty filtered parameters saving))

(defun restart-emacs--restore-frames-using-desktop (file)
  "Restore the frames using the desktop FILE."
  ;; We let-bind a bunch of variables from desktop mode to make sure
  ;; the changes done while restoring from the desktop file are not
  ;; leaked into normal functioning of the desktop-mode
  (let* (desktop-file-modtime
         (desktop-dirname (file-name-directory file))
         (desktop-base-file-name (file-name-base file))
         (desktop-base-lock-name (concat desktop-base-file-name ".lock"))
         (desktop-restore-reuses-frames nil)
         ;; Add filter for tty frames, the filter simply logs a message on
         ;; the parent ttys of the frame
         (frameset-filter-alist (append '((tty . restart-emacs--frameset-tty-filter))
                                        frameset-filter-alist))
         ;; Disable prompts for safe variables during restoration
         (enable-local-variables :safe)
         ;; We mock these two functions while restoring frames
         ;; Calls to `display-color-p' blocks Emacs in daemon mode (possibly)
         ;; because the call fails
         (display-color-p (symbol-function 'display-color-p))
         ;; We mock `display-graphic-p' since desktop mode has changed to
         ;; not restore frames when we are not on graphic display
         ;; TODO: Report Emacs bug
         (display-graphic-p (symbol-function 'display-graphic-p)))
    (unwind-protect
        (progn
          ;; TODO: The following might break things
	  (when (daemonp)
	    (fset 'display-color-p (lambda (&rest ignored) t))
	    (fset 'display-graphic-p (lambda (&rest ignored) t)))
          (desktop-read desktop-dirname)
          (desktop-release-lock desktop-dirname))
      ;; Restore display-color-p's definition
      (fset 'display-color-p display-color-p)
      ;; Restore display-graphic-p's definition
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
         ;; For tty frames record the currently selected file
         (frameset-filter-alist (append '((client . restart-emacs--record-tty-file))
                                        frameset-filter-alist)))
    (desktop-save temporary-file-directory t t)
    (expand-file-name desktop-base-file-name desktop-dirname)))

(defun restart-emacs--frame-restore-args ()
  "Get the arguments for restoring frames."
  ;; frameset was not available on old versions
  (when (and (locate-library "frameset")
             ;; If user has enabled desktop-save-mode leave him alone unless she
             ;; is restarting the daemon since right now Emacs does not restore
             ;; the frames in daemon mode. Also ignore the `restart-emacs-restore-frames'
             ;; configuration since restarting the daemon without restoring frames
             ;; doesn't really help
             (or (daemonp)
                 (and restart-emacs-restore-frames
                      (not (bound-and-true-p desktop-save-mode)))))
    (list "--restart-emacs-desktop"
          (restart-emacs--save-frames-using-desktop))))

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
                     (restart-emacs--string-join (cons (concat "--daemon=" server-name)
                                                       args)
                                                 " ")))

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
             (not (yes-or-no-p "Current Emacs daemon has tty frames, `restart-emacs' cannot restore them, continue anyway? ")))
    (restart-emacs--user-error "Current Emacs daemon has tty frames, aborting `restart-emacs'.
Set `restart-emacs-with-tty-frames-p' to non-nil to restart Emacs irrespective of tty frames")))

(defun restart-emacs--launch-other-emacs (arguments)
  "Launch another Emacs session with ARGUMENTS according to current platform."
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
         (list arguments)))

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

(defun restart-emacs--guess-startup-directory-using-proc ()
  "Get the startup directory of current Emacs session from /proc."
  (when (file-exists-p (format "/proc/%d/cwd" (emacs-pid)))
    (file-chase-links (format "/proc/%d/cwd" (emacs-pid)))))

(defun restart-emacs--guess-startup-directory-using-lsof ()
  "Get the startup directory of the current Emacs session using the `lsof' program."
  (when (executable-find "lsof")
    (let* ((default-directory "/")
           (lsof-op (shell-command-to-string (format "lsof -d cwd -a -Fn -p %d"
                                                     (emacs-pid))))
           (raw-cwd (car (last (split-string lsof-op "\n" t))))
           (cwd (substring raw-cwd 1)))
      (when (< 0 (length cwd))
        cwd))))

(defun restart-emacs--guess-startup-directory-using-buffers ()
  "Guess the startup directory for current Emacs session from some buffer.

This tries to get Emacs startup directory from the *Messages* or *scratch*
buffer, needless to say this would be wrong if the user has killed and recreated
these buffers."
  (or (and (get-buffer "*Messages*")
           (with-current-buffer "*Messages*" default-directory))
      (and (get-buffer "*scratch*")
           (with-current-buffer "*scratch*" default-directory))))

(defun restart-emacs--guess-startup-directory-from-env ()
  "Guess the startup directory for current Emacs session from USERPROFILE or HOME."
  (or (getenv "HOME")
      (getenv "USERPROFILE")))

(defun restart-emacs--guess-startup-directory ()
  "Guess the directory the new Emacs instance should start from.

On Linux it figures out the startup directory by reading /proc entry for current
Emacs instance.  Otherwise it falls back to guessing the startup directory by
reading `default-directory' of *Messages* or *scratch* buffers falling back to
the HOME or USERPROFILE (only applicable on Window) environment variable and
finally just using whatever is the current `default-directory'."
  (or (restart-emacs--guess-startup-directory-using-proc)
      (restart-emacs--guess-startup-directory-using-lsof)
      (restart-emacs--guess-startup-directory-using-buffers)
      (restart-emacs--guess-startup-directory-from-env)
      default-directory))



;; User interface

;;;###autoload
(defun restart-emacs-handle-command-line-args (&rest ignored)
  "Handle the --restart-emacs-desktop command line argument.

The value of the argument is the desktop file from which the frames should be
restored.  IGNORED are ignored."
  (restart-emacs--restore-frames-using-desktop (pop command-line-args-left)))

;;;###autoload
(add-to-list 'command-switch-alist '("--restart-emacs-desktop" . restart-emacs-handle-command-line-args))

(defvar restart-emacs--inhibit-kill-p nil
  "Non-nil means inhibit killing the current session when restarting.
This means that `restart-emacs' will spawn a new instance of
Emacs without killing the current one.

This is used internally to implement the command
`restart-emacs-start-new-emacs'.")

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
  (let* ((default-directory (restart-emacs--guess-startup-directory))
         (translated-args (if (called-interactively-p 'any)
                              (restart-emacs--translate-prefix-to-args args)
                            args))
         (restart-args (append translated-args
                               ;; When Emacs is started with a -Q
                               ;; restart-emacs's autoloads would not be present
                               ;; causing the the --restart-emacs-desktop
                               ;; argument to be unhandled
                               (unless (member "-Q" translated-args)
                                 (restart-emacs--frame-restore-args))))
         (kill-emacs-hook (append kill-emacs-hook
                                  (unless restart-emacs--inhibit-kill-p
                                    (list (apply-partially #'restart-emacs--launch-other-emacs
                                                           restart-args))))))
    (if restart-emacs--inhibit-kill-p
        (restart-emacs--launch-other-emacs restart-args)
        (save-buffers-kill-emacs))))

;;;###autoload
(defun restart-emacs-start-new-emacs (&optional args)
  "Start a new instance of Emacs.

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') the new Emacs is started
  with `--debug-init' flag
- with two `universal-argument' (`C-u') the new Emacs is started with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which the new Emacs should be started."
  (interactive "P")
  (let ((restart-emacs--inhibit-kill-p t))
    (restart-emacs args)))

(provide 'restart-emacs)
;;; restart-emacs.el ends here
