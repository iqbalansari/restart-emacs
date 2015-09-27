;;; restart-emacs.el --- Restart emacs from within emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Iqbal Ansari

;; Author: Iqbal Ansari <iqbalansari02@yahoo.com>
;; Keywords: convenience

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

;; This package provides a simple command to restart emacs from within emacs

;;; Code:

(defun restart-emacs-get-emacs-binary ()
  "Get absolute path to binary of currently running Emacs."
  (expand-file-name invocation-name invocation-directory))

(defun restart-emacs-start-gui-on-using-sh ()
  "Start GUI version of Emacs using sh."
  (call-process "sh" nil nil nil "-c" (format "%s &" (restart-emacs-get-emacs-binary))))

(defun restart-emacs-start-gui-on-windows ()
  "Start GUI version of Emacs on windows."
  (w32-shell-execute "open" (restart-emacs-get-emacs-binary)))

(defun restart-emacs-start-emacs-in-terminal ()
  "Start Emacs in current terminal.

This requires a shell with `fg' command and `;' construct.  This has been
tested to work on sh, bash, zsh and fish shells"
  (suspend-emacs (format "fg ; %s -nw" (restart-emacs-get-emacs-binary))))

(defun restart-emacs-ensure-can-restart ()
  "Ensure we can restart Emacs on current platform."
  (when (and (not (display-graphic-p))
             (memq system-type '(windows-nt ms-dos)))
    (user-error (format "Cannot restart emacs running in terminal on system of type `%s'" system-type))))

(defun restart-emacs-launch-other-emacs ()
  "Launch another Emacs session according to current platform."
  (apply (if (display-graphic-p)
             (if (memq system-type '(windows-nt msdos))
                 #'restart-emacs-start-gui-on-windows
               #'restart-emacs-start-gui-on-using-sh)
           (if (memq system-type '(windows-nt msdos))
               ;; This should not happen since we check this before triggering a restart
               (user-error "Cannot restart Emacs running in a windows terminal")
             #'restart-emacs-start-emacs-in-terminal))
         nil))

;;;###autoload
(defun restart-emacs ()
  "Restart Emacs."
  (interactive)
  ;; Do not trigger a restart unless we are sure, we can restart emacs
  (restart-emacs-ensure-can-restart)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list #'restart-emacs-launch-other-emacs))))
    (save-buffers-kill-emacs)))

(provide 'restart-emacs)
;;; restart-emacs.el ends here
