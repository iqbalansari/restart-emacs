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

(defun restart-emacs-under-x ()
  "Restart GUI Emacs running in X."
  (call-process "sh" nil nil nil "-c" (format "%s &" (restart-emacs-get-emacs-binary))))

(defun restart-emacs-in-terminal ()
  "Restart Emacs running in a terminal."
  (suspend-emacs (format "fg ; %s -nw" (restart-emacs-get-emacs-binary))))

;;;###autoload
(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'restart-emacs-under-x
                                                         #'restart-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

(provide 'restart-emacs)
;;; restart-emacs.el ends here
