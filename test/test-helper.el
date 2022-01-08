;;; test-helper.el --- Tests for restart-emacs             -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022  Iqbal Ansari

;;; Commentary:
;;; Helpers to write tests for restart-emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;; Setup load-path, some of this is redundant when tests are run from the
;; command line
(let ((project-dir (locate-dominating-file (or (buffer-file-name) load-file-name)
                                           ".cask")))
  (if (not project-dir)
      (user-error "Could not locate project root")
    (let ((default-directory (expand-file-name (format ".cask/%d.%d"
                                                       emacs-major-version
                                                       emacs-minor-version)
                                               project-dir)))
      (normal-top-level-add-subdirs-to-load-path))

    (add-to-list 'load-path project-dir)))

;;; test-helper.el ends here
