;;; buffer-groups.el --- Automatically group buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;;; License:

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

;;; Commentary:

;;

;; NOTE: When grouping lists (as opposed to other sequences),
;; `-group-by' from dash.el would be faster.

;;; Code:

;;;; Requirements

(require 'group-tree)

;;;; Variables

(defvar buffer-groups-groups nil)

;;;; Grouping macro

(group-tree-defmacro buffer-groups-defgroups
  `((dir (&rest dirs) `(group-by 'buffer-groups-group-dir ',dirs))
    (mode (name &rest modes) `(group-by 'buffer-groups-group-mode ',modes ,name))))

;;;; Customization


;;;; Commands

(defun buffer-groups-apply ()
  "Apply `buffer-groups' grouping to buffers."
  (interactive)
  (group-tree buffer-groups-groups (buffer-list) ))

;;;; Functions

(defun buffer-groups-group-dir (dirs buffer)
  (when-let* ((file-name (buffer-file-name buffer)))
    (when (cl-some (lambda (dir)
                     (string-prefix-p (expand-file-name dir) file-name))
                   dirs)
      (car dirs))))

(defun buffer-groups-group-mode (modes name buffer)
  (when (member (buffer-local-value 'major-mode buffer) modes)
    (or name (car modes))))


;;;; Footer

(provide 'buffer-groups)

;;; buffer-groups.el ends here
