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

(defvar buffer-groups-current-group nil)

;;;; Grouping macro

(group-tree-defmacro buffer-groups-defgroups
  `((dir (&rest dirs) `(group-by 'buffer-groups-group-dir ',dirs))
    (mode (name &rest modes) `(group-by 'buffer-groups-group-mode ',modes ,name))
    (mode-match (name &rest regexps) `(group-by 'buffer-groups-group-mode-match ',regexps ,name))
    (name-match (name &rest regexps) `(group-by 'buffer-groups-group-name-match ',regexps ,name))
    (auto-project () `(group-by 'buffer-groups-group-auto-project))))

;;;; Customization

(defgroup buffer-groups nil
  "FIXME"
  :group 'convenience)

(defcustom buffer-groups-filter-fns
  (list (lambda (buffer)
          "Return non-nil if BUFFER's name starts with a space."
          (string-prefix-p " " (buffer-name buffer))))
  "FIXME"
  :type '(repeat function))

;;;; Commands

(defun buffer-groups-apply ()
  "Apply `buffer-groups' grouping to buffers."
  (interactive)
  (group-tree buffer-groups-groups (buffer-list) ))

(defun buffer-groups-grouped ()
  "Return buffers grouped."
  (group-tree buffer-groups-groups
              (cl-loop with buffers = (buffer-list)
                       for fn in buffer-groups-filter-fns
                       do (setf buffers (cl-remove-if fn buffers))
                       finally return buffers)))

(defun buffer-groups-switch-group ()
  (interactive)
  (when-let* ((selected-group
               (completing-read "Group: " (mapcar #'car (buffer-groups-grouped)))))
    (setf buffer-groups-current-group selected-group)))

(defun buffer-groups-switch-buffer ()
  (interactive)
  (let* ((group (completing-read "Group: " (mapcar #'car (buffer-groups-grouped))))
         (buffer (completing-read "Buffer: "
                                  (mapcar #'buffer-name
                                          (alist-get group (buffer-groups-grouped) nil nil #'string=)))))
    (switch-to-buffer buffer)))

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

(defun buffer-groups-group-mode-match (regexps name buffer)
  (cl-loop with mode-name = (symbol-name (buffer-local-value 'major-mode buffer))
           for regexp in regexps
           when (string-match-p regexp mode-name)
           return (or name mode-name)))

(defun buffer-groups-group-name-match (regexps name buffer)
  (cl-loop for regexp in regexps
           when (string-match-p regexp (buffer-name buffer))
           return (or name regexp)))

(defun buffer-groups-group-auto-project (buffer)
  (when-let* ((project (with-current-buffer buffer
                         (project-current)))
              (project-root (car (project-roots project))))
    (concat "Project: " project-root)))

;;;; Footer

(provide 'buffer-groups)

;;; buffer-groups.el ends here
