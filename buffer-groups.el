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

(require 'map)
(require 'subr-x)

(require 'group-tree)

;;;; Variables

(defvar buffer-groups-current-group nil)

;;;; Grouping macro

(group-tree-defmacro buffer-groups-defgroups
  `((dir (name &rest dirs) `(group-by 'buffer-groups-group-dir ',dirs ,name))
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

(defcustom buffer-groups-groups
  (buffer-groups-defgroups
    (group (name-match "*Special*" "^\\*"))
    (group (mode-match "*Helm*" "^helm-"))
    (group (dir "~/org" "~/org"))
    (group (mode "*Org*" org-mode))
    (auto-project))
  "Groups in which to put buffers."
  :type 'list)

;;;; Commands

(defun buffer-groups-switch-group ()
  "Switch the active buffer group."
  (interactive)
  (setf buffer-groups-current-group
        (buffer-groups-read-group-path (buffer-groups-grouped))))

(defun buffer-groups-switch-buffer (&optional all-p)
  "Switch to another buffer in the current group.
If ALL-P (interactively, with prefix), select a group first."
  (interactive "P")
  (cl-labels ()
    (let* ((group-path (if all-p
                           (buffer-groups-read-group-path (buffer-groups-grouped))
                         (or buffer-groups-current-group
                             (buffer-groups-switch-group))))
           (buffers (mapcar #'buffer-name
                            (cl-letf* ((alist-get-orig (symbol-function 'alist-get))
                                       ((symbol-function 'alist-get)
                                        (lambda (key alist &optional default remove _testfn)
                                          (funcall alist-get-orig key alist default remove #'string=))))
                              ;; `map-nested-elt' uses `alist-get', but it does not permit its TESTFN
                              ;; to be set, so we have to rebind it to one that uses `string='.
                              (map-nested-elt (buffer-groups-grouped) group-path)))))
      (if buffers
          (switch-to-buffer (completing-read "Buffer: " buffers))
        ;; Group has no buffers anymore: switch group then try again.
        (buffer-groups-switch-group)
        (buffer-groups-switch-buffer)))))

;;;; Functions

(cl-defun buffer-groups-read-item (items &key (leaf-key #'identity))
  (cl-labels ((read-item
               (items) (cl-typecase (car items)
                         (list (let ((key (completing-read "Group: " (mapcar #'car items))))
                                 (read-item (alist-get key items nil nil #'string=))))
                         (atom (completing-read "Buffer: " (mapcar leaf-key items))))))
    (read-item items)))

(defun buffer-groups-read-group-path (groups)
  (cl-labels ((read-group
               (items last-key)
               (cl-typecase (car items)
                 (list (list last-key
                             (let ((key (completing-read "Group: " (mapcar #'car items))))
                               (read-group (alist-get key items nil nil #'string=) key))))
                 (atom last-key))))
    (let ((path (cadr (read-group groups nil))))
      (cl-typecase path
        (list path)
        (atom (list path))))))

(cl-defun buffer-groups-grouped (&optional (groups buffer-groups-groups))
  "Return buffers grouped by GROUPS."
  (group-tree groups (cl-loop with buffers = (buffer-list)
                              for fn in buffer-groups-filter-fns
                              do (setf buffers (cl-remove-if fn buffers))
                              finally return buffers)))

;;;;; Grouping predicates

;; FIXME: Other docstrings.

(defun buffer-groups-group-dir (dirs name buffer)
  "When BUFFER matches one of DIRS, return NAME or the first DIR."
  (when-let* ((file-name (buffer-file-name buffer)))
    (when (cl-some (lambda (dir)
                     (string-prefix-p (expand-file-name dir) file-name))
                   dirs)
      (or name (car dirs)))))

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
