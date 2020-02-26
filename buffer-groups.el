;;; buffer-groups.el --- Group buffers automatically with rules  -*- lexical-binding: t; -*-

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

;; This is an early WIP.  The implementation is surprisingly simple
;; and flexible.  It's sort of like perspectives and such packages,
;; but rather than assigning buffers to groups manually, it's done
;; automatically with rules written by the user in a simple DSL.  The
;; groups can also be nested (if you are comfortable with that).

;; To see how grouping rules work, see the default groups defined in
;; `buffer-groups-groups' at the bottom of the file.

;;; Code:

;;;; Requirements

(require 'map)
(require 'project)
(require 'subr-x)

(require 'group-tree)

;;;; Variables

(defvar buffer-groups-emacs-source-directory
  (cl-reduce (lambda (val fn)
               (funcall fn val))
             ;; I feel like Emacs needs a function like `f-parent'.
             '(file-name-directory directory-file-name file-name-directory
                                   directory-file-name file-name-directory)
             :initial-value (locate-library "cl-lib"))
  "The directory containing the installed source code for this Emacs.
Usually this will be something like \"/usr/share/emacs/VERSION\".")

;; Silence byte-compiler.
(defvar buffer-groups-groups)

;;;; Customization

(defgroup buffer-groups nil
  "FIXME"
  :group 'convenience)

(defcustom buffer-groups-filter-fns
  (list (lambda (buffer)
          "Return non-nil if BUFFER's name starts with a space."
          (string-prefix-p " " (buffer-name buffer))))
  "Buffers that match these functions are not shown."
  :type '(repeat function))

;;;; Commands

(defun buffer-groups-switch-group ()
  "Switch the active buffer group for the current frame.
Return the path."
  (interactive)
  (let ((path (buffer-groups-read-group-path (buffer-groups-grouped))))
    (set-frame-parameter nil 'buffer-groups-current-group-path path)
    path))

(defun buffer-groups-switch-buffer (&optional all-p)
  "Switch to another buffer in the current group.
If ALL-P (interactively, with prefix), select a group first."
  (interactive "P")
  (let* ((group-path (if all-p
                         (buffer-groups-switch-group)
                       (or (frame-parameter nil 'buffer-groups-current-group-path)
                           (buffer-groups-switch-group))))
         (buffer-names (mapcar #'buffer-name (buffer-groups-buffers group-path))))
    (if buffer-names
        (switch-to-buffer (completing-read "Buffer: " buffer-names))
      ;; Group has no buffers anymore: switch group then try again.
      (buffer-groups-switch-group)
      (buffer-groups-switch-buffer))))

;;;; Functions

(cl-defun buffer-groups-buffers
    (&optional (group-path (or (frame-parameter nil 'buffer-groups-current-group-path)
                               (buffer-groups-switch-group))))
  "Return list of buffers for GROUP-PATH.
By default, use the current frame's current group."
  (cl-letf* ((alist-get-orig (symbol-function 'alist-get))
             ((symbol-function 'alist-get)
              (lambda (key alist &optional default remove _testfn)
                (funcall alist-get-orig key alist default remove #'string=))))
    ;; `map-nested-elt' uses `alist-get', but it does not permit its TESTFN
    ;; to be set, so we have to rebind it to one that uses `string='.
    (map-nested-elt (buffer-groups-grouped) group-path)))

(cl-defun buffer-groups-read-item (tree &key (leaf-key #'identity))
  "Return a leaf read from TREE with completion.
Completion is done in steps when descending into branches."
  (cl-labels ((read-item
               (tree) (cl-typecase (car tree)
                        (list (let ((key (completing-read "Group: " (mapcar #'car tree))))
                                (read-item (alist-get key tree nil nil #'string=))))
                        (atom (completing-read "Buffer: " (mapcar leaf-key tree))))))
    (read-item tree)))

(defun buffer-groups-read-group-path (groups)
  "Return a path to a group in GROUPS read with completion."
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

(defun buffer-groups-group-dir (dirs buffer)
  "If BUFFER's filename is in one of DIRS, return the first of DIRS."
  (when-let* ((file-name (or (buffer-file-name buffer)
                             (buffer-file-name (buffer-base-buffer buffer)))))
    (when (cl-some (lambda (dir)
                     (string-prefix-p (expand-file-name dir) file-name))
                   dirs)
      (car dirs))))

(defun buffer-groups-group-mode (modes name buffer)
  "Return whether BUFFER's major mode is one of MODES.
If it is, return NAME if non-nil, otherwise the first of MODES."
  (when (member (buffer-local-value 'major-mode buffer) modes)
    (or name (car modes))))

;; Checkdoc insists that "matches" should be "match" in these
;; two docstrings.  I would like to have a clean linting.  :/

(defun buffer-groups-group-mode-match (regexps name buffer)
  "Return whether BUFFER's major mode name does match one of REGEXPS.
If it does, return NAME if non-nil, otherwise the mode's name."
  (cl-loop with mode-name = (symbol-name (buffer-local-value 'major-mode buffer))
           for regexp in regexps
           when (string-match-p regexp mode-name)
           return (or name mode-name)))

(defun buffer-groups-group-name-match (regexps name buffer)
  "Return whether BUFFER's name does match one of REGEXPS.
If it does, return NAME if non-nil, otherwise the matching
regexp."
  (cl-loop for regexp in regexps
           when (string-match-p regexp (buffer-name buffer))
           return (or name regexp)))

(defun buffer-groups-special-buffer-p (buffer)
  "Return non-nil if BUFFER is special.
That is, if its name starts with \"*\"."
  (string-match-p (rx bos (optional (1+ blank)) "*")
                  (buffer-name buffer)))

;;;;;; Auto groups

(defmacro buffer-groups-defauto-group (name &rest body)
  "Define a grouping function named `buffer-groups-group-auto-NAME'.
It takes one argument, a buffer, which is bound to `buffer' in
BODY.  It should return a key by which to group its buffer, or
nil if it should not be grouped.

NAME, okay, `checkdoc'?"
  (declare (indent defun))
  (let* ((fn-name (intern (concat "buffer-groups-group-auto-" (symbol-name name))))
         (docstring (format "Group buffers by %s." name)))
    `(defun ,fn-name (buffer)
       ,docstring
       ,@body)))

(buffer-groups-defauto-group file
  (when-let* ((filename (or (buffer-file-name buffer)
                            (buffer-file-name (buffer-base-buffer buffer)))))
    (concat "File: " (file-name-nondirectory filename))))

(buffer-groups-defauto-group directory
  (concat "Dir: " (file-truename (buffer-local-value 'default-directory buffer))))

(buffer-groups-defauto-group mode
  (symbol-name (buffer-local-value 'major-mode buffer)))

(buffer-groups-defauto-group indirect
  (when (buffer-base-buffer buffer)
    "*indirect*"))

(buffer-groups-defauto-group hidden
  (if (string-prefix-p " " (buffer-name buffer))
      "*hidden*"
    "Normal"))

(buffer-groups-defauto-group special
  (when (buffer-groups-special-buffer-p buffer)
    "*special*"))

(buffer-groups-defauto-group project
  (when-let* ((project (with-current-buffer buffer
                         (project-current)))
              (project-root (car (project-roots project))))
    (concat "Project: " project-root)))

;;;; Grouping macro

(group-tree-defmacro buffer-groups-defgroups
  `((dir (&rest dirs) `(group-by 'buffer-groups-group-dir (list ,@dirs)))
    (mode (name &rest modes) `(group-by 'buffer-groups-group-mode (list ,@modes) ,name))
    (mode-match (name &rest regexps) `(group-by 'buffer-groups-group-mode-match (list ,@regexps) ,name))
    (name-match (name &rest regexps) `(group-by 'buffer-groups-group-name-match (list ,@regexps) ,name))
    (auto-directory () `(group-by 'buffer-groups-group-auto-directory))
    (auto-indirect () `(group-by 'buffer-groups-group-auto-indirect))
    (auto-project () `(group-by 'buffer-groups-group-auto-project))
    (auto-special () `(group-by 'buffer-groups-group-auto-special))))

;; This option must be defined after the macro which its default value uses.
(defcustom buffer-groups-groups
  (buffer-groups-defgroups
    (group (dir "~/org")
           (auto-indirect))
    (group (dir buffer-groups-emacs-source-directory))
    (group (auto-special))
    (group (mode-match "*Helm*" (rx bos "helm-")))
    (auto-project))
  "Groups in which to put buffers."
  :type 'list)

;;;; Footer

(provide 'buffer-groups)

;;; buffer-groups.el ends here
