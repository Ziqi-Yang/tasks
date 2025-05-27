;;; tasks.el --- A General-Purpose Task Runner for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Meow King <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Meow King <mr.meowking@anche.no>
;; Keywords: processes
;; URL: https://codeberg.org/meow_king/tasks
;; License: GNU General Public License >= 3
;; Package-Requires: ()  ;FIXME: `package-lint-current-buffer'

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
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

;; A General-Purpose Task Runner for Emacs

;;; Code:

(defgroup tasks nil
  "Tasks."
  :prefix "tasks")

(defcustom tasks-function nil
  "Function used for `tasks-run'."
  :type 'function
  :group 'tasks)

(defcustom tasks-project-function nil
  "Function used for `tasks-project-run'."
  :type 'function
  :group 'tasks)


(defvar tasks--project-last-run-command-map (make-hash-table :size 100))
(defvar tasks--last-run-command nil)

(defun tasks-wrap-function (fn)
  "Integrate function FN with tasks."
  (setq tasks--last-run-command fn)
  (puthash (project-current) fn tasks--project-last-run-command-map)
  (call-interactively fn))

(defun tasks-wrap-compile-command (command)
  "Integrate compile COMMAND with tasks.
COMMAND: string or Emacs Lisp expression, used as the first parameter of
`compile' command.
Some variables are provided to use in COMMAND lisp expression.  To learn these
variables, please see the source code of this function."
  (let* ((file-extension (file-name-extension buffer-file-name))
         (file-name (file-name-nondirectory buffer-file-name))

         (directory-path (file-name-directory buffer-file-name))

         (project-root-path (when (project-current) (project-root (project-current))))
         (project-rel-file-path
          (when project-root-path
            (file-relative-name buffer-file-name project-root-path)))
         (bare-project-rel-file-path (file-name-sans-extension project-rel-file-path))

         (base-path (if project-root-path project-root-path directory-path))
         (rel-file-path (file-relative-name buffer-file-name base-path))
         (bare-rel-file-path (file-name-sans-extension rel-file-path))
         (command (eval command `((file-extension . ,file-extension)
                                  (file-name . ,file-name)
                                  (directory-path . ,directory-path)
                                  (project-root-path . ,project-root-path)
                                  (bare-project-rel-file-path . ,bare-project-rel-file-path)
                                  (base-path . ,base-path)
                                  (rel-file-path . ,rel-file-path)
                                  (bare-rel-file-path . ,bare-rel-file-path)))))
    (tasks-wrap-function (lambda () (interactive) (compile (eval command))))))

(defun tasks-wrap-thing (thing)
  "Wrap THING based on its type.
symbol -> `tasks-wrap-function';
others -> `tasks-wrap-compile-command'"
  (if (symbolp thing)
      (tasks-wrap-function thing)
    (tasks-wrap-compile-command thing)))

(defun tasks-run ()
  "Run function FN."
  (interactive)
  (funcall tasks-function))

(defun tasks-run-last-cmd ()
  "Run last command."
  (interactive)
  (if tasks--last-run-command
      (call-interactively tasks--last-run-command)
    (call-interactively #'tasks-run)))

(defun tasks-project-run ()
  "Run function FN at project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (funcall tasks-project-function)))

(defun tasks-project-run-last-cmd ()
  "Run project scope last command."
  (interactive)
  (let ((cmd (gethash (project-current) tasks--project-last-run-command-map)))
    (if cmd
        (call-interactively cmd)
      (call-interactively #'tasks-project-run))))

(defmacro tasks-transient-define-prefix (name arglist &rest args)
  "Define transient menu for tasks.
NAME, ARGLIST and ARGS see `transient-define-prefix'."
  (declare (debug ( &define name lambda-list
                    [&optional lambda-doc]
                    [&rest keywordp sexp]
                    [&rest vectorp]
                    [&optional ("interactive" interactive) def-body]))
           (indent defun)
           (doc-string 3))
  (let ((wrapped-args (tasks--wrap-suffix-args args)))
    `(transient-define-prefix ,name ,arglist ,@wrapped-args)))


(defun tasks--wrap-suffix-args (args)
  "Wrap commands in suffix ARGS before they get parsed.
COMPILE: boolean.  Non-nil means the user use use compile string in the place of
function."
  (mapcar (lambda (arg)
            (cond
             ((vectorp arg)
              (tasks--wrap-suffix-vector arg))
             (t arg))) ; leave docstrings, keywords, etc. unchanged
          args))

(defun tasks--wrap-suffix-vector (vec)
  "Wrap commands in suffix vector VEC.
COMPILE: boolean.  Non-nil means the user use use compile string in the place of
function."
  (vconcat
   (mapcar (lambda (item)
             (cond
              ((vectorp item)
               (tasks--wrap-suffix-vector item))
              ((and (listp item)
                    (>= (length item) 3)
                    (stringp (nth 0 item))
                    (stringp (nth 1 item)))
               ;; This is a suffix definition like ("key" "desc" command ...)
               (tasks--wrap-suffix-tuple item))
              (t item))) ; group descriptions, etc.
           (append vec nil))))

(defun tasks--wrap-suffix-tuple (tuple)
  "Wrap command in suffix TUPLE.
COMPILE: boolean.  Non-nil means use `tasks-wrap-compile-command' instead of
`tasks-wrap-command' to wrap command."
  (let ((key (nth 0 tuple))
        (desc (nth 1 tuple))
        (cmd (nth 2 tuple))
        (rest (nthcdr 3 tuple)))
    `(,key ,desc (lambda () (interactive) (tasks-wrap-thing #',cmd)) ,@rest)))


(provide 'tasks)

;;; tasks.el ends here
