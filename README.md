# Tasks - a general-purpose task runner for Emacs

## Installation

For elpaca user:

```emacs-lisp
(use-package tasks
  :ensure (:host codeberg :repo "meow_king/tasks"))
```

For other users, please figure it yourself.

## Usage

Note: example file see my own configuration:
[my-tasks.el](https://github.com/Ziqi-Yang/.emacs.d/blob/main/modules/my-tasks.el)

1. Define your custom task run function, and set customizable variable `tasks-function`,
   `tasks-project-function`

Example:

```emacs-lisp
  (defun mk/tasks/run ()
    (interactive)
    (let ((file-extension (file-name-extension buffer-file-name)))
      (cond
       ((derived-mode-p '(c-mode c-ts-mode))
        (mk/tasks/c))
       ((derived-mode-p '(python-base-mode))
        (mk/tasks/python))
       ((derived-mode-p '(rust-ts-mode))
        (mk/tasks/rust))
       ((derived-mode-p '(zig-mode zig-ts-mode))
        (mk/tasks/zig))
       ((derived-mode-p '(kotlin-ts-mode))
        (mk/tasks/kotlin))
       ((string-match-p (regexp-opt '("puml" "plantuml")) file-extension)
        (tasks-wrap-compile-command
         '(concat "env PLANTUML_LIMIT_SIZE=327680 plantuml " rel-file-name
                  " && imv " bare-rel-file-name ".png")))
       (t (message "No tasks defined for current condition.")))))

  (defun mk/tasks/project-run ()
    (interactive)
    (let ((project-root-path (project-root (project-current))))
      (cond
       ((file-exists-p (expand-file-name "Cargo.toml" project-root-path))
        (mk/tasks/rust))
       ((file-exists-p (expand-file-name "build.zig" project-root-path))
        (mk/tasks/zig))
       (t (mk/tasks/run)))))

  (setq tasks-function #'mk/tasks/run)
  (setq tasks-project-function #'mk/tasks/project-run))
```

2. Use utility functions provided by `tasks` to define specific tasks for various
   condition.

Currently, `tasks` provides:

- `tasks-transient-define-prefix` as a modified version of `transient-define-prefix`,
  through which you can define a transient menu for easily managing and invoking
  tasks. (NOTE excellent resource to learn transient: [transient-showcase](https://github.com/positron-solutions/transient-showcase))
  Example Usage:
  ```emacs-lisp
  (tasks-transient-define-prefix mk/tasks/rust ()
    "Tasks for Rust language."
    ["Options"
     ("-p" "package" mk/tasks-infix/rust/package)]
    [["Cargo"
      ("pc" "check" (concat "cargo clippy " (tasks-transient-get-arg "--package=")))
      ("pC" "clippy" (concat "cargo clippy" (tasks-transient-get-arg "--package=")))
      ("pf" "clippy fix" (concat "cargo clippy fix" (tasks-transient-get-arg "--package=")))
      ("pr" "run" (concat "cargo run" (tasks-transient-get-arg "--package=")))]])
  ```

  `tasks-transient-define-prefix` has some special modifications of `transient-define-prefix`.
  For command definition `("pc" "check" "echo 1")`, for the third argument:
  - when it is function-alias type (e.g defined by `transient-define-argument` macro), it is handled the same as `transient-define-prefix`
  - when it is symbol type, it is treated as a command (interactive function) and
    wrapped by `tasks-wrap-thing ` to make sure `run-last-command` function working.
  - when it is of other types, it is evaluated as an Emacs expression using `eval`. Some
    convenient variables such as `directory-path`, `file-name`, `rel-file-path` are
    provided so you can directly use them like `(concat "echo " file-name)`. More
    information can be referred from `tasks-wrap-compile-command` command (see its source
    code).
    Since transient normally treat expression as infix expression when the third
    argument is string type or list type, e.g. `("-s" "switch" "--switch")`, We
    recommend to use utility macros like `transient-define-argument` to define infix
    arguments. For example:
    ```emacs-slip
    (transient-define-argument mk/tasks-infix/rust/package ()
        :class 'transient-option
        :argument "--package=")
    ```

- `tasks-transient-get-arg`: get the value of transient infix argument and
  return a `-a=v` like string.
     
- `tasks-wrap-thing`, `tasks-wrap-function` and `tasks-wrap-compile-command`
  for wrap your command with `tasks`.
  Currently this wrapper record the last run command so that you can invoke
  the last run command.
   

3. commands provided by tasks

`tasks-run`, `tasks-run-last-cmd`, `tasks-project-run`, `tasks-project-run-last-cmd`
   
