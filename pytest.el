;;; pytest.el --- Easy Python test running in Emacs

;; Copyright (C) 2009 Eric Larson

;; Licensed under the same terms as Emacs.

;; Version: 0.2.1
;; Keywords: pytest python testing
;; URL: https://github.com/ionrock/pytest-el
;; Package-Requires: ((s "1.9.0"))
;; Created: 07 Oct 2011

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running pytest on a
;; particular buffer or part of a buffer.  This started as a direct
;; port of nosemacs (https://bitbucket.org/durin42/nosemacs).  A
;; special thanks to Jason Pellerin and Augie Fackler for writing
;; nose.el.

;;; Installation

;; In your Emacs config:
;;
;;   (require 'pytest)
;;
;; If you don't use a global installation of py.test (ie in
;; virtualenv) then add something like the following that points to
;; either the non-global version or a test runner script.:
;;
;;   (add-to-list 'pytest-project-names "my/crazy/runner")
;;
;; You can generate a script with py.test:
;;
;;   py.test --genscript=run-tests.py

;; Another option is if your global pytest isn't called "pytest" is to
;; redefine pytest-global-name to be the command that should be used.

;; By default, the root of a project is found by looking for any of the files
;; 'setup.py', '.hg' and '.git'.  You can add files to check for to the file
;; list:
;;
;; ; (add-to-list 'pytest-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;; ; (setq pytest-project-root-test (lambda (dirname) (equal dirname "foo")))

;;; Code:
(require 's)
(require 'cl-lib)
(require 'python)
(require 'seq)


(defgroup pytest nil
  "Easy Python test running in Emacs"
  :group 'python)

(defcustom pytest-project-names '("runtests")
  "The name of the script that starts the tests.")

(defcustom pytest-project-root-files '("setup.py" ".hg" ".git")
  "Names of files or directories that signify the root of a project.")

(defcustom pytest-project-root-test 'pytest-project-root
  "A function used to determine the directory the tests will be run from.")

(defcustom pytest-global-name "py.test"
  "The name of the py.test executable.")
(put 'pytest-global-name 'safe-local-variable 'stringp)

(defcustom pytest-cmd-flags "-x -s"
  "These are the flags passed to the pytest runner.")

(defcustom pytest-cmd-format-string "cd '%s' && %s %s '%s'"
  "Format string used to run the py.test command.")

(defcustom pytest-mode-keymap-prefix "C-c t"
  "Keymap preffix."
  :type 'string)

(defcustom pytest-test-module-regex "^test_.+\\.py$"
  "Regex for identifying test modules."
  :type 'string)

(defcustom pytest-test-module-name-candidate-functions
  '(pytest-candidate-current-module-if-test-module
    pytest-candidate-src-test-subdir
    pytest-candidate-pkg-test-subdir)
  "List of functions returning candidates for the test module."
  :type '(repeat function))

(defvar pytest-mode-map (make-sparse-keymap "pytest-mode") "pytest-mode keymap")

(defvar pytest--last-command-args nil
  "Arguments passed to `pytest-run' the last time it was called,
`nil' if never called.")

(defvar pytest-debug nil
  "If non nil enable logging in order to help debug pytest.")

(defun pytest-mode-setup-keymap ()
  "Setup a default keymap."
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "a")) 'pytest-all)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "m")) 'pytest-module)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "c")) 'pytest-class)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "f")) 'pytest-last-failed)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix ".")) 'pytest-one)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "r")) 'pytest-rerun-last)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "d")) 'pytest-directory)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "pa")) 'pytest-pdb-all)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "pm")) 'pytest-pdb-module)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "pc")) 'pytest-pdb-class)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "pf")) 'pytest-pdb-last-failed)
  (define-key pytest-mode-map (kbd (concat pytest-mode-keymap-prefix "p.")) 'pytest-pdb-one))

(define-minor-mode pytest-mode
  "Minor mode for running pytest from emacs." nil " pytest" pytest-mode-map
  (pytest-mode-setup-keymap))

;;;###autoload
(defun pytest-mode-enable-if-test-module ()
  "Activate `pytest-mode' when visiting a python test file."
  ;; emacs lisp check if value is string
  (when (and (stringp buffer-file-name)
             (string-match-p pytest-test-module-regex (file-name-nondirectory buffer-file-name)))
    (pytest-mode 1)))

(defun pytest-cmd-format (format-string working-directory test-runner command-flags test-names)
  "Create the string used for running the py.test command.
FORMAT-STRING is a template string used by (format) to compose
the py.test command invocation.  The string should contain enough
'%s' placeholders to satisfy the remaining arguments to this
function.
WORKING-DIRECTORY is the directory to run py.test in.
TEST-RUNNER is the name of the command to run.
COMMAND-FLAGS are the flags to pass into py.test.
TEST-NAMES are the names of the tests to run.

The function returns a string used to run the py.test command.  Here's an example:
'cd WORKING-DIRECTORY && TEST-RUNNER COMMAND-FLAGS TEST-NAMES'"
  (format format-string working-directory test-runner command-flags test-names))

(defun pytest-check-test-file (path)
  (let ((actual-path (car (s-split "::" path))))
    (if (not (file-exists-p actual-path))
        (error (format "'%s' is not an extant file." actual-path)))))

(defun pytest-run (&optional tests flags)
  "Run pytest.
Optional argument TESTS Tests to run.
Optional argument FLAGS py.test command line flags."
  (interactive "fTest directory or file: \nspy.test flags: ")
  (setq pytest--last-command-args (cons tests flags))
  (let* ((pytest (pytest-find-test-runner))
         (where (if tests
                    (let ((testpath (if (listp tests) (car tests) tests)))
                      (pytest-find-project-root (file-name-directory testpath)))
                  (pytest-find-project-root)))
         (tests (cond ((not tests) (list "."))
                      ((listp tests) tests)
                      ((stringp tests) (split-string tests))))
         (tnames (mapconcat (apply-partially 'format "'%s'") tests " "))
         (cmd-flags (if flags flags pytest-cmd-flags))
         (use-comint (s-contains? "pdb" cmd-flags)))
    (funcall #'(lambda (command)
                 (compilation-start command use-comint
                                    (lambda (mode) (concat (pytest-get-temp-buffer-name)))))
             (pytest-cmd-format pytest-cmd-format-string where pytest cmd-flags tnames))
    (if use-comint
        (with-current-buffer (get-buffer (pytest-get-temp-buffer-name))
          (inferior-python-mode)))))

(defun pytest-get-temp-buffer-name ()
  "Get name of temporary buffer.
Includes projectile support if installed.
This allows one test buffer per project."
  (let ((postfix (if (and (fboundp 'projectile-project-p)
                          (projectile-project-p))
                     (concat "-" (projectile-project-name) "*")
                   "*")))
    (concat "*pytest" postfix)))

;;; Run entire test suite
;;;###autoload
(defun pytest-all (&optional flags)
  "Run all tests.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run nil flags))

;;;###autoload
(defun pytest-failed ()
  "Quit test suite on first failed test."
  (interactive)
  (pytest-all "-x "))

;;;###autoload
(defun pytest-pdb-all ()
  "Start pdb on error."
  (interactive)
  (pytest-all (concat "--pdb " pytest-cmd-flags)))

;;; Run tests that failed last time
;;;###autoload
(defun pytest-last-failed (&optional flags)
  "Run tests that failed last time.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-all (concat "--last-failed " flags)))

;;;###autoload
(defun pytest-pdb-last-failed ()
  "Run tests that failed last time, enter debugger on error."
  (interactive)
  (pytest-last-failed (concat "--pdb " pytest-cmd-flags)))

;;; Run all the tests in a directory (and its child directories)
;;;###autoload
(defun pytest-directory (&optional flags)
  "Run pytest on all the files in the current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run (file-name-directory buffer-file-name) flags))

;;;###autoload
(defun pytest-pdb-directory (&optional flags)
  "Run pytest on all the files in the current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-directory (concat "--pdb " pytest-cmd-flags)))

;;; Run all the tests in a file
;;;###autoload
(defun pytest-module (&optional flags)
  "Run pytest (via eggs/bin/test) on current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run buffer-file-name flags))

;;;###autoload
(defun pytest-pdb-module ()
  "Run pytest on a module, enter debugger on error."
  (interactive)
  (pytest-module (concat "--pdb " pytest-cmd-flags)))

;;;###autoload
(defun pytest-class (&optional flags)
  "Run pytest on a class."
  (interactive)
  (pytest-run (concat (buffer-file-name)
                      (format "::%s" (cdr (pytest-outer-testable))))
              flags))

;;;###autoload
(defun pytest-pdb-class ()
  "Run pytest on a class, enter debugger on error."
  (interactive)
  (pytest-class (concat "--pdb " pytest-cmd-flags)))

;;; Run the test surrounding the current point
;;;###autoload
(defun pytest-one (&optional flags)
  "Run pytest (via eggs/bin/test) on testable thing at point in current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run (format "%s" (pytest-py-testable)) flags))

;;;###autoload
(defun pytest-pdb-one ()
  "Run pytest on testable thing at point, enter debugger on error."
  (interactive)
  (pytest-one (concat "--pdb " pytest-cmd-flags)))

;;;###autoload
(defun pytest-rerun-last ()
  "Repeats the last test run."
  (interactive)
  (if (null pytest--last-command-args)
      (error "No previous test command run.")
    (pytest-run (car pytest--last-command-args) (cdr pytest--last-command-args))))

;;;###autoload
(defun pytest-run-tests-for-current-module ()
  "Run the tests for the current module.

The current module is, presumably, not a test module. Calls the
functions from `pytest-test-module-name-candidate-functions', in
turn, until one returns the path of an existing file and then
runs the tests from that module. If all functions return nil no
test is run and an user error is signaled.

The functions proposing candidates receive a property list with
the keys:

- :module-name : filename, without the directory part, of the
  current buffer.

- :module-path : path of the current buffer.

- :module-dir : path for the current buffer directory

- :source-dir : path for the root of the source directory

- :package-dir : path for the root of the package

And must return either nil or an string containing a path, not
necessarily an existing path.

All directory paths already end with a directory delimiter, so
it's safe to concat directly to them."
  (interactive)
  (when (stringp buffer-file-name)
    (let* ((current-module (file-name-nondirectory buffer-file-name))
           (current-path (file-name-directory buffer-file-name))
           (source-root-path (expand-file-name
                              (locate-dominating-file current-path
                                                      (lambda (x) (file-exists-p (concat (file-name-directory (directory-file-name x))
                                                                                    "setup.py"))))))
           (package-root-path (expand-file-name (locate-dominating-file current-path "setup.py")))
           (candidate (seq-some (lambda (f)
                                  (let ((result (funcall f (list :module-name current-module
                                                                 :module-path buffer-file-name
                                                                 :module-dir current-path
                                                                 :source-dir source-root-path
                                                                 :package-dir package-root-path))))
                                    (when pytest-debug
                                      (message (format "candidate %s" f))
                                      (message (format "          current-module = %s" current-module))
                                      (message (format "          buffer-file-name = %s" buffer-file-name))
                                      (message (format "          current-path = %s" current-path))
                                      (message (format "          source-root-path = %s" source-root-path))
                                      (message (format "          package-root-path = %s" package-root-path))
                                      (message (format "  result = %s" result)))
                                    (and (stringp result)
                                         (file-exists-p result)
                                         result)))
                                pytest-test-module-name-candidate-functions)))
      (if candidate
          (pytest-run candidate)
        (user-error "No test module found for the current module")))))

(defun pytest-candidate-current-module-if-test-module (args)
  "Propose the module itself if it's a test module."
  (let ((module (plist-get args :module-name))
        (path (plist-get args :module-path)))
    (when (string-match-p "^test_" module)
      path)))

(defun pytest-candidate-src-test-subdir (args)
  "Propose a candidate under src/tests directory."
  (let ((src-root (plist-get args :source-dir))
        (module (plist-get args :module-name)))
    (concat src-root (file-name-as-directory "tests") "test_" module)))

(defun pytest-candidate-pkg-test-subdir (args)
  "Propose a candidate under package/tests directory."
  (let ((pkg-root (plist-get args :package-dir))
        (module (plist-get args :module-name)))
    (concat pkg-root (file-name-as-directory "tests") "test_" module)))

;;; Utility functions
(defun pytest-find-test-runner ()
  (let ((result
         (cl-reduce '(lambda (x y) (or x y))
                    (mapcar 'pytest-find-test-runner-names pytest-project-names))))
    (if result
        result
      pytest-global-name)))

(defun pytest-find-test-runner-names (runner)
  "Find eggs/bin/test in a parent dir of current buffer's file."
  (pytest-find-test-runner-in-dir-named
   (file-name-directory buffer-file-name) runner))

(defun pytest-find-test-runner-in-dir-named (dn runner)
  (let ((fn (expand-file-name runner dn)))
    (cond ((file-regular-p fn) fn)
          ((equal dn "/") nil)
          (t (pytest-find-test-runner-in-dir-named
              (file-name-directory (directory-file-name dn))
              runner)))))

(defun pytest-py-testable ()
  "Create a path to a test.
This uses the `::` delimiter between the
filename, class and method in order to find the specific test
case.  This requires pytest >= 1.2."
  (let* ((inner-obj (pytest-inner-testable))
         (outer (pytest-outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (concat
     (buffer-file-name)
     (cond ((equal outer-def "def") (format "::%s" outer-obj))
           ((equal inner-obj outer-obj) (format "::%s" outer-obj))
           (t (format "::%s::%s" outer-obj inner-obj))))))

(defun pytest-inner-testable ()
  "Find the function name for `pytest-one'."
  (save-excursion
    (re-search-backward
     "^[ \t]\\{0,4\\}\\(class\\|\\(?:async \\)?def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun pytest-outer-testable ()
  "Find the class for the `pytest-one'."
  (save-excursion
    (re-search-backward
     "^\\(class\\|\\(?:async \\)?def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
           (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun pytest-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory buffer-file-name))))
    (cond ((funcall pytest-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
          (t (pytest-find-project-root
              (file-name-directory (directory-file-name dn)))))))

(defun pytest-project-root (dirname)
  (cl-reduce '(lambda (x y) (or x y))
             (mapcar (lambda (d) (member d (directory-files dirname)))
                     pytest-project-root-files)))

(defun pytest-current-root ()
  (if (not (buffer-file-name))
      (expand-file-name default-directory)
    (file-name-directory (expand-file-name (buffer-file-name)))))

(provide 'pytest)

;;; pytest.el ends here
