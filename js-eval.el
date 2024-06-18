;;; js-eval.el --- JavaScript/TypeScript evaluation -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-eval
;; Keywords: lisp, languages
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (transient "0.5.3") (request "0.3.2"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a set of tools for evaluating JavaScript and TypeScript
;; code within Emacs.

;; It includes functions for compiling, running, and
;; inspecting JavaScript code, as well as managing project dependencies and
;; configurations.

;; js-eval supports various JavaScript environments,including Node.js and
;; Babel, and offers integration with Emacs' transient interface for easy access
;; to its features.

;;; Code:


(eval-and-compile
  (require 'cc-mode))

(eval-when-compile
  (require 'subr-x))

(require 'request)

(require 'transient)
(require 'json)

(declare-function ansi-color-filter-region "ansi-color")
(declare-function ansi-color-apply-on-region "ansi-color")

(defcustom js-eval-tsconfig-filename "tsconfig.json"
  "Filename for TypeScript configuration used by `js-eval' command.

Specifies the filename of the TypeScript configuration file used when evaluating
TypeScript projects. The default value is \"tsconfig.json\".

The value should be a string representing the filename of the TypeScript
configuration file. This filename is used to locate the TypeScript project
settings when performing operations that require project context, such as type
checking or compiling TypeScript code."
  :group 'js-eval
  :type 'string)

(defcustom js-eval-node-modules-priority-section-to-read
  '("jsnext:main" "module" "types" "typings")
  "Priority list of package.json fields for module resolution.

A list of keys to prioritize when reading `package.json' for module resolution
in JavaScript evaluations. The default keys are \"jsnext:main\", \"module\",
\"types\", and \"typings\".

Each element in the list is a string that corresponds to a key in the
`package.json' file. These keys typically point to alternative module entry
points or type definitions that should be considered when resolving modules. The
order of the keys in the list reflects their priority, with earlier entries
having higher precedence."
  :group 'js-eval
  :type '(repeat string))

(defcustom js-eval-package-json-sections
  '("dependencies" "devDependencies" "peerDependencies")
  "List of JSON sections to evaluate in JavaScript projects.

A list of sections to extract from `package.json' when evaluating
JavaScript projects. The default sections are \"dependencies\",
\"devDependencies\", and \"peerDependencies\".

Each element in the list should be a string representing a key in the
`package.json' file that corresponds to a section containing module
dependencies. The list can be customized to include any section that
might be relevant for a specific workflow or project setup.

To modify this list, use the customization interface or set the value
directly in an Emacs Lisp file. When setting the value directly, ensure
that each section name is a string and the list is properly quoted."
  :group 'js-eval
  :type '(repeat string))

(defcustom js-eval-preffered-extensions
  '("ts" "tsx" "jsx" "es6" "es" "mjs" "js" "cjs" "ls" "sjs" "iced" "liticed"
    "json")
  "List of preferred file extensions for JavaScript evaluation.

A list of file extensions that should be considered when evaluating JavaScript
code. The default extensions are \"ts\", \"tsx\", \"jsx\", \"es6\", \"es\",
\"mjs\", \"js\", \"cjs\", \"ls\", \"sjs\", \"iced\", \"liticed\", and \"json\".

Each element in the list must be a string representing a file extension
without the leading dot. These extensions are used to determine which
files to include during the evaluation process."
  :group 'js-eval
  :type '(repeat string))

(defcustom js-eval-project-aliases nil
  "Alist mapping JS aliases to project directory paths.

A list of project-specific aliases for module resolution when evaluating
JavaScript code. Each entry in the list is a cons cell where the car is a string
representing the alias and the cdr is a list of directory paths that the alias
should resolve to.

When setting this variable, use an alist where each key is an alias string and
the associated value is a list of directory paths. The paths can be absolute or
relative to the project's root directory.

The `:set' function ensures that the aliases are normalized and converted to
absolute paths using the `js-eval-normalize-aliases' function. The normalization
process includes expanding any wildcard characters and resolving relative paths.

The `:type' specifier indicates that the value should be an alist with string
keys and list values, where each list contains directory paths."
  :group 'js-eval
  :set (lambda (var value &rest _ignored)
         (let ((aliases (js-eval-normalize-aliases value)))
           (set var aliases)))
  :type '(alist
          :key-type (string :tag "Alias")
          :value-type (repeat :tag "Path" directory)))

(defcustom js-eval-babel-node-modules-path "~/js-eval/node_modules/"
  "Path to node_modules for Babel transpilation.

Specifies the path to the `node_modules' directory used by Babel for
transpiling JavaScript code. The default path is \"~/js-eval/node_modules/\".

The value should be a string representing the absolute or relative
directory path where Babel can find the necessary node modules. Ensure
that the specified path is accessible and contains the required Babel
packages for code evaluation and transpilation."
  :type 'directory
  :group 'js-eval-javascript)

(defcustom js-eval-node-modules-dir "node_modules"
  "Directory path for Node.js modules.

Specifies the directory name where Node.js modules are located for JavaScript
evaluation. The default value is \"node_modules\".

The value should be a string representing the directory name relative to the
JavaScript project's root. This directory is where `npm' or `yarn' installs the
project's dependencies. Adjust this value if using a non-standard directory
structure."
  :group 'js-eval
  :type 'string)

(defcustom js-eval-babel-options nil
  "Options for Babel transpilation in `js-eval' function.

Specifies options to pass to Babel when evaluating JavaScript code.

When non-nil, should be a list of strings, each representing a command-line
option to be passed to the Babel transpiler.

For example, to specify a particular preset, include a string like
\"--presets=@babel/preset-env\" in the list.

The default value is nil, which means no additional options are passed to Babel.
To modify this list, use the customization interface or set the value in your
Emacs configuration file with `setq'.

Each option should be provided in the same format as it would be on the command
line."
  :type '(repeat string)
  :group 'js-eval-javascript)

(defconst js-eval-file-ext-regexp
  (concat "\\.\\("
          (string-join
           '("\\(d\\.\\)?tsx?"
             "jsx" "es6" "es"
             "mjs" "js" "cjs" "ls"
             "sjs" "iced" "liticed" "json")
           "\\|")
          "\\)\\'")
  "Regexp matching js, jsx and ts extensions files.")

(defvar js-eval-file-index-regexp
  (concat "\\(/\\|^\\)" "index"
          (concat "\\($\\|" js-eval-file-ext-regexp "\\)"))
  "Regular expression to match JavaScript index file names.")

(defun js-eval-string-match-p (regexp str &optional start)
  "Check if REGEXP matches STR from START, return t if so.

Argument REGEXP is a regular expression string.

Argument STR is the string to search for a match.

Optional argument START is the position in STR to start the search; it defaults
to the beginning of the string."
  (when (and str (stringp str)
             (string-match-p regexp str start))
    t))

(defun js-eval-add-ext-if-not (file extension)
  "Append EXTENSION to FILE if it lacks an extension.

Argument FILE is a string representing the file name to be checked for the
extension.

Argument EXTENSION is a string representing the extension to be added to the
FILE name if it is not already present."
  (if (js-eval-string-match-p js-eval-file-ext-regexp file)
      file
    (concat file "." extension)))

(defun js-eval-is-index-file-p (path)
  "Check if PATH matches the index file regexp.

Argument PATH is the file path to check against the index file pattern."
  (js-eval-string-match-p js-eval-file-index-regexp path))


(defvar js-eval-popup-inspect-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 0") #'kill-this-buffer)
    (define-key map (kbd "C-c C-o") #'js-eval-visit-compiled)
    map)
  "Keymap for JS evaluation pop-up inspection commands.")

(defvar js-eval-popup-inspect-buffer-name "*js-eval-popup-inspect*"
  "Buffer name for JavaScript evaluation results popup.")

(defvar js-eval-server-params nil
  "Parameters for JavaScript evaluation server.")

(defvar js-eval-popup-content nil
  "Content displayed in JavaScript evaluation popup.")
(defvar js-eval-popup-meta nil
  "Metadata for JavaScript evaluation results popup display.")
(defvar js-eval-popup-switch-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'js-eval-popup-open-inspector)
    map)
  "Keymap for switching to JavaScript evaluation popup.")

(defvar js-eval-server-status nil
  "Server status for JavaScript evaluation.")
(defvar js-eval-last-result nil
  "Holds the result of the last JavaScript evaluation.")

(defvar js-eval-aliases nil
  "List of aliases for evaluating JavaScript code.")

(defvar js-eval-util-string
  "(function (path, eoe, outputPath) {
  (function(obj) {
    if (outputPath) {
      if (obj instanceof Buffer) {
        require('fs').writeFileSync(outputPath, obj);
      } else if (obj && 'function' === typeof obj.pipe) {
        obj.pipe(require('fs').createWriteStream(outputPath));
      }
    } else {
      console.log(
        require('util').inspect(obj, {
          compact: false,
          depth: 20,
          breakLength: 120
        }),
      );
    }
    process.stdout.write(eoe);
  })((function() {
    return eval(require('fs').readFileSync(path, { encoding: 'utf8' }));
  })());
})"
  "JavaScript utility to evaluate and serialize expressions.")

(defvar js-eval-server-process-name "emacs-jsdom-run"
  "Name of the process running the JavaScript evaluation server.")

(defvar js-eval-server-buffer-name
  (concat "*" js-eval-server-process-name "*")
  "Buffer name for JavaScript evaluation server output.")

(defvar js-eval-response nil
  "Storage for the latest JavaScript evaluation result.")
(defvar js-eval-callback nil
  "Function called with the result of JavaScript evaluation.")

(defvar js-eval-files-cache (make-hash-table :test 'equal)
  "Hash table caching JavaScript file evaluations.")


(defconst js-eval-node-modules-regexp
  "\\(/\\|^\\)node_modules\\(/\\|$\\)"
  "Regexp matching path with node_modules.")

(defvar js-eval-current-project-root nil
  "Path to the root directory of the current JavaScript project.")


(defvar js-eval-json-hash (make-hash-table :test 'equal)
  "Hash table for storing JSON evaluation results.")

(defvar js-eval--regexp-js-regexp
  "\\([!]*\\)/\\(?:[^/[\\]\\|\\\\.\\|\\[\\(?:[^]\\]\\|\\\\.\\)*]\\)*\\(/?\\)"
  "Regular expression to match JavaScript regex literals.")

(defvar js-eval-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for JavaScript evaluation mode.")

(defconst js-eval-regexp-name-set
  "[_$A-Za-z0-9]"
  "Regexp matching the start of an identifier.")

(defconst js-eval-regexp-name
  "_$A-Za-z0-9"
  "A character set matching an identifier.
Supposed to use as argument of `skip-chars-forward'.")

(defconst js-eval-reserved-js-words
  '("abstract" "any" "as" "async" "async*" "await" "boolean" "bigint"
    "break" "case" "catch" "class" "const" "constructor" "continue"
    "declare" "default" "delete" "do" "else" "enum" "export" "exports"
    "extends" "extern" "false" "finally" "for" "function" "function*"
    "from" "get" "goto" "if" "implements" "import" "in" "instanceof"
    "interface" "keyof" "let" "module" "namespace" "never" "new"
    "null" "number" "object" "of" "private" "protected" "public"
    "readonly" "return" "set" "static" "string" "super" "switch"
    "this" "throw" "true" "try" "type" "typeof" "unknown"
    "var" "void" "while" "yield")
  "List of reserved words in javascript.")

(defvar js-eval-assignment-operators
  '("=" "+=" "-=" "*=" "/=" "%=")
  "List of JavaScript assignment operators for evaluation.")

(defvar js-eval-use-window nil
  "Determines if JavaScript evaluation should use a window.")

(defmacro js-eval-with-temp-buffer (&rest body)
  "Evaluate BODY in a temporary JavaScript buffer with custom settings.

Remaining arguments BODY are Lisp expressions to be evaluated in the context of
a temporary buffer set up for JavaScript evaluation."
  `(with-temp-buffer
     (erase-buffer)
     (progn
       (set-syntax-table js-eval-mode-syntax-table)
       (setq-local open-paren-in-column-0-is-defun-start nil)
       (setq-local syntax-propertize-function #'js-eval-syntax-propertize)
       (setq-local parse-sexp-ignore-comments t)
       (setq-local comment-start "// ")
       (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
       (setq-local comment-end "")
       (syntax-ppss-flush-cache (point-min))
       (js-eval-syntax-propertize (point-min) (point-max))
       ,@body)))

(defun js-eval-path-at-point ()
  "Extract a substring from a JavaScript path at point."
  (when-let ((str (js-eval-parse-string)))
    (substring str 1 (1- (length str)))))

(defun js-eval-parse-es-export-braces ()
  "Parse JavaScript ES6 export braces and create items."
  (let ((body (js-eval-parse-object t))
        (parent-end)
        (display-path))
    (setq parent-end
          (if
              (null (save-excursion
                      (js-eval-forward-whitespace)
                      (equal (js-eval-get-word)
                             "from")))
              (point)
            (js-eval-re-search-forward "from")
            (js-eval-forward-whitespace)
            (setq display-path (js-eval-path-at-point))
            (js-eval-ensure-semicolon)
            (point)))
    (mapcar (lambda (cell) (let ((real-name
                             (car cell))
                            (as-name
                             (cdr cell)))
                        (js-eval-make-item
                         (or as-name real-name)
                         :start (js-eval-get-prop
                                 real-name :start)
                         :end (js-eval-get-prop
                               (or as-name real-name) :end)
                         :type (if
                                   (and (equal
                                         (js-eval-strip-text-props
                                          real-name)
                                         "default")
                                        (or
                                         (null as-name)
                                         (equal
                                          (js-eval-strip-text-props as-name)
                                          "default")))
                                   1 4)
                         :var-type "export"
                         :parent-end parent-end
                         :display-path display-path)))
            body)))

(defun js-eval-get-assigmnent-operator ()
  "Return assignment operator at point if it's in the predefined list."
  (when-let ((operator (js-eval-get-operator-at-point)))
    (when (member operator js-eval-assignment-operators)
      operator)))

(defun js-eval-parse-amd-export ()
  "Parse AMD export from JavaScript code."
  (when-let* ((define-keyword (and (looking-at "\\_<\\(define\\)\\_>")
                                   (js-eval-get-obj-key)))
              (args (progn
                      (js-eval-forward-whitespace)
                      (js-eval-parse-arguments t)))
              (func (seq-find (lambda (it) (js-eval-get-prop
                                       it :args))
                              args))
              (found
               (seq-find (lambda (it) (equal
                                  (js-eval-get-prop
                                   it :var-type) "return"))
                         (js-eval-get-prop func :children))))
    (js-eval-ensure-semicolon)
    (js-eval-make-item (js-eval-get-prop found :value)
                          :type 1
                          :export t
                          :parent-start
                          (js-eval-get-prop define-keyword :start)
                          :parent-end (point))))

(defun js-eval-apply-on-one-many (func items)
  "Apply FUNC to each item in ITEMS, recursively if nested lists.

Argument FUNC is a function to be applied to ITEMS.

Argument ITEMS is a list of items or a single item to which FUNC will be
applied."
  (if (listp items)
      (mapcar (lambda (it) (js-eval-apply-on-one-many func it))
              items)
    (and items (funcall func items))))

(defun js-eval-parse-es-import-braces ()
  "Parse JavaScript ES6 import braces and return a list of items."
  (when-let ((obj (js-eval-parse-object t)))
    (mapcar (lambda (cell) (let ((real-name
                             (car cell))
                            (as-name
                             (cdr cell)))
                        (js-eval-make-item
                         (or as-name real-name)
                         :as-name (or as-name real-name)
                         :real-name (or real-name as-name)
                         :start (js-eval-get-prop
                                 real-name :start)
                         :end (js-eval-get-prop
                               (or as-name real-name) :end)
                         :type 4
                         :var-type "import")))
            obj)))

(defun js-eval--looking-at (regexp)
  "Check if text after point matches REGEXP without case sensitivity.

Argument REGEXP is a string containing the regular expression to match against
the text at the current point position."
  (let ((case-fold-search nil))
    (looking-at regexp)))

(defun js-eval-get-require-path ()
  "Extract the file path from a JavaScript require statement."
  (let ((case-fold-search nil))
    (when-let* ((bounds
                 (when (looking-at "\\_<\\(require\\)\\_>")
                   (save-excursion
                     (js-eval-get-obj-key)
                     (js-eval-forward-whitespace)
                     (when (equal (js-eval-get-next-char) "(")
                       (js-eval-forward-scope)))))
                (path (save-excursion (goto-char (1+ (car bounds)))
                                      (js-eval-forward-whitespace)
                                      (js-eval-parse-string))))
      (goto-char (cdr bounds))
      (js-eval-maybe-strip-quotes path))))

(defun js-eval-propertize (item &rest props)
  "Apply text properties to a JavaScript-evaluated ITEM.

Argument ITEM is the object to be converted to a string and propertized.

Remaining arguments PROPS are property-value pairs to be applied to the string
representation of ITEM."
  (apply #'propertize
         (js-eval-stringify item)
         props))

(defun js-eval-reserved-word-p (str)
  "Check if STR is a reserved JavaScript word.

Argument STR is a string to check against the list of reserved JavaScript words."
  (when (stringp str)
    (member str js-eval-reserved-js-words)))

(defun js-eval-ensure-semicolon ()
  "Ensure semicolon presence after JavaScript code."
  (let ((pos))
    (setq pos (if (looking-at ";")
                  (1+ (point))
                (save-excursion
                  (and (> (js-eval-forward-whitespace) 0)
                       (looking-at ";")
                       (1+ (point))))))
    (when pos
      (goto-char pos)
      pos)))

(defun js-eval-parse-iife ()
  "Parse JavaScript Immediately Invoked Function Expression (IIFE) syntax."
  (when-let ((children
              (pcase (js-eval-get-next-char)
                ((or "!" ";")
                 (forward-char 1)
                 (js-eval-forward-whitespace)
                 (when (looking-at "[(]")
                   (forward-char 1)
                   (js-eval-forward-whitespace))
                 (js-eval-parse-function-declaration t))
                ("("
                 (forward-char 1)
                 (js-eval-forward-whitespace)
                 (js-eval-parse-function-declaration t)))))
    (js-eval-forward-whitespace)
    (when (looking-at "[)]")
      (forward-char 1)
      (js-eval-forward-whitespace))
    (when (looking-at "[(]")
      (setq children (append (if (listp children)
                                 children
                               (list children))
                             (js-eval-parse-arguments t))))
    children))

(defun js-eval-parse-export ()
  "Parse JavaScript export statements and create export items."
  (let ((parent-start (point))
        (result
         (cond
          ((looking-at "\\_<\\(export\\)\\_>")
           (let ((next))
             (js-eval-get-obj-key)
             (js-eval-forward-whitespace)
             (setq next (js-eval-get-next-char-or-word))
             (pcase next
               ("*"
                (forward-char 1)
                (js-eval-forward-whitespace)
                (let ((as-name-or-from (js-eval-get-obj-key))
                      (item))
                  (js-eval-forward-whitespace)
                  (setq item
                        (cond
                         ((equal as-name-or-from "from")
                          (js-eval-make-item "*"
                                                :display-path
                                                (js-eval-path-at-point)
                                                :var-type "export"
                                                :type 16
                                                :export t))
                         ((equal "as" as-name-or-from)
                          (js-eval-forward-whitespace)
                          (let ((token (js-eval-get-obj-key t)))
                            (js-eval-forward-whitespace)
                            (js-eval-make-item
                             token
                             :display-path
                             :real-name (js-eval-strip-text-props token)
                             :as-name (js-eval-strip-text-props token)
                             (and
                              (equal
                               (js-eval-get-obj-key)
                               "from")
                              (js-eval-forward-whitespace)
                              (js-eval-path-at-point))
                             :type 4
                             :export t)))))
                  (js-eval-ensure-semicolon)
                  (setq item
                        (js-eval-make-item item :parent-end (point)))))
               ("{"
                (js-eval-parse-es-export-braces))
               ("type"
                (if-let ((var (js-eval-parse-variable)))
                    var
                  (js-eval-get-obj-key)
                  (js-eval-forward-whitespace)
                  (js-eval-parse-es-export-braces)))
               ((or "const" "var" "let" "class" "interface" "enum")
                (js-eval-parse-variable))
               ("="
                (forward-char 1)
                (js-eval-forward-whitespace)
                (when-let ((value (js-eval-parse-value)))
                  (js-eval-forward-whitespace)
                  (when (js-eval-forward-lists)
                    (js-eval-get-obj-key))
                  (js-eval-make-item value
                                        :type 1
                                        :export t
                                        :value value
                                        :var-type "export")))
               ("default"
                (skip-chars-forward next)
                (js-eval-forward-whitespace)
                (js-eval-make-item
                 (or (js-eval-parse-function-declaration)
                     (js-eval-get-obj-key))
                 :var-type "export default"
                 :exports
                 (or (js-eval-parse-object t)
                     (js-eval-parse-function-declaration)
                     (js-eval-parse-value))
                 :export t
                 :type 1
                 :end (point)))
               ((or "async" "function" "function*")
                (when-let ((func (js-eval-parse-function-declaration)))
                  (js-eval-make-item
                   func
                   :export t
                   :value func
                   :type 4
                   :var-type
                   (concat
                    "export "
                    (js-eval-get-prop
                     func :var-type))))))))
          ((looking-at "\\_<\\(\\(module\\.\\)?exports\\)\\_>")
           (let ((start (point))
                 (node (match-string-no-properties 0))
                 (id)
                 (value))
             (goto-char (+ start (length node)))
             (if (member (js-eval-get-next-char 1) '("." "["))
                 (progn
                   (forward-char 1)
                   (setq id (js-eval-get-obj-key t))
                   (js-eval-forward-whitespace)
                   (when (js-eval-get-assigmnent-operator)
                     (js-eval-forward-whitespace)
                     (setq value (js-eval-parse-value))
                     (js-eval-make-item
                      id
                      :type 4
                      :var-type "module.exports"
                      :export t
                      :value value
                      :parent-end (point))))
               (js-eval-forward-whitespace)
               (and
                (js-eval-get-assigmnent-operator)
                (if-let ((named (progn
                                  (js-eval-forward-whitespace)
                                  (js-eval-parse-object t)))
                         (obj-end (point)))
                    (mapcar (lambda (it)
                              (let ((real-name)
                                    (as-name))
                                (when (consp it)
                                  (setq real-name (car it))
                                  (setq as-name (if (stringp (cdr it))
                                                    (cdr it)
                                                  it)))
                                (js-eval-make-item
                                 (or real-name it)
                                 :type 4
                                 :as-name (or as-name it)
                                 :export t
                                 :var-type "module.exports"
                                 :parent-end obj-end)))
                            named)
                  (if-let ((path
                            (progn (when (looking-at "\\_<\\(require\\)\\_>")
                                     (skip-chars-forward
                                      js-eval-regexp-name)
                                     (js-eval-forward-whitespace))
                                   (when (equal
                                          (js-eval-get-next-char) "(")
                                     (forward-char 1)
                                     (js-eval-forward-whitespace)
                                     (js-eval-path-at-point)))))
                      (js-eval-make-item (format "require(%s)" path)
                                            :type 16
                                            :display-path path
                                            :export t
                                            :var-type "module.exports"
                                            :parent-end (point))
                    (setq value (js-eval-parse-value))
                    (js-eval-make-item value
                                          :type 1
                                          :value value
                                          :export t
                                          :var-type "module.exports"
                                          :parent-end (point))))))))
          (t (js-eval-parse-amd-export))))
        (export-node))
    (setq export-node
          (js-eval-apply-on-one-many (lambda (it)
                                          (js-eval-make-item
                                           it
                                           :export t
                                           :as-name
                                           (or (js-eval-get-prop it :as-name)
                                               (js-eval-strip-text-props it))
                                           :real-name
                                           (or
                                            (js-eval-get-prop it :real-name)
                                            (js-eval-strip-text-props it))
                                           :type
                                           (or (js-eval-get-prop it :type)
                                               4)
                                           :parent-start parent-start))
                                        result))
    export-node))

(defun js-eval-parse-es-import ()
  "Parse JavaScript ES6 import statement for evaluation."
  (when (looking-at "\\_<\\(import\\)\\_>")
    (let ((start (point))
          (end)
          (named-imports)
          (dynamic-import)
          (namespace-import)
          (default-import)
          (display-path)
          (children))
      (skip-chars-forward js-eval-regexp-name)
      (js-eval-forward-whitespace)
      (while
          (pcase (js-eval-get-next-char-or-word)
            ("from"
             (skip-chars-forward js-eval-regexp-name)
             (js-eval-forward-whitespace))
            ("*"
             (let ((beg (point))
                   (as-name))
               (forward-char 1)
               (js-eval-forward-whitespace)
               (js-eval-get-obj-key)
               (js-eval-forward-whitespace)
               (setq as-name (js-eval-get-obj-key t))
               (js-eval-forward-whitespace)
               (setq namespace-import
                     (js-eval-make-item as-name
                                        :real-name "*"
                                        :as-name as-name
                                        :type 16
                                        :var-type "import"
                                        :start beg
                                        :end (point)))))
            ("type" (skip-chars-forward js-eval-regexp-name)
             (js-eval-forward-whitespace)
             (setq named-imports
                   (js-eval-parse-es-import-braces)))
            ("("
             (js-eval-get-obj-key)
             (setq dynamic-import (js-eval-parse-value)))
            ("{"
             (setq named-imports
                   (js-eval-parse-es-import-braces)))
            ("\"" (setq display-path (js-eval-path-at-point)))
            ("'" (setq display-path (js-eval-path-at-point)))
            ("=" (progn
                   (forward-char 1)
                   (js-eval-forward-whitespace)
                   (js-eval-parse-value)
                   nil))
            (_
             (when-let ((default (js-eval-get-word-if-valid))
                        (beg (point)))
               (skip-chars-forward js-eval-regexp-name)
               (setq default-import
                     (js-eval-make-item default
                                        :real-name default
                                        :type 1
                                        :parent-end end
                                        :parent-start start
                                        :var-type "import"
                                        :start beg
                                        :end (point))))))
        (js-eval-forward-whitespace)
        (skip-chars-forward ",")
        (js-eval-forward-whitespace))
      (js-eval-backward-whitespace)
      (js-eval-ensure-semicolon)
      (setq end (point))
      (setq children
            (mapcar
             (lambda (it)
               (js-eval-make-item
                it
                :display-path display-path
                :import t
                :type (or (js-eval-get-prop it :type)
                          4)
                :var-type "import"
                :parent-end end
                :parent-start start))
             (delete nil
                     (append
                      `(,default-import ,namespace-import)
                      named-imports))))
      (or children
          dynamic-import
          (and display-path (js-eval-make-item
                             display-path
                             :var-type "import"
                             :parent-start start
                             :parent-end end))))))

(defun js-eval-parse-variable (&optional node)
  "Parse JavaScript variable declarations and return a list of items.

Optional argument NODE is the AST node representing the current parse context."
  (when (js-eval--looking-at
         "\\_<\\(const\\|let\\|enum\\|var\\|type\\|class\\|interface\\|declare\\|namespace\\)\\_>")
    (let ((start (or (and
                      node
                      (js-eval-get-prop
                       node :parent-start))
                     (point)))
          (node-type)
          (nodes)
          (count))
      (pcase (setq node-type (js-eval-get-obj-key))
        ("declare"
         (js-eval-forward-whitespace)
         (if (not (looking-at "\\_<\\(module\\)\\_>"))
             (when-let ((item (or (js-eval-parse-function-declaration)
                                  (js-eval-parse-variable))))
               (if (listp item)
                   (mapcar (lambda (it)
                             (js-eval-make-item it
                                                   :parent-start start
                                                   :var-type
                                                   (if-let
                                                       ((var-type
                                                         (js-eval-get-prop
                                                          it :var-type)))
                                                       (concat
                                                        "declare " var-type)
                                                     "declare")))
                           item)
                 (js-eval-make-item item
                                       :parent-start start
                                       :var-type
                                       (if-let ((var-type
                                                 (js-eval-get-prop
                                                  item :var-type)))
                                           (concat "declare " var-type)
                                         "declare"))))
           (js-eval-get-obj-key)
           (js-eval-forward-whitespace)
           (js-eval-get-obj-key)
           (js-eval-forward-whitespace)
           (js-eval-parse-scope-inner)))
        ("namespace"
         (js-eval-forward-whitespace)
         (js-eval-get-obj-key)
         (js-eval-forward-whitespace)
         (js-eval-parse-scope-inner))
        ("type"
         (js-eval-forward-whitespace)
         (when-let ((id (js-eval-get-obj-key)))
           (let ((args (js-eval-forward-angles)))
             (js-eval-forward-whitespace)
             (js-eval-make-item
              id
              :args (if (stringp args) (list args) args)
              :var-type (or node "type")
              :value
              (when (equal (js-eval-get-operator-at-point) "=")
                (forward-char 1)
                (js-eval-forward-whitespace)
                (js-eval-parse-value))))))
        ((or "const" "let" "var")
         (js-eval-forward-whitespace)
         (while (or (null count)
                    (when-let ((pos (save-excursion
                                      (js-eval-forward-whitespace)
                                      (when
                                          (or
                                           (looking-at ",")
                                           (member
                                            (js-eval-get-operator-at-point)
                                            js-eval-assignment-operators))
                                        (point)))))
                      (goto-char pos)
                      t))
           (if (null count)
               (setq count 0)
             (skip-chars-forward ",")
             (setq count (1+ count))
             (js-eval-forward-whitespace))
           (let ((token (js-eval-parse-token-at-point))
                 (value)
                 (ts-type)
                 (import-path))
             (js-eval-forward-whitespace)
             (if (equal (js-eval-strip-text-props token) "enum")
                 (when-let ((id (js-eval-get-obj-key)))
                   (js-eval-forward-whitespace)
                   (push (js-eval-make-item
                          id
                          :var-type (string-join `(,node-type "enum") "\s")
                          :value (js-eval-parse-value))
                         nodes))
               (when (equal (js-eval-get-next-char) ":")
                 (forward-char 1)
                 (js-eval-forward-whitespace)
                 (setq ts-type (js-eval-parse-value))
                 (js-eval-forward-whitespace))
               (setq value
                     (or (when (looking-at "=[^=]")
                           (forward-char 1)
                           (js-eval-forward-whitespace)
                           (setq import-path (save-excursion
                                               (js-eval-get-require-path)))
                           (or
                            (js-eval-parse-arrow-function)
                            (js-eval-parse-value)))
                         ts-type))
               (if (not (listp token))
                   (push (js-eval-make-item
                          token
                          :import (when import-path t)
                          :display-path import-path
                          :type (when import-path 1)
                          :var-type
                          (string-join
                           (delete nil
                                   `(,(and
                                       node
                                       (js-eval-strip-text-props
                                        node))
                                     ,node-type)) "\s")
                          :value value)
                         nodes)
                 (dolist (subitem token)
                   (let ((it
                          (js-eval-make-item
                           subitem
                           :import (when import-path t)
                           :type (when import-path 4)
                           :display-path import-path
                           :var-type
                           (string-join
                            (delete nil `(,(and
                                            node
                                            (js-eval-strip-text-props node))
                                          ,node-type)) "\s")
                           :value value)))
                     (push it nodes)))))))
         (js-eval-ensure-semicolon)
         (setq nodes (mapcar (lambda (it)
                               (js-eval-propertize it
                                                      :parent-start start
                                                      :parent-end
                                                      (point)))
                             nodes)))
        ((or "class" "interface" "enum")
         (js-eval-get-obj-key)
         (js-eval-forward-whitespace)
         (let ((id (js-eval-get-obj-key)))
           (js-eval-forward-whitespace)
           (js-eval-forward-angles)
           (js-eval-forward-whitespace)
           (while (js-eval-get-obj-key)
             (js-eval-forward-angles)
             (when (> (js-eval-forward-whitespace "\s\t\n\r\f\v,") 0)
               (js-eval-forward-angles)))
           (js-eval-make-item id
                                 :var-type (format "%s" (or node node-type))
                                 :value (js-eval-parse-object))))))))

(defun js-eval-parse-statement ()
  "Parse JavaScript statements for evaluation."
  (when-let ((item (and (js-eval-reserved-word-p (js-eval-which-word))
                        (js-eval-get-obj-key))))
    (js-eval-forward-whitespace)
    (let ((parts))
      (while (looking-at "[\\[]")
        (let ((prop (js-eval-get-obj-key)))
          (push prop parts)))
      (when parts
        (push item parts)
        (setq item (string-join parts ""))
        (js-eval-forward-whitespace)))
    (pcase item
      ("try" (let ((items (js-eval-parse-scope-inner))
                   (next))
               (js-eval-forward-whitespace)
               (setq next (js-eval-get-obj-key))
               (when (equal next "catch")
                 (js-eval-forward-whitespace)
                 (setq items (append items (js-eval-parse-arguments)))
                 (js-eval-forward-lists)
                 (js-eval-forward-whitespace)
                 (setq items (append items (js-eval-parse-scope-inner))))
               (when (equal (save-excursion
                              (js-eval-forward-whitespace)
                              (js-eval-get-obj-key)) "finally")
                 (js-eval-re-search-forward "finally" nil t 1)
                 (js-eval-forward-whitespace)
                 (setq items (append items (js-eval-parse-scope-inner))))
               items))
      ("do" (let ((items (js-eval-parse-scope-inner)))
              (js-eval-re-search-forward "\\_<\\(while\\)\\_>" nil t 1)
              (js-eval-forward-whitespace)
              (js-eval-forward-lists)
              items))
      ("while"
       (js-eval-forward-whitespace)
       (js-eval-forward-lists))
      ((or "if" "for" "switch")
       (js-eval-forward-lists)
       (js-eval-forward-whitespace)
       (if (not (looking-at "{"))
           (js-eval-get-obj-key)
         (js-eval-parse-scope-inner)))
      ("else"
       (pcase (js-eval-get-next-char-or-word)
         ("{" (js-eval-re-search-forward "{" nil t 1)
          (forward-char -1)
          (js-eval-parse-scope-inner))
         ("if" (js-eval-re-search-forward "if" nil t 1)
          (js-eval-forward-whitespace)
          (if (js-eval-get-obj-key)
              (js-eval-parse-value)
            (when (js-eval-forward-lists)
              (js-eval-forward-whitespace)
              (js-eval-parse-scope-inner))))))
      ("return"
       (let ((value (js-eval-parse-value)))
         (js-eval-make-item value
                               :var-type "return"
                               :value value)))
      ((or "yield" "yield*")
       (js-eval-forward-whitespace)
       (let ((value (js-eval-parse-value)))
         (js-eval-make-item value
                               :value value
                               :start (js-eval-get-prop item :start)
                               :end (js-eval-get-prop item :end)
                               :var-type item))))))

(defun js-eval-parse-assignment ()
  "Parse JavaScript assignments and return their AST nodes."
  (let ((start (point))
        (nodes))
    (while (when-let* ((id (or
                            (js-eval-parse-funcall)
                            (js-eval-get-obj-key t)))
                       (operator (progn
                                   (js-eval-forward-whitespace)
                                   (js-eval-get-assigmnent-operator))))
             (push id nodes))
      (js-eval-forward-whitespace))
    (unless nodes (goto-char start))
    (when nodes
      (let ((value (or (js-eval-parse-value)
                       (pop nodes))))
        (or (mapcar (lambda (it) (js-eval-make-item it
                                                  :var-type "Assignment"
                                                  :value value))
                    (reverse nodes))
            (js-eval-make-item value :var-type "Assignment"))))))

(defun js-eval-valid-identifier-p (string)
  "Check if STRING is a valid JavaScript identifier.

Argument STRING is the string to check for being a valid JavaScript identifier."
  (not (or
        (null string)
        (js-eval-string-match-p (concat "[" "^" js-eval-regexp-name "]")
                                   string)
        (js-eval-reserved-word-p string))))

(defun js-eval-parse-node-at-point (&optional deep)
  "Parse JavaScript code node at the current point in the buffer.

Optional argument DEEP is a boolean indicating whether to parse the node deeply.
If non-nil, the function will parse nested structures within the node."
  (let ((node-start)
        (node-end)
        (node))
    (js-eval-forward-whitespace)
    (when (js-eval-ensure-semicolon)
      (js-eval-forward-whitespace))
    (setq node-start (point))
    (setq node (or
                (js-eval-parse-iife)
                (js-eval-parse-export)
                (js-eval-parse-es-import)
                (js-eval-parse-function-declaration deep)
                (js-eval-parse-variable)
                (js-eval-parse-statement)
                (js-eval-parse-assignment)
                (js-eval-parse-value)))
    (unless node
      (setq node-start (point)))
    (when (and (listp node)
               (= (length node) 1))
      (setq node (pop node)))
    (js-eval-ensure-semicolon)
    (setq node-end (point))
    (if (listp node)
        node
      (js-eval-make-item node :start node-start :end node-end))))

(defun js-eval-get-word-if-valid ()
  "Return word at point if it's a valid JavaScript identifier."
  (when-let ((word (js-eval-which-word)))
    (when (js-eval-valid-identifier-p word)
      word)))

(defun js-eval-parse-funcall-params ()
  "Parse JavaScript function call parameters."
  (js-eval-parse-arguments nil 'js-eval-parse-object))

(defun js-eval-parse-scope (&optional start end deep callback)
  "Parse JavaScript scope between START and END, optionally DEEP, with CALLBACK.

Optional argument START is the position in the buffer where parsing should
begin.

Optional argument END is the position in the buffer where parsing should stop.

Optional argument DEEP is a boolean indicating whether to parse deeply.

Optional argument CALLBACK is a function to be called with each parsed node."
  (when start (goto-char start))
  (let ((node)
        (prev-pos)
        (nodes))
    (js-eval-forward-whitespace)
    (while (or (not (equal prev-pos (point))))
      (setq prev-pos (point))
      (setq node (if end
                     (and (>= end (point))
                          (js-eval-parse-node-at-point deep))
                   (js-eval-parse-node-at-point deep)))
      (when callback
        (funcall callback node))
      (if (listp node)
          (setq nodes (append node nodes))
        (push node nodes))
      (js-eval-forward-whitespace "\s\t\n\r\f\v,=?:+#-"))
    (reverse nodes)))

(defun js-eval-get-word (&optional regexp)
  "Extract a word from buffer using optional REGEXP.

Optional argument REGEXP is a regular expression to match a word. If nil, a
default word-matching regular expression is used."
  (when-let ((bounds (js-eval-get-bounds regexp)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun js-eval-skip-string ()
  "Skip over a JavaScript string literal at point."
  (with-syntax-table js-eval-mode-syntax-table
    (when-let ((beg (nth 8 (syntax-ppss (point)))))
      (goto-char beg)
      (forward-sexp 1))))

(defun js-eval-parse-funcall ()
  "Parse JavaScript function call and its chained calls into a structured list."
  (let ((name)
        (end)
        (start (point))
        (args)
        (results))
    (when (and (js-eval-get-word-if-valid)
               (save-excursion
                 (setq name (js-eval-get-obj-key))
                 (js-eval-forward-whitespace "\s\t")
                 (setq args (js-eval-parse-funcall-params))
                 (setq end (point))
                 (when (and args
                            end)
                   (js-eval-forward-whitespace "\s\t")
                   (null (looking-at "{")))))
      (goto-char end)
      (while (or (looking-at "\\.")
                 (save-excursion
                   (js-eval-forward-whitespace "\s\t")
                   (looking-at "[({]")))
        (if-let ((calls (progn
                          (js-eval-forward-whitespace "\s\t")
                          (js-eval-parse-funcall-params))))
            (push calls results)
          (push (list "." (js-eval-get-obj-key)) results))
        (setq end (point)))
      (js-eval-make-item name
                            :args args
                            :start start
                            :end (point)
                            :children results
                            :var-type "Funcall"))))

(defun js-eval-parse-regexp ()
  "Parse and return a JavaScript regular expression with flags from buffer."
  (when-let ((re (and (looking-at js-eval--regexp-js-regexp)
                      (js-eval-re-search-forward
                       js-eval--regexp-js-regexp nil t 1)
                      (match-string-no-properties 0))))
    (let ((pos (point)))
      (if (> (skip-chars-forward "gimsuy") 0)
          (concat re (buffer-substring-no-properties pos (point)))
        re))))

(defun js-eval-get-operator-at-point (&optional n)
  "Retrieve the operator at the cursor in a buffer.

Optional argument N is the number of characters to move; it can be negative to
move backwards. If omitted, it defaults to moving forward."
  (let ((start (point)))
    (if (and n (< n 0))
        (when (< (skip-chars-backward "!@#%^&*=><~|\\-+/?:") 0)
          (buffer-substring-no-properties (point) start))
      (when (> (skip-chars-forward "!@#%^&*=><~|\\-+/?:") 0)
        (buffer-substring-no-properties start (point))))))

(defun js-eval-forward-angles ()
  "Navigate forward past angle brackets in pairs."
  (when-let ((count (when (looking-at "<")
                      (forward-char 1)
                      1))
             (beg (1- (point))))
    (while (and (> count 0)
                (js-eval-re-search-forward "[[{(<]\\|>" nil t 1))
      (let ((chars (split-string (js-eval-get-prev-char 2) "" t)))
        (pcase (nth 1 chars)
          ((or "{" "(" "[")
           (forward-char -1)
           (forward-sexp 1))
          ("<" (setq count (1+ count)))
          (_ (unless (equal (car chars) "=")
               (setq count (1- count)))))))
    (buffer-substring-no-properties beg (point))))

(defun js-eval-parse-string ()
  "Extract a string literal at point without properties."
  (when-let ((start (and (looking-at "[`'\"]")
                         (point))))
    (forward-sexp 1)
    (buffer-substring-no-properties start (point))))

(defun js-eval-parse-arguments (&optional deep parse-object-fn)
  "Parse function arguments from a buffer point.

Optional argument DEEP is a boolean indicating whether to parse deeply.

Optional argument PARSE-OBJECT-FN is a function used to parse an object; it
defaults to `js-eval-parse-token-at-point'."
  (unless parse-object-fn (setq parse-object-fn
                                'js-eval-parse-token-at-point))
  (when-let ((end (save-excursion
                    (when (looking-at "[(]")
                      (forward-sexp 1)
                      (point)))))
    (forward-char 1)
    (js-eval-forward-whitespace)
    (let ((args)
          (count))
      (while (or (null count)
                 (looking-at ","))
        (if (null count)
            (setq count 0)
          (skip-chars-forward ",")
          (setq count (1+ count))
          (js-eval-forward-whitespace))
        (if-let ((children (js-eval-parse-function-declaration deep)))
            (setq args (append args
                               (if (stringp children) (list children)
                                 children)))
          (let ((token (if (looking-at "[{]")
                           (funcall parse-object-fn)
                         (js-eval-parse-value)))
                (value)
                (item))
            (js-eval-forward-whitespace)
            (setq value
                  (when (looking-at "=[^=]")
                    (forward-char 1)
                    (js-eval-forward-whitespace)
                    (js-eval-parse-value)))
            (js-eval-forward-whitespace)
            (if (stringp token)
                (progn
                  (let ((ts-type (string-match-p ":" token)))
                    (setq item (js-eval-make-item
                                (if ts-type
                                    (substring token 0 ts-type)
                                  token)
                                :var-type
                                (or (js-eval-get-prop token :var-type)
                                    "Argument")
                                :value
                                (or value (and ts-type
                                               (string-trim
                                                (substring token ts-type)))))))
                  (push item args))
              (setq args (append args
                                 (mapcar
                                  (lambda (param)
                                    (js-eval-make-item
                                     param
                                     :var-type (or (js-eval-get-prop
                                                    param
                                                    :var-type)
                                                   "Argument")
                                     :value value))
                                  token)))))))
      (goto-char end)
      (or (reverse args) '("")))))

(defun js-eval-get-return-type ()
  "Parse and return the JavaScript object's return type."
  (when (looking-at ":")
    (forward-char 1)
    (js-eval-forward-whitespace)
    (if (looking-at "{")
        (js-eval-parse-object-recoursive)
      (let ((type)
            (types))
        (while (setq type (js-eval-get-obj-key t))
          (js-eval-forward-whitespace)
          (push type types))
        (string-join (reverse types) "\s")))))

(defun js-eval-skip-arrow ()
  "Skip => in JavaScript code and move point forward."
  (when-let ((next (js-eval-get-next-char 2)))
    (when (string= next "=>")
      (forward-char 2)
      (prog1
          (point)
        (js-eval-forward-whitespace)))))

(defun js-eval-get-word-or-char ()
  "Retrieve a word or the next character."
  (or (js-eval-get-word)
      (js-eval-get-next-char)))

(defun js-eval-get-comments-bounds ()
  "Find and return positions of all comments in a buffer."
  (save-excursion
    (save-restriction
      (with-syntax-table js-eval-mode-syntax-table
        (let (comments)
          (goto-char (point-min))
          (while (re-search-forward "\\(/\\*\\)\\|\\(//\\)\\|[\"'`]" nil t 1)
            (if (save-excursion
                  (backward-char 1)
                  (looking-at "[\"'`]"))
                (js-eval-skip-string)
              (save-excursion
                (backward-char 1)
                (if (looking-at-p "\\*")
                    (progn
                      (let* ((p1 (1- (point)))
                             (p2 (re-search-forward "\\(\\*/\\)" nil t 1)))
                        (push (cons p1 p2) comments)))
                  (let* ((p1 (1- (point)))
                         (p2 (line-end-position)))
                    (push (cons p1 p2) comments))))))
          comments)))))

(defun js-eval-find-package-json ()
  "Find and return the path to `package.json' in the project root."
  (when-let ((root (js-eval-find-project-root)))
    (js-eval-join-file root "package.json")))

(defun js-eval-get-next-char (&optional nth position)
  "Extract the next NTH character(s) from buffer starting at POSITION.

Optional argument NTH is the number of characters to move forward from the
current POSITION or position if provided. It defaults to 1.

Optional argument POSITION is the buffer position from which to start. If not
provided, the current point is used."
  (let* ((beg (or position (point)))
         (end (+ (or nth 1) beg)))
    (when (> (point-max) end)
      (buffer-substring-no-properties beg end))))

(defun js-eval-parse-value ()
  "Parse JavaScript values from buffer text."
  (let ((start)
        (end)
        (value)
        (count)
        (nodes))
    (setq start (point))
    (while
        (progn
          (and
           (setq count (or (and count (1+ count))
                           0))
           (setq value
                 (or
                  (when-let ((v (or (js-eval-parse-funcall)
                                    (js-eval-parse-regexp))))
                    (if (equal (js-eval-get-next-char 1) ".")
                        (setq v (progn (forward-char 1) (concat v ".")))
                      v))
                  (if-let ((operator
                            (save-excursion
                              (js-eval-get-operator-at-point))))
                      (or
                       (js-eval-forward-angles)
                       (progn
                         (skip-chars-forward operator)
                         (js-eval-forward-whitespace)
                         operator))
                    (pcase (js-eval-get-next-char-or-word)
                      ((or "export" "const" "module.exports"
                           "class" "let" "var" "enum" "interface")
                       nil)
                      ((or "async" "function" "function*")
                       (js-eval-parse-function-declaration))
                      ("as"
                       (js-eval-get-obj-key)
                       (js-eval-forward-whitespace)
                       (js-eval-get-obj-key))
                      ("{"
                       (let ((a (point)))
                         (forward-sexp 1)
                         (if (= count 0)
                             (js-eval-make-item
                              (buffer-substring-no-properties a (point))
                              :value-type "object")
                           (buffer-substring-no-properties a (point)))))
                      ("["
                       (let ((a (point)))
                         (forward-sexp 1)
                         (if (= count 0)
                             (js-eval-make-item
                              (buffer-substring-no-properties a (point))
                              :value-type "array")
                           (buffer-substring-no-properties a (point)))))
                      ("<"
                       (js-eval-forward-angles))
                      ("("
                       (let ((a (point)))
                         (forward-sexp 1)
                         (if (> count 1)
                             (js-eval-make-item
                              (buffer-substring-no-properties a (point))
                              :value-type "call"
                              :args
                              (buffer-substring-no-properties a (point)))
                           (buffer-substring-no-properties a (point)))))
                      ((or "\"" "'" "`")
                       (js-eval-make-item
                        (js-eval-parse-string)
                        :value-type "string"))
                      (_ (let ((key (js-eval-get-obj-key)))
                           (if (looking-at ":")
                               (progn (forward-char 1)
                                      (format "%s:" key))
                             key)))))))))
      (push value nodes)
      (js-eval-forward-whitespace "\s\t")
      (when-let ((func (and (looking-at "{")
                            (equal (and value
                                        (js-eval-get-prop value :value-type))
                                   "call"))))
        (let ((children (js-eval-parse-scope-inner)))
          (js-eval-forward-whitespace "\s\t")
          (js-eval-make-item
           func
           :children children
           :value-end (point)
           :value-type "function"))
        (js-eval-forward-whitespace "\s\t"))
      (when (looking-at "\\(\n\\|\r\\)[\s\t\f]*[?:|><,]")
        (js-eval-forward-whitespace))
      (setq end (point))
      (when (looking-at "\\.")
        (forward-char 1)
        (js-eval-get-obj-key)
        (setq end (point))
        (js-eval-forward-whitespace "\s\t")))
    (when nodes
      (if (> (length nodes) 1)
          (js-eval-make-item
           (string-join (reverse nodes) "\s")
           :start start
           :end end)
        (let ((val (pop nodes)))
          (if (listp val)
              (js-eval-make-item
               (mapconcat #'js-eval-stringify val "")
               :start start
               :end end)
            (js-eval-make-item
             (format "%s" val)
             :start start
             :end end)))))))

(defun js-eval-get-obj-key (&optional with-props &rest args)
  "Extract JavaScript object key under cursor.

Optional argument WITH-PROPS is a boolean indicating whether to include text
properties in the result.

Remaining arguments ARGS are additional properties to include if WITH-PROPS is
non-nil."
  (let ((re "*_~$A-Za-z0-9.")
        (start)
        (end)
        (key))
    (if (looking-at "[\\|['\"`]")
        (progn
          (setq start (point))
          (forward-sexp 1)
          (setq end (point)))
      (when (looking-at (concat "[" re "]"))
        (setq start (point))
        (when (> (skip-chars-forward (concat re "?")) 0)
          (setq end (point)))
        (when (looking-at "\\[")
          (forward-sexp 1)
          (setq end (point)))))
    (setq key (and start end (buffer-substring-no-properties start end)))
    (if (and key with-props)
        (apply #'js-eval-propertize
               (append (list key)
                       (list :id key
                             :start start
                             :end end)
                       args))
      key)))

(defun js-eval-get-next-char-or-word ()
  "Retrieve next character or entire word from buffer."
  (when-let ((char (js-eval-get-next-char)))
    (if (string-match-p "[_$A-Za-z0-9]" char)
        (js-eval-which-word)
      char)))

(defun js-eval-parse-object-method (&optional id)
  "Parse JavaScript object method and return its structure.

Optional argument ID is an identifier for the parsed object method; if not
provided, \"method\" is used as the default identifier."
  (if-let* ((pos (point))
            (args (progn
                    (js-eval-get-obj-key)
                    (js-eval-forward-whitespace)
                    (when (js-eval-forward-angles)
                      (js-eval-forward-whitespace))
                    (js-eval-forward-whitespace)
                    (js-eval-parse-arguments)))
            (body-bounds (progn
                           (js-eval-forward-whitespace)
                           (js-eval-get-return-type)
                           (or (js-eval-forward-scope)
                               (cons
                                (point) (point))))))
      (js-eval-make-item
       (or id "method")
       :args args
       :var-type "Method"
       :value-start (car body-bounds)
       :value-end (cdr body-bounds))
    (goto-char pos)
    nil))

(defun js-eval-parse-arrow-function ()
  "Parse JavaScript arrow function syntax into structured data."
  (if-let* ((parent-start (point))
            (args
             (progn
               (js-eval-forward-whitespace)
               (when (js-eval-forward-angles)
                 (js-eval-forward-whitespace))
               (or (js-eval-parse-arguments)
                   (js-eval-get-obj-key))))
            (arrow (progn
                     (js-eval-forward-whitespace)
                     (let ((returns (js-eval-get-return-type)))
                       (when returns
                         (js-eval-forward-whitespace)))
                     (js-eval-skip-arrow))))
      (let ((children (progn
                        (js-eval-forward-whitespace)
                        (js-eval-parse-scope-inner)))
            (value))
        (unless children (setq value (js-eval-parse-value)))
        (js-eval-make-item
         "Anonymus"
         :args args
         :value value
         :children children
         :return-type (or value)
         :var-type "function"
         :parent-start parent-start
         :parent-end (point)))
    (goto-char parent-start)
    nil))

(defun js-eval-skip-to-char-same-scope (&optional stop-chars)
  "Skip to a character within the same scope.

Optional argument STOP-CHARS is a string of characters to stop at. It defaults
to \"[;]\"."
  (unless stop-chars (setq stop-chars "[;]"))
  (let ((open-chars-re "[^]({[;}]+")
        (search-point)
        (scope-start)
        (scope-end)
        (stop))
    (when (looking-at "[({[]")
      (forward-sexp 1))
    (while (and
            (null stop)
            (not (looking-at stop-chars))
            (not (looking-at "[]})]"))
            (progn
              (setq search-point
                    (save-excursion
                      (js-eval-re-search-forward stop-chars nil t 1)))
              (save-excursion
                (setq scope-start
                      (js-eval-re-search-forward open-chars-re nil t 1))
                (forward-char -1)
                (setq scope-end
                      (when (looking-at "[({[]")
                        (forward-sexp 1)
                        (setq scope-end (point)))))
              (cond ((and scope-start search-point
                          (> search-point scope-start))
                     (if scope-end
                         (goto-char scope-end)
                       (goto-char scope-start)))
                    ((and search-point)
                     (goto-char search-point)
                     (skip-chars-backward stop-chars)
                     (setq stop t)))))
      (if (looking-at "[({[]")
          (forward-sexp 1)
        (setq stop t)))
    (and (looking-at stop-chars)
         (point))))

(defun js-eval-parse-function-declaration (&optional deep)
  "Parse JavaScript function declarations for evaluation.

Optional argument DEEP is a boolean indicating whether to parse deeply; if
non-nil, the function will parse the scope inner deeply."
  (let ((parent-start))
    (when (looking-at "\\_<\\(async\\)\\_>")
      (setq parent-start (point))
      (skip-chars-forward js-eval-regexp-name)
      (js-eval-forward-whitespace))
    (pcase (js-eval-get-word-or-char)
      ((or "function" "function*")
       (let ((node-start (or parent-start (point)))
             (node-end (+ (point)
                          (skip-chars-forward "*_~$A-Za-z0-9")))
             (id-pos)
             (body-start)
             (body-end)
             (func-type)
             (result)
             (id)
             (children))
         (setq func-type
               (buffer-substring-no-properties node-start node-end))
         (js-eval-forward-whitespace)
         (when (looking-at "\\*")
           (setq func-type (concat func-type "*"))
           (forward-char 1))
         (setq id-pos (point))
         (setq id
               (or (and
                    (> (skip-chars-forward "_~$A-Za-z0-9") 0)
                    (buffer-substring-no-properties
                     id-pos (point)))
                   "Anonymus"))
         (js-eval-forward-whitespace)
         (when (js-eval-forward-angles)
           (js-eval-forward-whitespace))
         (when-let ((args (js-eval-parse-arguments deep)))
           (js-eval-forward-whitespace)
           (when (looking-at ":")
             (js-eval-forward-whitespace)
             (js-eval-parse-value))
           (setq body-start (point))
           (setq children
                 (and (looking-at "{")
                      (if deep
                          (js-eval-parse-scope-inner body-start)
                        (forward-sexp 1))))
           (setq body-end (point))
           (js-eval-forward-angles)
           (setq result
                 (js-eval-make-item
                  id
                  :var-type func-type
                  :start node-start
                  :end node-end
                  :id id
                  :value-start body-start
                  :value-end body-end
                  :children children
                  :args args))
           (if deep
               (append (list result) children)
             result))))
      (_ (js-eval-parse-arrow-function)))))

(defun js-eval-normalize-value (value)
  "Convert string VALUE to number if numeric, trim whitespace.

Argument VALUE is a string that will be trimmed and possibly converted to a
number if it is numeric."
  (setq value
        (string-trim value))
  (if (string-match-p "^[0-9]+$" value)
      (setq value (string-to-number value))
    value))

(defmacro js-eval-with-buffer-or-file-content (filename &rest body)
  "Evaluate JavaScript with buffer or file content.

Argument FILENAME is the name of the file whose content will be used.

Remaining arguments BODY are forms that are evaluated with the content of
FILENAME."
  (declare (indent 2))
  `(when (and ,filename (file-exists-p ,filename))
     (js-eval-with-temp-buffer
      (save-excursion (insert (with-temp-buffer
                                (let ((inhibit-read-only t))
                                  (erase-buffer)
                                  (if-let ((buff (get-file-buffer ,filename)))
                                      (insert-buffer-substring-no-properties
                                       buff)
                                    (insert-file-contents ,filename))
                                  (buffer-string)))))
      (let ((buffer-file-name ,filename)
            (default-directory (funcall
                                (js-eval-compose
                                 #'js-eval-slash
                                 #'js-eval-dirname)
                                ,filename)))
        ,@body))))

(defun js-eval-remove-comments ()
  "Strip comments from JavaScript code."
  (let ((comments (js-eval-get-comments-bounds))
        (cell))
    (save-excursion
      (while (setq cell (pop comments))
        (when-let* ((start (car cell))
                    (end (or (cdr cell)
                             (save-excursion (goto-char start)
                                             (end-of-line)
                                             (point)))))
          (let ((content (buffer-substring-no-properties start end))
                (lines))
            (setq lines (split-string content "\n"))
            (goto-char start)
            (delete-region start end)
            (insert (string-join (mapcar
                                  (lambda (it)
                                    (string-join
                                     (append
                                      (make-vector
                                       (1+ (length it)) "")
                                      nil)
                                     "\s"))
                                  lines) "\n"))))))))

(defun js-eval-read-package-json-section (&optional package-json-path
                                                       section)
  "Extract a SECTION from a package.json file.

Optional argument PACKAGE-JSON-PATH is a string specifying the path to the
package.json file. If not provided, the function will attempt to find the
package.json file automatically.

Optional argument SECTION is a string specifying the section of the package.json
to read, such as \"dependencies\". If not provided, it defaults to
\"dependencies\"."
  (unless section (setq section "dependencies"))
  (let ((path (or package-json-path (js-eval-find-package-json)))
        (json-object-type 'hash-table))
    (when-let ((content
                (condition-case nil
                    (decode-coding-string
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (setq buffer-file-coding-system 'binary)
                       (insert-file-contents-literally path)
                       (buffer-substring-no-properties
                        (point-min) (point-max)))
                     'utf-8)
                  (error nil))))
      (condition-case nil
          (gethash section (json-read-from-string content))
        (error nil)))))

(defun js-eval-recode-string (str &optional coding-system)
  "Convert STR to a different CODING-SYSTEM and return the result.

Argument STR is the string to be evaluated and recoded.

Optional argument CODING-SYSTEM is the coding system to use for recoding the
string. It defaults to `utf-8'."
  (setq coding-system (or coding-system 'utf-8))
  (with-temp-buffer
    (insert str)
    (js-eval-recode-buffer-or-region coding-system)
    (buffer-string)))

(defun js-eval-recode-buffer-or-region (&optional coding-system)
  "Evaluate and recode buffer or region with optional CODING-SYSTEM.

Optional argument CODING-SYSTEM is the coding system to use for encoding and
decoding the buffer or region. It defaults to `utf-8' when not provided."
  (setq coding-system (or coding-system
                          (read-coding-system "Coding system:\s"
                                              'utf-8)))
  (let ((buffer-read-only nil)
        (bounds (if (and
                     (region-active-p)
                     (use-region-p))
                    (cons (region-beginning)
                          (region-end))
                  (cons (point-min)
                        (point-max))))
        (text))
    (setq text (buffer-substring-no-properties (car bounds)
                                               (cdr bounds)))
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    ;; string-make-unibyte => encode-coding-string
    (insert (decode-coding-string
             (encode-coding-string text coding-system) coding-system))))

(defun js-eval-forward-scope ()
  "Evaluate JavaScript scope from current point and return its bounds."
  (when (looking-at "[{[]")
    (let ((scope-start (point))
          (scope-end))
      (forward-sexp 1)
      (setq scope-end (point))
      (cons scope-start scope-end))))

(defun js-eval-read-json (file &optional json-type)
  "Parse JSON from FILE, caching results by modification time.

Argument FILE is the name of the file containing the JSON to read.

Optional argument JSON-TYPE specifies the type used to represent JSON objects;
it can be `hash-table', `alist', or `plist'. It defaults to `plist'."
  (condition-case nil
      (let* ((json-object-type (or json-type 'plist))
             (cache-key (format "%s:%s" file json-object-type))
             (cache (gethash cache-key js-eval-json-hash))
             (cache-tick (and cache (plist-get cache :tick)))
             (tick (file-attribute-modification-time (file-attributes
                                                      file
                                                      'string)))
             (content-json))
        (when (or (null cache)
                  (not (equal tick cache-tick)))
          (setq content-json
                (js-eval-with-buffer-or-file-content
                    file
                    (js-eval-remove-comments)
                  (when-let ((str (buffer-substring-no-properties
                                   (point-min)
                                   (point-max))))
                    (json-read-from-string str))))
          (setq cache (list :tick tick
                            :json content-json))
          (puthash cache-key cache js-eval-json-hash))
        (plist-get cache :json))
    (error (message "Cannot read %s" file)
           nil)))

(defun js-eval-extract-subpackages (path)
  "Extract subdirectories excluding specific ones from PATH.

Argument PATH is a string representing the file system path where the function
will search for subpackages."
  (let ((dirs (and path
                   (file-directory-p path)
                   (seq-filter #'file-directory-p
                               (mapcar
                                (js-eval-flip
                                 'expand-file-name
                                 path)
                                (funcall
                                 (js-eval-compose
                                  (apply-partially #'delete "node_modules")
                                  (apply-partially #'delete "..")
                                  (apply-partially #'delete ".")
                                  'directory-files)
                                 path))))))
    (append dirs (mapcan #'js-eval-extract-subpackages dirs))))

(defun js-eval-try-json-sections (json-file sections)
  "Evaluate JSON SECTIONS until one is found or return nil.

Argument JSON-FILE is the path to the JSON file to be processed.

Argument SECTIONS is a list of strings representing the sections of the JSON
file to be evaluated in order."
  (let (section)
    (while sections
      (setq section (js-eval-read-package-json-section
                     json-file
                     (pop sections)))
      (if section
          (setq sections nil)
        (setq section nil)))
    section))

(defun js-eval-try-ext (path &optional dir)
  "Evaluate JavaScript file at PATH with optional DIR, trying extensions.

Argument PATH is a string representing the file path to evaluate.

Optional argument DIR is a string representing the directory to expand PATH
relative to; if not provided, PATH is used as is."
  (let ((expanded-path (if dir (expand-file-name path dir) path)))
    (if (js-eval-string-match-p js-eval-file-ext-regexp path)
        expanded-path
      (seq-find #'file-exists-p
                (mapcar (apply-partially #'js-eval-add-ext-if-not
                                         expanded-path)
                        js-eval-preffered-extensions)))))

(defun js-eval-directory-files (dir &optional
                                       recursive regexp include-dirs pred)
  "Evaluate JavaScript files in a directory, optionally recursively.

Argument DIR is the directory to list files from.

Optional argument RECURSIVE is a boolean; when non-nil, list files recursively.

Optional argument REGEXP is a string used as a regular expression to match
files. It defaults to the value of `js-eval-file-ext-regexp'.

Optional argument INCLUDE-DIRS is a boolean; when non-nil, include directories
in the listing.

Optional argument PRED is a predicate function to filter files."
  (unless regexp (setq regexp js-eval-file-ext-regexp))
  (if recursive
      (directory-files-recursively dir regexp include-dirs pred)
    (directory-files dir t regexp t)))

(defun js-eval-looking-at-comment-p (&optional max)
  "Check if point is at the start of a JavaScript comment.

Optional argument MAX is the maximum position in the buffer to look at, defaults
to one less than `point-max'."
  (and (> (or max (1- (point-max))) (point))
       (car (member (buffer-substring-no-properties
                     (point)
                     (+ 2 (point))) '("#!" "/*" "//")))))

(defun js-eval-get-rebounds-at-point (&optional rechars)
  "Retrieve bounds of JavaScript identifier at point.

Optional argument RECHARS is a string of characters to be considered as part of
the word for rebounding. It defaults to \"*_$A-Za-z0-9\"."
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward (or rechars "*_$A-Za-z0-9"))
                (point)))
           (b (save-excursion
                (skip-chars-forward (or rechars "*_$A-Za-z0-9"))
                (point))))
      (if (string-blank-p
           (buffer-substring-no-properties a b))
          nil
        (cons a b)))))

(defun js-eval-dirname (path)
  "Evaluate JavaScript file's directory name relative to project.

Argument PATH is the file path for which the directory name is evaluated."
  (let (parent)
    (setq parent (file-name-directory
                  (directory-file-name
                   (expand-file-name path default-directory))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (js-eval-relative-p path)
          (file-relative-name parent)
        (directory-file-name parent)))))

(defun js-eval-expand-alias-path (path &optional base-url)
  "Expand file PATH aliases to absolute paths.

Argument PATH is a string representing the file path to expand.

Optional argument BASE-URL is a string representing the base URL to use for
resolving relative paths."
  (when-let ((filepath
              (when path (replace-regexp-in-string "\\*[^$]+" "" path))))
    (cond
     ((and (file-name-absolute-p filepath)
           (file-exists-p filepath))
      (if (file-directory-p filepath)
          (js-eval-slash filepath)
        filepath))
     (t
      (if-let* ((root (or js-eval-current-project-root
                          (js-eval-find-project-root)))
                (file (expand-file-name filepath
                                        (if base-url
                                            (expand-file-name base-url root)
                                          root))))
          (js-eval-slash file)
        filepath)))))

(defun js-eval-server-get-buffer ()
  "Retrieve the buffer named `js-eval-server-buffer-name'."
  (get-buffer js-eval-server-buffer-name))

(defun js-eval-syntax-propertize (start end)
  "Apply syntax properties to JavaScript code between START and END.

Argument START is the position in the buffer where syntax propertization should
begin.

Argument END is the position in the buffer where syntax propertization should
end."
  (goto-char start)
  (js-eval-syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
         (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1) (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (js-eval-syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b")))
   (point) end))

(defun js-eval-make-opt-symbol-regexp (words)
  "Generate a regexp for matching optional symbols from WORDS.

Argument WORDS is either a single string or a list of strings to be included in
the regular expression."
  (concat "\\_<" (regexp-opt (if (listp words)
                                 words
                               (list words)) t) "\\_>"))

(defun js-eval-get-package-json-modules (&optional project-root)
  "Extract module keys from a project's package.json.

Optional argument PROJECT-ROOT is the root directory of the project. If not
provided, the function attempts to find the project root automatically."
  (when-let*
      ((root (or project-root (js-eval-find-project-root)))
       (package-json-path
        (expand-file-name
         "package.json" (if (js-eval-string-match-p
                             js-eval-node-modules-regexp
                             root)
                            (car
                             (split-string
                              root
                              js-eval-node-modules-regexp))
                          root)))
       (package-json (js-eval-read-json package-json-path 'hash-table)))
    (seq-remove #'null
                (mapcan (lambda (section)
                          (when-let ((hash (gethash section
                                                    package-json)))
                            (hash-table-keys hash)))
                        js-eval-package-json-sections))))

(defun js-eval-get-file-cache (cache-key)
  "Retrieve cached JavaScript evaluation result if not modified.

Argument CACHE-KEY is a string used as the key to retrieve the cached data."
  (let* ((cache (gethash cache-key js-eval-files-cache))
         (cache-tick (and cache (plist-get cache :tick)))
         (tick (file-attribute-modification-time (file-attributes
                                                  cache-key
                                                  'string))))
    (when (equal cache-tick tick)
      (plist-get cache :cache))))

(defun js-eval-set-file-cache (path content)
  "Cache file CONTENT at PATH with modification time TICK.

Argument PATH is the file path for which the cache is being set.

Argument CONTENT is the content to be cached for the specified file path."
  (let* ((cache (gethash path js-eval-files-cache))
         (tick (file-attribute-modification-time (file-attributes
                                                  path
                                                  'string))))
    (setq cache (list :tick tick
                      :cache content))
    (puthash path cache
             js-eval-files-cache)
    (plist-get cache :cache)))

(defun js-eval-find-node-modules-submodules (node-modules-path modules)
  "Find submodules in a Node.js project's node_modules directory.

Argument NODE-MODULES-PATH is a string representing the path to the node_modules
directory.

Argument MODULES is a list of strings representing the names of the modules to
find submodules for."
  (let ((submodules)
        (prefix-regexp (concat "^" (js-eval-slash node-modules-path))))
    (dolist (element modules)
      (let ((path (expand-file-name element node-modules-path))
            (dirs))
        (setq dirs
              (mapcar (lambda (it) (replace-regexp-in-string prefix-regexp ""
                                                        it))
                      (js-eval-extract-subpackages path)))
        (setq submodules (append dirs submodules))))
    submodules))

(defun js-eval-sort-by-exts (files &optional extensions)
  "Sort FILES by extension priority, using EXTENSIONS or defaults.

Argument FILES is a list of file names to be sorted.

Optional argument EXTENSIONS is a list of file extensions to sort FILES by; it
defaults to the value of `js-eval-preffered-extensions'."
  (setq extensions (or extensions js-eval-preffered-extensions))
  (seq-sort-by (lambda (a)
                 (if-let ((ext (and a (file-name-extension a))))
                     (or (seq-position extensions ext 'string=) -1)
                   -1))
               #'>
               files))

(defun js-eval-get-path-ext-candidates (path)
  "List matching files for a given PATH with extensions.

Argument PATH is a string representing the file path to evaluate."
  (let ((parts (reverse (split-string path "/")))
        (module-re)
        (parent-dir))
    (setq module-re (concat "\\(/\\|^\\)" (pop parts)
                            js-eval-file-ext-regexp))
    (setq parent-dir (concat (or (string-join
                                  (reverse parts) "/") "") "/"))
    (directory-files parent-dir t module-re)))

(defun js-eval-slash (str)
  "Append a slash to STR if it doesn't end with one.

Argument STR is a string to be evaluated and potentially modified."
  (cond ((string= "" str) str)
        ((string= "/" str) "")
        ((stringp str)
         (if (string-match "/$" str)
             str
           (concat str "/")))))

(defun js-eval-join-file (&rest args)
  "Join file paths in ARGS and return the resulting path.

Remaining arguments ARGS are strings representing file path components to be
joined. If the first string is a relative path, the resulting path will be
relative; otherwise, it will be absolute."
  (let (path (relative (not (file-name-absolute-p (car args)))))
    (mapc (lambda (arg)
            (unless (null arg)
              (setq path (expand-file-name arg path))))
          args)
    (if relative (file-relative-name path) path)))

(defun js-eval-try-find-real-path (path)
  "Resolve JavaScript module's real file path.

Argument PATH is a string representing the file path to be resolved."
  (if (or (null path) (and (js-eval-string-match-p
                            js-eval-file-ext-regexp path)
                           (file-exists-p path)))
      path
    (or (when-let* ((package-json (js-eval-join-when-exists
                                   path
                                   "package.json"))
                    (module
                     (js-eval-try-json-sections
                      package-json
                      js-eval-node-modules-priority-section-to-read)))
          (if (js-eval-string-match-p js-eval-file-ext-regexp
                                      module)
              (expand-file-name module path)
            (js-eval-try-find-real-path (js-eval-try-ext module path))))
        (when-let* ((dir (js-eval-join-file path "src"))
                    (exists (file-exists-p dir))
                    (files (js-eval-directory-files dir)))
          (if (= 1 (length files))
              (car files)
            (seq-find #'js-eval-is-index-file-p files)))
        (car (js-eval-resolve-paths path))
        (js-eval-try-ext path)
        (js-eval-try-ext (js-eval-join-file path "index"))
        (when-let* ((package-json (js-eval-join-when-exists
                                   path
                                   "package.json"))
                    (module (js-eval-try-json-sections
                             package-json
                             '("main"))))
          (if (js-eval-string-match-p js-eval-file-ext-regexp
                                      module)
              (expand-file-name module path)
            (js-eval-try-find-real-path
             (js-eval-try-ext module path)))))))

(defun js-eval-forward-whitespace (&optional skip-chars)
  "Skip whitespace and comments in JavaScript code.

Optional argument SKIP-CHARS is a string of characters to skip. It defaults to
whitespace characters: space, tab, newline, carriage return, form feed, and
vertical tab."
  (unless skip-chars (setq skip-chars "\s\t\n\r\f\v"))
  (let ((total (skip-chars-forward skip-chars))
        (max (1- (point-max))))
    (while (and (>= max (point))
                (pcase (js-eval-looking-at-comment-p max)
                  ("//" (forward-line 1) t)
                  ("/*" (js-eval-re-search-forward "\\([*]/\\)" nil t 1))
                  ("#!" (when (js-eval-re-search-forward "." nil t 1)
                          (forward-char -1)))))
      (setq total (+ total (skip-chars-forward skip-chars))))
    total))

(defun js-eval-inside-comment-p ()
  "Check if point is inside a JavaScript comment."
  (with-syntax-table js-eval-mode-syntax-table
    (let ((comment-start "//")
          (comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
          (comment-use-syntax t)
          (result (nth 4 (syntax-ppss))))
      result)))

(defun js-eval-get-prev-char (&optional nth position)
  "Retrieve the character before point or NTH chars back.

Optional argument NTH is the number of characters to look back from the current
position. It defaults to 1.

Optional argument POSITION is the buffer position from which to look back. If
nil, the current point is used."
  (let* ((end (or position (point)))
         (beg (- end (or nth 1))))
    (when (>= beg (point-min))
      (buffer-substring-no-properties
       beg end))))

(defun js-eval-re-search-backward (re &optional bound noerror count)
  "Search backward for regex RE, optionally up to BOUND, without error.

Argument RE is the regular expression string to search backward for.

Optional argument BOUND is the position in the buffer to stop searching; nil
means search to the beginning of the buffer.

Optional argument NOERROR, if non-nil, means do not signal an error if the
search fails, just return nil.

Optional argument COUNT is the number of times to match; negative means search
backward, positive means search forward."
  (let ((case-fold-search nil))
    (js-eval-re-search-forward re bound noerror (if count (- count) -1))))

(defun js-eval-re-search-backward-inner (regexp &optional bound count)
  "Search backward for REGEXP, skipping comments and strings.

Argument REGEXP is the regular expression to search for.

Optional argument BOUND is the position in the buffer to stop the search; nil
means search to the beginning of the buffer.

Optional argument COUNT is the number of successful matches to find; nil means
search until the beginning of the buffer."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table js-eval-mode-syntax-table
        (re-search-backward regexp bound)
        (when (and (> (point) (point-min))
                   (save-excursion (backward-char) (looking-at "/[/*]")))
          (forward-char))
        (setq parse (syntax-ppss))
        (cond ((nth 8 parse)
               (goto-char (nth 8 parse)))
              ((or (nth 4 parse)
                   (and (eq (char-before) ?/) (eq (char-after) ?*)))
               (re-search-backward "/\\*"))
              (t
               (setq count (1- count)))))))
  (point))

(defun js-eval-re-search-forward-inner (regexp &optional bound count)
  "Search forward for REGEXP, skipping strings and comments.

Argument REGEXP is a regular expression string to search for.

Optional argument BOUND is a buffer position that bounds the search; it must be
a number or a marker, or nil to specify no bound.

Optional argument COUNT is the number of successful matches to find; it defaults
to 1."
  (let ((parse)
        str-terminator)
    (while (> count 0)
      (with-syntax-table js-eval-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((setq str-terminator (nth 3 parse))
               (when (eq str-terminator t)
                 (setq str-terminator ?/))
               (re-search-forward
                (concat "\\([^\\]\\|^\\)" (string str-terminator))
                (line-end-position) t))
              ((nth 7 parse)
               (forward-line))
              ((or (nth 4 parse)
                   (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
               (re-search-forward "\\*/"))
              (t
               (setq count (1- count)))))))
  (point))

(defun js-eval-which-word (&optional regexp)
  "Evaluate JavaScript expression at point or matching REGEXP.

Optional argument REGEXP is a regular expression to match against the word at
point. If nil, a default word detection mechanism is used."
  (when-let ((bounds (js-eval-get-rebounds-at-point regexp)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun js-eval-inside-string-p (&optional position)
  "Check if point is inside a string at POSITION.

Optional argument POSITION is the buffer position to check. If nil, the current
point is used."
  (with-syntax-table js-eval-mode-syntax-table
    (nth 3 (syntax-ppss (or position (point))))))

(defun js-eval-resolve-module (dir &optional file)
  "Resolve and return the module path from DIR and optional FILE.

Argument DIR is a directory path where the module resolution starts.

Optional argument FILE is a file name to locate within the directory hierarchy
starting at DIR. If FILE is not provided, DIR is used as the result if it
exists."
  (when-let ((result (if file
                         (let ((default-directory (expand-file-name dir))
                               (found))
                           (setq found (locate-dominating-file
                                        default-directory file))
                           (when found
                             (expand-file-name file found)))
                       (and (file-exists-p dir)
                            dir))))
    (if (and (file-directory-p result)
             (not (string-match-p result "/$")))
        (concat result "/")
      result)))

(defun js-eval-read-tsconfig (&optional project-root tsconfig-name)
  "Parse TypeScript configuration from a project's tsconfig or jsconfig.

Optional argument PROJECT-ROOT is a string representing the root directory of
the project.

Optional argument TSCONFIG-NAME is a string specifying the name of the tsconfig
file."
  (unless project-root (setq project-root (js-eval-find-project-root)))
  (unless tsconfig-name (setq tsconfig-name js-eval-tsconfig-filename))
  (let ((config)
        (compiler-options)
        (found)
        (base-url)
        (extends (seq-find #'file-exists-p
                           (and project-root
                                (delete nil
                                        (list
                                         tsconfig-name
                                         (expand-file-name "tsconfig.json"
                                                           project-root)
                                         (expand-file-name "jsconfig.json"
                                                           project-root)))))))
    (while (and (not found)
                extends
                (file-exists-p extends))
      (setq config (js-eval-read-json extends 'alist))
      (setq compiler-options (cdr-safe (assoc 'compilerOptions config)))
      (setq base-url (cdr (assoc 'baseUrl compiler-options)))
      (setq found (cdr (assoc 'paths (cdr-safe compiler-options))))
      (unless found
        (setq extends
              (and extends (assoc 'extends config)
                   (expand-file-name (cdr (assoc 'extends config))
                                     (js-eval-dirname extends))))))
    (setq base-url (and base-url extends
                        (expand-file-name
                         base-url
                         (js-eval-dirname extends))))
    (js-eval-normalize-aliases found base-url)))

(defun js-eval-normalize-aliases (paths &optional base-url)
  "Normalize aliases in PATHS with optional BASE-URL.

Argument PATHS is a list of cons cells where the car is the alias and the cdr is
the path or PATHS associated with that alias.

Optional argument BASE-URL is a string representing the base URL to which the
PATHS should be resolved."
  (let ((alist (mapcar
                (lambda (it)
                  (let ((alias (js-eval-slash
                                (string-join
                                 (split-string (format "%s" (car it))
                                               "\\*"))))
                        (alias-paths (cond
                                      ((vectorp (cdr it))
                                       (append (cdr it) nil))
                                      ((listp (cdr it))
                                       (cdr it))
                                      (t `(,(cdr it))))))
                    (setq alias-paths (mapcar
                                       (lambda (p)
                                         (setq p (replace-regexp-in-string
                                                  "\\*" "" p))
                                         (js-eval-expand-alias-path
                                          p base-url))
                                       alias-paths))
                    (cons alias alias-paths)))
                paths)))
    (seq-sort-by (lambda (it) (length (car it))) #'> alist)))

(defun js-eval-response-success (&rest props)
  "Parse response data and execute callback with result.

Remaining arguments PROPS are property-value pairs from which the response is
extracted and processed."
  (let*
      ((response
        (car
         (cdr
          (plist-member props ':response)))))
    (let
        ((alist
          (request-response-data response)))
      (setq js-eval-response
            (mapcar (lambda (it)
                      (let ((key (car it))
                            (value (cdr it)))
                        (pcase key
                          ('result
                           (if (stringp value)
                               (cons key
                                     (js-eval-recode-string
                                      value))))
                          (_ (cons key value)))))
                    alist))
      (setq js-eval-last-result
            (js-eval-recode-string
             (or (alist-get 'result alist) ""))))
    (when js-eval-callback
      (funcall js-eval-callback js-eval-last-result)
      (setq js-eval-callback nil))))

(defvar js-eval-babel-config-string "{
  \"sourceType\": \"module\",
  \"babelrc\": false,
  \"env\": {
    \"development\": {
      \"sourceType\": \"module\",
      \"assumptions\": {
        \"iterableIsArray\": true
      }
    }
  },
  \"presets\": [
    [
      \"@babel/preset-env\",
      {
        \"loose\": true,
        \"debug\": false,
        \"ignoreBrowserslistConfig\": true,
        \"targets\": {
          \"node\": \"current\"
        },
        \"useBuiltIns\": false,
        \"spec\": true,
        \"modules\": \"cjs\",
        \"forceAllTransforms\": true
      }
    ],
    \"@babel/preset-typescript\",
    \"@babel/preset-react\"
  ],
  \"plugins\": [
    [
      \"@babel/plugin-transform-runtime\",
      {
        \"absoluteRuntime\": true,
        \"helpers\": false,
        \"corejs\": false,
        \"regenerator\": true
      }
    ],
    \"@babel/plugin-transform-arrow-functions\",
    \"babel-plugin-remove-use-strict\"
  ]
}"
  "Default Babel configuration string for JavaScript evaluation.")


(defconst js-eval-from-keyword--re
  (js-eval-make-opt-symbol-regexp "from"))

(defconst js-eval-regexp-import-keyword
  (eval-and-compile
    (concat "\\_<" (regexp-opt `(,"import") t) "\\_>"))
  "Regexp matching keyword import.")

(defvar js-eval-overlay-at nil
  "Overlay position for JavaScript evaluation results.")

(defun js-eval-popup-minibuffer-select-window ()
  "Focus on the active minibuffer window."
  (when-let ((wind (active-minibuffer-window)))
    (select-window wind)))

(define-minor-mode js-eval-popup-mode
  "Toggle JavaScript evaluation popup keybindings.

Enable `js-eval-popup-mode' to display JavaScript evaluation results in a popup
window. This mode provides a convenient way to see the output of JavaScript code
without leaving the current context. Use the keymap
`js-eval-popup-switch-keymap' to interact with the popup, allowing for quick
toggling and navigation of evaluation results. This is a global minor mode,
meaning it will be available across all buffers once enabled."
  :lighter " js-eval-popup"
  :keymap js-eval-popup-switch-keymap
  :global nil)
(defvar js-ts-mode-hook)
(defvar js-base-mode-hook)
(defvar js-mode-hook)

(define-minor-mode js-eval-popup-inspect-mode
  "Enable JavaScript evaluation and inspection popup.

Enable `js-eval-popup-inspect-mode' to interactively inspect JavaScript
evaluation results in a popup window. Use the provided keymap to navigate and
manipulate the inspection interface. This mode is global and does not target a
specific buffer."
  :lighter " js-eval-popup"
  :keymap js-eval-popup-inspect-keymap
  :global nil
  (cond ((and (fboundp 'treesit-ready-p)
              (treesit-ready-p 'javascript)
              (fboundp 'js-ts-mode))
         (let ((js-ts-mode-hook)
               (js-base-mode-hook))
           (js-ts-mode)))
        (t
         (let ((js-mode-hook)
               (js-base-mode-hook))
           (js-mode)))))

(defun js-eval-read-babel-config ()
  "Parse JSON from `js-eval-babel-config-string' into an alist."
  (with-temp-buffer
    (save-excursion (insert js-eval-babel-config-string))
    (let ((json-object-type 'alist)
          (json-array-type 'list))
      (json-read))))

(defun js-eval-flatten (items)
  "Flatten nested lists in ITEMS to a single level list.

Argument ITEMS is a list of elements to flatten."
  (mapcar (lambda (opt)
            (cond
             ((symbolp opt)
              opt)
             ((stringp opt)
              opt)
             ((null opt)
              opt)
             ((eq opt t)
              opt)
             ((consp opt)
              (if (listp (car opt))
                  (js-eval-flatten (car opt))
                (car opt)))
             ((and (listp opt))
              (mapcar #'js-eval-flatten opt))
             (t opt)))
          items))

(defun js-eval-get-config-dependencies ()
  "Retrieve Babel config dependencies for JavaScript evaluation."
  (when-let ((config
              (js-eval-read-babel-config)))
    (let ((dependencies))
      (let ((presets (js-eval-flatten (cdr (assoc 'presets config))))
            (plugins (js-eval-flatten (cdr (assoc 'plugins config)))))
        (setq dependencies (append dependencies presets plugins)))
      (append '("@babel/core" "@babel/cli") (seq-uniq
                                             (delete nil
                                                     dependencies))))))

(defun js-eval-make-npm-install-command ()
  "Generate an npm install command for missing dependencies."
  (when-let ((dependencies (js-eval-get-missing-dependencies)))
    (string-join (append '("npm install --save-dev") dependencies) "\s")))

(defun js-eval-get-missing-dependencies ()
  "List missing JavaScript dependencies for evaluation."
  (seq-remove (lambda (it) (file-exists-p
                       (expand-file-name
                        it
                        js-eval-babel-node-modules-path)))
              (js-eval-get-config-dependencies)))

(defun js-eval-exec-in-dir (command project-dir &optional callback)
  "Execute COMMAND in PROJECT-DIR, optionally calling CALLBACK.

Argument COMMAND is a string representing the shell command to execute.

Argument PROJECT-DIR is a string specifying the directory in which to execute
COMMAND.

Optional argument CALLBACK is a function to call when COMMAND execution
finishes."
  (let ((proc)
        (buffer (generate-new-buffer (format "*%s*" command))))
    (progn (switch-to-buffer buffer)
           (with-current-buffer buffer
             (if (file-exists-p project-dir)
                 (setq default-directory project-dir)
               (mkdir project-dir)
               (setq default-directory project-dir))
             (setq proc (start-process-shell-command
                         (nth 0
                              (split-string command))
                         buffer command))
             (shell-command-save-pos-or-erase)
             (require 'shell)
             (when (fboundp 'shell-mode)
               (shell-mode))
             (view-mode +1))
           (set-process-sentinel
            proc
            (lambda (process _state)
              (let ((output (with-current-buffer
                                (process-buffer process)
                              (buffer-string))))
                (kill-buffer (process-buffer process))
                (if (= (process-exit-status process) 0)
                    (progn
                      (message "finished")
                      (if callback
                          (funcall callback)
                        (dired project-dir)))
                  (user-error (format "%s\n%s" command output))))))
           (require 'comint)
           (when (fboundp 'comint-output-filter)
             (set-process-filter proc #'comint-output-filter)))))

(defun js-eval-get-prop (item property)
  "Retrieve PROPERTY from ITEM if it's a string or list.

Argument ITEM is a string or a list from which to retrieve the property.

Argument PROPERTY is the property to retrieve from ITEM."
  (if (stringp item)
      (get-text-property 0 property item)
    (when (listp item)
      (plist-get item property))))

(defun js-eval-make-item (candidate &rest plist)
  "Create a propertized string from CANDIDATE and property list PLIST.

Argument CANDIDATE is the item to be processed.

Remaining arguments PLIST are property-value pairs to be applied to CANDIDATE."
  (let ((pl plist)
        (key)
        (filtered-pl))
    (while (setq key (pop pl))
      (when-let ((val (pop pl)))
        (push val filtered-pl)
        (push key filtered-pl)))
    (apply #'js-eval-propertize candidate filtered-pl)))

(defun js-eval-stringify (x)
  "Convert X to a string representation.

Argument X is the value to be converted to a string representation."
  (cond
   ((stringp x)
    x)
   ((stringp x)
    (symbol-name x))
   ((numberp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun js-eval-normalize-object-prop-position (item)
  "Normalize object property position in ITEM.

Argument ITEM is a string representing the JavaScript object property path."
  (if-let ((found (seq-find
                   (lambda (it) (and it (js-eval-get-prop it :start)))
                   (reverse (split-string item "\\.\\|]\\|\\[")))))
      (js-eval-make-item item
                         :start (js-eval-get-prop found :start)
                         :end (js-eval-get-prop found :end)
                         :var-type "Property")
    item))

(defun js-eval-get-object-items (obj &optional parent-key)
  "Extract and sort object properties from OBJ with optional PARENT-KEY.

Argument OBJ is the JavaScript object from which to retrieve keys.

Optional argument PARENT-KEY is a string representing the key of the parent
object from which OBJ is derived."
  (js-eval-sort-object-props-by-pos
   (mapcar #'js-eval-normalize-object-prop-position
           (js-eval-get-object-keys obj parent-key))))

(defun js-eval-node-modules-candidates (&optional project-root)
  "List project's Node.js modules and submodules.

Optional argument PROJECT-ROOT is the root directory of the project. If not
provided, it defaults to the result of `js-eval-find-project-root'."
  (unless project-root (setq project-root (js-eval-find-project-root)))
  (when (js-eval-string-match-p js-eval-node-modules-regexp project-root)
    (setq project-root (car
                        (split-string project-root
                                      js-eval-node-modules-regexp))))
  (let ((modules (js-eval-get-package-json-modules project-root)))
    (if-let* ((node-modules-path (js-eval-find-node-modules project-root))
              (submodules (or (js-eval-get-file-cache node-modules-path)
                              (js-eval-set-file-cache
                               node-modules-path
                               (js-eval-find-node-modules-submodules
                                node-modules-path
                                modules)))))
        (append modules submodules)
      modules)))

(defun js-eval-find-node-modules (&optional project-dir)
  "Find and return the absolute path to `node_modules'.

Optional argument PROJECT-DIR is the directory to start searching for
node_modules from. If not provided, the search starts from the project root
directory."
  (if (file-name-absolute-p js-eval-node-modules-dir)
      js-eval-node-modules-dir
    (when-let ((root (or project-dir (js-eval-find-project-root))))
      (setq root (car (split-string root js-eval-node-modules-regexp)))
      (js-eval-join-when-exists root js-eval-node-modules-dir))))

(defun js-eval-resolve-paths (path &optional dir)
  "Resolve file paths for JavaScript evaluation.

Argument PATH is a string representing the file path to resolve.

Optional argument DIR is a string representing the directory to use as the base
for resolving PATH. If not provided, the current directory is used as the base."
  (let ((fullpath (expand-file-name path dir)))
    (js-eval-sort-by-exts
     (if (file-exists-p fullpath)
         (if (not (file-directory-p fullpath))
             (list fullpath)
           (or (js-eval-get-path-ext-candidates
                fullpath)
               (js-eval-get-path-ext-candidates
                (expand-file-name "index"
                                  fullpath))))
       (js-eval-get-path-ext-candidates fullpath)))))

(defun js-eval-strip-text-props (item)
  "Strip text properties from ITEM if it's a string or symbol.

Argument ITEM is the object from which to strip text properties. It can be a
string, a symbol, or nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun js-eval-find-project-root (&optional directory)
  "Find the root DIRECTORY of a JavaScript project.

Optional argument DIRECTORY is the directory from which to start searching for
the project root. If not provided, it defaults to `default-directory'."
  (unless directory (setq directory default-directory))
  (let ((parent (expand-file-name ".." directory)))
    (unless (or (string= parent directory)
                (string= directory "")
                (string= directory "/"))
      (if (file-exists-p (expand-file-name "package.json" directory))
          directory
        (js-eval-slash (js-eval-find-project-root parent))))))

(defun js-eval-node-module-to-real (module &optional project-root)
  "Resolve the real path of a Node.js module.

Argument MODULE is the name of the Node.js module to resolve to an absolute
path.

Optional argument PROJECT-ROOT is the root directory of the project; if not
provided, the function attempts to find the project root automatically."
  (when-let* ((node-modules (or (js-eval-find-node-modules project-root)
                                (js-eval-find-node-modules)))
              (real-path (js-eval-join-file node-modules module)))
    (if (and (js-eval-string-match-p js-eval-file-ext-regexp real-path)
             (file-name-absolute-p real-path))
        real-path
      (js-eval-try-find-real-path real-path))))

(defun js-eval-get-es-imports-bounds ()
  "Find bounds of ES import statements in a buffer."
  (save-excursion
    (goto-char (point-min))
    (let (alist)
      (while (js-eval-re-search-forward
              js-eval-regexp-import-keyword nil t)
        (let ((beg (- (point) (length "import")))
              (end))
          (js-eval-forward-whitespace)
          (unless (looking-at-p "[\"']")
            (js-eval-re-search-forward js-eval-from-keyword--re nil t 1)
            (js-eval-forward-whitespace))
          (when (looking-at-p "[\"']")
            (forward-sexp 1)
            (setq end (point))
            (js-eval-forward-whitespace)
            (when (looking-at ";")
              (setq end (1+ (point))))
            (push (cons beg end)
                  alist))))
      (reverse alist))))

(defun js-eval-backward-whitespace (&optional skip-chars)
  "Skip whitespace and comments backward, return chars skipped.

Optional argument SKIP-CHARS is a string of characters to skip. It defaults to
whitespace characters including space, tab, newline, carriage return, form feed,
and vertical tab."
  (unless skip-chars (setq skip-chars "\s\t\n\r\f\v"))
  (let ((total (skip-chars-backward skip-chars))
        (min (1+ (point-min)))
        (pos))
    (while (and (> (point) min)
                (or (js-eval-inside-comment-p)
                    (equal (js-eval-get-prev-char) "/"))
                (setq pos
                      (js-eval-re-search-backward
                       "[^\s\t\n\r\f\v]" nil t 1)))
      (setq total (+ total (skip-chars-backward skip-chars))))
    (when pos
      (goto-char pos)
      (unless (looking-at "//\\|/[*]\\|#!")
        (forward-char 1)))
    total))

(defun js-eval-re-search-forward (regexp &optional bound noerror count)
  "Search forward for REGEXP, optionally up to BOUND, COUNT times.

Argument REGEXP is a regular expression string to search for.

Optional argument BOUND is a buffer position that bounds the search; it must be
a number or nil.

Optional argument NOERROR, if non-nil, means do not signal an error if the
search fails, just return nil.

Optional argument COUNT is the number of times to search; it defaults to 1 and
can be negative to search backwards."
  (let ((case-fold-search nil))
    (unless count (setq count 1))
    (let ((init-point (point))
          (search-fun
           (cond ((< count 0) (setq count (- count))
                  #'js-eval-re-search-backward-inner)
                 ((> count 0) #'js-eval-re-search-forward-inner)
                 (t #'ignore))))
      (condition-case err
          (funcall search-fun regexp bound count)
        (search-failed
         (goto-char init-point)
         (unless noerror
           (signal (car err) (cdr err))))))))

(defun js-eval-get-path-at-point ()
  "Extract JavaScript import/export path at cursor."
  (save-excursion
    (when-let* ((word (js-eval-which-word))
                (meta-word (or (string= "import" word)
                               (string= "export" word)
                               (string= "from" word))))
      (if (string= word "from")
          (search-forward-regexp "['\"]" nil t 1)
        (search-forward-regexp "[\s\t\n]+from[\s\t\n]+['\"]" nil t 1)))
    (when (js-eval-inside-string-p)
      (let (p0 p1 p2 stops)
        (setq stops "^ \t\n\"`'‘’“”|[]{}·")
        (setq p0 (point))
        (skip-chars-backward stops)
        (setq p1 (point))
        (goto-char p0)
        (skip-chars-forward stops)
        (setq p2 (point))
        (goto-char p0)
        (buffer-substring-no-properties p1 p2)))))

(defun js-eval-make-babel-command (&optional source-file target-file)
  "Create a shell command to run Babel on SOURCE-FILE.

Optional argument SOURCE-FILE is the path to the source file to be processed by
Babel.

Optional argument TARGET-FILE is the path where the processed file will be
saved."
  (unless (file-exists-p (concat (temporary-file-directory)
                                 ".babelrc"))
    (write-region
     js-eval-babel-config-string
     nil
     (concat (temporary-file-directory)
             ".babelrc")))
  (string-join
   (delete nil (append
                `(,(and js-eval-babel-node-modules-path
                    (concat "NODE_PATH="
                     js-eval-babel-node-modules-path)))
                `(,(expand-file-name ".bin/babel"
                    js-eval-babel-node-modules-path)
                  ,source-file)
                `,(and target-file (list "--out-file" target-file))
                (list "--config-file"
                      (concat (temporary-file-directory)
                              ".babelrc"))))
   "\s"))

(defun js-eval-resolve-node-modules-dir (dir)
  "Find `node_modules' directory starting from DIR.

Argument DIR is a directory path where the function will attempt to resolve the
\"node_modules\" directory."
  (js-eval-resolve-module dir "node_modules"))

(defun js-eval-node-modules-global ()
  "Evaluate global Node.js modules directory path."
  (when-let ((dir (shell-command-to-string "npm config get prefix")))
    (setq dir (expand-file-name "lib/node_modules/" dir))
    (when (file-exists-p dir)
      dir)))

(defun js-eval-get-aliases ()
  "Retrieve and sort JavaScript project aliases."
  (setq js-eval-project-aliases
        (seq-sort-by
         (lambda (it) (length (car it)))
         #'>
         (or (js-eval-read-tsconfig)
             (js-eval-normalize-aliases
              js-eval-project-aliases))))
  (setq js-eval-aliases
        (seq-sort-by #'length #'> (mapcar #'car js-eval-project-aliases))))

(defun js-eval-find-project-files (&optional project-root)
  "Find project files recursively from the root.

Optional argument PROJECT-ROOT is the root directory of the project. If not
provided, it defaults to the value of `js-eval-current-project-root' or the
result of `js-eval-find-project-root'."
  (unless project-root
    (setq project-root (or js-eval-current-project-root
                           (js-eval-find-project-root))))
  (if (and js-eval-current-project-root
           (string-match-p js-eval-current-project-root
                           default-directory))
      (let ((files)
            (processed-dirs)
            (node-modules (js-eval-find-node-modules project-root))
            (dir (replace-regexp-in-string "/$" "" default-directory))
            (proj-root (replace-regexp-in-string
                        "/$"
                        ""
                        js-eval-current-project-root)))
        (push dir processed-dirs)
        (while (string-match-p proj-root dir)
          (when (file-readable-p dir)
            (setq files (append files
                                (reverse
                                 (directory-files-recursively
                                  dir
                                  js-eval-file-ext-regexp
                                  nil
                                  (lambda (it)
                                    (and (file-readable-p it)
                                         (not
                                          (or
                                           (member it processed-dirs)
                                           (string= it dir)
                                           (string=
                                            (or node-modules "") it))))))))))
          (setq dir (expand-file-name ".." dir))
          (push dir processed-dirs))
        files)
    (message "Cannot find project files")
    nil))

(defun js-eval-server-eval-request-async (&optional payload-alist cb)
  "Send asynchronous eval request to a JavaScript server.

Optional argument PAYLOAD-ALIST is an association list representing the payload
to send with the request.

Optional argument CB is a callback function that is called with the result of
the evaluation."
  (setq js-eval-callback cb)
  (setq js-eval-server-params payload-alist)
  (request "http://localhost:24885/eval"
    :type "POST"
    :data (json-encode payload-alist)
    :headers
    `(("accept" . "application/json")
      ("content-type" . "application/json"))
    :parser 'json-read
    :error
    #'(lambda (&rest args)
        (message "Got error: %S" (plist-get args :error-thrown)))
    :complete (lambda (&rest _)
                (let ((buff
                       (get-buffer-create js-eval-server-buffer-name)))
                  (unless (get-buffer-window buff)
                    (with-selected-window (js-eval--get-other-wind)
                      (pop-to-buffer-same-window buff)))))
    :success #'js-eval-response-success))

(defun js-eval-server-get-process ()
  "Retrieve the process named `js-eval-server-process-name'."
  (get-process js-eval-server-process-name))

;;;###autoload
(defun js-eval-server-run ()
  "Start or check status of the JavaScript evaluation server process."
  (interactive)
  (if-let ((proc (js-eval-server-get-process)))
      (setq js-eval-server-status (process-status proc))
    (let ((process-environment
           (append '("NODE_NO_WARNINGS=1") process-environment)))
      (js-eval-server-clear)
      (let ((proc (start-process js-eval-server-process-name
                                 js-eval-server-buffer-name
                                 "emacs-jsdom-run")))
        (setq js-eval-server-status (process-status proc))
        (set-process-sentinel
         proc
         (lambda (_process state)
           (setq js-eval-server-status state)))))))

(defvar js-eval-current-alias nil
  "Alias for the current JavaScript evaluation context.")

(defvar js-eval-ast-node-builtins-js-str
  "
const annotateFunction = (str) => {
        let parts = str.split('').reverse();
        let processed = [];
        let curr;
        let bracketsOpen = 0;
        let bracketsClosed = 0;
        let openCount = 0;
        let closedCount = 0;
        let result;

        while ((curr = !result && parts.pop())) {
          if (curr === '(') {
            openCount += 1;
          } else if (curr === ')') {
            closedCount += 1;
          }
          if (openCount > 0) {
            processed.push(curr);
            if (curr === '{') {
              bracketsOpen += 1;
            } else if (curr === '}') {
              bracketsClosed += 1;
            }
          }
          result =
            result ||
            (bracketsOpen === bracketsClosed &&
              openCount === closedCount &&
              openCount > 0)
              ? processed.join('')
              : undefined;
        }

        return result
          ? 'function'.concat(result).concat('{}')
          : result;
      }
const mapNodeBuiltins = () => {
  const annotateFn = (fn) => {
    const len = fn.length || 0;
    let idx = 0;
    let list;
    list = new Array(len);
    while (idx < len) {
      list[idx] = `arg${idx}`;
      idx += 1;
    }
    return `function ${fn.name && fn.name.split(' ')[0]} ${list.join(', ')} {}`;
  };

  const mapper = (data, depth = 0) => {
    return Object.keys(data)
      .filter((k) => !/^_/g.test(k))
      .reduce((obj, key) => {
        const v = data[key];
        const val =
          v instanceof Function || typeof v === 'function'
            ? annotateFunction(v
                .toString()
                .replace(/\\/\\*[\\s\\S]*?\\*\\/|\\/\\/.*/g, '')
                .replace(/\\r?\\n/gmi, '')
                .trim())
            : Array.isArray(v)
            ? []
            : typeof v === 'object' && depth < 3 && v
            ? mapper(v, depth + 1)
            : v;
        obj[key] = val;
        return obj;
      }, {});
  };
  const obj = require('module')
    .builtinModules.filter((k) => !/^_/g.test(k))
    .reduce((acc, moduleName) => {
      acc[moduleName] = mapper(require(moduleName));
      return acc;
    }, {});
  return obj;
};
mapNodeBuiltins();"
  "JavaScript string for evaluating AST node built-ins.")

;;;###autoload
(defun js-eval-overlay-copy ()
  "Copy overlay string to clipboard and display \"Copied\" message."
  (interactive)
  (when-let ((str (overlay-get js-eval-overlay-at 'after-string)))
    (kill-new str)
    (message "Copied")
    str))

(defun js-eval-node-modules-bin-files ()
  "Evaluate Node.js module binaries in the current project."
  (when-let* ((node-modules
               (locate-dominating-file
                default-directory
                "node_modules"))
              (exec-dir
               (expand-file-name "node_modules/.bin/" node-modules))
              (commands
               (seq-filter #'file-executable-p
                           (and (file-exists-p exec-dir)
                                (directory-files-recursively exec-dir
                                                             directory-files-no-dot-files-regexp)))))
    commands))

(defun js-eval-prettier-js-local-command ()
  "Find local `prettier' command in node_modules/.bin."
  (seq-find (lambda (it)
              (string= "prettier"
                       (file-name-base it)))
            (js-eval-node-modules-bin-files)))

(defun js-eval-prettier-string (&optional string parser)
  "Prettify a given STRING or buffer content using Prettier.

Optional argument STRING is the string to be formatted by Prettier. If not
provided, the default is the entire buffer content from `point-min' to
`point-max'.

Optional argument PARSER is a string specifying the parser to be used by
Prettier. It defaults to \"typescript\"."
  (let ((string (or string (buffer-substring-no-properties (point-min)
                                                           (point-max))))
        (command (js-eval-prettier-js-local-command))
        (args))
    (setq args (if command
                   `(,command t t nil "--parser"
                              ,(or parser "typescript"))
                 `("npx" t t nil "prettier"
                   "--parser"
                   ,(or parser "typescript"))))
    (with-temp-buffer
      (insert string)
      (let ((default-directory
             (or
              js-eval-babel-node-modules-path
              (js-eval-resolve-module default-directory)
              (js-eval-resolve-module "~/"))))
        (list
         (eq 0
             (apply #'call-process-region
                    (append
                     (list (point-min)
                           (point-max))
                     args)))
         (buffer-string))))))

(defun js-eval-popup-fontify (content &optional mode-fn &rest args)
  "Fontify CONTENT string using MODE-FN or default to `emacs-lisp-mode'.

Argument CONTENT is the string to be inserted and fontified in the buffer.

Optional argument MODE-FN is the major mode function used for fontification. It
defaults to `emacs-lisp-mode'.

Remaining arguments ARGS are additional arguments passed to the major mode
function MODE-FN."
  (with-temp-buffer
    (delay-mode-hooks
      (apply (or mode-fn 'emacs-lisp-mode) args)
      (goto-char (point-min))
      (insert (if (or (eq major-mode 'emacs-lisp-mode)
                      (not (stringp content)))
                  (pp-to-string content)
                content))
      (font-lock-ensure)
      (buffer-string))))

(defun js-eval-popup-inspect (content &rest setup-args)
  "Display CONTENT in a popup buffer with optional syntax highlighting.

Argument CONTENT is a string or a buffer containing the JavaScript code to be
evaluated and displayed in the popup.

Remaining arguments SETUP-ARGS are used to configure the popup buffer's keymaps,
syntax table, and major mode function."
  (let ((buffer (get-buffer-create js-eval-popup-inspect-buffer-name))
        (keymaps (seq-filter #'keymapp setup-args))
        (stx-table (seq-find #'syntax-table-p setup-args)))
    (setq js-eval-popup-content content)
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons (or 'display-buffer-in-direction)
                '((window-height . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (js-eval-popup-inspect-mode)
                (visual-line-mode 1)
                (progn  (save-excursion
                          (insert js-eval-popup-content))
                        (add-hook 'kill-buffer-hook
                                  #'js-eval-popup-minibuffer-select-window
                                  nil t)
                        (use-local-map
                         (let ((map (copy-keymap
                                     js-eval-popup-inspect-keymap)))
                           (add-hook
                            'read-only-mode-hook
                            (lambda ()
                              (if buffer-read-only
                                  (define-key map (kbd "q")
                                              #'kill-this-buffer)
                                (define-key map (kbd "q")
                                            #'self-insert-command)))
                            t)
                           (when keymaps
                             (setq map (make-composed-keymap
                                        keymaps
                                        map)))
                           (set-keymap-parent map (current-local-map))
                           map))))))
        (insert js-eval-popup-content))
      (when stx-table
        (set-syntax-table stx-table))
      (setq header-line-format (or header-line-format "*Inspect*"))
      (unless (active-minibuffer-window)
        (select-window (get-buffer-window buffer))))))

;;;###autoload
(defun js-eval-popup-open-inspector ()
  "Open JavaScript inspector with current popup content."
  (interactive)
  (apply #'js-eval-popup-inspect
         (or js-eval-popup-content "")
         js-eval-popup-meta))

(defun js-eval-relative-p (path)
  "Determine if PATH is relative using regex matching.

Argument PATH is a string representing the path to be evaluated."
  (js-eval-string-match-p "^\\(\\(\\.\\)?[\\.]\\)\\(/\\|$\\)"
                             path))

(defun js-eval-extract-node-builins ()
  "Extract Node.js built-in modules and functions."
  (let ((alist (js-eval-parse-object-from-string
                (with-temp-buffer
                  (let ((process-environment (append '("NODE_NO_WARNINGS=1")
                                                     process-environment)))
                    (call-process "node" nil (current-buffer) nil "-p"
                                  js-eval-ast-node-builtins-js-str)
                    (let ((res (string-join (split-string (buffer-string)
                                                          nil t)
                                            "\s")))
                      res))))))
    (mapcar (lambda (it) (if-let ((value (js-eval-get-prop it :value)))
                        (js-eval-make-item it :value (js-eval-stringify value))
                      it))
            (js-eval-get-object-items alist))))

(defun js-eval-dependency-p (module &optional project-root)
  "Check if MODULE is a dependency in PROJECT-ROOT.

Argument MODULE is a string representing the module to check for.

Optional argument PROJECT-ROOT is a string representing the root directory of
the project; it defaults to nil."
  (or (member module (js-eval-node-modules-candidates project-root))
      (let ((node-dir (js-eval-find-node-modules project-root)))
        (and node-dir
             (file-exists-p
              (expand-file-name (car (split-string module "/"))
                                node-dir))))))

(defun js-eval-path-to-relative (path &optional dir)
  "Convert PATH to a relative path, prepending \"./\" if needed.

Argument PATH is the file path to be evaluated for relativity.

Optional argument DIR is the directory against which PATH will be made relative;
it defaults to `default-directory'."
  (if (js-eval-relative-p path)
      path
    (let ((relative-path (file-relative-name path (or dir default-directory))))
      (unless (js-eval-relative-p relative-path)
        (setq relative-path (concat "./" relative-path)))
      relative-path)))

(defun js-eval-alias-path-to-real (path)
  "Resolve JavaScript alias PATH to actual file paths.

Argument PATH is a string representing the path to be resolved."
  (when-let ((alias-cell (seq-find (lambda (it)
                                     (string-match-p
                                      (concat "^" (car it)) path))
                                   js-eval-project-aliases)))
    (let* ((alias (car alias-cell))
           (paths (cdr alias-cell))
           (trimmed-path (if (string-empty-p alias)
                             path
                           (replace-regexp-in-string
                            "^/" "" (replace-regexp-in-string
                                     (concat  "^" alias)
                                     ""
                                     path)))))
      (delete nil (mapcan (lambda (it)
                            (js-eval-resolve-paths trimmed-path it))
                          paths)))))

(defun js-eval-path-to-real (path &optional dir)
  "Resolve JavaScript file PATH to its real filesystem path.

Argument PATH is a string representing the path to be resolved.

Optional argument DIR is a string representing the directory from which PATH
should be resolved if PATH is not absolute."
  (when (and path (stringp path))
    (setq path (js-eval-strip-text-props path))
    (when-let ((result
                (cond ((file-name-absolute-p path)
                       (car (js-eval-resolve-paths path)))
                      ((js-eval-relative-p path)
                       (car (js-eval-resolve-paths path dir)))
                      ((js-eval-dependency-p path
                                                (js-eval-find-project-root))
                       (js-eval-node-module-to-real path))
                      (t
                       (car (js-eval-alias-path-to-real path))))))
      (if (and (file-exists-p result)
               (not (file-directory-p result)))
          result
        nil))))

(defun js-eval-inside-import-p (&optional position)
  "Check if point is inside a JavaScript import statement.

Optional argument POSITION is the buffer position to check. If nil, the current
point is used."
  (let ((pos (or position (point)))
        (imports (js-eval-get-es-imports-bounds))
        (result))
    (setq result (seq-find (lambda (it) (and (>= pos (car it))
                                             (<= pos (cdr it))))
                           imports))
    result))

(defun js-eval-extract-import-path-bounds (&optional import-bounds)
  "Extract bounds of a JavaScript import path.

Optional argument IMPORT-BOUNDS is a cons cell representing the start and end
positions of the import statement. If not provided, it defaults to nil."
  (when-let ((end (cdr import-bounds)))
    (save-excursion
      (goto-char end)
      (skip-chars-backward ";\s\t\n")
      (js-eval-backward-whitespace)
      (when (looking-back "[\"']" 1)
        (let ((p2 (1- (point)))
              (p1))
          (backward-sexp 1)
          (setq p1 (1+ (point)))
          (cons p1 p2))))))

(defun js-eval-find-imported-files ()
  "Find and return a list of imported JavaScript files."
  (save-excursion
    (goto-char (point-min))
    (let (imported-files)
      (with-syntax-table js-eval-mode-syntax-table
        (while (js-eval-re-search-forward
                js-eval-regexp-import-keyword nil t 1)
          (when-let ((path (js-eval-get-path-at-point)))
            (push path imported-files))))
      (reverse imported-files))))

(defun js-eval-babel-compile (body)
  "Compile TypeScript to JavaScript using Babel.

Argument BODY is a string containing the code to be compiled by Babel."
  (js-eval-ensure-babel-project)
  (let ((temp-file (concat (temporary-file-directory)
                           (make-temp-name "script") ".tsx"))
        (temp-compiled-file (concat (temporary-file-directory)
                                    (make-temp-name "script") ".js"))
        (result)
        (command)
        (status))
    (with-temp-file temp-file
      (insert body)
      (write-region nil nil temp-file)
      (setq command
            (js-eval-make-babel-command
             temp-file temp-compiled-file))
      (with-output-to-temp-buffer (current-buffer)
        (setq status (shell-command command))))
    (setq result (with-temp-buffer
                     (insert-file-contents temp-compiled-file nil)
                     (buffer-string)))
    (when (zerop status)
      (setq result (with-temp-buffer
                     (insert-file-contents temp-compiled-file nil)
                     (buffer-string)))
      (list 0 result))))

(defun js-eval-join-when-exists (&rest args)
  "Join ARGS into a path if it exists.

Remaining arguments ARGS are strings representing file paths to be joined
together."
  (let ((joined-path (apply #'js-eval-join-file args)))
    (when (file-exists-p joined-path)
      joined-path)))

(defun js-eval-strip-props (item)
  "Strip text properties from strings in ITEM.

Argument ITEM is the object to be stripped of text properties; it can be a
string, a symbol, or nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun js-eval-group-by (prop items)
  "Group ITEMS by PROP using JavaScript evaluation.

Argument PROP is a property name used to group ITEMS.

Argument ITEMS is a list of objects to be grouped by PROP."
  (seq-reduce (lambda (acc it)
                (let* ((key (js-eval-get-prop it prop))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell) (list it))
                                (list it))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              items '()))

(defun js-eval-compose (&rest functions)
  "Compose FUNCTIONS and apply them to ARGS.

Remaining arguments FUNCTIONS are functions to be composed, where the output of
each function is passed as the input to the next."
  (lambda (&rest args)
    (car (seq-reduce (lambda (xs fn) (list (apply fn xs)))
                     (reverse functions) args))))

(defun js-eval-flip (func &optional arg-b)
  "Swap arguments A and B, then apply FUNC with optional ARG-B.

Argument FUNC is a function that takes at least two arguments.

Optional argument ARG-B is the second argument to pass to FUNC; if not provided,
FUNC is expected to be called with two or more arguments later."
  (if arg-b
      (apply-partially (lambda (a b &rest others) (apply func (append
                                                          `(,b ,a) others)))
                       arg-b)
    (lambda (a b &rest others) (apply func (append `(,b ,a) others)))))

(defun js-eval-sort-by-prop (prop items)
  "Sort ITEMS by PROP using JavaScript evaluation.

Argument PROP is a property name used to retrieve values from ITEMS for sorting.

Argument ITEMS is a list or vector of items to be sorted based on PROP."
  (seq-sort-by (lambda (it)
                 (or (js-eval-get-prop it prop) -1))
               #'< (if (vectorp items)
                       (append items nil)
                     items)))

(defun js-eval--node-path (&optional dir)
  "Determine the Node.js module path, optionally from DIR.

Optional argument DIR is the directory from which to resolve the node modules
path. If not provided, the current directory is used."
  (let ((result (replace-regexp-in-string
                 "[:]+" ":"
                 (concat
                  (string-join
                   (seq-uniq
                    (mapcar
                     #'expand-file-name
                     (delete
                      nil
                      (append
                       `(,js-eval-babel-node-modules-path
                         ,(and dir (js-eval-resolve-node-modules-dir
                                    dir))
                         ,(js-eval-node-modules-global))
                       (when-let ((node-path (getenv "NODE_PATH")))
                         (split-string node-path ":" t))))))
                   ":")
                  ":"))))
    result))

(defun js-eval--output (result file)
  "Display RESULT unless FILE is specified.

Argument RESULT is the value to be processed by the function.

Optional argument FILE is the file where the RESULT should be written; if not
provided, RESULT is returned directly."
  (unless file result))

(defun js-eval--shell-command-to-string (environ command)
  "Execute shell COMMAND with ENVIRON, return output.

Argument ENVIRON is a list of environment variables to set for the command.

Argument COMMAND is a list where the first element is the executable to run and
the remaining elements are the arguments to pass to it."
  (with-temp-buffer
    (let ((process-environment
           (append
            '("NODE_NO_WARNINGS=1") environ process-environment)))
      (apply #'call-process (car command) nil t nil (cdr command))
      (buffer-string))))

(defvar js-eval-project-files nil
  "List of JavaScript project files for evaluation.")
(defvar js-eval-current-buffer nil
  "Evaluates JavaScript code in the current buffer.")
(defun js-eval-init-project ()
  "Initialize project settings for JavaScript evaluation."
  (let ((root (js-eval-find-project-root)))
    (setq js-eval-current-buffer (current-buffer))
    (when (and js-eval-current-project-root
               (not (equal root js-eval-current-project-root)))
      (setq js-eval-aliases nil
            js-eval-current-alias nil))
    (setq js-eval-current-project-root root)
    (setq js-eval-aliases (delete-dups
                           (js-eval-get-aliases)))
    (when js-eval-current-alias
      (unless (member js-eval-current-alias js-eval-aliases)
        (setq js-eval-current-alias nil)))
    (unless js-eval-current-project-root
      (setq js-eval-current-project-root
            default-directory))
    (setq js-eval-project-files
          (js-eval-find-project-files
           js-eval-current-project-root))))

(defun js-eval-f-parent (path)
  "Evaluate JavaScript file's parent directory path.

Argument PATH is a string representing the file path for which the parent
directory is to be evaluated."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defun js-eval-syntax-propertize-regexp (end)
  "Highlight JavaScript regex literals up to END.

Argument END is the position in the buffer up to which the function will apply
syntax properties."
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      (goto-char (nth 8 ppss))
      (when (looking-at
             "/\\(?:[^/[\\]\\|\\\\.\\|\\[\\(?:[^]\\]\\|\\\\.\\)*]\\)*\\(/?\\)")
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defun js-eval-get-prev-token-ignore-whitespace ()
  "Retrieve previous code token, skipping whitespace."
  (save-excursion
    (js-eval-backward-whitespace)
    (when-let* ((end (point))
                (start
                 (when (or (< (skip-chars-backward "*_~$A-Za-z0-9") 0)
                           (< (skip-chars-backward "!@#%^&*=><~|\\-+/?:") 0)
                           (when (>= (1- (point))
                                     (point-min))
                             (forward-char -1)
                             t))
                   (point))))
      (cons (buffer-substring-no-properties start end) start))))

(defun js-eval-backward-up-list (&optional arg)
  "Move point to the start of the enclosing list or string.

Optional argument ARG is the number of levels to go up; it defaults to 1."
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((pos (point)))
    (when-let ((end (ignore-errors
                      (backward-up-list arg)
                      (point))))
      (unless (= end pos)
        end))))

(defun js-eval-backward-list ()
  "Move point to the start of the previous list."
  (when (looking-back "[)]" 0)
    (forward-sexp -1)
    (point)))

(defun js-eval-find-by-node-pos (pos node)
  "Find nodes by position within a JavaScript AST.

Argument POS is the position in the buffer to find the node.

Argument NODE is the syntax tree node to be searched for the given position."
  (cond ((consp node)
         (mapcar (apply-partially #'js-eval-find-by-node-pos pos) node))
        ((listp node)
         (seq-find (apply-partially #'js-eval-find-by-node-pos pos) node))
        ((stringp node)
         (let ((value (js-eval-get-prop node :value)))
           (when-let
               ((node-start
                 (car
                  (seq-sort
                   #'< (or
                       (delete
                        nil (append
                             (list
                              (js-eval-get-prop
                               value :start)
                              (js-eval-get-prop
                               value :parent-start)
                              (js-eval-get-prop
                               value :value-start))
                             (list
                              (js-eval-get-prop
                               node
                               :parent-start)
                              (js-eval-get-prop
                               node :start)
                              (js-eval-get-prop
                               node
                               :value-start))))))))
                (node-end
                 (car (seq-sort
                       #'> (delete nil (append (list
                                               (js-eval-get-prop
                                                value
                                                :end)
                                               (js-eval-get-prop
                                                value
                                                :parent-end)
                                               (js-eval-get-prop
                                                value
                                                :value-end))
                                              (list
                                               (js-eval-get-prop node
                                                                    :end)
                                               (js-eval-get-prop
                                                node
                                                :parent-end)
                                               (js-eval-get-prop
                                                node
                                                :value-end))))))))
             (and (<= node-start pos)
                  (> node-end pos)))))))

(defun js-eval-parse-context (&optional deep)
  "Parse JavaScript context at point, optionally deeply.

Optional argument DEEP is a boolean indicating whether to parse recursively. If
non-nil, the parsing will be deep."
  (let ((top-scope)
        (scopes)
        (pos (point))
        (parent-node)
        (children))
    (setq top-scope (save-excursion
                      (js-eval-parse-scope (point-min) (point-max) deep)))
    (setq parent-node
          (seq-find
           (apply-partially #'js-eval-find-by-node-pos pos) top-scope))
    (while (progn (setq children
                        (or
                         (delete nil
                                 (append (js-eval-get-prop parent-node
                                                          :args)
                                         (js-eval-get-prop parent-node
                                                          :children)))
                         (delete nil
                                 (append
                                  (js-eval-get-prop
                                   (js-eval-get-prop parent-node :value)
                                   :args)
                                  (js-eval-get-prop
                                   (js-eval-get-prop parent-node :value)
                                   :children))))))
      (setq scopes (append scopes children))
      (setq parent-node
            (seq-find
             (apply-partially #'js-eval-find-by-node-pos pos) children)))
    (js-eval-sort-by-prop :start (if scopes
                                    (append scopes top-scope)
                                  top-scope))))

(defun js-eval-parse-context-from-current-buffer (&optional start end deep)
  "Parse JavaScript context from buffer between START and END.

Optional argument START is the position in the buffer from which to start
parsing. If nil, parsing starts from the beginning of the buffer.

Optional argument END is the position in the buffer at which to stop parsing. If
nil, parsing goes until the END of the buffer.

Optional argument DEEP is a boolean indicating whether to parse the context
deeply. If nil, the parsing is shallow."
  (let ((result)
        (content (buffer-substring-no-properties
                  (or start (point-min))
                  (or end (point-max))))
        (p (point)))
    (js-eval-with-temp-buffer
     (if start
         (insert (make-string (1- start) ?\s) content)
       (insert content))
     (goto-char p)
     (setq result (js-eval-parse-current-scope))
     (setq result (append result (js-eval-parse-context deep)))
     result)))

(defun js-eval-parse-scope-inner (&optional start)
  "Parse JavaScript scope starting at optional START position.

Optional argument START is the position in the buffer from which to start
parsing. If nil, parsing starts from the current point."
  (when start (goto-char start))
  (when-let ((bounds (js-eval-forward-scope)))
    (save-excursion (js-eval-parse-scope (1+ (car bounds)) (cdr bounds)))))

(defun js-eval-parse-current-scope ()
  "Parse JavaScript scope for variables and arguments."
  (let ((items))
    (save-excursion
      (while (js-eval-backward-up-list)
        (let ((prev-token-cell (js-eval-get-prev-token-ignore-whitespace))
              (looking-at-brackets (looking-at "{"))
              (curr-pos (point))
              (prev-token)
              (prev-token-pos))
          (setq prev-token (car prev-token-cell))
          (setq prev-token-pos (cdr prev-token-cell))
          (when-let ((args
                      (cond ((equal prev-token "=>")
                             (goto-char prev-token-pos)
                             (js-eval-backward-whitespace)
                             (js-eval-backward-list)
                             (skip-chars-backward js-eval-regexp-name)
                             (let ((args
                                    (or
                                     (js-eval-parse-arguments)
                                     (js-eval-make-item
                                      (js-eval-get-obj-key)
                                      :var-type "Argument")))
                                   (children))
                               (goto-char curr-pos)
                               (setq children (js-eval-parse-scope-inner))
                               (append args children)))
                            ((and (equal prev-token "(")
                                  looking-at-brackets)
                             (goto-char prev-token-pos)
                             (save-excursion
                               (js-eval-parse-arguments)))
                            ((and (equal prev-token ")")
                                  looking-at-brackets)
                             (let* ((body (save-excursion
                                            (js-eval-parse-scope-inner)))
                                    (args (progn (goto-char (1+ prev-token-pos))
                                                 (forward-sexp -1)
                                                 (js-eval-parse-arguments))))
                               (append args body)))
                            ((and looking-at-brackets
                                  (equal prev-token "="))
                             (save-excursion
                               (js-eval-parse-object))))))
            (if (listp args)
                (setq items (append items args))
              (push args items))))))
    items))

(defun js-eval-get-deep-prop (item &rest path)
  "Retrieve nested property value from ITEM by following PATH.

Argument ITEM is the initial object or plist from which to start retrieving the
nested property.

Remaining arguments PATH are strings or symbols representing the keys in the
nested object or plist to traverse to get the desired value."
  (let ((value))
    (while (and item path (setq item (js-eval-get-prop item (pop path))))
      (setq value item))
    (if path
        nil
      value)))

(defun js-eval-parse-object-recoursive (&optional with-props)
  "Parse JavaScript objects recursively.

Optional argument WITH-PROPS is a boolean indicating whether to include
properties in the parsed object. If nil, properties are not included."
  (when (looking-at-p "{")
    (forward-char 1))
  (let ((alist)
        (key)
        (divider))
    (js-eval-forward-whitespace)
    (while (setq key
                 (progn
                   (js-eval-get-obj-key with-props :var-type "Property")))
      (let ((value)
            (value-start)
            (value-end))
        (js-eval-forward-whitespace)
        (setq divider (js-eval-get-next-char-or-word))
        (when (and (stringp divider)
                   (not (member divider '("(" "{" "<" "}" ")"))))
          (forward-char (length divider))
          (js-eval-forward-whitespace))
        (pcase divider
          ("as" (js-eval-forward-whitespace)
           (setq value-start (point))
           (setq value (js-eval-get-obj-key))
           (setq value-end (point)))
          ;; end key
          ((or "," ";")
           (setq value-start nil)
           (setq value-end nil))
          ;; object method
          ("("
           (setq value-start (point))
           (if-let ((method (js-eval-parse-object-method key)))
               (setq value method)
             (goto-char value-start)
             (forward-sexp 1)
             (setq value-end (point))
             (js-eval-forward-whitespace)
             (when (looking-at "{")
               (forward-sexp 1)
               (setq value-end (point))
               (js-eval-forward-whitespace))))
          ;; object value
          (":"
           (setq value-start (point))
           (cond
            ((looking-at "(")
             (if-let ((arrow (js-eval-parse-arrow-function)))
                 (setq value arrow)
               (js-eval-skip-to-char-same-scope "[,;]")
               (setq value-end (point))))
            ((looking-at "function[*\s\t\n]")
             (setq value (js-eval-parse-function-declaration)))
            ((looking-at "{")
             (setq key (js-eval-make-item key :value-start (point)))
             (setq value (js-eval-parse-object-recoursive with-props))
             (when (looking-at "}")
               (forward-char 1)
               (setq key (js-eval-make-item key :value-end (point)))
               (js-eval-forward-whitespace)
               (when (looking-at "|" )
                 (forward-char 1)
                 (js-eval-forward-whitespace)
                 (js-eval-skip-to-char-same-scope ";")))
             (js-eval-forward-whitespace))
            ((looking-at "[[]")
             (let ((arr-bounds (save-excursion (js-eval-forward-scope))))
               (setq value (js-eval-parse-array with-props))
               (goto-char (cdr arr-bounds))))
            (t (js-eval-parse-value)))
           (setq value-end (point)))
          (_ (js-eval-skip-to-char-same-scope "[,;}]")
             (setq value-end (point))))
        (setq value-end (point))
        (js-eval-forward-whitespace)
        (when (looking-at "[,;]")
          (forward-char 1)
          (js-eval-forward-whitespace))
        (when (and with-props key value-start value-end)
          (setq key (js-eval-make-item
                     key
                     :value-start
                     value-start
                     :value-end value-end))
          (when (and (or (stringp value)
                         (null value))
                     value-start value-end)
            (setq value (js-eval-make-item
                         (js-eval-normalize-value
                          (buffer-substring-no-properties
                           value-start
                           value-end))
                         :start value-start
                         :end value-end))))
        (cond ((and value)
               (push (cons key value) alist))
              ((and value-start value-end)
               (setq value
                     (js-eval-normalize-value
                      (buffer-substring-no-properties
                       value-start
                       value-end)))
               (push (cons key value) alist))
              (t
               (push (cons key nil) alist)))))
    (setq alist (reverse alist))))

(defun js-eval-parse-array (&optional with-props)
  "Parse a JavaScript array at point, optionally with properties.

Optional argument WITH-PROPS is a boolean indicating whether to include
properties in the parsed array. If nil, properties are not included."
  (when (looking-at "[[]")
    (let ((arr-items)
          (value))
      (save-excursion
        (forward-char 1)
        (js-eval-forward-whitespace)
        (while (setq value (pcase (js-eval-get-next-char 1)
                             ("{" (js-eval-parse-object with-props))
                             ("["
                              (let ((arr-bounds
                                     (save-excursion
                                       (js-eval-forward-scope))))
                                (prog1 (js-eval-parse-array with-props)
                                  (goto-char (cdr arr-bounds)))))
                             (_ (js-eval-parse-value))))
          (push value arr-items)
          (js-eval-forward-whitespace)
          (when (looking-at ",")
            (forward-char 1)
            (js-eval-forward-whitespace))))
      (apply #'vector (reverse arr-items)))))

(defun js-eval-parse-object (&optional with-props)
  "Parse JavaScript object or array at point.

Optional argument WITH-PROPS is a boolean indicating whether to include
properties in the parsed object."
  (with-syntax-table js-eval-mode-syntax-table
    (when-let ((bounds (save-excursion
                         (js-eval-forward-scope))))
      (let ((obj (or
                  (js-eval-parse-array with-props)
                  (js-eval-parse-object-recoursive with-props))))
        (goto-char (cdr bounds))
        (if with-props
            obj
          (js-eval-strip-object-props obj))))))

(defun js-eval-forward-lists ()
  "Evaluate forward s-expressions and whitespace in JavaScript code."
  (let ((count)
        (pos (point))
        (end))
    (while (looking-at "(")
      (forward-sexp 1)
      (setq count (1+ (or count 0)))
      (setq end (point))
      (js-eval-forward-whitespace))
    (if (null count)
        (progn
          (goto-char pos)
          nil)
      (goto-char end))))

(defun js-eval-parse-object-from-string (content &optional with-props)
  "Parse JSON object from string CONTENT.

Argument CONTENT is a string containing the JavaScript object to parse.

Optional argument WITH-PROPS is a boolean; when non-nil, the parsed items are
propertized."
  (js-eval-with-temp-buffer
   (insert content)
   (goto-char (point-min))
   (js-eval-parse-object with-props)))

(defun js-eval-add-parent-key (parent-key &optional key)
  "Add PARENT-KEY to KEY with proper JavaScript notation.

Argument PARENT-KEY is a string representing the parent KEY in a nested
JavaScript object.

Optional argument KEY is a string representing the key to be combined with
PARENT-KEY. If not provided, the default is nil."
  (cond ((or (null key)
             (string-empty-p parent-key))
         key)
        ((string= "[" (substring-no-properties key 0 1))
         (concat parent-key key))
        ((string-match-p "^['\"]" key)
         (concat parent-key "[" key "]"))
        (t (concat parent-key "." key))))

(defun js-eval-sort-object-props-by-pos (items)
  "Sort ITEMS by their start or parent-start positions.

Argument ITEMS is a list of objects to be sorted by their position properties."
  (let ((max (point-max)))
    (seq-sort-by (lambda (it)
                   (or (js-eval-get-prop it :start)
                       (js-eval-get-prop it :parent-start)
                       max))
                 #'< items)))

(defun js-eval-maybe-strip-quotes (item)
  "Strip quotes from ITEM if it starts and ends with them.

Argument ITEM is a string to be evaluated, which may have quotes to be stripped."
  (if (and
       (> (length item) 1)
       (string-match-p "^[\"']" item)
       (string-match-p "[\"']$" item)
       (not (string-match-p "[-/]" item)))
      (substring item 1 (1- (length item)))
    item))

(defun js-eval-get-object-keys (object &optional parent-key)
  "Extract keys from a JavaScript OBJECT or array.

Argument OBJECT is the JavaScript object from which to extract keys.

Optional argument PARENT-KEY is a string representing the parent key to prepend
to the current key, if any."
  (let ((keys))
    (when (vectorp object)
      (setq object
            (seq-map-indexed
             (lambda (it idx) (cons (js-eval-make-item
                                (concat
                                 "[" (js-eval-stringify idx) "]")
                                :start (js-eval-get-prop it :start)
                                :end (js-eval-get-prop it :end))
                               it))
             (append object nil))))
    (dolist (item object)
      (if (and (consp item)
               (car item)
               (not (functionp item)))
          (let ((key)
                (value (cdr (assoc (car item) object)))
                (vect)
                (subitems))
            (setq key (if parent-key
                          (js-eval-add-parent-key
                           parent-key
                           (js-eval-maybe-strip-quotes
                            (car item)))
                        (js-eval-maybe-strip-quotes (car item))))
            (setq subitems (cond
                            ((vectorp value)
                             (setq vect
                                   (seq-map-indexed
                                    (lambda (it idx)
                                      (cons (js-eval-make-item
                                             (concat
                                              "["
                                              (js-eval-stringify idx)
                                              "]")
                                             :start
                                             (js-eval-get-prop it :start)
                                             :end
                                             (js-eval-get-prop it :end))
                                            it))
                                    (append value nil)))
                             (js-eval-get-object-keys vect key))
                            ((and (listp value)
                                  (not (functionp value)))
                             (js-eval-get-object-keys value key))))
            (if subitems
                (setq keys (append `(,key) keys subitems))
              (push (js-eval-make-item key :value value) keys)))
        (when (stringp (car item))
          (push (car item) keys))))
    keys))

(defun js-eval-parse-token-at-point ()
  "Parse JavaScript token at point."
  (if-let
      ((obj
        (and
         (member
          (js-eval-get-next-char)
          '("[" "{"))
         (mapcar
          (lambda (it)
            (if (and (stringp it) (string-match-p "^[\\.]\\{3\\}" it))
                (js-eval-make-item
                 (replace-regexp-in-string
                  "^[\\.]\\{3\\}" "" it)
                 :var-type "...")
              it))
          (js-eval-parse-object-pattern
           (js-eval-parse-object t))))))
      obj
    (js-eval-get-obj-key t)))

(defun js-eval-parse-object-pattern (object &optional parent-key)
  "Parse and flatten JavaScript OBJECT pattern from OBJECT.

Argument OBJECT is the object pattern to parse.

Optional argument PARENT-KEY is the key of the parent OBJECT, used to build
nested paths."
  (let ((results))
    (when (vectorp object)
      (setq object (append object nil)))
    (dolist (item object)
      (when (listp item)
        (let ((key (car item))
              (value (cdr (assoc (car item) object)))
              (path)
              (vect))
          (setq path (if parent-key
                         (js-eval-add-parent-key
                          parent-key
                          (js-eval-maybe-strip-quotes key))
                       (js-eval-maybe-strip-quotes key)))
          (setq value (cond
                       ((vectorp value)
                        (setq vect
                              (seq-map-indexed
                               (lambda (it idx)
                                 (cons (js-eval-make-item
                                        (concat
                                         "["
                                         (js-eval-stringify idx)
                                         "]")
                                        :start
                                        (js-eval-get-prop it :start)
                                        :end
                                        (js-eval-get-prop it :end))
                                       it))
                               (append value nil)))
                        (js-eval-parse-object-pattern vect path))
                       ((and value (listp value))
                        (js-eval-parse-object-pattern value path))
                       ((stringp value)
                        (js-eval-make-item value
                                          :id value
                                          :as-name value
                                          :real-name path))
                       (t (js-eval-make-item key
                                            :id key
                                            :real-name path))))
          (push value results))))
    (flatten-list results)))

(defun js-eval-strip-object-props (item)
  "Strip properties from JavaScript object ITEM.

Argument ITEM is the object to be stripped of text properties."
  (cond
   ((and item (consp item)
         (stringp (car item)))
    (let ((key (js-eval-strip-text-props (car item)))
          (value (and item (listp item) (cdr item))))
      (cons (js-eval-maybe-strip-quotes key)
            (js-eval-strip-object-props value))))
   ((vectorp item)
    (apply #'vector
           (mapcar #'js-eval-strip-object-props item)))
   ((listp item)
    (mapcar #'js-eval-strip-object-props item))
   ((numberp item)
    item)
   ((stringp item)
    (cond ((or (js-eval-get-prop item :args)
               (js-eval-get-deep-prop item :value :args))
           (let ((args (or (js-eval-get-prop item :args)
                           (js-eval-get-deep-prop item :value :args))))
             (cond ((and (= 1 (length args))
                         (equal "" (car args)))
                    `((lambda ())))
                   (t
                    `((lambda
                        ,(seq-reduce
                          (lambda (acc it)
                            (setq it (js-eval-strip-text-props it))
                            (let ((value (if (string-empty-p it)
                                             nil
                                           (if (string-match-p "^[.]" it)
                                               (append '(&rest)
                                                       (list
                                                        (intern
                                                         (car
                                                          (split-string
                                                           it
                                                           "[.]"
                                                           t)))))
                                             (intern it)))))
                              (if (listp value)
                                  (append acc value)
                                (append acc (list value)))))
                          args
                          '())))))))
          (t (pcase item
               ("null" 'null)
               ("true" 't)
               ("false" '(nil))
               (_ (js-eval-maybe-strip-quotes
                   (js-eval-strip-text-props item)))))))))

(defun js-eval-get-bounds (&optional chars)
  "Find bounds of JavaScript expression at point.

Optional argument CHARS is a string of characters to include in the bounds. It
defaults to \"-*_~$A-Za-z0-9:\\\\.\"."
  (unless chars (setq chars "-*_~$A-Za-z0-9:\\."))
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward chars)
                (point)))
           (b (save-excursion
                (skip-chars-forward chars)
                (point))))
      (if (string-blank-p (buffer-substring-no-properties a b))
          nil
        (cons a b)))))

(defun js-eval-tokenize ()
  "Tokenize JavaScript code for evaluation."
  (pcase (js-eval-get-next-char 2)
    ("/*" (js-eval-forward-whitespace))
    ("//" (js-eval-forward-whitespace)))
  (or (js-eval-get-operator-at-point)
      (when-let ((char (js-eval-get-next-char 1)))
        (pcase char
          ("/" (let ((beg (point)))
                 (when (re-search-forward "[^/]/" nil t 1)
                   (skip-chars-forward "a-z")
                   (buffer-substring-no-properties beg (point)))))
          ((or "'" "\"" "`")
           (let ((beg (point)))
             (when (js-eval-re-search-forward "." nil t 1)
               (buffer-substring-no-properties beg (point)))))
          ((or "\n" "\s" "\t")
           (js-eval-forward-whitespace)
           (js-eval-tokenize))
          (_ (if (string-match-p "[_$A-Za-z0-9]" char)
                 (progn
                   (let ((beg (point)))
                     (skip-chars-forward "_$A-Za-z0-9")
                     (buffer-substring-no-properties beg (point))))
               (forward-char 1)
               char))))))

(defun js-eval-get-tokens ()
  "Extract JavaScript tokens and return them as a list."
  (let ((node)
        (nodes))
    (while (setq node (js-eval-tokenize))
      (let ((next (js-eval-tokenize)))
        (push node nodes)
        (when next
          (push next nodes))))
    (reverse nodes)))

(defun js-eval-comint-maybe-format-result (str)
  "Format STR using Prettier, return formatted or original.

Argument STR is a string to be formatted."
  (let ((result (js-eval-prettier-string
                 str
                 "babel"))
        (prefix "const result = "))
    (if (car result)
        (cadr result)
      (setq result (js-eval-prettier-string (concat prefix str)))
      (if (car result)
          (substring (cadr result) (length prefix))
        str))))


(defun js-eval-show-result (result &optional compiled-name)

  "Display JavaScript evaluation RESULT or visit compiled file.

Argument RESULT is the value to be shown after JavaScript evaluation.

Optional argument COMPILED-NAME is the name of the compiled file to visit if
RESULT is nil."
  (require 'js)
  (setq result (when result (js-eval-comint-maybe-format-result result)))
  (cond ((and (null result)
              (null compiled-name))
         (message "No result and compiled file"))
        ((and (null result)
              compiled-name)
         (if (file-exists-p compiled-name)
             (js-eval-visit-compiled compiled-name)
           (message "%s doesn't exists" compiled-name)))
        (t
         (with-selected-window (selected-window)
           (js-eval-popup-inspect result))))
  result)

(defun js-eval-transform-import-path (path dir &optional node-modules-path)
  "Transform JavaScript import PATH relative to DIR or NODE-MODULES-PATH.

Argument PATH is the import path to transform.

Argument DIR is the directory from which the import PATH should be resolved.

Optional argument NODE-MODULES-PATH is the PATH to the node_modules directory;
if provided, it is used to resolve dependencies."
  (cond ((or
          (js-eval-relative-p path)
          (member path (js-eval-extract-node-builins)))
         nil)
        ((and node-modules-path
              (js-eval-dependency-p path))
         (expand-file-name path node-modules-path))
        (t
         (replace-regexp-in-string
          "\\.\\(tsx\\|ts\\)$" ""
          (js-eval-path-to-relative
           (car (js-eval-alias-path-to-real path))
           dir)))))

(defun js-eval-transform-import-path-abs (path dir &optional
                                                  node-modules-path)
  "Transform a JS import PATH to an absolute path or nil.

Argument PATH is a string representing the module import path.

Argument DIR is a string representing the directory from which the import PATH
should be resolved.

Optional argument NODE-MODULES-PATH is a string representing the PATH to the
`node_modules' directory."
  (let ((result (cond ((assoc path (js-eval-extract-node-builins))
                       nil)
                      ((file-name-absolute-p path)
                       nil)
                      ((and node-modules-path
                            (not (js-eval-relative-p path))
                            (js-eval-dependency-p path))
                       (expand-file-name path node-modules-path))
                      (t
                       (when-let ((abs-path (js-eval-path-to-real path dir)))
                         (js-eval-get-temp-file-name abs-path))))))
    result))

(defun js-eval-transform-aliases-imports-to-abs (file &optional
                                                            node-modules-path
                                                            code)
  "Transform relative import paths to absolute in JavaScript code.

Argument FILE is the path to the JavaScript file to transform.

Optional argument NODE-MODULES-PATH is the path to the `node_modules' directory.

Optional argument CODE is the JavaScript code as a string to transform."
  (js-eval-with-buffer-or-file-content
      file
      (when code
        (erase-buffer)
        (insert code))
    (goto-char (point-min))
    (while (js-eval-re-search-forward
            js-eval-regexp-import-keyword nil t 1)
      (when-let* ((bounds (js-eval-inside-import-p (point)))
                  (path-bounds (js-eval-extract-import-path-bounds bounds))
                  (path (buffer-substring-no-properties
                         (car path-bounds)
                         (cdr path-bounds))))
        (when-let ((choice
                    (js-eval-transform-import-path-abs
                     path
                     default-directory node-modules-path)))
          (unless (null choice)
            (goto-char (car path-bounds))
            (delete-char (length path))
            (insert choice)))))
    (buffer-string)))

(defun js-eval-backward-chars (chars)
  "Extract a substring from the buffer without text properties.

Argument CHARS is a string of characters to skip backward over."
  (let* ((word-end (point))
         (word-start (progn (skip-chars-backward
                             chars)
                            (point))))
    (when (> word-end word-start)
      (buffer-substring-no-properties word-start word-end))))

(defun js-eval--backward-node ()
  "Navigate backward through JavaScript code nodes."
  (let ((init-pos (point))
        (pos))
    (when (save-excursion
            (js-eval-backward-whitespace)
            (member (js-eval-get-prev-char) '(";" ",")))
      (js-eval-re-search-backward "[,;]" nil t 1))
    (let ((count-brackets 0))
      (while
          (pcase (progn
                   (js-eval-backward-whitespace)
                   (js-eval-get-prev-char))
            ((pred null)
             nil)
            ((pred (lambda (val)
                     (string-match-p
                      js-eval-regexp-name-set
                      val)))
             (let ((word (js-eval-backward-chars js-eval-regexp-name)))
               (pcase word
                 ("function"
                  (js-eval-backward-whitespace)
                  (when (or (equal "=" (js-eval-get-operator-at-point -1))
                            (member (js-eval-backward-chars
                                     js-eval-regexp-name)
                                    '("async" "declare")))
                    (js-eval-backward-whitespace)
                    (point)))
                 ("return" nil)
                 ((or "import"
                      "const" "let" "class" "interface" "enum"
                      "var" "export" "module.exports" "interface"
                      "declare")
                  (while (progn
                           (js-eval-backward-whitespace)
                           (member (js-eval-backward-chars
                                    js-eval-regexp-name)
                                   '("import"
                                     "const" "let" "class" "interface"
                                     "enum"
                                     "var" "export" "module.exports"
                                     "interface"
                                     "declare")))))
                 (_ (point)))))
            ((pred (lambda (val)
                     (string-match-p
                      "[-!@#%^&*=><~|+/?:]"
                      val)))
             (skip-chars-backward "!@#%^&*=><~|\\-+/?:"))
            ("." (skip-chars-backward ".")
             (point))
            ("}"
             (when (= count-brackets 0)
               (setq count-brackets (1+ count-brackets))
               (forward-sexp -1)
               (point)))
            ((or "]" "'" "\"" "`")
             (forward-sexp -1)
             (point))
            (_ (js-eval-backward-list)))
        (setq pos (point)))
      (when (and pos (> init-pos pos))
        (js-eval-forward-whitespace)
        pos))))

;;;###autoload
(defun js-eval-backward-node ()
  "Evaluate JavaScript code from the previous node."
  (interactive)
  (js-eval--backward-node))

;;;###autoload
(defun js-eval-forward-node ()
  "Move forward and parse the next JavaScript node."
  (interactive)
  (let ((pos (point)))
    (pcase (js-eval-get-next-char)
      ("," (forward-char 1)))
    (js-eval-parse-node-at-point)
    (when (and (equal pos (point))
               (> (js-eval-forward-whitespace) 0))
      (js-eval-forward-node))))

;;;###autoload
(defun js-eval-export-it ()
  "Insert \"export\" before a JavaScript code block if not present."
  (interactive)
  (save-excursion
    (while (js-eval-backward-up-list))
    (unless (member (save-excursion
                      (js-eval-forward-whitespace)
                      (js-eval-which-word))
                    '("import"
                      "const" "let" "class" "interface"
                      "enum" "type"
                      "var" "export" "module.exports"
                      "interface"
                      "declare"))
      (js-eval-backward-node))
    (unless (member (js-eval-which-word) '("export"))
      (insert "export" (if (looking-at "\s") "" " ")))))

(defun js-eval-get-js-sexp-bounds ()
  "Determine JavaScript expression boundaries for evaluation."
  (when-let* ((b (save-excursion
                   (js-eval--backward-node)))
              (e (point)))
    (cons b (if (> (point-max) e)
                (+ e 1)
              e))))

(defun js-eval-get-project-current-name (&optional root)
  "Retrieve the current project's name from its path.

Optional argument ROOT is the root directory of the project. If not provided,
the function uses the value of `js-eval-current-project-root'."
  (and (or root js-eval-current-project-root)
       (car (reverse
             (split-string
              (or root js-eval-current-project-root) "/" t)))))

(defun js-eval-get-temp-project-dir (&optional root)
  "Create a temporary directory path for a JavaScript project.

Optional argument ROOT is the root directory from which to derive the project
name. If nil, the current directory is used as the root."
  (file-name-as-directory
   (expand-file-name
    (js-eval-get-project-current-name root)
    (temporary-file-directory))))

(defun js-eval-get-temp-file-name (filename)
  "Generate a temporary JavaScript file name from a TypeScript file.

Argument FILENAME is a string representing the name of the file for which to
generate a temporary file name."
  (when-let* ((project-re (and js-eval-current-project-root
                               (regexp-quote
                                js-eval-current-project-root)))
              (target-dir (js-eval-get-temp-project-dir
                           js-eval-current-project-root)))
    (replace-regexp-in-string "\\.tsx?$" ".js"
                              (replace-regexp-in-string
                               project-re
                               target-dir
                               filename))))

(defvar js-eval-files nil
  "List of JavaScript files to evaluate when `js-mode' starts.")

(defvar js-eval-node-path nil
  "Path to the Node.js executable for JavaScript evaluation.")

(defvar js-eval-node-modules-dir)

(defun js-eval-collect-imported-files (&optional init-file code)
  "Collect imports from JavaScript files.

Optional argument INIT-FILE is a string specifying the initial file to start
collecting imports from. If nil, the current buffer's file name is used.

Optional argument CODE is a string containing code to be inserted into the
buffer before collecting imports. If nil, the buffer's content is used as is."
  (let ((files `(,(or init-file buffer-file-name)))
        (file)
        (processed-files)
        (count 0))
    (while (setq file (pop files))
      (setq count (1+ count))
      (message "Collecting imports in %s" file)
      (unless (member file processed-files)
        (js-eval-with-buffer-or-file-content
            file
            (when (and code (= count 1))
              (erase-buffer)
              (insert code))
          (let ((paths (delete nil
                               (js-eval-find-imported-files)))
                (real-paths)
                (imports))
            (setq real-paths
                  (delete nil (mapcar #'js-eval-path-to-real
                                      paths)))
            (setq imports
                  (seq-remove
                   (lambda (it) (or
                                 (string= it file)
                                 (member it files)
                                 (member it processed-files)
                                 (string-match-p
                                  "/node_modules/" it)
                                 (null
                                  (when-let ((ext
                                              (file-name-extension
                                               it)))
                                    (member ext '("ts" "js" "jsx" "tsx" "json"))))))
                   real-paths))
            (setq files (append files imports))
            (push file processed-files)))))
    (seq-uniq (delete nil processed-files))))

(defvar js-eval-files-modified-time-cache (make-hash-table :test 'equal)
  "Cache mapping file paths to their last modified times.")

(defun js-eval-should-use-cache-p (source-file-name target-file-name)
  "Determine if cache should be used for evaluating JavaScript.

Argument SOURCE-FILE-NAME is the name of the source file to check for caching.

Argument TARGET-FILE-NAME is the name of the target file to check against the
cache."
  (when-let ((cached
              (when (file-exists-p target-file-name)
                (gethash source-file-name
                         js-eval-files-modified-time-cache)))
             (modified-time (file-attribute-modification-time
                             (file-attributes source-file-name 'string))))
    (equal cached modified-time)))

(defun js-eval--compile-file (source-filename target-filename &optional
                                              node-modules-path)
  "Compile JavaScript file SOURCE-FILENAME to TARGET-FILENAME.

Argument SOURCE-FILENAME is the path to the source file to be compiled.

Argument TARGET-FILENAME is the path where the compiled file will be saved.

Optional argument NODE-MODULES-PATH is the path to the node_modules directory;
it defaults to nil."
  (let ((parent-dir)
        (content))
    (setq content
          (cadr (js-eval-babel-compile
                 (js-eval-transform-aliases-imports-to-abs
                  source-filename node-modules-path))))
    (setq parent-dir (file-name-directory target-filename))
    (unless (file-exists-p parent-dir)
      (make-directory parent-dir t))
    (write-region content nil target-filename nil nil nil nil)))

(defun js-eval-compile-files (files)
  "Compile JavaScript FILES if not cached.

Argument FILES is a list of cons cells where the car is the source file name and
the cdr is the target file name."
  (let ((node-modules-path
         (when js-eval-current-project-root
           (js-eval-join-when-exists
            js-eval-current-project-root js-eval-node-modules-dir))))
    (dolist (cell files)
      (let ((source-file-name (car cell))
            (target-file-name (cdr cell)))
        (unless (js-eval-should-use-cache-p source-file-name target-file-name)
          (js-eval--compile-file source-file-name
                                 target-file-name
                                 node-modules-path)
          (puthash source-file-name
                   (file-attribute-modification-time
                    (file-attributes source-file-name
                                     'string))
                   js-eval-files-modified-time-cache))))))

(defun js-eval-import-to-fullname (imported-item &optional delimiter)
  "Convert IMPORTED-ITEM to a unique, delimited full name string.

Argument IMPORTED-ITEM is the item to be imported.

Optional argument DELIMITER is the string used to separate the parts of the
fullname. It defaults to \" as \"."
  (unless delimiter
    (setq delimiter " as "))
  (when imported-item
    (string-join
     (seq-uniq
      (mapcar
       #'js-eval-strip-text-props
       (delete
        nil (list (js-eval-get-prop imported-item :real-name)
                  (js-eval-strip-props imported-item)
                  (js-eval-get-prop imported-item :as-name)))))
     delimiter)))

(defun js-eval-regenerate-imports (imports &optional dir node-modules-dir)
  "Regenerate JavaScript IMPORTS from a list of IMPORTS.

Argument IMPORTS is a list of import objects to regenerate.

Optional argument DIR is the directory from which to resolve relative import
paths.

Optional argument NODE-MODULES-DIR is the directory containing node modules for
resolving package imports."
  (let ((lines)
        (grouped-imports (js-eval-group-by :display-path imports))
        (imp))
    (while (setq imp (pop grouped-imports))
      (let ((display-path (car imp))
            (named-imports
             (seq-filter
              (js-eval-compose
               (apply-partially #'equal 4)
               (js-eval-flip
                'js-eval-get-prop :type))
              (cdr imp)))
            (default
             (js-eval-import-to-fullname
              (seq-find
               (lambda (it)
                 (member (js-eval-get-prop it :type)
                         '(1 16)))
               (cdr imp))))
            (result)
            (transformed-path))
        (when named-imports
          (setq named-imports
                (string-join
                 (delete
                  nil
                  (mapcar
                   #'js-eval-import-to-fullname
                   named-imports))
                 ", "))
          (setq named-imports
                (when named-imports
                  (concat "{ " named-imports " }"))))
        (setq result
              (string-join
               (delete nil
                       (list default named-imports))
               ", "))
        (setq transformed-path (js-eval-transform-import-path
                                display-path
                                dir node-modules-dir))
        (when result
          (push (format "import %s from '%s';" result
                        (or
                         transformed-path
                         display-path))
                lines))))
    (string-join lines "\n")))

(defun js-eval-comint-tokenise (code)
  "Tokenize JavaScript CODE string CODE.

Argument CODE is a string containing the JavaScript code to be tokenised."
  (with-temp-buffer
    (save-excursion (insert code))
    (js-eval-get-tokens)))

(defun js-eval-copy-item (item)
  "Copy JavaScript evaluation result from buffer without properties.

Argument ITEM is the item from which properties are retrieved and used to get
the substring."
  (when-let ((start (js-eval-get-prop item :start))
             (end (js-eval-get-prop item :end)))
    (buffer-substring-no-properties start end)))

(defun js-eval-get-region ()
  "Extract and trim text from the active region."
  (when
      (and (region-active-p)
           (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end)))))

(defun js-eval-extract-code ()
  "Extract JavaScript code considering imports and regions."
  (when-let* ((bounds (or
                       (when (and (region-active-p)
                                  (use-region-p))
                         (cons (region-beginning)
                               (region-end)))
                       (js-eval-get-js-sexp-bounds)))
              (init-code (string-trim (buffer-substring-no-properties
                                       (car bounds)
                                       (cdr bounds)))))
    (let* ((ast (js-eval-parse-context-from-current-buffer (point-min)
                                                           (car bounds)))
           (imports))
      (let ((codes `(,init-code))
            (included-codes)
            (code)
            (tokens)
            (ast-items))
        (while (setq code (pop codes))
          (setq tokens (seq-uniq (js-eval-comint-tokenise code)))
          (when-let ((filtered-ast (seq-filter
                                    (lambda (it)
                                      (and
                                       (stringp it)
                                       (null (member it ast-items))
                                       (member
                                        (js-eval-strip-text-props it)
                                        tokens)))
                                    ast)))
            (setq ast-items (append ast-items filtered-ast))
            (dolist (item filtered-ast)
              (unless (or (member item included-codes)
                          (member item imports))
                (if (js-eval-get-prop item :import)
                    (push item imports)
                  (push item included-codes)))
              (let ((str (js-eval-copy-item item)))
                (push str codes)))))
        (setq included-codes
              (mapcar #'js-eval-copy-item
                      (js-eval-sort-by-prop :start included-codes)))
        (unless (member init-code included-codes)
          (setq included-codes (append included-codes (list init-code))))
        (setq code (string-join included-codes "\n\n"))
        (string-join
         (delete nil
                 (list
                  (and
                   imports
                   (js-eval-regenerate-imports
                    imports
                    default-directory
                    (js-eval-join-when-exists
                     js-eval-current-project-root
                     js-eval-node-modules-dir)))
                  code))
         "\n\n")))))



(defun js-eval-eval-compiled (compiled-name)
  "Execute JavaScript code from a compiled file.

Argument COMPILED-NAME is a string representing the file name of the compiled
JavaScript code to be evaluated."
  (when-let ((tmp (and (file-exists-p compiled-name)
                       (replace-regexp-in-string
                        "\\.[a-z]+$" "-temp.js"
                        compiled-name))))
    (with-temp-file tmp
      (insert js-eval-util-string (format "('%s', '', '%s')" compiled-name "")))
    (setq js-eval-node-path (js-eval--node-path default-directory))
    (js-eval--output
     (js-eval--shell-command-to-string
      (list (format "NODE_PATH=%s" js-eval-node-path))
      (list "node" tmp))
     nil)))

(defun js-eval-files-to-compile-alist (files)
  "Map FILES to temporary filenames for JavaScript evaluation.

Argument FILES is a list of file names to be processed by
`js-eval-files-to-compile-alist'."
  (mapcar (lambda (it) (cons it (js-eval-get-temp-file-name it))) files))


(defun js-eval-eval-0 (arg)
  "Evaluate JavaScript code from Emacs buffer or string.

Optional argument ARG is a string of JavaScript code to evaluate. If arg is nil,
the function prompts the user to input the code."
  (js-eval-init-project)
  (let ((current-file buffer-file-name))
    (let* ((target-filename (js-eval-get-temp-file-name current-file)))
      (let ((code (or arg
                      (read-string "Eval:\n" (js-eval-extract-code)))))
        (let ((files-to-compile (delete current-file
                                        (js-eval-collect-imported-files
                                         current-file code))))
          (setq code (if js-eval-current-project-root
                         (js-eval-transform-aliases-imports-to-abs
                          current-file
                          (js-eval-join-when-exists
                           js-eval-current-project-root
                           js-eval-node-modules-dir)
                          code)
                       code))
          (setq js-eval-files
                (js-eval-files-to-compile-alist files-to-compile))
          (js-eval-compile-files js-eval-files)
          (when-let ((compiled-code (cadr (js-eval-babel-compile code))))
            (unless (file-exists-p (file-name-directory target-filename))
              (make-directory (file-name-directory target-filename) t))
            (write-region compiled-code nil target-filename nil nil nil nil)
            (if js-eval-use-window
                (progn
                  (js-eval-server-eval-request-async
                   (seq-filter
                    #'cdr
                    `(("code" . ,compiled-code)
                      ("file" . ,current-file)
                      ("dir" . ,default-directory)
                      ("reset" . ,(js-eval-f-parent buffer-file-name))
                      ("nodeModulesPaths"
                       .
                       ,(when js-eval-current-project-root
                         (expand-file-name
                          "node_modules/" js-eval-current-project-root)))
                      ("rootDir" . ,js-eval-current-project-root)))
                   'js-eval-show-result))
              (js-eval-eval-compiled target-filename))))))))

;;;###autoload
(defun js-eval-server-clear ()
  "Clear the JavaScript REPL buffer's contents."
  (interactive)
  (when-let* ((buf (js-eval-server-get-buffer))
              (old-buf (current-buffer)))
    (save-excursion
      (cond
       ((buffer-live-p buf)
        (switch-to-buffer buf)
        (erase-buffer)
        (goto-char (point-min))
        (switch-to-buffer old-buf)
        (message "Javascript REPL cleared."))
       (t
        (error "Javascript REPL buffer doesn't exist!"))))))

;;;###autoload
(defun js-eval-compile-file (&optional
                             source-filename
                             target-filename
                             node-modules-path)
  "Compile JavaScript file, optionally specifying paths.

Optional argument SOURCE-FILENAME is the name of the file to compile.

Optional argument TARGET-FILENAME is the name of the file where the compiled
code will be saved.

Optional argument NODE-MODULES-PATH is the path to the node_modules directory."
  (interactive)
  (unless source-filename
    (setq source-filename (read-file-name "File to compile:\s")))
  (unless target-filename
    (setq target-filename (read-file-name "Save file to:\s")))
  (when (and
         (null node-modules-path)
         (called-interactively-p 'any))
    (setq node-modules-path
          (read-directory-name "Node modules:\s")))
  (js-eval--compile-file source-filename
                             target-filename
                             node-modules-path))

;;;###autoload
(defun js-eval-visit-compiled (&optional file)
  "Open compiled JS FILE in Emacs.

Optional argument FILE is the path to the compiled JavaScript file. If not
provided, it defaults to the temporary FILE name derived from the current
buffer's FILE name."
  (interactive)
  (when-let ((compiled-file
              (or file
                  (and buffer-file-name
                       (js-eval-get-temp-file-name buffer-file-name))
                  (let* ((buffs (mapcar
                                 #'window-buffer
                                 (window-list)))
                         (files (mapcar
                                 #'js-eval-get-temp-file-name
                                 (delq nil
                                       (mapcar (apply-partially
                                                #'buffer-local-value
                                                'buffer-file-name)
                                               buffs)))))
                    (if (length> files 1)
                        (completing-read "File to visit: " files)
                      (car files))))))
    (when (file-exists-p compiled-file)
      (let* ((actions '((?o "other" find-file-other-window)
                        (?c "current" find-file)))
             (answer (read-multiple-choice "promp?\s"
                                           actions))
             (action (caddr answer)))
        (funcall action compiled-file)))))

;;;###autoload
(defun js-eval-eval (&optional arg)
  "Evaluate and compile JavaScript code and display the result.

Optional argument ARG is a string of JavaScript code to be evaluated."
  (interactive "P")
  (let ((result (js-eval-eval-0 arg)))
    (when (stringp result)
      (js-eval-show-result result
                           (when buffer-file-name
                             (js-eval-get-temp-file-name
                              buffer-file-name))))))

;;;###autoload
(defun js-eval-compile-region-or-buffer ()
  "Compile JavaScript code from region or buffer."
  (interactive)
  (js-eval-popup-inspect
   (cadr
    (js-eval-babel-compile (or (js-eval-get-region)
                               (buffer-substring-no-properties
                                (point-min)
                                (point-max)))))))

;;;###autoload
(defun js-eval-cleanup ()
  "Delete temporary files and buffers created by `js-eval-files'."
  (interactive)
  (mapc (lambda (it) (when-let ((buff (get-file-buffer it)))
                  (kill-buffer buff))
          (delete-file it))
        (mapcar #'cdr js-eval-files)))

;;;###autoload
(defun js-eval-toggle-use-window ()
  "Toggle the use of a separate window for JavaScript evaluation."
  (interactive)
  (unless js-eval-use-window
    (unless (js-eval-server-get-process)
      (js-eval-server-run)))
  (setq js-eval-use-window (not js-eval-use-window)))

(defun js-eval--change-file-ext (file new-ext)
  "Replace extension of FILE with NEW-EXT."
  (concat (file-name-sans-extension file) "." new-ext))




(defun js-eval--compile-typescript-string (code &rest tsc-args)
  "Compile typescript CODE string with TSC-ARGS."
  (setq tsc-args (flatten-list tsc-args))
  (let* ((temp-file (concat (temporary-file-directory)
                            (make-temp-name "script") ".ts"))
         (outfile (js-eval--change-file-ext temp-file "js"))
         (command (string-join
                   (delq nil (append (list "tsc"
                                           (shell-quote-argument
                                            temp-file))
                                     tsc-args))
                   "\s")))
    (when-let ((buff (get-file-buffer outfile)))
      (with-current-buffer buff
        (set-buffer-modified-p nil))
      (kill-buffer buff))
    (let ((inhibit-message nil))
      (write-region code nil temp-file)
      (with-temp-buffer
        (shell-command command (current-buffer)
                       (current-buffer))
        (if (file-exists-p outfile)
            (with-temp-buffer
              (insert-file-contents outfile)
              (buffer-string))
          (minibuffer-message "An error: %s" (buffer-string))
          nil)))))

(defun js-eval--get-other-wind ()
  "Return another window or split sensibly if needed."
  (let ((wind-target
         (if (minibuffer-selected-window)
             (with-minibuffer-selected-window
               (let ((wind (selected-window)))
                 (or
                  (window-right wind)
                  (window-left wind)
                  (split-window-sensibly)
                  wind)))
           (let ((wind (selected-window)))
             (or
              (window-right wind)
              (window-left wind)
              (split-window-sensibly)
              wind)))))
    wind-target))



;;;###autoload
(defun js-eval-esbuild-str (input-string)
  "Pass INPUT-STRING to esbuild for bundling using an asynchronous process."
  (interactive (list (buffer-string)))
  (require 'ansi-color)
  (let* ((process-name "esbuild-process")
         (buffer-name "*esbuild-output*")
         (process-buffer (get-buffer-create buffer-name))
         (esbuild-cmd "esbuild")
         (process (start-process
                   process-name
                   process-buffer
                   esbuild-cmd
                   "--loader=ts"
                   "--bundle")))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p _e)
                            (when (eq (process-status p)
                                      'exit)
                              (message
                               "esbuild process finished"))))
    (process-send-string process (concat input-string "\n"))
    (process-send-eof process)
    (with-current-buffer process-buffer
      (setq buffer-read-only t)
      (goto-char (point-max)))
    (display-buffer process-buffer)))

;;;###autoload
(defun js-eval-esbuild (file &rest args)
  "Eval a JavaScript FILE with ARGS and display output in a buffer.

Argument PROGRAM is the name of the executable to run.

Argument ARGS is a list of strings to be passed as arguments to the PROGRAM.

Optional argument ENV is a list of environment variables to set in the form of
\"VAR=VALUE\"."
  (interactive (list (or buffer-file-name
                         (read-file-name "Eval: "))))
  (require 'ansi-color)
  (let* ((buff-name (format "*js-eval-esbuild-%s*" file))
         (buff (get-buffer-create buff-name))
         (input-string (with-temp-buffer (insert-file-contents file)
                                         (buffer-string)))
         (process-environment (cons "NODE_NO_WARNINGS=1"
                                    process-environment))
         (proc (apply #'start-file-process buff-name buff
                      "esbuild" "--bundle"
                      "--platform=node"
                      "--tree-shaking=false"
                      (concat "--sourcefile=" file)
                      "--loader=tsx"
                      args)))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (set-process-sentinel
     proc
     (lambda (process _)
       (let ((proc-status (process-status process))
             (proc-exit-status (process-exit-status process)))
         (when (memq proc-status '(exit signal))
           (message "js-eval: process status %s" proc-status)
           (cond ((zerop proc-exit-status)
                  (require 'js-comint)
                  (require 'nodejs-repl)
                  (with-current-buffer (process-buffer process)
                    (ansi-color-apply-on-region
                     (point-min)
                     (point-max))
                    (let* ((str (buffer-substring-no-properties (point-min)
                                                                (point-max)))
                           (dir (file-name-directory file))
                           (file (make-temp-file "js-eval"  nil ".js" str))
                           (new-filename (concat (expand-file-name (file-name-base
                                                                    file)
                                                                   dir)
                                                 ".js"))
                           (repl-str (concat js-eval-util-string (format
                                                                  "('%s', '', '%s')"
                                                                  new-filename
                                                                  ""))))
                      (rename-file file new-filename t)
                      (let ((default-directory dir))
                        (js-eval--exec "node" "-p" repl-str
                                       (lambda ()
                                         (when (file-exists-p new-filename)
                                           (delete-file new-filename))))))))
                 ((and
                   (not (member "--platform=node" args))
                   (with-current-buffer (process-buffer
                                         process)
                     (ansi-color-filter-region
                      (point-min)
                      (point-max))
                     (goto-char (point-max))
                     (re-search-backward
                      "You can use \"--platform=node\"" nil t 1)))
                  (message "js-eval: Retrying")
                  (js-eval-esbuild file (append args "--platform=node")))
                 (t
                  (message "js-eval: other")
                  (with-selected-window (js-eval--get-other-wind)
                    (pop-to-buffer-same-window (with-current-buffer (process-buffer
                                                                     process)
                                                 (ansi-color-apply-on-region
                                                  (point-min)
                                                  (point-max))
                                                 (current-buffer))))))))))
    (set-process-filter
     proc
     (lambda (proc string)
       (when-let ((buf (process-buffer proc)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (point-max))
               (insert string)))))))
    (process-send-string proc (concat input-string "\n"))
    (process-send-eof proc)
    proc))

(defun js-eval--exec (program &rest args)
  "Execute a JavaScript PROGRAM with ARGS and display output in a buffer.

Argument PROGRAM is the name of the executable to run.

Argument ARGS is a list of strings to be passed as arguments to the PROGRAM.

Optional argument ENV is a list of environment variables to set in the form of
\"VAR=VALUE\"."
  (require 'ansi-color)
  (let* ((buff-name (format "*js-eval-%s*" program))
         (buff (get-buffer-create buff-name))
         (on-done (seq-find #'functionp args))
         (process-environment (cons "FORCE_COLOR=1"
                                    (cons "NODE_NO_WARNINGS=1"
                                          process-environment)))
         (proc (apply #'start-file-process buff-name buff
                      program (seq-remove #'functionp args))))
    (with-selected-window (js-eval--get-other-wind)
      (pop-to-buffer-same-window buff))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (set-process-sentinel
     proc
     (lambda (process _)
       (let ((proc-status (process-status process)))
         (when (memq proc-status '(exit signal))
           (with-current-buffer (process-buffer process)
             (ansi-color-apply-on-region (point-min)
                                         (point-max))
             (when on-done
               (funcall on-done)))))))
    (set-process-filter
     proc
     (lambda (proc string)
       (when-let ((buf (process-buffer proc)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (point-max))
               (insert string)))))))
    proc))

;;;###autoload
(defun js-eval-current-file-with-ts-node (file)
  "Evaluate JavaScript FILE in Node.js and display result in color."
  (interactive (list (or buffer-file-name
                         (read-file-name "Eval file: "))))
  (let ((process-environment (cons "FORCE_COLOR=1" process-environment)))
    (js-eval--exec "ts-node" file)))

;;;###autoload
(defun js-eval-current-file-with-node (file)
  "Evaluate JavaScript FILE in Node.js and display result."
  (interactive (list (or buffer-file-name
                         (read-file-name "Eval file: "))))
  (let ((process-environment (cons "FORCE_COLOR=1" process-environment)))
    (js-eval--exec "node" file)))

;;;###autoload
(defun js-eval-buffer ()
  "Evaluate entire buffer as JavaScript code."
  (interactive)
  (js-eval-eval
   (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun js-eval-ensure-babel-project ()
  "Ensure Babel project directory and dependencies."
  (interactive)
  (when-let ((project-dir (replace-regexp-in-string
                           "/node_modules/?$" ""
                           js-eval-babel-node-modules-path)))
    (when (and (not (file-exists-p project-dir))
               (yes-or-no-p (format "Create directory %s?" project-dir)))
      (make-directory project-dir t))
    (when-let ((command (js-eval-make-npm-install-command)))
      (unless (file-exists-p (expand-file-name "package.json" project-dir))
        (setq command (concat "npm init -y && " command)))
      (when (yes-or-no-p (format "Run %s?" command))
        (js-eval-exec-in-dir command project-dir)))))

(defun js-eval-current-file-compiled-p ()
  "Check if compiled JS file exists for current buffer."
  (when-let ((file (when buffer-file-name
                     (js-eval-get-temp-file-name
                      buffer-file-name))))
    (file-exists-p file)))

;;;###autoload
(define-minor-mode js-eval-eval-on-save-mode
  "Runs js-eval-inspect on file save when this mode is turned on."
  :lighter " js-eval-inspect"
  :global nil
  (if js-eval-eval-on-save-mode
      (add-hook 'before-save-hook #'js-eval-buffer nil 'local)
    (remove-hook 'before-save-hook #'js-eval-buffer 'local)))


;;;###autoload (autoload 'js-eval-transient "js-eval" nil t)
(transient-define-prefix js-eval-transient ()
  "Toggle JavaScript evaluation modes and run actions."
  [["Eval"
    ("t" js-eval-toggle-use-window
     :transient t
     :description
     (lambda ()
       (concat "Window (DOM) mode "
               (if
                   (bound-and-true-p js-eval-use-window)
                   (propertize "(On)" 'face 'success)
                 (propertize "(Off)" 'face 'error)))))
    ("e" "Region or sexp" js-eval-eval)
    ("b" "Buffer" js-eval-buffer)
    ("l" "Cleanup" js-eval-cleanup)
    ("c" "File without compiling" js-eval-current-file-with-node)]
   ["Compile"
    ("o" "File" js-eval-compile-file)
    ("C" "Region or buffer" js-eval-compile-region-or-buffer)
    ("v"  js-eval-visit-compiled
     :description
     (lambda ()
       (if (js-eval-current-file-compiled-p)
           (format
            "Visit compiled file (%s)" (js-eval-get-temp-file-name
                                        buffer-file-name))
         "Visit compiled file "))
     :inapt-if-not js-eval-current-file-compiled-p)]]
  [["Results"
    ("w" "Copy results" js-eval-overlay-copy
     :inapt-if-not
     (lambda ()
       js-eval-overlay-at))
    ("i" "Open inspector" js-eval-popup-open-inspector)]
   ["Navigation"
    ("x" "Export node at point" js-eval-export-it)
    ("B" "Backward node" js-eval-backward-node)
    ("f" "Forward node" js-eval-forward-node)]
   ["Server"
    ("S" "Run" js-eval-server-run)
    ("r" "Clear" js-eval-server-clear)
    ("E" "Setup" js-eval-ensure-babel-project)]])

(provide 'js-eval)
;;; js-eval.el ends here
;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:
