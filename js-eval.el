;;; js-eval.el --- Configure eval -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-eval
;; Keywords: lisp, languages
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

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

;; This file configures operations with eval

;;; Code:


(eval-and-compile
  (require 'cc-mode))

(eval-when-compile
  (require 'subr-x))

(require 'request)

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
  "Regexp matching index file.")

(defun js-eval-string-match-p (regexp str &optional start)
  "Check STR for a match with REGEXP and return t or nil whether it exists."
  (when (and str (stringp str)
             (string-match-p regexp str start))
    t))

(defun js-eval-add-ext-if-not (file extension)
  "Add EXTENSION to FILE if it doesn't match `js-eval-file-ext-regexp'."
  (if (js-eval-string-match-p js-eval-file-ext-regexp file)
      file
    (concat file "." extension)))

(defun js-eval-is-index-file-p (path)
  "Return t if base name of PATH equals index, nil otherwise."
  (js-eval-string-match-p js-eval-file-index-regexp path))

(require 'json)

(defvar js-eval-popup-inspect-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 0") 'kill-this-buffer)
    (define-key map (kbd "C-c C-o") 'js-eval-popup-maybe-find-file)
    map))

(defvar js-eval-popup-inspect-buffer-name "*js-eval-popup-insepct*")

(defvar js-eval-popup-momentary-buffer-name "*js-eval-popup*")
(defvar js-eval-server-params nil)
(defvar js-eval-popup-window-last-key nil)

(defvar js-eval-popup-content nil)
(defvar js-eval-popup-meta nil)
(defvar js-eval-popup-switch-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") #'js-eval-popup-open-inspector)
    map)
  "Keymap with commands to execute just before exiting.")

(defvar js-eval-server-status nil)
(defvar js-eval-last-result nil)

(defvar js-eval-aliases nil
  "A list of aliases to use in projects.")

(defvar js-eval-util-string
  "module.exports = __js_eval__ = function (path, eoe, outputPath) { result = eval(require('fs').readFileSync(path, { encoding: 'utf8' })); stringify = function (thingToStringify, isFunctionFull, maxLengthForStrings) { class Serialize { constructor({ thing, verboseFunctions, maxStringLength }) { this.seen = []; this.thing = thing; this.verboseFunctions = verboseFunctions; this.maxStringLength = maxStringLength; this.circular = JSON.stringify('#<Circular>'); this.isFunction = this.isFunction.bind(this); this.isString = this.isString.bind(this); this.isArray = this.isArray.bind(this); this.isBoolean = this.isBoolean.bind(this); this.isNumber = this.isNumber.bind(this); this.isWindow = this.isWindow.bind(this); this.isWindowNode = this.isWindowNode.bind(this); this.isDate = this.isDate.bind(this); this.tryStringify = this.tryStringify.bind(this); this.annotateFunction = this.annotateFunction.bind(this); this.stringify = this.stringify.bind(this); this.serialize = this.serialize.bind(this); this.tryStorage = this.tryStorage.bind(this); this.annotateFn2 = this.annotateFn2.bind(this); } static makeSerialize(params) { return new Serialize(params); } seen; thing; maxStringLength; verboseFunctions; circular = JSON.stringify('#<Circular>'); isFunction(v) { return v instanceof Function || typeof v === 'function'; } removeComments(str) { return str.replace(/\\/\\*[\\s\\S]*?\\*\\/|\\/\\/.*/g, '').trim(); } isString(v) { return typeof v === 'string'; } isArray(v) { return v instanceof Array || Array.isArray(v); } isNumber(v) { return typeof v === 'number'; } isBoolean(v) { return typeof v === 'boolean'; } isWindow(it) { return globalThis.window && globalThis.window === it; } isWindowNode(v) { return ( globalThis.window && globalThis.window.Node != null && v instanceof globalThis.window.Node ); } isDate(v) { return Object.prototype.toString.call(v) === '[object Date]'; } annotateFunction(str) { let parts = this.removeComments(str.toString()).split('').reverse(); let processed = []; let curr; let bracketsOpen = 0; let bracketsClosed = 0; let openCount = 0; let closedCount = 0; let result; while ((curr = !result && parts.pop())) { if (curr === '(') { openCount += 1; } else if (curr === ')') { closedCount += 1; } if (openCount > 0) { processed.push(curr); if (curr === '{') { bracketsOpen += 1; } else if (curr === '}') { bracketsClosed += 1; } } result = result || (bracketsOpen === bracketsClosed && openCount === closedCount && openCount > 0) ? processed.join('') : undefined; } return result ? 'function'.concat(result).concat('{}') : this.annotateFn2(str); } annotateFn2(fn) { const name = fn.name || fn.toString() || ''; const len = fn.length || 0; let idx = 0; let list; list = new Array(len); while (idx < len) { list[idx] = `arg${idx}`; idx += 1; } return 'function ' + name + '(' + list.join(', ') + ') {}'; } stringify(obj) { if (this.isBoolean(obj)) { return obj.toString(); } else if (obj === undefined) { return 'undefined'; } else if (obj === null) { return 'null'; } else if (this.isNumber(obj)) { return obj.toString(); } else if (this.isString(obj)) { return this.maxStringLength && obj.length > this.maxStringLength ? this.tryStringify(obj.substring(0, 100).concat('...')) : JSON.stringify(obj); } else if (this.isWindowNode(obj)) { return this.tryStringify(obj); } else if (this.isFunction(obj)) { return this.verboseFunctions ? obj.toString().replace(/{\\s\\[native code\\]\\s}/g, '{}') : this.annotateFunction(obj); } else if (this.isDate(obj)) { return this.tryStringify(obj); } else if (this.isArray(obj)) { if (this.seen.indexOf(obj) >= 0) { return this.circular; } else { this.seen.push(obj); return `[${obj.map(this.serialize).join(', ')}]`; } } else { if (this.seen.indexOf(obj) >= 0) { return this.circular; } else { this.seen.push(obj); const pairs = []; for (let key in obj) { if ( obj && obj.hasOwnProperty && this.isFunction(obj.hasOwnProperty) && obj.hasOwnProperty(key) ) { let pair = this.stringify(key) + ': '; pair += this.serialize(obj[key]); pairs.push(pair); } } return `{ ${pairs.join(', ')} }`; } } } tryStorage(storage, storageType) { return this.stringify({}); } serialize(...args) { const it = args.length > 0 ? args[0] : this.thing; if (!this.isWindow(it)) { return this.stringify(it); } if (this.seen.indexOf(it) >= 0) { return this.circular; } else { this.seen.push(it); } const storageMock = { sessionStorage: this.tryStorage, localStorage: this.tryStorage, }; const res = Object.keys(it).reduce((pairs, key) => { const value = storageMock[key] ? storageMock[key]() : this.isWindow(it[key]) ? this.circular : this.stringify(it[key]); pairs.push(`${this.tryStringify(key)}: ${value}`); return pairs; }, []); return `{ ${res.join(', ')} }`; } tryStringify(it) { let result; try { result = JSON.stringify(it); } catch (error) { result = JSON.stringify(error.message || error); } return result; } } return Serialize.makeSerialize({ thing: thingToStringify, verboseFunctions: isFunctionFull, maxStringLength: maxLengthForStrings, }).serialize(); }; write = function (obj) { if (outputPath) { if (obj instanceof Buffer) { require('fs').writeFileSync(outputPath, obj); } else if (obj && 'function' === typeof obj.pipe) { obj.pipe(require('fs').createWriteStream(outputPath)); } } else { console.log(stringify(obj)); } process.stdout.write(eoe); }; if (result && 'function' === typeof result.then) { result.then(write, write); } else { write(result); } };")

(defvar js-eval-server-process-name "emacs-jsdom-run")
(defvar js-eval-server-buffer-name
  (concat "*" js-eval-server-process-name "*"))

(defvar js-eval-response nil)
(defvar js-eval-callback nil)

(defcustom js-eval-tsconfig-filename "tsconfig.json"
  "Name of tsconfig or jsconfig."
  :group 'js-eval
  :type 'string)

(defcustom js-eval-node-modules-priority-section-to-read
  '("jsnext:main" "module" "types" "typings")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-eval
  :type '(repeat string))

(defvar js-eval-files-cache (make-hash-table :test 'equal))

(defcustom js-eval-package-json-sections
  '("dependencies" "devDependencies" "peerDependencies")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-eval
  :type '(repeat string))

(defconst js-eval-node-modules-regexp
  "\\(/\\|^\\)node_modules\\(/\\|$\\)"
  "Regexp matching path with node_modules.")

(defvar js-eval-current-project-root nil)

(defcustom js-eval-preffered-extensions
  '("ts" "tsx" "jsx" "es6" "es" "mjs" "js" "cjs" "ls" "sjs" "iced" "liticed"
    "json")
  "Preferred suffixes for files with different extension."
  :group 'js-eval
  :type '(repeat string))

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

(defvar js-eval-json-hash (make-hash-table :test 'equal))

(defvar js-eval--regexp-js-regexp
  "\\([!]?+\\)?/\\(?:[^/[\\]\\|\\\\.\\|\\[\\(?:[^]\\]\\|\\\\.\\)*]\\)*\\(/?\\)"
  "Regexp matching javascript regular expression.")

(defvar js-eval-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for command `js-eval-mode'.")

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
  "List of javascript assignment operators.")

(defmacro js-eval-with-temp-buffer (&rest body)
  "Setup temp buffer and execute BODY."
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
  "Parse and forward path at point."
  (when-let ((str (js-eval-parse-string)))
    (substring str 1 (1- (length str)))))

(defun js-eval-parse-es-export-braces ()
  "Parse and forward defined with esm syntax braces at point."
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
  "Get assigmnent operator at point or nil."
  (when-let ((operator (js-eval-get-operator-at-point)))
    (when (member operator js-eval-assignment-operators)
      operator)))

(defun js-eval-parse-amd-export ()
  "Parse amd export at point."
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
  "If ITEMS is list apply FUNC to each element otherwise call FUNC with ITEMS."
  (if (listp items)
      (mapcar (lambda (it) (js-eval-apply-on-one-many func it))
              items)
    (and items (funcall func items))))

(defun js-eval-parse-es-import-braces ()
  "Parse es import braces at point."
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
  "Return t if text after point matches regular expression REGEXP.
Case is not ignored."
  (let ((case-fold-search nil))
    (looking-at regexp)))

(defun js-eval-get-require-path ()
  "If text after point is require call, return it's path."
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
  "Stringify and `propertize' ITEM with PROPS."
  (apply #'propertize
         (js-eval-stringify item)
         props))

(defun js-eval-reserved-word-p (str)
  "Check if STR is a reserved keyword.
Keywords specifiied in the variable `js-eval-reserved-js-words'."
  (when (stringp str)
    (member str js-eval-reserved-js-words)))

(defun js-eval-ensure-semicolon ()
  "If text after point is semicolon forward it and return point position.
Whitespace and comments are ignored."
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
  "Parse and forward iife at point."
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
  "Parse and forward export at point."
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
  "Parse and forward es import at point."
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
      (while (pcase (js-eval-get-next-char-or-word)
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
               (_ (when-let ((default (js-eval-get-word-if-valid))
                             (beg (point)))
                    (skip-chars-forward js-eval-regexp-name-set)
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
  "Parse variable at point.
With optional argument NODE use it's property :parent-start."
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
  "Parse and forward statement at point."
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
  "Parse assignment at point and return propertized string."
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
  "Return t if STRING is a valid variable name, otherwise nil."
  (not (or
        (null string)
        (js-eval-string-match-p (concat "[" "^" js-eval-regexp-name "]")
                                   string)
        (js-eval-reserved-word-p string))))

(defun js-eval-parse-node-at-point (&optional deep)
  "Parse node and forward node at point.
If optional argument DEEP is non nil, parse functions recoursively."
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
  "Return word at point if it is valid and not reserved, otherwise nil."
  (when-let ((word (js-eval-which-word)))
    (when (js-eval-valid-identifier-p word)
      word)))

(defun js-eval-parse-funcall-params ()
  "Parse arguments of non-recoursively."
  (js-eval-parse-arguments nil 'js-eval-parse-object))

(defun js-eval-parse-scope (&optional start end deep callback)
  "Parse scope in current buffer.
Optional argument START is inital point.
END is max point to parse.
DEEP is whether to parse recoursive.
CALLBACK is function to be called with every node."
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
  "Get thing at point matching REGEXP."
  (when-let ((bounds (js-eval-get-bounds regexp)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun js-eval-skip-string ()
  "Jumps to the end of string."
  (with-syntax-table js-eval-mode-syntax-table
    (when-let ((beg (nth 8 (syntax-ppss (point)))))
      (goto-char beg)
      (forward-sexp 1))))

(defun js-eval-parse-funcall ()
  "Parse function call at point and return propertized string."
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
  "Forward regexp at point and return string."
  (when-let ((re (and (looking-at js-eval--regexp-js-regexp)
                      (js-eval-re-search-forward
                       js-eval--regexp-js-regexp nil t 1)
                      (match-string-no-properties 0))))
    (let ((pos (point)))
      (if (> (skip-chars-forward "gimsuy") 0)
          (concat re (buffer-substring-no-properties pos (point)))
        re))))

(defun js-eval-get-operator-at-point ()
  "Forward and return operator at point as string."
  (let ((start (point)))
    (when (> (skip-chars-forward "!@#%^&*=><~|\\-+/?:") 0)
      (buffer-substring-no-properties start (point)))))

(defun js-eval-forward-angles ()
  "Forward angles at point and return string."
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
  "Forward and substring js string at point including quotes."
  (when-let ((start (and (looking-at "[`'\"]")
                         (point))))
    (forward-sexp 1)
    (buffer-substring-no-properties start (point))))

(defun js-eval-parse-arguments (&optional deep parse-object-fn)
  "If text after point is open bracket parse arguments at point.
With optional argument DEEP functions will be parsed recoursively.
PARSE-OBJECT-FN specifies how to parse objects."
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
  "Return string with typescript value if char at point is colon."
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
  "Forward arrow at point and whitespace after.
Return poisiton of arrow's end."
  (when-let ((next (js-eval-get-next-char 2)))
    (when (string= next "=>")
      (forward-char 2)
      (prog1
          (point)
        (js-eval-forward-whitespace)))))

(defun js-eval-get-word-or-char ()
  "Get word or char at point."
  (or (js-eval-get-word)
      (js-eval-get-next-char)))

(defun js-eval-get-comments-bounds ()
  "Return alist of bounds of comments in the current buffer."
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
                         (p2 (point-at-eol)))
                    (push (cons p1 p2) comments))))))
          comments)))))

(defun js-eval-find-package-json ()
  "Return the path to package.json."
  (when-let ((root (js-eval-find-project-root)))
    (js-eval-join-file root "package.json")))

(defun js-eval-get-next-char (&optional nth position)
  "Return concatenated string with NTH chars after POSITION.
Default value for NTH is 1 and for POSITION - value of current point."
  (let* ((beg (or position (point)))
         (end (+ (or nth 1) beg)))
    (when (> (point-max) end)
      (buffer-substring-no-properties beg end))))

(defun js-eval-parse-value ()
  "Forward and parse value and point."
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
           func :children children
           :value-end (point) :value-type "function"))
        (js-eval-forward-whitespace "\s\t"))
      (when (looking-at "\\(\n\\|\r\\)[\s\t\f]?+[?:|><,]")
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
           :start start :end end)
        (let ((val (pop nodes)))
          (if (listp val)
              (js-eval-make-item
               (mapconcat #'js-eval-stringify val "")
               :start start :end end)
            (js-eval-make-item
             (format "%s" val) :start start :end end)))))))

(defun js-eval-get-obj-key (&optional with-props &rest args)
  "Forward symbol at point and return string.
If optional argument WITH-PROPS is non-nil, return string propertied with ARGS
and props :id, :start and :end."
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
  "Return word or character after current point."
  (when-let ((char (js-eval-get-next-char)))
    (if (string-match-p "[_$A-Za-z0-9]" char)
        (js-eval-which-word)
      char)))

(defun js-eval-parse-object-method (&optional id)
  "Parse object method at point and return propertized string.
Optional argument ID is will b."
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
  "Parse arrow function at point."
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
  "Skip to STOP-CHARS in same scope."
  (unless stop-chars (setq stop-chars "[;]"))
  (let ((open-chars-re "[^]({[;}]+")
        (search-point)
        (scope-start)
        (scope-end)
        (stop))
    (when (looking-at "[[({[]")
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
                      (when (looking-at "[[({[]")
                        (forward-sexp 1)
                        (setq scope-end (point)))))
              (cond
               ((and scope-start search-point
                     (> search-point scope-start))
                (if scope-end
                    (goto-char scope-end)
                  (goto-char scope-start)))
               ((and search-point)
                (goto-char search-point)
                (skip-chars-backward stop-chars)
                (setq stop t)))))
      (if (looking-at "[[({[]")
          (forward-sexp 1)
        (setq stop t)))
    (and (looking-at stop-chars)
         (point))))

(defun js-eval-parse-function-declaration (&optional deep)
  "Parse function declaration at point.
Depending of optional argument DEEP return propertized string or list.
If value of DEEP is not nil, return list."
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
         (when (looking-at "*")
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
  "Trim VALUE and if maybe convert it to number."
  (setq value
        (string-trim value))
  (if (string-match-p "^[0-9]+$" value)
      (setq value (string-to-number value))
    value))

(defmacro js-eval-with-buffer-or-file-content (filename &rest body)
  "Execute BODY in temp buffer with file or buffer content of FILENAME.
Bind FILENAME to variables `buffer-file-name' and `current-path''.
It is also bind `default-directory' into FILENAME's directory."
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
  "Replace comments in buffer with empty lines."
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
  "Read a SECTION from PACKAGE-JSON-PATH and return its hash.
By default PACKAGE-JSON-PATH is a value of `js-eval-find-package-json'.
Default section is `dependencies'"
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
  "Replace STR with a recoded by CODING-SYSTEM text."
  (setq coding-system (or coding-system 'utf-8))
  (with-temp-buffer
    (insert str)
    (js-eval-recode-buffer-or-region coding-system)
    (buffer-string)))

(defun js-eval-recode-buffer-or-region (&optional coding-system)
  "Replace the region with a recoded with CODING-SYSTEM text."
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
  "Move forward across one balanced expression and return it's bounds."
  (when (looking-at "[{[]")
    (let ((scope-start (point))
          (scope-end))
      (forward-sexp 1)
      (setq scope-end (point))
      (cons scope-start scope-end))))

(defun js-eval-read-json (file &optional json-type)
  "Read the json object in FILE, return object converted to JSON-TYPE.
JSON-TYPE must be one of `alist', `plist', or `hash-table'."
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
  "Return directories in PATH with package.json."
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
  "Read JSON-FILE and return first section from SECTIONS."
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
  "Add to PATH extensions and return first existing result or nil otherwise.
Extensions is stored in a variable `js-eval-preffered-extensions'.
With optional argument DIR expand PATH to DIR.
If PATH match `js-eval-file-ext-regexp' just expands PATH to DIR."
  (let ((expanded-path (if dir (expand-file-name path dir) path)))
    (if (js-eval-string-match-p js-eval-file-ext-regexp path)
        expanded-path
      (seq-find #'file-exists-p
                (mapcar (apply-partially #'js-eval-add-ext-if-not
                                         expanded-path)
                        js-eval-preffered-extensions)))))

(defun js-eval-directory-files (dir &optional
                                       recursive regexp include-dirs pred)
  "Return files in DIR whose names match REGEXP.
Default value of REGEXP is specified in a variable `js-eval-file-ext-regexp'.
Optional argument RECURSIVE non-nil means to search recursive.
PRED can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into)."
  (unless regexp (setq regexp js-eval-file-ext-regexp))
  (if recursive
      (directory-files-recursively dir regexp include-dirs pred)
    (directory-files dir t regexp t)))

(defun js-eval-looking-at-comment-p (&optional max)
  "Return if point located at the start of comment.
Optional argument MAX defines a limit."
  (and (> (or max (1- (point-max))) (point))
       (car (member (buffer-substring-no-properties
                     (point)
                     (+ 2 (point))) '("#!" "/*" "//")))))

(defun js-eval-get-rebounds-at-point (&optional rechars)
  "Get bounds of thing at point depending on RECHARS."
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
  "Return the parent directory to PATH."
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
  "Convert PATH to absolute filename and append slash to PATH.
Without BASE-URL resolve PATH as relative to project directory,
otherwise firstly expand BASE-URL to project directory."
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
	"Get buffer named `js-eval-server-buffer-name'."
  (get-buffer js-eval-server-buffer-name))

(defun js-eval-syntax-propertize (start end)
  "Propertize text between START and END."
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
  "Return regexp from WORDS surrounded with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt (if (listp words)
                                 words
                               (list words)) t) "\\_>"))

(defun js-eval-get-package-json-modules (&optional project-root)
  "Return dependencies of PROJECT-ROOT from package.json."
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
  "Return value of CACHE-KEY from a variable `js-eval-files-cache'.
CACHE-KEY should be a filename due to invalidation which uses modification time."
  (let* ((cache (gethash cache-key js-eval-files-cache))
         (cache-tick (and cache (plist-get cache :tick)))
         (tick (file-attribute-modification-time (file-attributes
                                                  cache-key
                                                  'string))))
    (when (equal cache-tick tick)
      (plist-get cache :cache))))

(defun js-eval-set-file-cache (path content)
  "Put CONTENT to hash table using PATH as key."
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
  "Extract nested packages from MODULES.
Every element in MODULES should be listed in packages.json.
NODE-MODULES-PATH is used to expand path of MODULES."
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
  "Sort FILES by optional argument EXTENSIONS.
Default value for EXTENSIONS keeps a variable `js-eval-preffered-extensions.'"
  (setq extensions (or extensions js-eval-preffered-extensions))
  (seq-sort-by (lambda (a)
                 (if-let ((ext (and a (file-name-extension a))))
                     (or (seq-position extensions ext 'string=) -1)
                   -1))
               #'>
               files))

(defun js-eval-get-path-ext-candidates (path)
  "Return files filtered by base name of PATH from parent directory of PATH.
PATH should be an absolute filename without extension."
  (let ((parts (reverse (split-string path "/")))
        (module-re)
        (parent-dir))
    (setq module-re (concat "\\(/\\|^\\)" (pop parts)
                            js-eval-file-ext-regexp))
    (setq parent-dir (concat (or (string-join
                                  (reverse parts) "/") "") "/"))
    (directory-files parent-dir t module-re)))

(defun js-eval-slash (str)
  "Append slash to non-empty STR unless one already."
  (cond ((string= "" str) str)
        ((string= "/" str) "")
        ((stringp str)
         (if (string-match "/$" str)
             str
           (concat str "/")))))

(defun js-eval-join-file (&rest args)
  "Join ARGS to a single path."
  (let (path (relative (not (file-name-absolute-p (car args)))))
    (mapc (lambda (arg)
            (unless (null arg)
              (setq path (expand-file-name arg path))))
          args)
    (if relative (file-relative-name path) path)))

(defun js-eval-try-find-real-path (path)
  "Resolve PATH as dependency."
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
  "Move point forward accross SKIP-CHARS and comments.
Returns the distance traveled, either zero or positive."
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
  "Return value of comment character in syntax table's or nil otherwise."
  (with-syntax-table js-eval-mode-syntax-table
    (let ((comment-start "//")
          (comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
          (comment-use-syntax t)
          (result (nth 4 (syntax-ppss))))
      result)))

(defun js-eval-get-prev-char (&optional nth position)
  "Return concatenated string with NTH chars before POSITION.
Default value for NTH is 1 and for POSITION - value of current point."
  (let* ((end (or position (point)))
         (beg (- end (or nth 1))))
    (when (>= beg (point-min))
      (buffer-substring-no-properties
       beg end))))

(defun js-eval-re-search-backward (re &optional bound noerror count)
  "Search backward from point for RE ignoring strings and comments.
Optional arguments BOUND, NOERROR, COUNT has the same meaning
as `re-search-backward'."
  (let ((case-fold-search nil))
    (js-eval-re-search-forward re bound noerror (if count (- count) -1))))

(defun js-eval-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `js-eval-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
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
  "This function is helper for `js-eval-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
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
                (point-at-eol) t))
              ((nth 7 parse)
               (forward-line))
              ((or (nth 4 parse)
                   (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
               (re-search-forward "\\*/"))
              (t
               (setq count (1- count)))))))
  (point))

(defun js-eval-which-word (&optional regexp)
  "Return string with a thing at point matched REGEXP or nil otherwise."
  (when-let ((bounds (js-eval-get-rebounds-at-point regexp)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun js-eval-inside-string-p (&optional position)
  "Return non-nil if point POSITION inside string, else nil.
Result depends on syntax table's string quote character."
  (with-syntax-table js-eval-mode-syntax-table
    (nth 3 (syntax-ppss (or position (point))))))

(defun js-eval-resolve-module (dir &optional file)
	"Resolve module FILE in DIR."
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
  "Expand TSCONFIG-NAME to PROJECT-ROOT and return alist of aliases and paths."
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
  "Convert and sort alist of PATHS to absolute filenames.
First element of each pair in PATHS supposed to be an alias and rest elements as
relative to BASE-URL if provided or project directory."
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
	"Take :response from PROPS and set it to `js-eval-response'."
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
}")

(defcustom js-eval-project-aliases nil
  "An associated list of ((ALIAS_A . DIRECTORY_A) (ALIAS_B . DIR_B DIR_C))."
  :group 'js-eval
  :set (lambda (var value &rest _ignored)
         (let ((aliases (js-eval-normalize-aliases value)))
           (set var aliases)))
  :type '(alist
          :key-type (string :tag "Alias")
          :value-type (repeat :tag "Path" directory)))

(defcustom js-eval-babel-node-modules-path "~/js-eval/node_modules/"
  "Directory with babel executable, plugins and presets."
  :type 'directory
  :group 'js-eval-javascript)

(defcustom js-eval-babel-options nil
  "Options for compiling with babel.
Plugins and presets used in options should exists in
`js-eval-babel-node-modules-path'."
  :type '(repeat string)
  :group 'js-eval-javascript)

(defconst js-eval-from-keyword--re
  (js-eval-make-opt-symbol-regexp "from"))

(defconst js-eval-regexp-import-keyword
  (eval-and-compile
    (concat "\\_<" (regexp-opt `(,"import") t) "\\_>"))
  "Regexp matching keyword import.")

(defcustom js-eval-node-modules-dir "node_modules"
  "Relative to project root or absolute path to node_modules directory."
  :group 'js-eval
  :type 'string)

(defvar js-eval-overlay-at nil
  "Overlay variable for `js-eval-overlay-show'.")

(defun js-eval-popup-minibuffer-select-window ()
  "Select minibuffer window if it is active."
  (when-let ((wind (active-minibuffer-window)))
    (select-window wind)))

(define-minor-mode js-eval-popup-mode
  "Toggle js eval pop mode."
  :lighter " js-eval-popup"
  :keymap js-eval-popup-switch-keymap
  :global nil)

(define-minor-mode js-eval-popup-inspect-mode
  "Toggle `js-eval-popup-inspect-mode'."
  :lighter " js-eval-popup"
  :keymap js-eval-popup-inspect-keymap
  :global nil)

(defun js-eval-overlay-cleanup (&rest _args)
  "Remove overlay defined in `js-eval-overlay-at'."
  (remove-hook 'before-change-functions 'js-eval-overlay-cleanup t)
  (when (and js-eval-overlay-at
             (overlayp js-eval-overlay-at))
    (delete-overlay js-eval-overlay-at))
  (setq js-eval-overlay-at nil))

(defun js-eval-read-babel-config ()
	"Read babel config from `js-eval-babel-config-string'."
  (with-temp-buffer
    (save-excursion (insert js-eval-babel-config-string))
    (let ((json-object-type 'alist)
          (json-array-type 'list))
      (json-read))))

(defun js-eval-flatten (items)
  "Flattenize ITEMS."
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
	"Get dependencies for babel."
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
	"Make npm install command."
  (when-let ((dependencies (js-eval-get-missing-dependencies)))
    (string-join (append '("npm install --save-dev") dependencies) "\s")))

(defun js-eval-get-missing-dependencies ()
  "Return list of missing dependencies for babel."
  (seq-remove (lambda (it) (file-exists-p
                       (expand-file-name
                        it
                        js-eval-babel-node-modules-path)))
              (js-eval-get-config-dependencies)))

(defun js-eval-exec-in-dir (command project-dir &optional callback)
  "Execute COMMAND in PROJECT-DIR.
If PROJECT-DIR doesn't exists, create new.
Invoke CALLBACK without args."
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
  "Return the value of zero position's PROPERTY in ITEM."
  (if (stringp item)
      (get-text-property 0 property item)
    (when (listp item)
      (plist-get item property))))

(defun js-eval-make-item (candidate &rest plist)
  "Propertize CANDIDATE with filtered PLIST."
  (let ((pl plist)
        (key)
        (filtered-pl))
    (while (setq key (pop pl))
      (when-let ((val (pop pl)))
        (push val filtered-pl)
        (push key filtered-pl)))
    (apply #'js-eval-propertize candidate filtered-pl)))

(defun js-eval-stringify (x)
  "Convert X to string."
  (cond
   ((stringp x)
    x)
   ((stringp x)
    (symbol-name x))
   ((numberp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun js-eval-normalize-object-prop-position (item)
  "Normalize ITEM position."
  (if-let ((found (seq-find
                   (lambda (it) (and it (js-eval-get-prop it :start)))
                   (reverse (split-string item "\\.\\|]\\|\\[")))))
      (js-eval-make-item item
                         :start (js-eval-get-prop found :start)
                         :end (js-eval-get-prop found :end)
                         :var-type "Property")
    item))

(defun js-eval-get-object-items (obj &optional parent-key)
	"Return nested paths from OBJ, optionally with PARENT-KEY."
  (js-eval-sort-object-props-by-pos
   (mapcar #'js-eval-normalize-object-prop-position
           (js-eval-get-object-keys obj parent-key))))

(defun js-eval-node-modules-candidates (&optional project-root)
  "Return dependencies of PROJECT-ROOT from package json."
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
  "Return the path to node-modules for PROJECT-DIR."
  (if (file-name-absolute-p js-eval-node-modules-dir)
      js-eval-node-modules-dir
    (when-let ((root (or project-dir (js-eval-find-project-root))))
      (setq root (car (split-string root js-eval-node-modules-regexp)))
      (js-eval-join-when-exists root js-eval-node-modules-dir))))

(defun js-eval-resolve-paths (path &optional dir)
  "Resolve PATH in DIR and return list of existing files."
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
  "If ITEM is string, return it without text properties.
If ITEM is symbol, return it is `symbol-name.'
Otherwise return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun js-eval-find-project-root (&optional directory)
  "Traverse up as long as package.json will be found, starting at DIRECTORY."
  (unless directory (setq directory default-directory))
  (let ((parent (expand-file-name ".." directory)))
    (unless (or (string= parent directory)
                (string= directory "")
                (string= directory "/"))
      (if (file-exists-p (expand-file-name "package.json" directory))
          directory
        (js-eval-slash (js-eval-find-project-root parent))))))

(defun js-eval-node-module-to-real (module &optional project-root)
  "Find MODULE from PROJECT-ROOT in node_modules and return it full path."
  (when-let* ((node-modules (or (js-eval-find-node-modules project-root)
                                (js-eval-find-node-modules)))
              (real-path (js-eval-join-file node-modules module)))
    (if (and (js-eval-string-match-p js-eval-file-ext-regexp real-path)
             (file-name-absolute-p real-path))
        real-path
      (js-eval-try-find-real-path real-path))))

(defun js-eval-get-es-imports-bounds ()
  "Return a cons with bounds of import stament of PATH."
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
  "Move point backward accross SKIP-CHARS and comments.
Returns the distance traveled, either zero or positive."
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
  "Search forward from point for REGEXP ignoring comments and strings.
Optional arguments BOUND, NOERROR, COUNT has the same meaning
as for `re-search-forward'."
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
  "Return closest module path at point."
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
  "Return shell command string for compiling SOURCE-FILE into TARGET-FILE."
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
                              ".babelrc"))
                (list "&> /dev/null")))
   "\s"))

(defun js-eval-resolve-node-modules-dir (dir)
	"Resolve node modules for DIR."
  (js-eval-resolve-module dir "node_modules"))

(defun js-eval-node-modules-global ()
	"Return path to global node modules."
  (when-let ((dir (shell-command-to-string "npm config get prefix")))
    (setq dir (expand-file-name "lib/node_modules/" dir))
    (when (file-exists-p dir)
      dir)))

(defun js-eval-get-aliases ()
  "Return a sorted list of aliases.
Value of allises is specified in variable `js-eval-project-aliases'."
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
  "Return files of PROJECT-ROOT without node_modules."
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
	"Eval request async with PAYLOAD-ALIST with callback CB."
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
                       (display-buffer
                        (get-buffer-create js-eval-server-buffer-name)))
           :success #'js-eval-response-success))

(defun js-eval-server-get-process ()
	"Get process with name `js-eval-server-process-name'."
  (get-process js-eval-server-process-name))

;;;###autoload
(defun js-eval-server-run ()
	"Run js window server."
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

(defvar js-eval-current-alias nil)

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
mapNodeBuiltins();")

;;;###autoload
(defun js-eval-overlay-copy ()
	"Copy after-string property from variable `js-eval-overlay-at'."
  (interactive)
  (when-let ((str (overlay-get js-eval-overlay-at 'after-string)))
    (kill-new str)
    (message "Copied")
    str))

(defvar js-eval-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'js-eval-overlay-cleanup)
    (define-key map (kbd "M-w") 'js-eval-overlay-copy)
    map))

(defun js-eval-node-modules-bin-files ()
  "Look up directory hierarchy for node_modules/.bin and return executable files."
  (when-let* ((node-modules
               (locate-dominating-file
                default-directory
                "node_modules"))
              (exec-dir
               (expand-file-name "node_modules/.bin/" node-modules))
              (commands
               (seq-filter #'file-executable-p
                           (and (file-exists-p exec-dir)
                                (directory-files-recursively exec-dir ".")))))
    commands))

(defun js-eval-prettier-js-local-command ()
	"Return full path to prettier executable in local node_modules dir."
  (seq-find (lambda (it)
              (string= "prettier"
                       (file-name-base it)))
            (js-eval-node-modules-bin-files)))

(defun js-eval-prettier-string (&optional string parser)
  "Apply prettier on STRING with PARSER.
Return list of two elements: status (t or nil) and string with result."
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
  "Fontify CONTENT according to MODE-FN called with ARGS.
If CONTENT is not a string, instead of MODE-FN emacs-lisp-mode will be used."
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

(defun js-eval-popup (content &rest setup-args)
  "Momentarily display CONTENT in popup window.
Display remains until next event is input.

Persist popup if input is a key binding of a command
 `js-eval-popup-open-inspector'in `js-eval-popup-switch-keymap'.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
See a function `js-eval-popup-open-inspector'."
  (let ((buffer (get-buffer-create
                 js-eval-popup-momentary-buffer-name))
        (mode-fn (seq-find #'functionp setup-args)))
    (setq js-eval-popup-content (if (or
                                     mode-fn
                                     (not (stringp content)))
                                    (apply #'js-eval-popup-fontify
                                           (list content mode-fn))
                                  content))
    (setq js-eval-popup-meta setup-args)
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-in-side-window
                '((window-height . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (setq header-line-format
                    (substitute-command-keys "\\<js-eval-popup-switch-keymap>\
Use `\\[js-eval-popup-open-inspector]' to open popup"))
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (js-eval-popup-mode)
                (set-keymap-parent js-eval-popup-switch-keymap
                                   (current-local-map))
                (when js-eval-popup-content
                  (insert js-eval-popup-content))
                (unwind-protect
                    (setq js-eval-popup-window-last-key
                          (read-key-sequence ""))
                  (quit-restore-window window 'kill)
                  (if (lookup-key js-eval-popup-switch-keymap
                                  js-eval-popup-window-last-key)
                      (run-at-time '0.5 nil 'js-eval-popup-open-inspector)
                    (setq unread-command-events
                          (append (this-single-command-raw-keys)
                                  unread-command-events)))
                  (setq js-eval-popup-window-last-key nil)))))
        (js-eval-popup-mode)
        (insert js-eval-popup-content)))))

(defun js-eval-popup-inspect (content &rest setup-args)
  "Display CONTENT in popup window.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
A filename can be opened with \\<js-eval-popup-inspect-keymap>\ `\\[js-eval-popup-maybe-find-file]'.
A function will be called without args inside quit function.

If SETUP-ARGS contains syntax table, it will be used in the inspect buffer."
  (let ((buffer (get-buffer-create js-eval-popup-inspect-buffer-name))
        (keymaps (seq-filter #'keymapp setup-args))
        (stx-table (seq-find #'syntax-table-p setup-args))
        (mode-fn (seq-find #'functionp setup-args)))
    (setq js-eval-popup-content (if (or
                                       mode-fn
                                       (not (stringp content)))
                                      (apply #'js-eval-popup-fontify
                                             (list content mode-fn))
                                    content))
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
                (progn  (save-excursion
                          (insert js-eval-popup-content))
                        (add-hook 'kill-buffer-hook
                                  'js-eval-popup-minibuffer-select-window
                                  nil t)
                        (when mode-fn
                          (funcall mode-fn))
                        (use-local-map
                         (let ((map (copy-keymap
                                     js-eval-popup-inspect-keymap)))
                           (add-hook
                            'read-only-mode-hook
                            (lambda ()
                              (if buffer-read-only
                                  (define-key map (kbd "q")
                                              'kill-this-buffer)
                                (define-key map (kbd "q")
                                            'self-insert-command)))
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
	"Open or restore popup in a buffer `js-eval-popup-inspect-buffer-name'."
  (interactive)
  (apply #'js-eval-popup-inspect
         (or js-eval-popup-content "")
         js-eval-popup-meta))

(defun js-eval-overlay-show (&optional str pos-bounds keymap)
  "Display STR in an overlay at POS-BOUNDS with KEYMAP."
  (js-eval-overlay-cleanup)
  (when-let ((value (and str
                         (concat
                          "\n"
                          (if (text-properties-at 0 str)
                              str
                            (propertize str 'face 'org-code))
                          "\n")))
             (start (if (consp pos-bounds) (car pos-bounds)
                      (or pos-bounds (point))))
             (end (if (consp pos-bounds)
                      (cdr pos-bounds)
                    (or pos-bounds (save-excursion
                                     (re-search-forward
                                      "\n" nil t 1)
                                     (point))))))
    (add-hook 'before-change-functions 'js-eval-overlay-cleanup nil t)
    (when (> end (point-max))
      (setq end (point-max)))
    (setq js-eval-overlay-at (make-overlay start end (current-buffer) nil t))
    (overlay-put js-eval-overlay-at 'face 'org-code)
    (overlay-put js-eval-overlay-at 'keymap
                 (if keymap
                     (make-composed-keymap js-eval-overlay-keymap keymap)
                   js-eval-overlay-keymap))
    (overlay-put js-eval-overlay-at 'priority 9999)
    (overlay-put js-eval-overlay-at 'help-echo "Quit to exit")
    (overlay-put js-eval-overlay-at 'after-string value)))

(defun js-eval-relative-p (path)
  "Return t if PATH is relative, nil otherwise."
  (js-eval-string-match-p "^\\(\\(\\.\\)?[\\.]\\)\\(/\\|$\\)"
                             path))

(defun js-eval-extract-node-builins ()
	"Extract node builins."
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
  "Check if MODULE is dependency of PROJECT-ROOT.
Dependencies are recognized by `package.json' or `node_modules' of
PROJECT-ROOT."
  (or (member module (js-eval-node-modules-candidates project-root))
      (let ((node-dir (js-eval-find-node-modules project-root)))
        (and node-dir
             (file-exists-p
              (expand-file-name (car (split-string module "/"))
                                node-dir))))))

(defun js-eval-path-to-relative (path &optional dir)
  "Transform PATH into relative to the DIR (default: ‘default-directory’).
If PATH is a relative file, it will be returned without changes."
  (if (js-eval-relative-p path)
      path
    (let ((relative-path (file-relative-name path (or dir default-directory))))
      (unless (js-eval-relative-p relative-path)
        (setq relative-path (concat "./" relative-path)))
      relative-path)))

(defun js-eval-alias-path-to-real (path)
  "Convert aliased PATH to absolute file name."
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
  "Resolve PATH to absolute filename.
Optional argument DIR is used as default directory."
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
  "Return import bounds if POSITION inside import statement."
  (let ((pos (or position (point)))
        (imports (js-eval-get-es-imports-bounds))
        (result))
    (setq result (seq-find (lambda (it) (and (>= pos (car it))
                                             (<= pos (cdr it))))
                           imports))
    result))

(defun js-eval-extract-import-path-bounds (&optional import-bounds)
  "Return path of import statement specified in IMPORT-BOUNDS."
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
  "Return list of with imported imported paths in current buffer."
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
  "Compile BODY and return list with status code and result."
  (js-eval-ensure-babel-project)
  (let ((temp-file (concat (temporary-file-directory)
                           (make-temp-name "script") ".tsx"))
        (temp-compiled-file (concat (temporary-file-directory)
                                    (make-temp-name "script") ".js"))
        (result)
        (command))
    (with-temp-file temp-file
      (insert body)
      (write-region nil nil temp-file)
      (setq command
            (js-eval-make-babel-command
             temp-file temp-compiled-file))
      (message command)
      (with-output-to-temp-buffer (current-buffer)
        (shell-command command)))
    (setq result (with-temp-buffer
                   (insert-file-contents temp-compiled-file nil)
                   (buffer-string)))
    (list 0 result)))

(defun js-eval-join-when-exists (&rest args)
  "Return joined ARGS when exists."
  (let ((joined-path (apply #'js-eval-join-file args)))
    (when (file-exists-p joined-path)
      joined-path)))

(defun js-eval-strip-props (item)
  "If ITEM is string, return it without text properties.

 If ITEM is symbol, return it is `symbol-name.'
 Otherwise return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun js-eval-group-by (prop items)
  "Group ITEMS by PROP into alist."
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
  "Return right-to-left composition from FUNCTIONS."
  (lambda (&rest args)
    (car (seq-reduce (lambda (xs fn) (list (apply fn xs)))
                     (reverse functions) args))))

(defun js-eval-flip (func &optional arg-b)
  "Swap the order of first two arguments for FUNC.
Second argument ARG-B is optional and can be passed later."
  (if arg-b
      (apply-partially (lambda (a b &rest others) (apply func (append
                                                          `(,b ,a) others)))
                       arg-b)
    (lambda (a b &rest others) (apply func (append `(,b ,a) others)))))

(defun js-eval-sort-by-prop (prop items)
	"Sort ITEMS by PROP."
  (seq-sort-by (lambda (it)
                 (or (js-eval-get-prop it prop) -1))
               #'< (if (vectorp items)
                       (append items nil)
                     items)))

(defun js-eval--node-path (&optional dir)
	"Return NODE-PATH string for DIR."
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
	"Return RESULT when FILE is nil."
  (unless file result))

(defun js-eval--shell-command-to-string (environ command)
	"Execute COMMAND in environment ENVIRON."
  (with-temp-buffer
    (let ((process-environment
           (append
            '("NODE_NO_WARNINGS=1") environ process-environment)))
      (apply #'call-process (car command) nil t nil (cdr command))
      (buffer-string))))

(defvar js-eval-project-files nil)
(defvar js-eval-current-buffer nil)
(defun js-eval-init-project ()
  "Initialize project by setting buffer, finding root and aliases."
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
  "Return the parent directory to PATH without slash."
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

(defconst js-eval-var-keywords '("const" "var" "let"))

(defconst js-eval-expression-keywords
  (append js-eval-var-keywords
            '("interface" "type" "class" "enum")))

(defconst js-eval-delcaration-keywords
  (append '("function" "function*") js-eval-expression-keywords))

(defvar js-eval-angles-syntax-table
  (let ((table (copy-syntax-table
                js-eval-mode-syntax-table)))
    (modify-syntax-entry ?< "(^" table)
    (modify-syntax-entry ?> ")$" table)
    table))

(defun js-eval-syntax-propertize-regexp (end)
  "Propertize regexp syntax and goto END position."
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

(defun js-eval-backward-angles ()
  "Backward angles at point and return string."
  (when (looking-back ">" 0)
    (let ((a)
          (b (point))
          (res))
      (with-syntax-table js-eval-angles-syntax-table
        (ignore-errors (forward-sexp -1))
        (setq a (point))
        (when (= b a)
          (forward-char -1))
        (setq res (buffer-substring-no-properties a b))
        res))))

(defun js-eval-get-prev-token-ignore-whitespace ()
  "Return string with previous token ignoring comments and whitespace."
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
  "Move backward out of one level of parentheses.
With ARG, do this that many times."
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((pos (point)))
    (when-let ((end (ignore-errors
                      (backward-up-list arg)
                      (point))))
      (unless (= end pos)
        end))))

(defun js-eval-backward-list ()
  "If text before point is close parenthesis move backward one balanced expression."
  (when (looking-back "[)]" 0)
    (forward-sexp -1)
    (point)))

(defun js-eval-find-by-node-pos (pos node)
  "Find by node pos.
at POS
NODE is ."
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
	"Parse context.
Optional argument DEEP is whether to parse function declarations recoursively."
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
  "Parse context of current buffer between START and END.
Optional argument DEEP is whether to parse function declarations recoursively."
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

(defun js-eval-find-parent-node ()
  "Return closest parent node in top scope."
  (let ((top-scope (save-excursion
                     (js-eval-parse-scope (point-min) (point-max)))))
    (seq-find
     (apply-partially #'js-eval-find-by-node-pos (point))
     top-scope)))

(defun js-eval-parse-scope-inner (&optional start)
  "Jump to START and parse scope at point."
  (when start (goto-char start))
  (when-let ((bounds (js-eval-forward-scope)))
    (save-excursion (js-eval-parse-scope (1+ (car bounds)) (cdr bounds)))))

(defun js-eval-parse-current-scope ()
  "Parse scopes upwards starting at current position."
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
                             (skip-chars-backward js-eval-regexp-name-set)
                             ;; (message "1")
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
                             (message "2")
                             (goto-char prev-token-pos)
                             (save-excursion (js-eval-parse-arguments)))
                            ((and (equal prev-token ")")
                                  looking-at-brackets)
                             (message "3")
                             (let* ((body (save-excursion
                                            (js-eval-parse-scope-inner)))
                                    (args (progn (goto-char (1+ prev-token-pos))
                                                 (forward-sexp -1)
                                                 (js-eval-parse-arguments))))
                               (append args body)))
                            ((and looking-at-brackets
                                  (equal prev-token "="))
                             (message "4")
                             (save-excursion (js-eval-parse-object))))))
            (if (listp args)
                (setq items (append items args))
              (push args items))))))
    items))

(defun js-eval-parse-from-string (&optional content fn &rest args)
  "Parse CONTENT with a function FN, called with ARGS."
  (js-eval-with-temp-buffer
   (if-let ((start (js-eval-get-prop content :start)))
       (progn (insert (make-string (1- start) ?\s))
              (save-excursion (insert content)))
     (insert content)
     (goto-char (point-min)))
   (if fn
       (apply fn args)
     (js-eval-parse-context t))))

(defun js-eval-get-deep-prop (item &rest path)
  "Retrieve the value at a given PATH in propertized ITEM."
  (let ((value))
    (while (and item path (setq item (js-eval-get-prop item (pop path))))
      (setq value item))
    (if path
        nil
      value)))

(defun js-eval-get-token-at-point ()
  "Return and forward token at point."
  (or
   (js-eval-parse-regexp)
   (js-eval-get-operator-at-point)
   (js-eval-get-obj-key)
   (when-let ((c (js-eval-get-next-char)))
     (forward-char 1)
     c)))

(defun js-eval-parse-object-recoursive (&optional with-props)
  "Recoursively parse object at point.
If optional argument WITH-PROPS is non-nil, propertize keys and values."
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
  "Parse array and current position.
If optional argument WITH-PROPS is non-nil, propertize items."
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
  "Parse object or array at point.
If optional argument WITH-PROPS is non-nil, propertize items."
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
	"Forward lists at point."
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
	"Parse object from string CONTENT.
If optional argument WITH-PROPS is non-nil, propertize items."
  (js-eval-with-temp-buffer
   (insert content)
   (goto-char (point-min))
   (js-eval-parse-object with-props)))

(defun js-eval-add-parent-key (parent-key &optional key)
  "Concat PARENT-KEY and KEY in javascript style."
  (cond ((or (null key)
             (string-empty-p parent-key))
         key)
        ((string= "[" (substring-no-properties key 0 1))
         (concat parent-key key))
        ((string-match-p "^['\"]" key)
         (concat parent-key "[" key "]"))
        (t (concat parent-key "." key))))

(defun js-eval-sort-object-props-by-pos (items)
  "Sort ITEMS by :start or :parent-start."
  (let ((max (point-max)))
    (seq-sort-by (lambda (it)
                   (or (js-eval-get-prop it :start)
                       (js-eval-get-prop it :parent-start)
                       max))
                 #'< items)))

(defun js-eval-transpile-alist (item &optional indent)
  "Convert ITEM alist to javascript object.
INDENT is used recoursively for nested objects."
  (unless indent (setq indent 0))
  (let ((margin (if indent (make-string indent ?\s)
                  (make-string indent ?\s))))
    (concat margin (cond ((and item (consp item)
                               (stringp (car item)))
                          (let ((key (js-eval-strip-text-props (car item)))
                                (value (and item (listp item) (cdr item))))
                            (when (and (string-match-p "[-/]" key)
                                       (not (string-match-p "['\"]" key)))
                              (setq key (concat "'" key "'")))
                            (format (concat "%s: %s") key
                                    (string-trim
                                     (js-eval-transpile-alist
                                      value indent)))))
                         ((vectorp item)
                          (format "[ %s ]"
                                  (mapconcat #'js-eval-transpile-alist
                                             (append item nil) ", ")))
                         ((listp item)
                          (format (concat "{\n" margin
                                          "%s" "\n" margin "}")
                                  (mapconcat (lambda (it)
                                               (concat
                                                margin
                                                (js-eval-transpile-alist
                                                 it (1+ indent))))
                                             item (concat ",\n" margin))))
                         ((numberp item)
                          (format "%s" item))
                         ((eq item t)
                          "true")
                         ((stringp item)
                          (prin1-to-string item))))))

(defun js-eval-maybe-strip-quotes (item)
  "Strip quotes from ITEM if quoted or return unchanged ITEM.
If ITEM contains chars `-' or `/' also return unchanged ITEM."
  (if (and
       (> (length item) 1)
       (string-match-p "^[\"']" item)
       (string-match-p "[\"']$" item)
       (not (string-match-p "[-/]" item)))
      (substring item 1 (1- (length item)))
    item))

(defun js-eval-get-object-keys (object &optional parent-key)
  "Flatten OBJECT and extract all keys prefixed with PARENT-KEY."
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
  "Parse token at point."
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
  "Parse OBJECT pattern at point.
PARENT-KEY is used recoursively for nested keys."
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

(defun js-eval-vector-to-obj-indexed (items)
  "Convert vector ITEMS to alist of cons (index-string . value).
If ITEMS is not vector return unchanged ITEMS."
  (if (vectorp items)
      (seq-map-indexed (lambda (it i) (cons (format "%s" i) it))
                       (append items nil))
    items))

(defun js-eval-strip-object-props (item)
  "Strip text properties at ITEM."
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
  "Return bounds of thing at point that match CHARS.

CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
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
  "Return and forward token and point without text properties."
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
  "Get tokens at point starting from current position."
  (let ((node)
        (nodes))
    (while (setq node (js-eval-tokenize))
      (let ((next (js-eval-tokenize)))
        (push node nodes)
        (when next
          (push next nodes))))
    (reverse nodes)))

(defun js-eval-comint-maybe-format-result (str)
  "Try to format STR with prettier js.
If prettier failed, return STR."
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

(defvar js-eval-result-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'js-eval-visit-compiled)
    (define-key map (kbd "C-c C-o") 'js-eval-visit-compiled)))

(defun js-eval-show-result (result &optional compiled-name)
	"Show RESULT in overlay or popup.
With COMPILED-NAME allow to jump to it."
  (require 'js)
  (setq result (when result (js-eval-comint-maybe-format-result result)))
  (cond
   ((and (null result)
         (null compiled-name))
    (message "No result and compiled file"))
   ((and (null result)
         compiled-name)
    (if (file-exists-p compiled-name)
        (js-eval-visit-compiled compiled-name)
      (message "%s doesn't exists" compiled-name)))
   ((and result
         (or
          (> (length result) 170)
          (> (length (split-string result "\n" t)) 1)))
    (js-eval-popup result 'js-mode js-eval-result-keymap))
   (t
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-o")
                  (lambda () (interactive)
                    (js-eval-popup-inspect result
                                           js-eval-result-keymap
                                           'js-mode)))
      (setq map (make-composed-keymap
                 (list js-eval-result-keymap map)))
      (js-eval-overlay-show result (point) map))))
  result)

(defun js-eval-transform-import-path (path dir &optional node-modules-path)
	"Transform import PATH in DIR to absolute or relative.
Convert to absolute if NODE-MODULES-PATH is present and PATH is dependency."
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
	"Transform PATH in DIR to absolute.
WIth NODE-MODULES-PATH expands also dependencies."
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
  "Return string of content in FILE with replaced alias imports to absolute.
If NODE-MODULES-PATH passed, also expands dependencies to absolute filenames.
If CODE is non-nil, insert it at the beginning."
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

(defun js-eval-get-js-sexp ()
	"Get sexp at point to eval."
  (let* ((b (save-excursion
              (backward-sexp)
              (move-beginning-of-line nil)
              (point)))
         (e (point)))
    (buffer-substring-no-properties b (if (> (point-max) e)
                                          (+ e 1)
                                        e))))

(defun js-eval-get-project-current-name (&optional root)
	"Get name of ROOT or `js-eval-current-project-root'."
  (and (or root js-eval-current-project-root)
       (car (reverse
             (split-string
              (or root js-eval-current-project-root) "/" t)))))

(defun js-eval-get-temp-project-dir (&optional root)
	"Return tmep directory for project ROOT."
  (file-name-as-directory
   (expand-file-name
    (js-eval-get-project-current-name root)
    (temporary-file-directory))))

(defun js-eval-get-temp-file-name (filename)
	"Return temp name for FILENAME."
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

(defvar js-eval-files nil)

(defvar js-eval-node-path nil)

(defvar js-eval-node-modules-dir)

(defun js-eval-collect-imported-files (&optional init-file code)
  "Extract imports starting at INIT-FILE and return list of absolute filenames.
Files from node_modules is not included.
If optional CODE is non nil, use it as content of INIT-FILE."
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

(defvar js-eval-files-modified-time-cache (make-hash-table :test 'equal))

(defun js-eval-should-use-cache-p (source-file-name target-file-name)
	"Check modification time of SOURCE-FILE-NAME.
Also check if TARGET-FILE-NAME exists."
  (when-let ((cached (when (file-exists-p target-file-name)
                       (gethash source-file-name
                                js-eval-files-modified-time-cache)))
             (modified-time (file-attribute-modification-time
                             (file-attributes source-file-name 'string))))
    (equal cached modified-time))
  nil)

(defun js-eval--compile-file (source-filename
                              target-filename
                              &optional
                              node-modules-path)
  "Compile SOURCE-FILENAME to TARGET-FILENAME.
Alias imports from SOURCE-FILENAME transform to relatives in TARGET-FILENAME..
IF NODE-MODULES-PATH passed, also expands dependencies to absolute filenames."
  (let ((parent-dir)
        (content))
    (setq content
          (cadr (js-eval-babel-compile
                 (js-eval-transform-aliases-imports-to-abs
                  source-filename node-modules-path))))
    (setq parent-dir (file-name-directory target-filename))
    (unless (file-exists-p parent-dir)
      (make-directory parent-dir t))
    (write-region content nil target-filename)))

(defun js-eval-compile-files (files)
  "Compile alist of FILES whose car is the source filename and cdr - the target."
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
	"Convert IMPORTED-ITEM to fullname divided with DELIMITER."
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

(defun js-eval-regenerate-imports (imports
                                   &optional dir
                                   node-modules-dir)
	"Regenerate IMPORTS using DIR as it's default DIR.
NODE-MODULES-DIR is used to resolve dependencies."
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
                       (list default named-imports)) ", "))
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
	"Return tokens in string CODE."
  (with-temp-buffer
    (save-excursion (insert code))
    (js-eval-get-tokens)))

(defun js-eval-copy-item (item)
	"Substring propertized with :start and :end ITEM without propertis."
  (when-let ((start (js-eval-get-prop item :start))
             (end (js-eval-get-prop item :end)))
    (buffer-substring-no-properties start end)))

(defun js-eval-get-region ()
  "Return current active region as string or nil."
  (when
      (and (region-active-p)
           (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end)))))

(defun js-eval-extract-code ()
	"Extract code for evaluating."
  (when-let ((init-code (or (js-eval-get-region)
                            (js-eval-get-js-sexp))))
    (let* ((ast (js-eval-parse-context-from-current-buffer))
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
	"Eval file COMPILED-NAME."
  (when-let ((tmp (and (file-exists-p compiled-name)
                       (replace-regexp-in-string
                        "\\.[a-z]+$" "-temp.js"
                        compiled-name))))
    (with-temp-file tmp
      (insert js-eval-util-string)
      (insert (format "__js_eval__('%s', '', '%s')" compiled-name "")))
    (setq js-eval-node-path (js-eval--node-path default-directory))
    (js-eval--output
     (js-eval--shell-command-to-string
      (list (format "NODE_PATH=%s" js-eval-node-path))
      (list "node" tmp))
     nil)))

(defun js-eval-files-to-compile-alist (files)
  "Map FILES to alist whose car is the original file and cdr is target."
  (mapcar (lambda (it) (cons it (js-eval-get-temp-file-name it))) files))

(defvar js-eval-use-window nil)

(defun js-eval-eval-0 (arg)
	"Eval code ARG."
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
            (write-region compiled-code nil target-filename)
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
                       ,(when
                            js-eval-current-project-root
                          (expand-file-name
                           "node_modules/" js-eval-current-project-root)))
                      ("rootDir" . ,js-eval-current-project-root)))
                   'js-eval-show-result))
              (js-eval-eval-compiled target-filename))))))))

;;;###autoload
(defun js-eval-server-clear ()
  "Clear the Javascript REPL."
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
	"Comile SOURCE-FILENAME to TARGET-FILENAME.
NODE-MODULES-PATH is full path to node_modules."
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
	"Find compiled version of FILE in temp directory."
  (interactive)
  (when-let ((compiled-file
              (or file
                  (and buffer-file-name
                       (js-eval-get-temp-file-name buffer-file-name)))))
    (when (file-exists-p compiled-file)
      (let* ((actions '((?o "other" find-file-other-window)
                        (?c "current" find-file)))
             (answer (read-multiple-choice "promp?\s"
                                           actions))
             (action (caddr answer)))
        (funcall action compiled-file)))))

;;;###autoload
(defun js-eval-eval (&optional arg)
	"Eval ARG and show result."
  (interactive "P")
  (let ((result (js-eval-eval-0 arg)))
    (when (stringp result)
      (js-eval-show-result result
                           (js-eval-get-temp-file-name
                            buffer-file-name)))))

;;;###autoload
(defun js-eval-compile-region-or-buffer ()
	"Compile active region or whole buffer and show it in popup."
  (interactive)
  (js-eval-popup
   (cadr
    (js-eval-babel-compile (or (js-eval-get-region)
                               (read-string "Compile:\s"))))
   'js-mode))

;;;###autoload
(defun js-eval-cleanup ()
	"Remove compiled files in temp directory."
  (interactive)
  (mapc (lambda (it) (when-let ((buff (get-file-buffer it)))
                  (kill-buffer buff))
          (delete-file it))
        (mapcar #'cdr js-eval-files)))

;;;###autoload
(defun js-eval-toggle-use-window ()
	"Toggle use window."
  (interactive)
  (unless js-eval-use-window
    (unless (js-eval-server-get-process)
      (js-eval-server-run)))
  (setq js-eval-use-window (not js-eval-use-window)))

;;;###autoload
(defun js-eval-current-file-with-node ()
	"Eval current file with node."
  (interactive)
  (let ((infile buffer-file-name))
    (let ((result (with-temp-buffer
                    (let ((status (call-process "node" infile t nil)))
                      (let ((r (string-trim (buffer-string))))
                        (list (eq 0 status) r))))))
      (let ((msg (if (car result)
                     (cadr result)
                   (propertize (cadr result) 'face 'error))))
        (js-eval-popup msg
                         (when (car result) 'js-mode))))))

;;;###autoload
(defun js-eval-ensure-babel-project ()
	"Interactivelly create directory with babel and plugins."
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

(provide 'js-eval)
;;; js-eval.el ends here