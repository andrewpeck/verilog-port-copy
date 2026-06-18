;;; verilog-port-copy.el --- Functions for working with verilog files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2026 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/verilog-port-copy
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (expand-region "0.11"))
;; Keywords: tools vhdl verilog

;; This file is not part of GNU Emacs.
;;
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; `vhdl-mode' provides a fanstastic feature of being able to copy a port at
;; point, and paste an instantiation template.
;;
;; This provides some slang-based commands that allow doing the same
;; thing from Verilog.
;;
;; It uses the existing data structure of `vhdl-mode' so this provides
;;
;;    verilog --> verilog
;;    verilog --> vhdl
;;    vhdl    --> verilog
;;
;; The data structure is defined by:
;;
;; (ent-name
;;  ((generic-names) generic-type generic-init generic-comment group-comment)
;;  ((port-names) port-object port-direct port-type port-comment group-comment)
;;  (lib-name pack-key))

;;; Code:

(require 'vhdl-mode)
(require 'cl-lib)
(require 'expand-region)
(require 'subr-x)

;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------

(defun verilog-port-copy--align-paren (start end)
  "Align columns by ampersand between START and END."
  (align-regexp start end "\\(\\s-*\\)(" 1 1 nil))

(defun verilog-port-copy--align-comment (start end)
  "Align columns by trailing comment between START and END."
  (align-regexp start end "\\(\\s-*\\)\/\/" 1 1 nil))

(defun verilog-port-copy--strip-trailing-whitespace (start end)
  "Strip trailing whitespace in region between START and END."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward " +)" nil t)
      (if (nth 4 (syntax-ppss (match-beginning 0)))
          (end-of-line)
        (replace-match ")" t t)))))

(defun verilog-port-copy--convert-comments (start end)
  "Convert lines starting with // to /* */ style between START and END."
  (save-excursion
    (let ((end-marker (copy-marker end)))
      (goto-char start)
      (while (re-search-forward "^\\(\\s-*\\)//\\(.*\\)$" end-marker t)
        (replace-match "\\1/* \\2 */" t)))))

;;;###autoload
(defun verilog-align-ports (&optional minimal)
  "Align verilog ports at point.

With prefix argument MINIMAL this will skip some more opinionated conversions.

1. Skip stripping trailing spaces before closing parentheses, e.g.

`(clk                 )'

does not convert to

`(clk)`.

2. Skip conversion of whole line comments, e.g.

// some comment

does not convert to

/* some comment */"
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (er/mark-inside-pairs)
    (let* ((raw-start (region-beginning))
           (start (save-excursion
                    (goto-char raw-start)
                    (if (looking-at "//")
                        (progn (backward-up-list) (1+ (point)))
                      raw-start)))
           (end   (copy-marker (save-excursion
                                 (goto-char (region-end))
                                 (end-of-line)
                                 (point)))))
      (deactivate-mark)
      (dolist (f `(verilog-port-copy--align-paren
                   ,(unless minimal
                      'verilog-port-copy--strip-trailing-whitespace)
                   ,(unless minimal
                      'verilog-port-copy--convert-comments)
                   verilog-port-copy--align-comment))
        (when f
          (funcall f start end))))))

;;------------------------------------------------------------------------------
;; Slang CST parsing
;;------------------------------------------------------------------------------

(defun verilog-port-copy--slang-parse (source)
  "Run slang on SOURCE string and return parsed CST as alist.
Returns nil if slang produces no usable output."
  (let* ((tmpv (make-temp-file "verilog-port-copy-" nil ".v"))
         (tmpj (make-temp-file "verilog-port-copy-cst-" nil ".json")))
    (unwind-protect
        (progn
          (write-region source nil tmpv nil 'silent)
          (call-process "slang" nil nil nil
                        "--cst-json" tmpj "--cst-json-mode" "no-trivia"
                        tmpv)
          (with-temp-buffer
            (insert-file-contents tmpj)
            (when (> (buffer-size) 0)
              (json-parse-buffer :object-type 'alist :array-type 'list))))
      (ignore-errors (delete-file tmpv))
      (ignore-errors (delete-file tmpj)))))

(defun verilog-port-copy--cst-nav (obj &rest keys)
  "Navigate nested alist OBJ via symbol KEYS, returning nil on missing paths."
  (cl-reduce (lambda (acc key)
               (when (consp acc) (alist-get key acc)))
             keys :initial-value obj))

(defun verilog-port-copy--cst-has-dims (datatype)
  "Return non-nil if DATATYPE alist has a non-empty dimensions list."
  (not (null (alist-get 'dimensions datatype))))

(defun verilog-port-copy--cst-port-type (header)
  "Determine vhdl-port-list type string from port HEADER alist."
  (let* ((dt   (alist-get 'dataType header))
         (kind (alist-get 'kind dt)))
    (if (equal kind "NamedType")
        (verilog-port-copy--cst-nav dt 'name 'identifier 'text)
      (if (verilog-port-copy--cst-has-dims dt)
          "std_logic_vector"
        "std_logic"))))

(defun verilog-port-copy--cst-dir (header)
  "Return direction text string from port HEADER alist, or nil if absent."
  (verilog-port-copy--cst-nav header 'direction 'text))

(defun verilog-port-copy--cst-find-module (cst)
  "Return the first module-declaration alist from parsed CST, or nil."
  (when-let* ((trees   (alist-get 'syntaxTrees cst))
              (members (verilog-port-copy--cst-nav (car trees) 'root 'members)))
    (cl-find "ModuleDeclaration" members
             :key (lambda (m) (alist-get 'kind m))
             :test #'equal)))

;;------------------------------------------------------------------------------
;; Parameters
;;------------------------------------------------------------------------------

(cl-defun verilog-port-copy--format-generic
    (name &key generic-type (generic-init "")
          generic-comment group-comment)
  "Format verilog generic NAME into a vhdl-port-list generic entry.

Return ((generic-names) GENERIC-TYPE GENERIC-INIT GENERIC-COMMENT
GROUP-COMMENT)."
  (list (list name)
        generic-type
        generic-init
        generic-comment
        (or group-comment "\n")))

(defun verilog-port-copy--cst-param-declarators->generics (declarators)
  "Convert DECLARATORS (a list of declarator alists) to generic entries."
  (cl-mapcan
   (lambda (d)
     (when (equal (alist-get 'kind d) "Declarator")
       (list (verilog-port-copy--format-generic
              (verilog-port-copy--cst-nav d 'name 'text)))))
   declarators))

(defun verilog-port-copy--cst-extract-generics (header mod-members)
  "Extract parameter list from HEADER (ANSI #(...)) and MOD-MEMBERS (body)."
  (let ((ansi
         (when-let* ((ppl   (alist-get 'parameters header))
                     (decls (alist-get 'declarations ppl)))
           (cl-mapcan
            (lambda (decl)
              (when (and (equal (alist-get 'kind decl) "ParameterDeclaration")
                         (not (equal (verilog-port-copy--cst-nav decl 'keyword 'text)
                                     "localparam")))
                (verilog-port-copy--cst-param-declarators->generics
                 (alist-get 'declarators decl))))
            decls)))
        (body
         (cl-mapcan
          (lambda (m)
            (when (and (equal (alist-get 'kind m) "ParameterDeclarationStatement")
                       (not (equal (verilog-port-copy--cst-nav m 'parameter 'keyword 'text)
                                   "localparam")))
              (verilog-port-copy--cst-param-declarators->generics
               (verilog-port-copy--cst-nav m 'parameter 'declarators))))
          mod-members)))
    (append ansi body)))

;;------------------------------------------------------------------------------
;; Ports
;;------------------------------------------------------------------------------

(cl-defun verilog-port-copy--format-port (name &key port-object port-direct
                                               port-type port-comment group-comment)
  "Format a port following the structure specified in vhdl-mode.el.

Structure:
   ((port-names) port-object port-direct port-type port-comment group-comment)

    e.g ((\"qn_m2\") nil \"in\" \"\" nil \"\\n\")\"

NAME is the name of the verilog module.

PORT-OBJECT is ???

PORT-DIRECT is a direction; in, out, or inout.

PORT-TYPE is a string of something like std_logic_vector (9 downto 0)

PORT-COMMENT is an optional comment string.

GROUP-COMMENT is ???"

  (list (list name)
        port-object
        port-direct
        port-type
        port-comment
        (or group-comment "")))

(defun verilog-port-copy--cst-extract-ports (header mod-members)
  "Extract port list from HEADER port list and MOD-MEMBERS body declarations."
  (let* ((plist (alist-get 'ports header))
         (style (alist-get 'kind plist)))
    (cond
     ((equal style "AnsiPortList")
      (verilog-port-copy--cst-ports-ansi plist))
     ((equal style "NonAnsiPortList")
      (verilog-port-copy--cst-ports-nonansi plist mod-members))
     (t nil))))

(defun verilog-port-copy--cst-ports-ansi (plist)
  "Extract ports from ANSI port list PLIST, with direction inheritance."
  (let ((last-dir nil))
    (cl-mapcan
     (lambda (p)
       (when (equal (alist-get 'kind p) "ImplicitAnsiPort")
         (let* ((hdr  (alist-get 'header p))
                (dir  (or (verilog-port-copy--cst-dir hdr) last-dir))
                (name (verilog-port-copy--cst-nav p 'declarator 'name 'text))
                (type (verilog-port-copy--cst-port-type hdr)))
           (setq last-dir dir)
           (list (verilog-port-copy--format-port name
                                                 :port-direct dir
                                                 :port-type type)))))
     (alist-get 'ports plist))))

(defun verilog-port-copy--cst-ports-nonansi (plist mod-members)
  "Extract ports from non-ANSI PLIST (order) and MOD-MEMBERS (direction+type)."
  (let ((decl-map (make-hash-table :test 'equal)))
    (dolist (m mod-members)
      (when (equal (alist-get 'kind m) "PortDeclaration")
        (let* ((hdr  (alist-get 'header m))
               (dir  (verilog-port-copy--cst-dir hdr))
               (type (verilog-port-copy--cst-port-type hdr)))
          (dolist (d (alist-get 'declarators m))
            (when (equal (alist-get 'kind d) "Declarator")
              (puthash (verilog-port-copy--cst-nav d 'name 'text)
                       (cons dir type) decl-map))))))
    (cl-mapcan
     (lambda (p)
       (when (equal (alist-get 'kind p) "ImplicitNonAnsiPort")
         (let* ((pname (verilog-port-copy--cst-nav p 'expr 'name 'text))
                (info  (gethash pname decl-map)))
           (when info
             (list (verilog-port-copy--format-port pname
                                                   :port-direct (car info)
                                                   :port-type (cdr info)))))))
     (alist-get 'ports plist))))

;;-----------------------------------------------------------------------------
;; Entrypoints
;;-----------------------------------------------------------------------------

(defun verilog-port-copy--module-source-at-point ()
  "Return the full source text of the module at point, module to endmodule."
  (save-excursion
    (let ((start (progn (re-search-backward "\\bmodule\\b" nil t) (point))))
      (re-search-forward "\\bendmodule\\b" nil t)
      (buffer-substring-no-properties start (point)))))

;;;###autoload
(defun verilog-port-copy ()
  "Copy the verilog module at point and put its definition into `vhdl-port-list`."
  (interactive)
  (let* ((source  (verilog-port-copy--module-source-at-point))
         (cst     (verilog-port-copy--slang-parse source))
         (mod     (verilog-port-copy--cst-find-module cst)))
    (unless mod
      (user-error "No Verilog module found at point"))
    (let* ((header      (alist-get 'header mod))
           (mod-members (alist-get 'members mod))
           (name        (verilog-port-copy--cst-nav header 'name 'text))
           (generics    (verilog-port-copy--cst-extract-generics header mod-members))
           (ports       (verilog-port-copy--cst-extract-ports header mod-members)))
      (unless name
        (user-error "Slang parse error: unable to parse module name"))
      (setq vhdl-port-list (list name generics ports nil)
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil)
      (message "Verilog module `%s' copied" name))))

;;;###autoload
(defun verilog-port-copy-paste-instance ()
  "Paste entity as an Verilog instantiation."
  (interactive)

  (unless vhdl-port-list (user-error "ERROR:  No port read"))

  (let ((module-name (car vhdl-port-list))
        (generics (cadr vhdl-port-list))
        (ports (caddr vhdl-port-list)))

    (insert module-name)

    ;; insert generics
    (when generics
      (insert " #(\n")

      (dolist (generic generics)
        (let ((gname (caar generic)))
          (insert (format "  .%s(%s),\n" gname gname))))
      ;; remove the last comma and newline
      (delete-char -2)

      (insert ")"))

    ;; instance name
    (insert (format "\nu_%s (\n" module-name))

    ;; insert ports
    (when ports
      (dolist (port ports)
        (let ((gname (caar port)))
          (insert (format "  .%s(%s),\n" gname gname))))
      ;; remove the last comma and newline
      (delete-char -2))

    ;; close
    (insert ");")

    (beginning-of-line)
    (save-excursion (verilog-align-ports))
    (end-of-line)))

(provide 'verilog-port-copy)
;;; verilog-port-copy.el ends here
;; LocalWords:  whitespace verilog downto inout el qn func endmodule declarator alists
