;;; verilog-port-copy.el --- Functions for working with verilog files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2026 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/verilog-port-copy
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (expand-region "0.11"))
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
;; This provides some tree-sitter based commands that allow doing the same
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
(require 'treesit)
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
    (dolist (f `(verilog-port-copy--align-paren
                 ,(unless minimal
                    'verilog-port-copy--strip-trailing-whitespace)
                 ,(unless minimal
                    'verilog-port-copy--convert-comments)
                 verilog-port-copy--align-comment))
      (when f
        (beginning-of-line)
        (er/mark-inside-pairs)
        (let ((start (region-beginning))
              (end (save-excursion (goto-char (region-end)) (end-of-line) (point))))
          (deactivate-mark)
          (funcall f start end))))))

;;-----------------------------------------------------------------------------
;; Tree-sitter helpers
;;-----------------------------------------------------------------------------

(defun verilog-port-copy--ts-find (node type)
  "Return the first descendant of NODE (inclusive) with node type TYPE."
  (when node
    (treesit-search-subtree node (lambda (n) (string= (treesit-node-type n) type)))))

(defun verilog-port-copy--ts-find-all (node type)
  "Return all descendants of NODE with node type TYPE, in document order."
  (mapcar #'cdr (treesit-query-capture node (format "(%s) @cap" type))))

(defun verilog-port-copy--ts-ancestor-p (node type)
  "Return t if NODE has any ancestor with node type TYPE."
  (when-let* ((parent (treesit-node-parent node)))
    (or (string= (treesit-node-type parent) type)
        (verilog-port-copy--ts-ancestor-p parent type))))

(defun verilog-port-copy--module-at-point ()
  "Return the node spanning the module at point, or nil.
Normally this is a module_declaration node.  When the grammar cannot
parse the module (e.g. due to unsupported syntax in parameter defaults)
tree-sitter produces an ERROR node containing a module_header instead;
that ERROR node is returned as a fallback so callers can still extract
whatever information the partial tree contains."
  (treesit-parser-create 'verilog)
  (treesit-search-subtree
   (treesit-buffer-root-node 'verilog)
   (lambda (node)
     (and (<= (treesit-node-start node) (point))
          (<= (point) (treesit-node-end node))
          (or (string= (treesit-node-type node) "module_declaration")
              (and (string= (treesit-node-type node) "ERROR")
                   (verilog-port-copy--ts-find node "module_header")))))))

;;-----------------------------------------------------------------------------
;; Parameters
;;-----------------------------------------------------------------------------

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

(defun verilog-port-copy--parse-generics (module-node)
  "Return verilog parameters from MODULE-NODE.

MODULE-NODE is a module_declaration tree-sitter node."
  (reverse (append
            (verilog-port-copy--parse-generics-non-ansi module-node)
            (verilog-port-copy--parse-generics-ansi module-node))))

(defun verilog-port-copy--parse-generics-ansi (module-node)
  "Return ANSI verilog parameters from MODULE-NODE.

ANSI-style parameters in #(...)

MODULE-NODE is a module_declaration tree-sitter node."
  (let ((parameters nil))
    (when-let* ((ppl (verilog-port-copy--ts-find module-node "parameter_port_list")))
      (dolist (pa (verilog-port-copy--ts-find-all ppl "param_assignment"))
        (when-let* ((id   (verilog-port-copy--ts-find pa "parameter_identifier"))
                    (name (let ((text (treesit-node-text id)))
                            (if (string-empty-p text)
                                ;; Grammar misread bare "NAME = VALUE" as
                                ;; data_type="NAME"; recover from grandparent ppd.
                                (treesit-node-text
                                 (verilog-port-copy--ts-find
                                  (verilog-port-copy--ts-find
                                   (treesit-node-parent (treesit-node-parent pa))
                                   "data_type")
                                  "simple_identifier"))
                              text))))
          (push (verilog-port-copy--format-generic name) parameters))))
    parameters))

(defun verilog-port-copy--parse-generics-non-ansi (module-node)
  "Return verilog parameters from MODULE-NODE.

MODULE-NODE is a module_declaration tree-sitter node."
  (let ((parameters nil))
    ;; Body parameters: parameter X; or parameter X = Y;
    ;; Exclude parameter_declaration nodes that are inside parameter_port_list
    (dolist (pd (verilog-port-copy--ts-find-all module-node "parameter_declaration"))
      (unless (verilog-port-copy--ts-ancestor-p pd "parameter_port_list")
        (dolist (pa (verilog-port-copy--ts-find-all pd "param_assignment"))
          (when-let* ((id (verilog-port-copy--ts-find pa "parameter_identifier")))
            (push (verilog-port-copy--format-generic (treesit-node-text id))
                  parameters)))))
    parameters))

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

(defun verilog-port-copy--parse-ports (module-node)
  "Parse and extract a list of ports from MODULE-NODE.

MODULE-NODE is a module_declaration tree-sitter node."
  (append
   (verilog-port-copy--parse-ports-ansi module-node)
   (verilog-port-copy--parse-ports-non-ansi module-node)))

(defun verilog-port-copy--parse-ports-non-ansi (module-node)
  "Parse and extract a list of ports from MODULE-NODE.

MODULE-NODE is a module_declaration tree-sitter node."

  ;; Non-ANSI ports: input_declaration, output_declaration, inout_declaration
  ;; Merge and sort by document position to preserve declaration order
  (cl-flet ((find-all-port-type (port-type)
              ;; provide "input", get all input ports
              ;; provide "output", get all output ports
              ;; provide "inout", get all inout ports
              (mapcar (lambda (n) (cons port-type n))
                      (verilog-port-copy--ts-find-all module-node
                                                      (concat port-type "_declaration"))))
            (compare-node-start (a b)
              (< (treesit-node-start (cdr a))
                 (treesit-node-start (cdr b))))

            (format-non-ansi-port (decl)
              (let* ((dir-str   (car decl))
                     (node      (cdr decl))
                     (has-dim   (or (verilog-port-copy--ts-find node "packed_dimension")
                                    (verilog-port-copy--ts-find node "unpacked_dimension")))
                     (name-node (car (verilog-port-copy--ts-find-all node "port_identifier"))))
                (verilog-port-copy--format-port
                 (treesit-node-text name-node)
                 :port-direct dir-str
                 :port-type (if has-dim "std_logic_vector" "std_logic")))))

    (thread-last (sort (append (find-all-port-type "input")
                               (find-all-port-type "output")
                               (find-all-port-type "inout"))
                       #'compare-node-start)
                 (mapcar #'format-non-ansi-port))))

(defun verilog-port-copy--parse-ports-ansi (module-node)
  "Parse and extract a list of ports from MODULE-NODE.

ANSI-style ports in list_of_port_declarations

MODULE-NODE is a module_declaration tree-sitter node."
  (cl-flet ((get-and-format-ansi-port (port)
              (when-let* ((dir-node  (verilog-port-copy--ts-find port "port_direction"))
                          (name-node (verilog-port-copy--ts-find port "port_identifier")))
                (verilog-port-copy--format-port
                 (treesit-node-text name-node)
                 :port-direct (treesit-node-text dir-node)
                 :port-type (if (or (verilog-port-copy--ts-find port "packed_dimension")
                                    (verilog-port-copy--ts-find port "unpacked_dimension"))
                                "std_logic_vector" "std_logic")))))
    (thread-last (verilog-port-copy--ts-find-all module-node "ansi_port_declaration")
                 (mapcar #'get-and-format-ansi-port))))

;;-----------------------------------------------------------------------------
;; Entrypoints
;;-----------------------------------------------------------------------------

(defun verilog-port-copy--simplify-param-defaults (text)
  "Replace complex parameter defaults with 0 in TEXT.
Handles system function calls and cast expressions in parameter
defaults that the tree-sitter-verilog grammar cannot parse."
  (with-temp-buffer
    (insert text)
    (dolist (pattern '("=[ \t]*\\$[a-zA-Z_][a-zA-Z0-9_$]*[ \t]*("
                       "=[ \t]*[a-zA-Z_][a-zA-Z0-9_]*'[ \t]*("))
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let ((match-start (match-beginning 0))
              (depth 1)
              done)
          (while (and (not done) (not (eobp)))
            (let ((c (char-after)))
              (cond ((= c ?\() (cl-incf depth))
                    ((= c ?\)) (cl-decf depth)
                     (when (= depth 0) (setq done t))))
              (unless done (forward-char 1))))
          (when done
            (forward-char 1)
            (delete-region match-start (point))
            (insert "= 0")
            (goto-char match-start)))))
    (buffer-string)))

(defun verilog-port-copy--module-source-at-point ()
  "Return the full source text of the module at point, module to endmodule."
  (save-excursion
    (let ((start (progn (re-search-backward "\\bmodule\\b" nil t) (point))))
      (re-search-forward "\\bendmodule\\b" nil t)
      (buffer-substring-no-properties start (point)))))

(defun verilog-port-copy--extract-from-node (module-node)
  "Return (name generic-list port-list) extracted from MODULE-NODE."
  (list (or (treesit-node-text
             (verilog-port-copy--ts-find module-node "module_identifier"))
            (treesit-node-text
             (verilog-port-copy--ts-find
              (verilog-port-copy--ts-find module-node "module_header")
              "simple_identifier")))
        (verilog-port-copy--parse-generics module-node)
        (verilog-port-copy--parse-ports module-node)))

;;;###autoload
(defun verilog-port-copy ()
  "Copy the verilog module at point and put its definition into `vhdl-port-list`."
  (interactive)
  (let ((module-node (verilog-port-copy--module-at-point)))
    (unless module-node
      (user-error "No Verilog module found at point"))
    (let ((result
           (if (string= (treesit-node-type module-node) "ERROR")
               ;; Grammar failed — preprocess to remove unsupported constructs
               ;; (e.g. $clog2(...), cast expressions) then re-parse.
               (let ((clean (verilog-port-copy--simplify-param-defaults
                             (verilog-port-copy--module-source-at-point))))
                 (with-temp-buffer
                   (insert clean)
                   (treesit-parser-create 'verilog)
                   (goto-char (point-min))
                   (let ((clean-node (verilog-port-copy--module-at-point)))
                     (unless clean-node
                       (user-error "Tree-sitter parse error; unable to parse module"))
                     (verilog-port-copy--extract-from-node clean-node))))
             (verilog-port-copy--extract-from-node module-node))))
      (setq vhdl-port-list (append result (list nil))
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil)
      (message "Verilog module `%s' copied" (car result)))))

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
;; LocalWords:  whitespace verilog downto inout el qn func endmodule
