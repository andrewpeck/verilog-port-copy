;;; verilog-port-copy.el --- Functions for working with verilog files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2025 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/verilog-port-copy
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (expand-region "0.11"))
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
;; This provides some buggy extensions that allow doing the same thing from
;; Verilog and Systemverilog.
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
;; (require 'expand-region)

;; try rewriting with svinst?
;; or add a full parser?

(defvar verilog-port-copy-verbose t
  "Set to non-nil for more diagnostic output during port copy.")

;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst verilog-port-copy--identifier-re
  "[a-zA-Z_][a-zA-Z_0-9]*"
  "Regular expression for a verilog-port-copy identifier (a name).")

(defconst verilog-port-copy--module-and-port-regexp
  (concat "module\s+"
          "\\([A-z]+[A-z0-9_]*\\)\s*" ; module name
          "\s*\\(#[[:blank:]]?(.*)\\)?"           ; verilog2001 style parameters #()
          "\\(\s*(.*);\\)")
  "Regexp to extract a module name and port list from a Verilog2001 style file.")

(defconst verilog-port-copy--module-regexp
  "module\s+\\([A-z0-9_]+\\)"
  "Regexp to extract a verilog module name.")

;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------

(defun verilog-port-copy--align-paren (start end)
  "Align columns by ampersand between START and END."
  (align-regexp start end "\\(\\s-*\\)(" 1 1 nil))

(defun verilog-port-copy--align-comment (start end)
  "Align columns by trailing comment between START and END."
  (align-regexp start end "\\(\\s-*\\)\/\/" 1 1 nil))

;;;###autoload
(defun verilog-align-ports ()
  "Align verilog ports at point."
  (interactive)
  (save-excursion
    (dolist (f '(verilog-port-copy--align-paren verilog-port-copy--align-comment))
      (beginning-of-line)
      (er/mark-inside-pairs)
      (goto-char (region-beginning))
      (funcall f (region-beginning) (region-end))
      (deactivate-mark))))

;;------------------------------------------------------------------------------
;; Module Extraction
;;------------------------------------------------------------------------------

(defun verilog-port-copy--get-module-as-string ()

  "Get a verilog module at point, return it as a string.

   Comments and newlines will be removed."

  (save-excursion

    (end-of-line)
    ;; forward word so it works on module itself.
    (when (string= "module" (symbol-at-point))
      (forward-word 2))

    (let* ((start (re-search-backward verilog-port-copy--module-regexp))
           (end   (re-search-forward "endmodule")) ; FIXME: make sure this is not in a comment
           (module (buffer-substring-no-properties start end)))

      (setq module (replace-regexp-in-string "\/\/.*\n" "" module))
      (setq module (replace-regexp-in-string "\n" " " module))
      (setq module (replace-regexp-in-string "\s\s+" " " module))

      module)))

(defun verilog-port-copy--get-module-name ()
  "Get the name of the Verilog module at point in the currently opened buffer."
  (save-excursion
    (forward-line 1)
    (when  (re-search-backward verilog-port-copy--module-regexp)
      (match-string-no-properties 1))))

;;-----------------------------------------------------------------------------
;; Parameters
;;-----------------------------------------------------------------------------

(cl-defun verilog-port-copy--format-generic
    (name &key generic-type generic-init
          generic-comment group-comment)

  "Format verilog generic NAME.

    GENERIC-TYPE:
    GENERIC-INIT:
    GENERIC-COMMENT:
    GROUP-COMMENT:

    ((generic-names) generic-type generic-init generic-comment group-comment)"


  (when (not group-comment)
    (setq group-comment "\n"))

  (list (list name)
        generic-type generic-init generic-comment group-comment))


(defun verilog-port-copy--parse-generics (module)

  "Return verilog parameters from a MODULE.

A MODULE should be a string with the entire contents of the
module with comments and newlines removed."



  (with-temp-buffer

    (insert module)

    (goto-char (point-min))

    (let ((parameters nil))


      (when (re-search-forward verilog-port-copy--module-and-port-regexp nil t)

        (let ((ansi-port-str (match-string 2)))
          (when ansi-port-str
            (with-temp-buffer

              (insert ansi-port-str)
              (goto-char (point-min))

              ;; get ansi params e.g. in #()

              (goto-char (point-min))

              (while (re-search-forward
                      (concat

                       ;;
                       "[[:blank:]]?"

                       ;; get the type
                       "\\(parameter[[:blank:]]integer\\|parameter[[:blank:]]int\\|parameter\\|logic\\|string\\|real\\|int\\)?" ;; could also have logic I think, anything else?

                       ;; get the range
                       "\s*\\(\\[[^]]*\\]\\s-*\\)?"
                       ;; "\\(" verilog-range-re "\\)?" ;; range?

                       ;; get the name
                       "\\(" verilog-port-copy--identifier-re "\\)"

                       ;; has a value?
                       "\s*=?\s*"

                       ;; get the value
                       "\\([^,]+\\|)\s*(\\)?"

                       ;; close
                       "\s*,?\\(\s*)\s*;\\)?")
                      nil t)


                (let ((_ (match-string 1))    ; type
                      (_ (match-string 2))    ; range
                      (name (match-string 3)) ; name
                      (default "")) ; just ignore defaults for now.. need a real parser for this (match-string 4)

                  (when verilog-port-copy-verbose
                    (message (format "parameter name = %s" name)))

                  (push (verilog-port-copy--format-generic name :generic-init default) parameters)))))))


      ;; TODO: these regexps can be combined
      ;; get uninitialized params, e.g. "parameter MXCNT;"
      (goto-char (point-min))

      (while (re-search-forward
              (concat
               "parameter\s+"    ;;
               "\\([A-z0-9]+\\)" ;; name
               "\s*;") nil t)
        (let ((name (match-string 1))
              (val nil))
          (push (verilog-port-copy--format-generic name :generic-init val) parameters)))

      ;; get initialized params, e.g. "parameter MXCNT = 12;"
      (goto-char (point-min))

      (while (re-search-forward
              (concat
               "parameter\s+"           ;
               "\\([A-z0-9_]+\\)"       ; name
               "\s*=\s*"                ;
               "\\([A-z0-9_]+\\)?"      ; number of bits
               "\\('[bdho]\\)?"         ; radix
               "\\([0-9A-z,_]+\\)"      ; val
               "\s*;") nil t)

        ;; account for different radixes, 'h3 / 7'h3 / 3'b10001 etc.
        (let* ((name (match-string 1))
               (radix  (pcase  (match-string 3)
                         ("'b" 2)
                         ("'o" 8)
                         ("'d" 10)
                         ("'h" 16)
                         (_    10)))
               (val  (format "%s" (string-to-number (match-string 4) radix))))


          (push (verilog-port-copy--format-generic name :generic-init val) parameters)))

      (reverse parameters))))


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

  (when (not group-comment)
    (setq group-comment ""))

  (list (list name) port-object port-direct port-type port-comment group-comment))

(defun verilog-port-copy--parse-ports (module)

  "Parse and extract a list of ports of the verilog MODULE at point."

  (with-temp-buffer

    (insert module)

    (let ((ports nil))

      (goto-char (point-min))

      ;; collect all of the ports
      (while (re-search-forward
              (concat
               "\\(\s+\\)"                    ; 1 = delimeter
               "\\(input\\|output\\|inout\\)" ; 2 = direction
               "\\(\s+\\|\\[\\)"              ; 3 = delimeter
               "\\([^,;]*\\)"                 ; 4 = body
               "\\(,\\|;\\|);\\)") nil t 1)   ; 5 = termination

        (let ((port (concat (match-string 2) (match-string 3) (match-string 4)))
              (direction (match-string 2))
              (name nil)
              (bitstring nil))


          (cond ((string-match
                  (concat
                   "\\(input\\|output\\|inout\\)\s*"    ; 1 = direction
                   "\\(reg\\|logic\\|wire\\|var\\)?\s*" ; 2 = type
                   "\\(unsigned\\|signed\\)?"           ; 3 == sign
                   "\s*"
                   "\\(\\[[^]]+:[^]]+\\]\\)\s?"         ; 4 == bit range
                   "\\([0-9A-z_]+\\)\s?"                ; 5 == name
                   "\\(\\[[^]]+:?[^]]*\\]\\)?\s?" ; 5 == 2nd dimension of a range
                   ) port)
                 (setq name (match-string 5 port))
                 (setq bitstring (match-string 4 port))
                 (when verilog-port-copy-verbose
                   (message port)
                   (message (format " > multidimensional port %s" name))))

                ((string-match
                  ;; single dimension ports, e.g.
                  ;;   input clock
                  ;;   input reg clock
                  ;;   input var clock
                  ;;   input wire clock
                  (concat
                   "\\(input\\|output\\|inout\\)" ; 1 = direction
                   "\s*"
                   "\\(reg\\|logic\\|wire\\|var\\)?" ; 2 = type
                   "\s*"
                   "\\(unsigned\\|signed\\)?"           ; 3 == sign
                   "\s*"
                   "\\([0-9A-Za-z_]+\\)" ; 3 == name
                   "\s*") port)
                 (setq name (match-string 4 port))
                 (when verilog-port-copy-verbose
                   (message port)
                   (message (format " > 1d port %s" name)))))

          (when (and name direction)
            (let* ((port-type (if bitstring "std_logic_vector" "std_logic"))
                   (port-entry (verilog-port-copy--format-port name :port-direct direction :port-type port-type)))
              (push port-entry ports )))))

      (reverse ports))))

;;-----------------------------------------------------------------------------
;; Entrypoints
;;-----------------------------------------------------------------------------

;;;###autoload
(defun verilog-port-copy ()

  "Copy the verilog module at point and put its definition into `vhdl-port-list`."

  (interactive)
  (save-excursion ; Save point, and current buffer; execute BODY; restore those things.

    (let* ((module (verilog-port-copy--get-module-as-string))
           (name (verilog-port-copy--get-module-name))
           (generic-list (verilog-port-copy--parse-generics module))
           (port-list (verilog-port-copy--parse-ports module))
           (context-clause nil))

      (setq vhdl-port-list (list name generic-list port-list context-clause)
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil)
      (message (concat "Verilog module `" name "' copied.")))))

;;;###autoload
(defun verilog-port-paste-instance ()

  "Paste as an Verilog instantiation."

  (interactive)
  (if (not vhdl-port-list)
      (error "ERROR:  No port read")
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

      ;; (beginning-of-line)
      ;; (save-excursion (verilog-port-copy--align-ports))
      ;; (end-of-line)

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
      (end-of-line))))

(provide 'verilog-port-copy)
;;; verilog-port-copy.el ends here
