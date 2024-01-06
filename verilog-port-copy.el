;;; verilog-port-copy.el --- Functions for working with verilog files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, 2024 Andrew Peck

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

;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------


(defconst verilog--identifier-re "[a-zA-Z_][a-zA-Z_0-9]*"
  "")

(defconst verilog--module-and-port-regexp
  (concat "module\s+"
          "\\([A-z]+[A-z0-9_]*\\)\s*" ; module name
          "\s*\\(#(.*)\\)?"           ; verilog2001 style parameters #()
          "\\(\s*(.*);\\)")
  "Regexp to extract a module name and port list from a Verilog2001 style file.")

(defconst verilog--module-regexp
  "module\s+\\([A-z0-9_]+\\)"
  "Regexp to extract a verilog module name.")

;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------

(defun verilog--align-paren (start end)
  "Align columns by ampersand"
  (align-regexp start end "\\(\\s-*\\)(" 1 1 nil))

(defun verilog--align-ports ()
  (interactive)
  "Align verilog ports at point."
  (save-excursion
    (beginning-of-line)
    (er/expand-region 2)
    (verilog--align-paren (region-beginning) (region-end))))

;;------------------------------------------------------------------------------
;; Module Extraction
;;------------------------------------------------------------------------------

(defun verilog--get-module-as-string ()

  "Get a verilog module at point, return it as a string.

   Comments and newlines will be removed."

  (save-excursion

    ;; forward word so it works on module itself.
    (when (string= "module" (symbol-at-point))
      (forward-word 2))

    (let* ((start (re-search-backward verilog--module-regexp))
           (end   (re-search-forward "endmodule")) ; FIXME: make sure this is not in a comment
           (module (buffer-substring-no-properties start end)))

      (setq module (replace-regexp-in-string "\/\/.*\n" "" module))
      (setq module (replace-regexp-in-string "\n" " " module))
      (setq module (replace-regexp-in-string "\s\s+" " " module))

      module)))

(defun verilog--get-module-name ()
  "Get the name of the Verilog module at point in the currently opened buffer."
  (save-excursion
    (forward-line 1)
    (when  (re-search-backward verilog--module-regexp)
      (match-string-no-properties 1))))

;;-----------------------------------------------------------------------------
;; Parameters
;;-----------------------------------------------------------------------------

(cl-defun verilog--format-generic
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


(defun verilog--parse-generics (module)

  "Return verilog parameters from a MODULE.

A MODULE should be a string with the entire contents of the
module with comments and newlines removed."

  (with-temp-buffer

    (insert module)

    (goto-char (point-min))

    (let ((parameters nil))

      (when (re-search-forward verilog--module-and-port-regexp nil t)
        (let ((ansi-port-str (match-string 2)))
          (with-temp-buffer

            (insert ansi-port-str)
            (goto-char (point-min))

            ;; get ansi params e.g. in #()

            (goto-char (point-min))
            (while (re-search-forward
                    (concat

                     ;; get the type
                     "\\(parameter\\|int\\)\s+" ;; could also have logic I think, anything else?

                     ;; get the range
                     "\\(\\[[^]]*\\]\\s-*\\)?"
                     ;; "\\(" verilog-range-re "\\)?" ;; range?

                     ;; get the name
                     "\\(" verilog--identifier-re "\\)"

                     ;; has a value?
                     "\s*=?\s*"

                     ;; get the value
                     "\\([^,]+\\|)\s*(\\)?"

                     ;; close
                     "\s*,?\\(\s*)\s*;\\)?")
                    nil t)

              (let ((type (match-string 1))
                    (range (match-string 2))
                    (name (match-string 3))
                    (default "")) ; just ignore defaults for now.. need a real parser for this (match-string 4)

                (push (verilog--format-generic name :generic-init default) parameters))))))


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
          (push (verilog--format-generic name :generic-init val) parameters)))

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
                         (x 10)))
               (val  (format "%s" (string-to-number (match-string 4) radix))))


          (push (verilog--format-generic name :generic-init val) parameters)))

      (reverse parameters))))


;;------------------------------------------------------------------------------
;; Ports
;;------------------------------------------------------------------------------


(cl-defun verilog--format-port (name &key port-object port-direct
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

(defun verilog--parse-ports (module)

  ""

  (with-temp-buffer

    (insert module)

    (let ((ports nil))

      (goto-char (point-min))

      ;; get buses
      (goto-char (point-min))
      (while
          (re-search-forward
           (concat
            "\s+"
            "\\(input\\|output\\|inout\\)+\s?"   ; direction
            "\\(reg\\|logic\\|wire\\|var\\)?\s?" ; type
            "\\(\\[[^]]+:[^]]+\\]\\)?\s?"        ; bit range?
            "\\([0-9A-z_]+\\)\s?"                ; name
            "\\(\\[[^]]+:?[^]]*\\]\\)?\s?"       ; 2nd dimension of a range
            "\\(,\\|)\s*;\\)")
           nil t 1)

        (let* ((direction (match-string 1))
               (bitstring (match-string 3))
               (name (match-string 4))
               (bitrange (when bitstring (split-string bitstring "\\[\\|:\\|\\]" t)))
               (bithi  (when bitrange (car bitrange)))
               (bitlo (when bitrange (cadr bitrange)))
               (port-type (if (and bithi bitlo)
                              (format "std_logic_vector (%s downto %s)" bithi bitlo)
                            "std_logic"))
               (port-entry (verilog--format-port name
                                                 :port-direct direction
                                                 :port-type port-type)))
          (push port-entry ports )))

      (reverse ports))))

;;-----------------------------------------------------------------------------
;; Entrypoints
;;-----------------------------------------------------------------------------

;;;###autoload
(defun verilog-port-copy ()

  "Copy the verilog module at point and put its definition into `vhdl-port-list`."

  (interactive)
  (save-excursion ; Save point, and current buffer; execute BODY; restore those things.

    (let* ((module (verilog--get-module-as-string))
           (name (verilog--get-module-name))
           (generic-list (verilog--parse-generics module))
           (port-list (verilog--parse-ports module))
           (context-clause nil))

      (setq vhdl-port-list (list name generic-list port-list context-clause)
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil))))

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
      (insert " #(\n")

      ;; insert generics
      (when generics
        (dolist (generic generics)
          (let ((gname (caar generic)))
            (insert (format "  .%s(%s),\n" gname gname))))
        ;; remove the last comma and newline
        (delete-char -2))

      (insert ")")

      ;; (beginning-of-line)
      ;; (save-excursion (verilog--align-ports))
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
      (save-excursion (verilog--align-ports))
      (end-of-line))))

(provide 'verilog-port-copy)
;;; verilog-port-copy.el ends here
