;;
;; "Variable to hold last port map parsed."
;; structure: (parenthesized expression means list of such entries)
;; (ent-name
;;  ((generic-names) generic-type generic-init generic-comment group-comment)
;;  ((port-names) port-object port-direct port-type port-comment group-comment)
;;  (lib-name pack-key))

(require 'vhdl-mode)
(require 'cl-lib)

(defun verilog--align-paren (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)(" 1 1 nil))

(defun verilog--align-ports ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (er/expand-region 2)
    (verilog--align-paren (region-beginning) (region-end))))

(defconst verilog--module-and-port-regexp
  (concat "module\s+"
          "\\([A-z0-9_]+\\)"            ; module name
          "\\(\s+#([^)]*)\\)?"          ; verilog2001 style parameters #()
          "\s*(\\([^)]*\\))")           ; port list
  "Regexp to extract a module name and port list from a Verilog2001 style file.")

(defconst verilog--module-regexp
  "module\s+\\([A-z0-9_]+\\)"
  "Regexp to extract a verilog module name.")

(defun verilog--get-module-as-string ()

  "Get a verilog module at point, return it as a string with comments and newlines removed."

  (save-excursion
    (let* ((start (re-search-backward verilog--module-regexp))
           (end   (re-search-forward "endmodule")) ; FIXME: make sure this is not in a comment
           (module (buffer-substring-no-properties start end)))

      (setq module (replace-regexp-in-string "\/\/.*\n" "" module))
      (setq module (replace-regexp-in-string "\n" " " module)) module)))

(defun verilog--get-module-name ()
  "Get the name of the Verilog module at point in the currently opened buffer."
  (save-excursion
    (forward-line 1)
    (when  (re-search-backward verilog--module-regexp)
      (match-string-no-properties 1))))

;;;-----------------------------------------------------------------------------
;;; Parameters
;;;-----------------------------------------------------------------------------

(defun verilog--parse-ansi-parameters (module)

  "Placeholder for ANSI style parameter parsing.

Takes in a MODULE string.

Not implemented yet."

  nil)

(defun verilog--parse-nonansi-parameters (module)

  "Gather up the non-ansi parameters in a Verilog MODULE string."

  ;; create a temp buffer as a copy of the current one
  (with-temp-buffer

    (insert module)

    (goto-char (point-min))

    (let ((parameters nil))

      (cl-flet ((push-to-params
                 (name val)
                 (when name (push (list (list name) nil val nil "\n") parameters))))

        ;; get uninitialized params, e.g. "parameter MXCNT;"
        (while (re-search-forward
                (concat
                 "parameter\s+" ;;
                 "\\([A-z0-9]+\\)" ;; name
                 "\s*;") nil t)
          (let ((name (match-string 1))
                (val nil))
            (push-to-params name val)))

        ;; get initialized params, e.g. "parameter MXCNT = 12;"
        (goto-char (point-min))
        (while (re-search-forward
                (concat
                 "parameter\s+"     ;
                 "\\([A-z0-9_]+\\)" ; name
                 "\s*=\s*"          ;
                 "\\([A-z0-9_]+\\)?"    ; number of bits
                 "\\('[bdho]\\)?"  ; radix
                 "\\([0-9A-z,_]+\\)" ; val
                 "\s*;"
                 ) nil t)

          ;; account for different radixes, 'h3 / 7'h3 / 3'b10001 etc.
          (let* ((name (match-string 1))
                 (radix  (pcase  (match-string 3)
                           ("'b" 2)
                           ("'o" 8)
                           ("'d" 10)
                           ("'h" 16)
                           (x 10)))
                 (val  (format "%s" (string-to-number (match-string 4) radix))))
            (push-to-params name val))))

      parameters)))

(defun verilog--parse-generics (module)

  "Return verilog parameters from a MODULE.

A MODULE should be a string with the entire contents of the
module with comments and newlines removed."

  (append (verilog--parse-nonansi-parameters module)
          (verilog--parse-ansi-parameters module)))


(cl-defun verilog--format-port-list (name &key port-object port-direct
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

  (interactive)

  (with-temp-buffer

    (insert module)

    (let ((ports nil))

      (goto-char (point-min))

      ;; get buses
      (goto-char (point-min))
      (while
          (re-search-forward
           (concat
            "\\(input\\|output\\|inout\\)+\s?"   ; direction
            "\\(reg\\|logic\\|wire\\|var\\)?\s?" ; type
            "\\(\\[[^]]+:[^]]+\\]\\)?\s?"        ; bit range?
            "\\([0-9A-z_]+\\)\s?"                ; name
            "[,;)]?")
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
               (port-entry (verilog--format-port-list name
                                                      :port-direct direction
                                                      :port-type port-type)))
          (add-to-list 'ports port-entry t))) ports)))

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
