(require 'vhdl-mode)

(defun align-paren (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)(" 1 1 nil))

(defun verilog-align-ports ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (er/expand-region 2)
    (align-paren (region-beginning) (region-end))))

(defconst verilog-module-and-port-regexp
  (concat "module\s+"
          "\\([A-z,0-9]+\\)"            ; module name
          "\\(\s+#([^)]*)\\)?"          ; verilog2001 style parameters #()
          "\s*(\\([^)]*\\))")           ; port list
  "Regexp to extract a module name and port list from a Verilog2001 style file.")

(defconst verilog-module-regexp
  "module\s+\\([A-z,0-9]+\\)"
  "Regexp to extract a verilog module name.")

(defun verilog-get-module-as-string ()

  (save-excursion
    (let* ((start (re-search-backward verilog-module-regexp))
           (end   (re-search-forward "endmodule")) ; FIXME: make sure this is not in a comment
           (module (buffer-substring-no-properties start end)))

      (setq module (replace-regexp-in-string "\/\/.*\n" "" module))
      (setq module (replace-regexp-in-string "\n" " " module)) module)))

(defun verilog-get-module-name ()
  "Get the name of the Verilog module at point in the currently opened buffer."
  (save-excursion
    (forward-line 1)
    (when  (re-search-backward verilog-module-regexp)
      (match-string-no-properties 1))))


;;;-----------------------------------------------------------------------------
;;; Top Level Port Copy Function
;;;-----------------------------------------------------------------------------

(defun verilog-port-copy ()
  ""
  (interactive)
  (save-excursion ; Save point, and current buffer; execute BODY; restore those things.

    (let* ((module (verilog-get-module-as-string))
           (name (verilog-get-module-name))
           (generic-list (verilog-parse-generics module))
           (port-list (verilog-parse-ports module))
           (context-clause nil))

      (setq vhdl-port-list (list name generic-list port-list context-clause)
            vhdl-port-reversed-direction nil
            vhdl-port-flattened nil))))

;;;-----------------------------------------------------------------------------
;;; Parameters
;;;-----------------------------------------------------------------------------

(defun verilog-parse-ansi-parameters (module)
  "Placeholder for ANSI style parameter parsing"
  ;; parse names (accept extended identifiers)
  nil)

(defun verilog-parse-nonansi-parameters (module)

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
                 "\\([A-z,0-9]+\\)" ;; name
                 "\s*;"
                 ) nil t)
          (let ((name (match-string 1))
                (val nil))
            (push-to-params name val)))

        ;; get initialized params, e.g. "parameter MXCNT = 12;"
        (goto-char (point-min))
        (while (re-search-forward
                (concat
                 "parameter\s+"     ;
                 "\\([A-z,0-9]+\\)" ; name
                 "\s*=\s*"          ;
                 "\\([0-9]+\\)?"    ; number of bits
                 "\\('[bdho]+\\)?"  ; radix
                 "\\([0-9,A-z]+\\)" ; val
                 "\s*               ;"
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

(defun verilog-parse-generics (module)
  "Wrapper to gather up both ANSI and non-ANSI parameters into a list"

  (append (verilog-parse-nonansi-parameters module)
          (verilog-parse-ansi-parameters module)))


;;;-----------------------------------------------------------------------------
;;; Ports
;;;-----------------------------------------------------------------------------

(defun verilog-parse-ansi-ports (module)
  ""

  (interactive)

  (with-temp-buffer

    (insert module)

    (let ((ports nil))

      (cl-flet ((push-to-ports
                 (name dir type)

                 ;; match to specified format, e.g.
                 ;; ((("qn_m2") nil "in" "std_logic_vector (9 downto 0)" nil "\n")
                 (when name
                   (push (list (list name) nil dir type nil "")
                         ports))))

        ;; get std_logics
        (goto-char (point-min))

        (while (re-search-forward
                (concat "\\(input\\|output\\)\s+"            ; direction
                        "\\(reg\\|logic\\|wire\\|var\s+\\)?" ; type
                        "\\([0-9,A-z,_]+\\)"                 ; name
                        "\s*\\(,\\|)\\)"                     ; trailing comma or paren
                        ) nil t 1)
          (let ((direction (match-string 1))
                (name (match-string 3)))

            (push-to-ports name direction "std_logic")
            ;; (message (format "%s : %s" name direction))
            (list name direction)))

        ;; get buses
        (goto-char (point-min))
        (while (re-search-forward
                (concat
                 "\\(input\\|output\\)\s+"            ; direction
                 "\\(reg\\|logic\\|wire\\|var\s+\\)?" ; type
                 "\\[\\([^]]+\\)\s*:"                 ; bit high
                 "\\([^]]\\)]\s*"                     ; bit low
                 "\\([0-9,A-z]+\\)"                   ; name
                 "\s*\\(,\\|)\\)"                     ; trailing comma or paren
                 ) nil t 1)
          (let ((direction (match-string 1))
                (bithi (match-string 3))
                (bitlo (match-string 4))
                (name (match-string 5)))
            (push-to-ports name direction (format "std_logic_vector (%s downto %s)" bithi bitlo))))) ports)))

(defun verilog-parse-ports (module)
  ""
  (verilog-parse-ansi-ports module))

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
      (dolist (generic generics)
        (let ((gname (caar generic)))
          (insert (format "  .%s(%s)\n" gname gname))))

      (verilog-align-ports)

      ;; instance name
      (insert (format ") u_%s (\n" module-name))

      ;; insert ports
      (dolist (port ports)
        (let ((gname (caar port)))
          (insert (format "  .%s(%s)\n" gname gname))))

      (verilog-align-ports)

      ;; close
      (insert ");"))))

(provide 'verilog-port-copy)
