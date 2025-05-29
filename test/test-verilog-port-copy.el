;;; test-hog.el -*- lexical-binding: t; -*-

(require 'verilog-port-copy)
(require 'ert)

(defmacro port-copy-test (file port-list)
  `(ert-deftest ,(intern file)
       nil
     (should (equal
              ,port-list
              (progn (find-file (concat (vc-root-dir) ,file)) (verilog-port-copy) vhdl-port-list)))))


(port-copy-test "test/test1.v"
                '("distrip"
                  ((("x") nil "" nil "\n")
                   (("test") nil "3" nil "\n"))
                  ((("qn_m2") nil "input" "std_logic_vector" nil "")
                   (("qn_m1") nil "input" "std_logic_vector" nil "")
                   (("qn") nil "input" "std_logic_vector" nil "")
                   (("qn_p1") nil "input" "std_logic_vector" nil "")
                   (("qn_p2") nil "input" "std_logic_vector" nil "")
                   (("qn_p3") nil "input" "std_logic_vector" nil "")
                   (("vth") nil "input" "std_logic_vector" nil "")
                   (("bypass_t5") nil "input" "std_logic" nil "")
                   (("bypass_t4") nil "input" "std_logic" nil "")
                   (("bypass_t3") nil "input" "std_logic" nil "")
                   (("distrip_out") nil "output" "std_logic" nil "")
                   (("t0") nil "output" "std_logic" nil "")
                   (("t1") nil "output" "std_logic" nil "")
                   (("t2") nil "output" "std_logic" nil "")) nil))

;; (port-copy-test "test/test2.v"
;;                 '("get_trig_vals_lut_tables_1_rom"
;;                   ((("someparam") nil "" nil "\n")
;;                    (("AWIDTH") nil "" nil "\n")
;;                    (("MEMSIZE") nil "" nil "\n"))
;;                   ((("addr0") nil "input" "std_logic_vector" nil "")
;;                    (("ce0") nil "input" "std_logic" nil "")
;;                    (("q0") nil "output" "std_logic_vector" nil "")
;;                    (("addr0") nil "input" "std_logic_vector" nil "")
;;                    (("ce1") nil "input" "std_logic" nil "")
;;                    (("q1") nil "output" "std_logic_vector" nil "")
;;                    (("clk") nil "input" "std_logic" nil "")) nil))

(port-copy-test "test/test3.v"
                '("some_obfusticated_module"
                  ((("WEIGHT") nil "" nil "\n")
                   (("WIDTH") nil "" nil "\n")
                   (("SOMEREAL") nil "" nil "\n")
                   (("LATENCY") nil "" nil "\n")
                   (("X") nil "" nil "\n")
                   (("Y") nil "" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("reset") nil "input" "std_logic" nil "")
                   (("some_2d_port_1") nil "input" "std_logic_vector" nil "")
                   (("some_2d_port_2") nil "input" "std_logic_vector" nil "")) nil))

(port-copy-test "test/test4.v"
                '("mock_module"
                  ((("N_IN") nil "" nil "\n")
                   (("N_OUT") nil "" nil "\n")
                   (("PERMB") nil "" nil "\n")
                   (("DELAY") nil "" nil "\n"))
                  ((("k") nil "input" "std_logic_vector" nil "")
                   (("i") nil "input" "std_logic_vector" nil "")
                   (("o") nil "output" "std_logic_vector" nil "")) nil))

(port-copy-test "test/test5.v"
                '("controller"
                  ((("N_X") nil "" nil "\n")
                   (("N_Y") nil "" nil "\n")
                   (("N_Z") nil "" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("reset") nil "input" "std_logic" nil "")
                   (("vara") nil "input" "std_logic_vector" nil "")
                   (("varb") nil "input" "std_logic_vector" nil "")
                   (("varc") nil "input" "std_logic_vector" nil "")
                   (("vard") nil "input" "std_logic_vector" nil "")
                   (("vare") nil "input" "std_logic_vector" nil "")
                   (("varf") nil "output" "std_logic_vector" nil "")
                   (("varg") nil "output" "std_logic_vector" nil "")
                   (("varh") nil "output" "std_logic_vector" nil "")
                   (("vari") nil "output" "std_logic" nil "")) nil))

(port-copy-test "test/test6.v"
                '("srl_multi"
                  ((("N") nil "" nil "\n")
                   (("W") nil "" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("en") nil "input" "std_logic" nil "")
                   (("sel") nil "input" "std_logic_vector" nil "")
                   (("din") nil "input" "std_logic_vector" nil "")
                   (("dout") nil "output" "std_logic_vector" nil "")) nil))

(port-copy-test "test/test7.v"
                '("oneshot" nil ((("clk") nil "input" "std_logic" nil "")
                                 (("rst") nil "input" "std_logic" nil "")
                                 (("d") nil "input" "std_logic" nil "")
                                 (("q") nil "output" "std_logic" nil "")) nil))

(port-copy-test "test/test8.v"
                '("mux"
                  ((("N") nil "" nil "\n")
                   (("W") nil "" nil "\n")
                   (("BR") nil "" nil "\n")
                   (("ADRB") nil "" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("din") nil "input" "std_logic_vector" nil "")
                   (("adr") nil "input" "std_logic_vector" nil "")
                   (("dout") nil "output" "std_logic_vector" nil "")) nil))

(port-copy-test "test/test9.v"
                '("reducer"
                  ((("N") nil "" nil "\n")
                   (("BRANCH_SIZE") nil "" nil "\n")
                   (("OP") nil "" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("din") nil "input" "std_logic_vector" nil "")
                   (("dout") nil "output" "std_logic" nil "")) nil))

(port-copy-test "test/test10.v"
                '("reset_synchronizer"
                  ((("RESET_POLARITY") nil "" nil "\n")
                   (("DEPTH") nil "" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("rst_in") nil "input" "std_logic" nil "")
                   (("rst_out") nil "output" "std_logic" nil "")) nil))
