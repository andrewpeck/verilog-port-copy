;;; test-hog.el -*- lexical-binding: t; -*-

(require 'verilog-port-copy)
(require 'ert)

(defmacro port-copy-test (file port-list)
  `(ert-deftest ,(intern file)
       nil
     (should (equal
              ,port-list
              (progn (find-file ,file) (verilog-port-copy) vhdl-port-list)))))


(port-copy-test "test/verilog3.sv"
                '("some_obfusticated_module"
                  ((("WEIGHT") nil "5" nil "\n")
                   (("WIDTH") nil "2" nil "\n")
                   (("LATENCY") nil "11 + (A_B_C==1 && D_E_F == 10 ? 4 : 0) + (N_OUTPUTS == 7 ? 1 : 0) + (N_OUTPUTS == 11 ? 3 : 0) + (N_OUTPUTS == 15 ? 5 : 0)" nil "\n")
                   (("X") nil "$clog2(WIDTH)" nil "\n")
                   (("Y") nil "$clog2(WEIGHT))" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("reset") nil "input" "std_logic" nil "")
                   (("some_2d_port_1") nil "input" "std_logic_vector (WEIGHT-1 downto 0)" nil "")
                   (("some_2d_port_2") nil "input" "std_logic_vector (WEIGHT-1 downto 0)" nil "")) nil))

(port-copy-test "test/verilog.v"
                '("distrip"
                  ((("test") nil "3" nil "\n"))
                  ((("qn_m2") nil "input" "std_logic_vector (9 downto 0)" nil "")
                   (("qn_m1") nil "input" "std_logic_vector (9 downto 0)" nil "")
                   (("qn") nil "input" "std_logic_vector (9 downto 0)" nil "")
                   (("qn_p1") nil "input" "std_logic_vector (9 downto 0)" nil "")
                   (("qn_p2") nil "input" "std_logic_vector (9 downto 0)" nil "")
                   (("qn_p3") nil "input" "std_logic_vector (9 downto 0)" nil "")
                   (("vth") nil "input" "std_logic_vector (9 downto 0)" nil "")
                   (("bypass_t5") nil "input" "std_logic" nil "")
                   (("bypass_t4") nil "input" "std_logic" nil "")
                   (("bypass_t3") nil "input" "std_logic" nil "")
                   (("distrip_out") nil "output" "std_logic" nil "")
                   (("t0") nil "output" "std_logic" nil "")
                   (("t1") nil "output" "std_logic" nil "")
                   (("t2") nil "output" "std_logic" nil "")) nil))
