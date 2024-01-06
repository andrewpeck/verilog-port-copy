;;; test-hog.el -*- lexical-binding: t; -*-

(require 'verilog-port-copy)
(require 'ert)

(defmacro port-copy-test (file port-list)
  `(ert-deftest ,(intern file)
       nil
     (should (equal
              ,port-list
              (progn (find-file ,file) (verilog-port-copy) vhdl-port-list)))))


(port-copy-test "test/test1.v"
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

(port-copy-test "test/test2.v"
                '("get_trig_vals_lut_tables_1_rom"
                  ((("someparam") nil "2" nil "\n")
                   (("AWIDTH") nil "8" nil "\n")
                   (("MEMSIZE") nil "197" nil "\n"))
                  ((("addr0") nil "input" "std_logic_vector (AWIDTH-1 downto 0)" nil "")
                   (("ce0") nil "input" "std_logic" nil "")
                   (("q0") nil "output" "std_logic_vector (DWIDTH-1 downto 0)" nil "")
                   (("addr0") nil "input" "std_logic_vector (AWIDTH-1 downto 0)" nil "")
                   (("ce1") nil "input" "std_logic" nil "")
                   (("q1") nil "output" "std_logic_vector (DWIDTH-1 downto 0)" nil "")
                   (("clk") nil "input" "std_logic" nil "")) nil))

(port-copy-test "test/test3.v"
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

(port-copy-test "test/test4.v"
                '("mock_module"
                  ((("N_IN") nil "64" nil "\n")
                   (("N_OUT") nil "15" nil "\n")
                   (("PERMB") nil "$clog2(N_IN)" nil "\n")
                   (("DELAY") nil "0" nil "\n"))
                  ((("k") nil "input" "std_logic_vector (PERMB-1 downto 0)" nil "")
                   (("i") nil "input" "std_logic_vector (N_IN-1 downto 0)" nil "")
                   (("o") nil "output" "std_logic_vector (N_OUT-1 downto 0)" nil "")) nil))

(port-copy-test "test/test5.v"
                '("controller"
                  ((("N_X") nil "2" nil "\n")
                   (("N_Y") nil "64" nil "\n")
                   (("N_Z") nil "6" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("reset") nil "input" "std_logic" nil "")
                   (("vara") nil "input" "" nil "")
                   (("varb") nil "input" "" nil "")
                   (("varc") nil "input" "" nil "")
                   (("vard") nil "input" "" nil "")
                   (("vare") nil "input" "" nil "")
                   (("varf") nil "output" "std_logic_vector (N_I-1 downto 0)" nil "")
                   (("varg") nil "output" "std_logic_vector (N_O-1 downto 0)" nil "")
                   (("varh") nil "output" "std_logic_vector (N_W-1 downto 0)" nil "")
                   (("vari") nil "output" "std_logic" nil "")) nil))

(port-copy-test "test/test6.v"
                '("srl_multi"
                  ((("N") nil "32" nil "\n")
                   (("W") nil "8" nil "\n"))
                  ((("clk") nil "input" "std_logic" nil "")
                   (("en") nil "input" "std_logic" nil "")
                   (("sel") nil "input" "std_logic_vector ($clog2(N)-1 downto 0)" nil "")
                   (("din") nil "input" "std_logic_vector (W-1 downto 0)" nil "")
                   (("dout") nil "output" "std_logic_vector (W-1 downto 0)" nil "")) nil))
