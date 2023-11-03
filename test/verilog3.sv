module some_obfusticated_module
  #(
  // parameters
  int WEIGHT = 5,
  int WIDTH = 2,
  int LATENCY = 11 + (A_B_C==1 && D_E_F == 10 ? 4 : 0)
      + (N_OUTPUTS == 7 ? 1 : 0)
      + (N_OUTPUTS == 11 ? 3 : 0)
      + (N_OUTPUTS == 15 ? 5 : 0),

  int X = $clog2(WIDTH),
  int Y = $clog2(WEIGHT))
  (
  input wire              clk,

  input wire              reset,

  // inputs from SSGs
  input wire [WEIGHT-1:0] some_2d_port_1 [WIDTH-1:0],
  input wire [WEIGHT-1:0] some_2d_port_2 [WIDTH-1:0]
);


endmodule
