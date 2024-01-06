module controller #(
  int N_X = 2,
  int N_Y = 64,
  int N_Z = 6
) (

  // clock and reset
  input wire clk,
  input wire reset,

  input wire [N_X-1:0] vara [N_Y],

  input wire [N_I-1:0] varb [N_Y],
  input wire [N_W-1:0] varc [N_Y],
  input wire [N_O-1:0] vard [N_Z],
  input wire [0:0]     vare [N_Y-1:0],

  output reg [N_I-1:0] varf,
  output reg [N_O-1:0] varg,
  output reg [N_W-1:0] varh,
  output reg           vari

);


endmodule

