module cell_sort # (
  int SORTB = 8,
  int METAB = 32,
  int DEPTH = 8,
  int REV   = 0
) (
  input wire             clk,
  input wire             rst,

  // common data input
  input wire [SORTB-1:0] data_i,
  input wire [METAB-1:0] metadata_i,
  input wire             dav_i,

  output reg [SORTB-1:0] data_o [DEPTH],
  output reg [METAB-1:0] metadata_o [DEPTH],
  output reg [DEPTH-1:0] updating_o
);


endmodule
