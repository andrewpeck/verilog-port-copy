module reset_synchronizer #(
  logic [0:0] RESET_POLARITY = 1, // 1 for active high reset; 0 for active low
  int         DEPTH          = 2
) (
  input  wire clk,
  input  wire rst_in,
  output wire rst_out
);

  // body

endmodule
