module mux # (
  parameter int N = 64, // number of inputs
  parameter int W = 8,  // width of each input
  parameter int BR = 8, // branching ratio
  parameter int ADRB = $clog2(N)
) (
  input wire            clk,
  input wire [W-1:0]    din [N],
  input wire [ADRB-1:0] adr,
  output reg [W-1:0]    dout
);

  initial begin
    assert (N % BR == 0 || N <= BR)
      else $error("Number of inputs must be divisible by or less than the branching ratio!");
  end

  generate

    if (N <= BR) begin : gen_final

      always_ff @(posedge clk) begin
        dout <= din[adr];
      end

    end else begin : gen_recursive

      // create BR instances of the multiplexer
      // e.g. for a 64 input multiplexer with a branching ratio of 8,
      // we would create 8x8 multiplexers here
      logic [W-1:0] muxed [N/BR];

      for (genvar i=0; i<N/BR; i=i+1) begin : gen_inst

        mux #(
          .N  (BR),
          .W  (W),
          .BR (BR))
        u_mux_s0 (
          .clk  (clk),
          .din  (din[i*BR : (i+1)*BR-1]),
          .dout (muxed[i]),
          .adr  (adr[$clog2(BR)-1:0])
        );

      end

      // mux together the outputs of the different multiplexers
      mux #(
        .N  (N/BR),
        .W  (W),
        .BR (BR))
      u_mux_s1 (
        .clk  (clk),
        .din  (muxed),
        .dout (dout),
        .adr  (adr[ADRB-1:$clog2(BR)])
      );

    end

  endgenerate

endmodule
