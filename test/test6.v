module srl_multi
  #(int N = 32, W=8 ) (
  input wire                 clk,
  input wire                 en,
  input wire [$clog2(N)-1:0] sel,
  input wire [W-1:0]         din,
  output wire [W-1:0]        dout
);

  reg [N-1:0] data [W-1:0];

  generate
    for (genvar i=0; i<W; i=i+1) begin : gen_output
      assign dout[i] = data[i][sel];
    end
  endgenerate

  always @(posedge clk)
    begin
      if (en == 1'b1)
        for (int i=0; i<W; i=i+1) begin
          data[i] <= {data[i][N-2:0], din[i]};
        end
    end

endmodule

