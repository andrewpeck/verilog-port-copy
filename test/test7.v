module oneshot (
  input wire clk,
  input wire rst,
  input wire d,
  output reg q
);

  reg last;
  always @(posedge clk) begin
    if (rst) begin
      last <= 0;
      q    <= 0;
    end else begin
      last <= d;
      q    <= d && ~last;
    end

  end

endmodule
