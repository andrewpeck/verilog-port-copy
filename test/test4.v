module mock_module #(
  int N_IN =64,
  int N_OUT=15,
  int PERMB=$clog2(N_IN),
  int DELAY=0

)(
  input logic [PERMB-1:0] k,
  input [N_IN-1:0]        i,
  output [N_OUT-1:0]      o
);

`ifdef SIMULATION
  always @(*) begin
    for (int ioutput=0; ioutput<N_OUT; ioutput++) begin : gen_assign_outputs
      assign o[ioutput] = #(DELAY) i[PERMB'(ioutput) ^ k];
    end
  end
`else
  initial $error("some error");
`endif

endmodule

