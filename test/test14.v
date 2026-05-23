module sample_alu #(
  parameter int OP_WIDTH = 4
) (
  input  wire           clk,
  input  wire           rst_n,
  input  var            cmd_t       cmd,
  input  var            data_word_t payload,
  output var            result_t    result,
  output wire           valid
);
endmodule
