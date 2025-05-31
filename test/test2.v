module get_trig_vals_lut_tables_1_rom #(parameter someparam = 2)
  (
addr0, ce0, q0, addr1, ce1, q1, clk);

parameter AWIDTH = 8;
parameter MEM_SIZE = 197;

input[AWIDTH-1:0] addr0;
input ce0;
output reg[DWIDTH-1:0] q0;
input[AWIDTH-1:0] addr1;
input ce1;
output reg[DWIDTH-1:0] q1;
input clk;

reg [DWIDTH-1:0] ram[0:MEM_SIZE-1];

initial begin
    $readmemh("./get_trig_vals_lut_tables_1_rom.dat", ram);
end

  // body

endmodule
