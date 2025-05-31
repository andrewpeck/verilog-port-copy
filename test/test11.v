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

  wire [0:0]       push [DEPTH+1];
  wire [SORTB-1:0] data [DEPTH+1];
  wire [METAB-1:0] metadata [DEPTH+1];
  wire [DEPTH-1:0] updating;

  assign push[DEPTH] = 0; // the "best" cell never gets pushed into

  // output
  always_ff @(posedge clk) begin
    for (int i=0; i<DEPTH; i=i+1) begin
      data_o[i] <= data[i];
      metadata_o[i] <= metadata[i];
      updating_o[i] <= updating[i];
    end
  end

  generate begin : gen_sorters
    for (genvar i=1; i<DEPTH+1; i=i+1) begin : gen_i

      unit_cell #(
        .SORTB    (SORTB),
        .METAB    (METAB),
        .REV      (REV)
      ) u_cell (
        .clk                 (clk),
        .rst                 (rst),

        // data inputs
        .data_i              (data_i),
        .metadata_i          (metadata_i),
        .dav_i               (dav_i),

        // data inputs
        .neighbor_data_i     (data[i]),
        .neighbor_metadata_i (metadata[i]),
        .neighbor_push_i     (push[i]),

        // data outputs
        .neighbor_data_o     (data[i-1]),
        .neighbor_metadata_o (metadata[i-1]),
        .neighbor_push_o     (push[i-1]),

        .updating            (updating[i])
      );
    end
  end
  endgenerate

endmodule
