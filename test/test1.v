module distrip # (int x)
  (
  input       clock,
  input [9:0] qn_m2,
  input [9:0] qn_m1,
  input [9:0] qn,
  input [9:0] qn_p1,
  input [9:0] qn_p2,
  input [9:0] qn_p3,
  input [9:0] vth,

  input       bypass_t5,
  input       bypass_t4,
  input       bypass_t3,

  output      distrip_out,
  output      t0,
  output      t1,
  output      t2
);

  // for simplicity, rename the inputs to be 0, 1, 2, 3, 4
  // the center "n" input is 2

  wire [9:0]  q00 = qn_m2;
  wire [9:0]  q01 = qn_m1;
  wire [9:0]  q02 = qn;
  wire [9:0]  q03 = qn_p1;
  wire [9:0]  q04 = qn_p2;
  wire [9:0]  q05 = qn_p3;


  // comparator arrays

  wire        OS01TH = (q01 > vth);
  wire        OS02TH = (q02 > vth);
  wire        OS03TH = (q03 > vth);
  wire        OS04TH = (q04 > vth);

  wire        OS0001 = (q01 > q00);
  wire        OS0002 = (q02 > q00);

  wire        OS0102 = (q02 > q01);
  wire        OS0103 = (q03 > q01);

  wire        OS0203 = (q03 > q02);
  wire        OS0204 = (q04 > q02);

  wire        OS0304 = (q03 > q02);
  wire        OS0305 = (q04 > q02);

  wire        OS0405 = (q04 > q05);

  wire        output_busy;

  reg         q01_is_peak;
  reg         q02_is_peak;
  reg         q03_is_peak;
  reg         q04_is_peak;

  always @(posedge clock) begin
    q01_is_peak <= ~OS0001 & OS0102;
    q02_is_peak <= ~OS0102 & OS0203;
    q03_is_peak <= ~OS0203 & OS0304;
    q04_is_peak <= ~OS0304 & OS0405;
  end

  parameter test = 3;

  reg [5:0] tot02sr;
  reg [5:0] tot03sr;

  wire [5:0] tot02;
  wire [5:0] tot03;

  always @(posedge clock) begin

    tot02sr[5] <= OS02TH;
    tot02sr[4] <= tot02sr[5];
    tot02sr[3] <= tot02sr[4];
    tot02sr[2] <= tot02sr[3];
    tot02sr[1] <= tot02sr[2];
    tot02sr[0] <= tot02sr[1];

    tot03sr[5] <= OS03TH;
    tot03sr[4] <= tot03sr[5];
    tot03sr[3] <= tot03sr[4];
    tot03sr[2] <= tot03sr[3];
    tot03sr[1] <= tot03sr[2];
    tot03sr[0] <= tot03sr[1];

  end

  wire [5:0] bypass_t = {bypass_t5, bypass_t4, bypass_t3, 1'b0, 1'b0, 1'b0};

  assign tot02[5:0] = tot02sr[5:0] | bypass_t[5:0];
  assign tot03[5:0] = tot03sr[5:0] | bypass_t[5:0];

  assign distrip0 = (!reset && (&tot02[5:0]) && q02_is_peak && !output_busy);
  assign distrip1 = (!reset && (&tot03[5:0]) && q03_is_peak && !output_busy);

  reg        OS0103_REG;
  reg        OS0002_REG;
  reg        OS0204_REG;
  reg        OS0305_REG;

  always @(posedge clock) begin
    OS0103_REG <= OS0103;
    OS0002_REG <= OS0002;
    OS0204_REG <= OS0204;
    OS0305_REG <= OS0305;
  end

  wire strip0_side = OS0103_REG & distrip0; // 1=strip left, 0=strip right
  wire strip1_side = OS0204_REG & distrip1; // 1=strip left, 0=strip right

  reg [1:0] triad0;
  reg [1:0] triad1;
  reg [2:0] triad2;

  always @(posedge clock) begin
    triad0[0] <= distrip0 | distrip1;
    triad0[1] <= triad0[0];

    triad1[0] <= distrip1;
    triad1[1] <= triad1[0];

    triad2[0] <= ~(strip0_side | strip1_side);
    triad2[1] <= triad2[0];
    triad2[2] <= triad2[1];

  end

  // writing triad flag
  assign output_busy = ~(triad0[0] | triad0[1]);

  // serialized output
  assign distrip_out = triad0 | triad1[1] | triad2[2];

  // parallel outputs
  assign t0 = triad0[0];
  assign t1 = triad1[0];
  assign t2 = triad2[0];

endmodule

module pizza
  (
  input [9:0] qn_m2,
  input [9:0] qn_m1,
  input [9:0] qn,
  input [9:0] qn_p1,
  input [9:0] qn_p2,
  input [9:0] qn_p3,
  input [9:0] vth,

  input       bypass_t5,
  input       bypass_t4,
  input       bypass_t3,

  output      distrip_out,
  output      t0,
  output      t1,
  output      t2
);

  // for simplicity, rename the inputs to be 0, 1, 2, 3, 4
  // the center "n" input is 2

  wire [9:0]  q00 = qn_m2;
  wire [9:0]  q01 = qn_m1;
  wire [9:0]  q02 = qn;
  wire [9:0]  q03 = qn_p1;
  wire [9:0]  q04 = qn_p2;
  wire [9:0]  q05 = qn_p3;


  // comparator arrays

  wire        OS01TH = (q01 > vth);
  wire        OS02TH = (q02 > vth);
  wire        OS03TH = (q03 > vth);
  wire        OS04TH = (q04 > vth);

  wire        OS0001 = (q01 > q00);
  wire        OS0002 = (q02 > q00);

  wire        OS0102 = (q02 > q01);
  wire        OS0103 = (q03 > q01);

  wire        OS0203 = (q03 > q02);
  wire        OS0204 = (q04 > q02);

  wire        OS0304 = (q03 > q02);
  wire        OS0305 = (q04 > q02);

  wire        OS0405 = (q04 > q05);

  wire        output_busy;

  reg         q01_is_peak;
  reg         q02_is_peak;
  reg         q03_is_peak;
  reg         q04_is_peak;

  always @(posedge clock) begin
    q01_is_peak <= ~OS0001 & OS0102;
    q02_is_peak <= ~OS0102 & OS0203;
    q03_is_peak <= ~OS0203 & OS0304;
    q04_is_peak <= ~OS0304 & OS0405;
  end

  parameter test = 3;

  reg [5:0] tot02sr;
  reg [5:0] tot03sr;

  wire [5:0] tot02;
  wire [5:0] tot03;

  always @(posedge clock) begin

    tot02sr[5] <= OS02TH;
    tot02sr[4] <= tot02sr[5];
    tot02sr[3] <= tot02sr[4];
    tot02sr[2] <= tot02sr[3];
    tot02sr[1] <= tot02sr[2];
    tot02sr[0] <= tot02sr[1];

    tot03sr[5] <= OS03TH;
    tot03sr[4] <= tot03sr[5];
    tot03sr[3] <= tot03sr[4];
    tot03sr[2] <= tot03sr[3];
    tot03sr[1] <= tot03sr[2];
    tot03sr[0] <= tot03sr[1];

  end

  wire [5:0] bypass_t = {bypass_t5, bypass_t4, bypass_t3, 1'b0, 1'b0, 1'b0};

  assign tot02[5:0] = tot02sr[5:0] | bypass_t[5:0];
  assign tot03[5:0] = tot03sr[5:0] | bypass_t[5:0];

  assign distrip0 = (!reset && (&tot02[5:0]) && q02_is_peak && !output_busy);
  assign distrip1 = (!reset && (&tot03[5:0]) && q03_is_peak && !output_busy);

  reg        OS0103_REG;
  reg        OS0002_REG;
  reg        OS0204_REG;
  reg        OS0305_REG;

  always @(posedge clock) begin
    OS0103_REG <= OS0103;
    OS0002_REG <= OS0002;
    OS0204_REG <= OS0204;
    OS0305_REG <= OS0305;
  end

  wire strip0_side = OS0103_REG & distrip0; // 1=strip left, 0=strip right
  wire strip1_side = OS0204_REG & distrip1; // 1=strip left, 0=strip right

  reg [1:0] triad0;
  reg [1:0] triad1;
  reg [2:0] triad2;

  always @(posedge clock) begin
    triad0[0] <= distrip0 | distrip1;
    triad0[1] <= triad0[0];

    triad1[0] <= distrip1;
    triad1[1] <= triad1[0];

    triad2[0] <= ~(strip0_side | strip1_side);
    triad2[1] <= triad2[0];
    triad2[2] <= triad2[1];

  end

  // writing triad flag
  assign output_busy = ~(triad0[0] | triad0[1]);

  // serialized output
  assign distrip_out = triad0 | triad1[1] | triad2[2];

  // parallel outputs
  assign t0 = triad0[0];
  assign t1 = triad1[0];
  assign t2 = triad2[0];

endmodule
