module SolverTop(
  input  wire        clock,
  input  wire        reset,
  output wire [2:0]  jtag_address,
  output wire [31:0] jtag_writedata,
  output wire        jtag_write,
  output wire        jtag_read,
  input  wire        jtag_waitrequest,
  input  wire [31:0] jtag_readdata
);

wire        serial_ready;
wire        serial_send;
wire [7:0]  serial_dataToSend;
reg  [1:0]  jtag_state = 0;
reg  [31:0] jtag_write_buffer = 0;

Solver solver_inst (
  .reset(reset),
  .clock(clock),
  .serial_ready(serial_ready),
  .serial_send(serial_send),
  .serial_dataToSend(serial_dataToSend));

assign jtag_address = jtag_state == 0 ? 3'd4 : 3'd0;
assign jtag_read = jtag_state == 0;
assign serial_ready = jtag_state == 1;
assign jtag_write = jtag_state == 2;
assign jtag_writedata = jtag_write_buffer;
//assign jtag_writedata = {24'd0, 8'd65};

always @(posedge clock) begin
  if (reset) begin
    jtag_state <= 0;
    jtag_write_buffer <= 0;
  end else begin
    if (jtag_state == 0) begin
      if (jtag_waitrequest == 0)
        jtag_state <= jtag_readdata[31:16] > 0 ? 1 : 0;
    end else if (jtag_state == 1 && serial_send == 1) begin
      jtag_write_buffer <= {24'd0, serial_dataToSend}; //{24'd0, 8'd65};
      jtag_state <= 2;
    end else if (jtag_state == 2) begin
      if (jtag_waitrequest == 0) jtag_state <= 0;
    end
  end
end

endmodule
