
module uintm_add (x, y, out);
    parameter BITS;

    input wire [BITS-1:0] x;
    input wire [BITS-1:0] y;
    output wire [BITS-1:0] out;

    assign out = x + y;
endmodule

module uintm_sub (x, y, out);
    parameter BITS;

    input wire [BITS-1:0] x;
    input wire [BITS-1:0] y;
    output wire [BITS-1:0] out;

    assign out = x - y;
endmodule

module uintm_mul (x, y, out);
    parameter BITS;

    input wire [BITS-1:0] x;
    input wire [BITS-1:0] y;
    output wire [BITS-1:0] out;

    assign out = x * y;
endmodule

module uintm_bit_and (x, y, out);
    parameter BITS;

    input wire [BITS-1:0] x;
    input wire [BITS-1:0] y;
    output wire [BITS-1:0] out;

    assign out = x & y;
endmodule

module uintm_bit_or (x, y, out);
    parameter BITS;

    input wire [BITS-1:0] x;
    input wire [BITS-1:0] y;
    output wire [BITS-1:0] out;

    assign out = x | y;
endmodule

module uintm_bit_xor (x, y, out);
    parameter BITS;

    input wire [BITS-1:0] x;
    input wire [BITS-1:0] y;
    output wire [BITS-1:0] out;

    assign out = x ^ y;
endmodule
