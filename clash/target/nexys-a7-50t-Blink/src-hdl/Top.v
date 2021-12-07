module Top(
           input         CLK100MHZ,
           output [15:0] LED
           );

   topEntity u_topEntity
     (.CLK(CLK100MHZ),
      .LED(LED[15:0])
      );

endmodule
