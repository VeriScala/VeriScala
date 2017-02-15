
module ram (
clock,
write_enable,
read_addr,
write_addr,
datain,
dataout

);

input [7:0] read_addr;
input [7:0] write_addr;
input [15:0] datain;
input clock;
input write_enable;
output [15:0] dataout;
reg [15:0] dataout;
reg [15:0] tmp_0 [0:255];




always @(negedge clock) begin
  if (write_enable == 1) 
  begin    
	tmp_0[write_addr] <= datain;
  end
  
  dataout <= tmp_0[read_addr];
end


endmodule


module hashmap (
reset,
clock,
set,
key,
datain,
operation,
dataout,
error,
ready,
//for modelsim
key_di,
key_do,
value_di,
value_do,
write_addr,
write_enable,
compare,
read_addr,
state,
num,
op_cached
); 

input reset;
input clock;
input set;
input [15:0] key;
input [15:0] datain;
input [1:0] operation;
output [15:0] dataout;
output [3:0] error;
output ready;


output [15:0] key_di,value_di,key_do,value_do;
output [7:0] write_addr,read_addr,num;
output write_enable;
output [3:0] state;
output [1:0] compare,op_cached;

reg [3:0] state;
reg ready;
reg [3:0] error;
reg [1:0] op_cached;
reg [15:0] key_cached;
reg [15:0] di_cached;
reg [7:0] num;
reg [7:0] read_addr;
reg [7:0] write_addr;
reg write_enable;
reg [15:0] dataout;
reg [15:0] key_di,value_di;
wire [15:0] key_do,value_do;
wire [1:0] compare;

ram k_ram(clock,write_enable,read_addr,write_addr,key_di,key_do);
ram v_ram(clock,write_enable,read_addr,write_addr,value_di,value_do);


assign compare = (key_cached < key_do)?1:((key_cached == key_do)?2:3);

always@(posedge clock) begin
	if (reset) begin
		num <= 0;
		state <= 0;
		ready <= 1;
		error <=0;
			
	end 
	else if (set) begin
		ready <= 0;
		error <= 0;
		write_addr <= 0;
		write_enable <= 0;
		op_cached <= operation;
		key_cached <= key;
		di_cached <= datain;
		read_addr <= 1;
		case(operation)
			0:begin
				state <= 1;
		    end
			1:begin
			
				state <= 3;
		    end
			2:begin
			
				state <= 5;
		    end
			3:begin
		
				state <= 9;
		    end
		endcase
	end
	else if (!ready) begin
		case(state)
			1:begin
				if (read_addr > num | compare == 1) begin
					error <= 1;
					ready <= 1;
				end else if (compare == 2) begin
					state <= 2;
				end else begin		
					write_enable <= 0;
					read_addr <= read_addr + 1;
				end
			end
			2:begin	
				dataout <= value_do;
				ready <= 1;
				write_enable <= 0;
				
			end
			3:begin
				if (compare == 1 | read_addr >num) begin
					error <= 1;
					ready <= 1;
				end else if (compare == 2) begin
					state <= 4;
					write_enable <= 1;
					write_addr <= read_addr;
					key_di <= key_cached;
					value_di <= di_cached;
				end else begin
					write_enable <= 0;
					read_addr <= read_addr + 1;
				end
			end
			4:begin
				dataout <= value_do;
				ready <= 1;
				write_enable <= 0;
			end
			5:begin
				if (read_addr > num ) begin
					state <= 8;
					write_enable <= 1;
					write_addr <= read_addr;
					key_di <= key_cached;
					value_di <= di_cached;
				end else if (compare == 1) begin
					state <= 6;
					write_enable <= 1;
					write_addr <= read_addr;
					key_di <= key_cached;
					value_di <= di_cached;
				end else if (compare == 2) begin
					state <= 7;
					write_enable <= 1;
					write_addr <= read_addr;
					key_di <= key_cached;
					value_di <= di_cached;
				end else begin
					write_enable <= 0;
					read_addr <= read_addr + 1;
				end
			end
			6:begin 
				if (read_addr == num) begin
					state <= 8;
					write_enable <= 1;
					write_addr <= read_addr + 1;
					key_di <= key_do;
					value_di <= value_do;
				end else begin
					write_enable <= 1;
					write_addr <= read_addr + 1;
					key_di <= key_do;
					value_di <= value_do;
					read_addr <= read_addr + 1;
				end
			end 
			7:begin
				dataout <= value_do;
				ready <= 1;
				write_enable <= 0;
			end
			8:begin
				dataout <= value_do;
				num <= num + 1;
				ready <= 1;
				write_enable <= 0;
			end
			9:begin
				if (compare == 1 | read_addr > num) begin
					error <= 1;
					ready <= 1;
				end else if (compare == 2) begin
					state <= 10;
					write_enable <= 0;
					read_addr <= read_addr + 1;
				end	else begin
				  read_addr <= read_addr + 1;
				  write_enable <= 0;
				end
			end
			10:begin
				if (read_addr > num) begin 
					state <= 11;
					write_enable <= 0;
				end else begin 
					write_enable <= 1;
					write_addr <= read_addr - 1;
					key_di <= key_do;
					value_di <= value_do;
					read_addr <= read_addr + 1;
				end
			end
			11:begin
				num <= num - 1;
				ready <= 1;
				write_enable <= 0;
			end
		endcase
	end
end

endmodule





