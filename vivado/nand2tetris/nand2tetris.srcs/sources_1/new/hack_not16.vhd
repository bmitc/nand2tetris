----------------------------------------------------------------------------------
-- Design Name: Not16
-- Module Name: hack_not16 - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: For i=0..15 output[i]=Not(input[i]).
-- 
-- Dependencies: hack_not
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_not16 is
    Port ( input  : in  STD_LOGIC_VECTOR (15 downto 0);
           output : out STD_LOGIC_VECTOR (15 downto 0));
end hack_not16;

architecture gate_level of hack_not16 is

begin

    generate_not16:
    for i in 0 to 15 generate
        use_not: entity work.hack_not
            port map ( input  => input(i),
                       output => output(i));
    end generate;

end gate_level;