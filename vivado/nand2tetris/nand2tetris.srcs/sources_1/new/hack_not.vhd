----------------------------------------------------------------------------------
-- Design Name: Not
-- Module Name: hack_not - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If input=0 then output=1 else output=0.
-- 
-- Dependencies: hack_nand
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_not is
    port ( input  : in  STD_LOGIC;
           output : out STD_LOGIC);
end hack_not;

architecture gate_level of hack_not is

begin

    use_nand: entity work.hack_nand
        port map ( a      => input,
                   b      => input,
                   output => output);
    
end gate_level;