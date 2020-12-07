----------------------------------------------------------------------------------
-- Design Name: And
-- Module Name: hack_and - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If a=b=1 then output=1 else output=0.
-- 
-- Dependencies: hack_nand, hack_not
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_and is
    port ( a      : in  STD_LOGIC;
           b      : in  STD_LOGIC;
           output : out STD_LOGIC);
end hack_and;

architecture gate_level of hack_and is

    signal s0 : STD_LOGIC;

begin

    use_nand: entity work.hack_nand
        port map ( a      => a,
                   b      => b,
                   output => s0);
    use_not:  entity work.hack_not
        port map ( input  => s0,
                   output => output);
    
end gate_level;