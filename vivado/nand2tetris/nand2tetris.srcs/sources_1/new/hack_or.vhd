----------------------------------------------------------------------------------
-- Design Name: Or
-- Module Name: hack_or - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If a=b=0 then output=0 else output=1.
-- 
-- Dependencies: hack_nand, hack_not
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_or is
    port ( a      : in  STD_LOGIC;
           b      : in  STD_LOGIC;
           output : out STD_LOGIC);
end hack_or;

architecture gate_level of hack_or is

    signal s0 : STD_LOGIC;
    signal s1 : STD_LOGIC;

begin

    use_not0: entity work.hack_not
        port map ( input  => a,
                   output => s0);
    use_not1: entity work.hack_not
        port map ( input  => b,
                   output => s1);
    use_nand: entity work.hack_nand
        port map ( a      => s0,
                   b      => s1,
                   output => output);
    
end gate_level;