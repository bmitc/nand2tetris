----------------------------------------------------------------------------------
-- Design Name: Xor
-- Module Name: hack_xor - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If a!=b then output=1 else output=0.
-- 
-- Dependencies: hack_not, hack_and, hack_or
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_xor is
    port ( a      : in  STD_LOGIC;
           b      : in  STD_LOGIC;
           output : out STD_LOGIC);
end hack_xor;

architecture gate_level of hack_xor is

    signal s0 : STD_LOGIC;
    signal s1 : STD_LOGIC;
    signal s2 : STD_LOGIC;

begin

    use_and0: entity work.hack_and
        port map ( a      => a,
                   b      => b,
                   output => s0);
    use_not:  entity work.hack_not
        port map ( input  => s0,
                   output => s1);
    use_or:   entity work.hack_or
        port map ( a      => a,
                   b      => b,
                   output => s2);
    use_and1: entity work.hack_and
        port map ( a      => s1,
                   b      => s2,
                   output => output);

end gate_level;