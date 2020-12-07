----------------------------------------------------------------------------------
-- Design Name: DMux
-- Module Name: hack_dmux - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If selector=0 then {a=input, b=0} else {a=0, b=input}.
-- 
-- Dependencies: hack_not, hack_and
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_dmux is
    port ( input    : in  STD_LOGIC;
           selector : in  STD_LOGIC;
           a        : out STD_LOGIC;
           b        : out STD_LOGIC);
end hack_dmux;

architecture gate_level of hack_dmux is

    signal s0 : STD_LOGIC;

begin

    use_not:  entity work.hack_not
        port map ( input  => selector,
                   output => s0);
    use_and0: entity work.hack_and
        port map ( a      => s0,
                   b      => input,
                   output => a);
    use_and1: entity work.hack_and
        port map ( a      => selector,
                   b      => input,
                   output => b);

end gate_level;