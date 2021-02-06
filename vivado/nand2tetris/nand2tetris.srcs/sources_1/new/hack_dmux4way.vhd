----------------------------------------------------------------------------------
-- Design Name: DMux4Way
-- Module Name: hack_dmux4way - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If      sel=00 then {a=in, b=c=d=0}
--              else if sel=01 then {b=in, a=c=d=0}
--              else if sel=10 then {c=in, a=b=d=0}
--              else if sel=11 then {d=in, a=b=c=0}.
-- 
-- Dependencies: hack_dmux
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_dmux4way is
    port ( input    : in  STD_LOGIC;
           selector : in  STD_LOGIC_VECTOR (1 downto 0);
           a        : out STD_LOGIC;
           b        : out STD_LOGIC;
           c        : out STD_LOGIC;
           d        : out STD_LOGIC);
end hack_dmux4way;

architecture gate_level of hack_dmux4way is

    signal s0 : STD_LOGIC;
    signal s1 : STD_LOGIC;

begin

    use_dmux0: entity work.hack_dmux
        port map (input    => input,
                  selector => selector(1),
                  a        => s0,
                  b        => s1);
              
    use_dmux1: entity work.hack_dmux
        port map (input    => s0,
                  selector => selector(0),
                  a        => a,
                  b        => b);
                  
    use_dmux2: entity work.hack_dmux
        port map (input    => s1,
                  selector => selector(0),
                  a        => c,
                  b        => d);
                  
end gate_level;