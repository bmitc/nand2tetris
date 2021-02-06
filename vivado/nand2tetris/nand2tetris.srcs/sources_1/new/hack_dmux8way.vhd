----------------------------------------------------------------------------------
-- Design Name: DMux8Way
-- Module Name: hack_dmux8way - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If      sel=000 then {a=in, b=c=d=e=f=g=h=0}
--              else if sel=001 then {b=in, a=c=d=e=f=g=h=0}
--              else if sel=010 then ...
--              ...
--              else if sel=111 then {h=in, a=b=c=d=e=f=g=0}.
-- 
-- Dependencies: hack_dmux, hack_dmux4way
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_dmux8way is
    port ( input    : in  STD_LOGIC;
           selector : in  STD_LOGIC_VECTOR (2 downto 0);
           a        : out STD_LOGIC;
           b        : out STD_LOGIC;
           c        : out STD_LOGIC;
           d        : out STD_LOGIC;
           e        : out STD_LOGIC;
           f        : out STD_LOGIC;
           g        : out STD_LOGIC;
           h        : out STD_LOGIC);
end hack_dmux8way;

architecture gate_level of hack_dmux8way is

    signal s0 : STD_LOGIC;
    signal s1 : STD_LOGIC;

begin

    use_dmux: entity work.hack_dmux
        port map (input    => input,
                  selector => selector(2),
                  a        => s0,
                  b        => s1);
              
    use_dmux4way0: entity work.hack_dmux4way
        port map (input    => s0,
                  selector => selector(1 downto 0),
                  a        => a,
                  b        => b,
                  c        => c,
                  d        => d);
                  
    use_dmux4way1: entity work.hack_dmux4way
        port map (input    => s1,
                  selector => selector(1 downto 0),
                  a        => e,
                  b        => f,
                  c        => g,
                  d        => h);
                  
end gate_level;