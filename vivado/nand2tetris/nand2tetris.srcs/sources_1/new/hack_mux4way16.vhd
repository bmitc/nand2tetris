----------------------------------------------------------------------------------
-- Design Name: Mux4Way16
-- Module Name: hack_mux4way16 - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If sel=00 then out=a else if sel=01 then out=b
--              else if sel=10 then out=c else if sel=11 then out=d.
-- 
-- Dependencies: hack_mux16
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_mux4way16 is
    port ( a        : in  STD_LOGIC_VECTOR (15 downto 0);
           b        : in  STD_LOGIC_VECTOR (15 downto 0);
           c        : in  STD_LOGIC_VECTOR (15 downto 0);
           d        : in  STD_LOGIC_VECTOR (15 downto 0);
           selector : in  STD_LOGIC_VECTOR ( 1 downto 0);
           output   : out STD_LOGIC_VECTOR (15 downto 0));
end hack_mux4way16;

architecture gate_level of hack_mux4way16 is

    signal s0 : STD_LOGIC_VECTOR (15 downto 0);
    signal s1 : STD_LOGIC_VECTOR (15 downto 0);

begin

    use_mux16_0: entity work.hack_mux16
        port map ( a        => a,
                   b        => c,
                   selector => selector(1),
                   output   => s0);

    use_mux16_1: entity work.hack_mux16
        port map ( a        => b,
                   b        => d,
                   selector => selector(1),
                   output   => s1);
                   
    use_mux16_2: entity work.hack_mux16
        port map ( a        => s0,
                   b        => s1,
                   selector => selector(0),
                   output   => output);
                   
end gate_level;