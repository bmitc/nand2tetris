----------------------------------------------------------------------------------
-- Design Name: Mux8Way16
-- Module Name: hack_mux8way16 - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If      selector=000 then output=a
--              else if selector=001 then output=b
--              else if selector=010 then output=c
--              ...
--              else if selector=111 then output=h.
-- 
-- Dependencies: hack_mux16, hack_mux4way16
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_mux8way16 is
    port ( a        : in  STD_LOGIC_VECTOR (15 downto 0);
           b        : in  STD_LOGIC_VECTOR (15 downto 0);
           c        : in  STD_LOGIC_VECTOR (15 downto 0);
           d        : in  STD_LOGIC_VECTOR (15 downto 0);
           e        : in  STD_LOGIC_VECTOR (15 downto 0);
           f        : in  STD_LOGIC_VECTOR (15 downto 0);
           g        : in  STD_LOGIC_VECTOR (15 downto 0);
           h        : in  STD_LOGIC_VECTOR (15 downto 0);
           selector : in  STD_LOGIC_VECTOR ( 2 downto 0);
           output   : out STD_LOGIC_VECTOR (15 downto 0));
end hack_mux8way16;

architecture gate_level of hack_mux8way16 is

    signal s0 : STD_LOGIC_VECTOR (15 downto 0);
    signal s1 : STD_LOGIC_VECTOR (15 downto 0);
    signal s2 : STD_LOGIC_VECTOR (15 downto 0);
    signal s3 : STD_LOGIC_VECTOR (15 downto 0);

begin

    use_mux16_0: entity work.hack_mux16
        port map ( a        => a,
                   b        => e,
                   selector => selector(2),
                   output   => s0);

    use_mux16_1: entity work.hack_mux16
        port map ( a        => b,
                   b        => f,
                   selector => selector(2),
                   output   => s1);
                   
    use_mux16_2: entity work.hack_mux16
        port map ( a        => c,
                   b        => g,
                   selector => selector(2),
                   output   => s2);
                   
    use_mux16_3: entity work.hack_mux16
        port map ( a        => d,
                   b        => h,
                   selector => selector(2),
                   output   => s3);
                   
    use_mux4way16: entity work.hack_mux4way16
        port map ( a        => s0,
                   b        => s1,
                   c        => s2,
                   d        => s3,
                   selector => selector(1 downto 0),
                   output   => output);

end gate_level;