----------------------------------------------------------------------------------
-- Design Name: Or8Way
-- Module Name: hack_or8way - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: output=OR(input[0],input[1],...,input[7]).
-- 
-- Dependencies: hack_or
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_or8way is
    port ( input  : in  STD_LOGIC_VECTOR (7 downto 0);
           output : out STD_LOGIC);
end hack_or8way;

architecture gate_level of hack_or8way is

    signal s0 : STD_LOGIC;
    signal s1 : STD_LOGIC;
    signal s2 : STD_LOGIC;
    signal s3 : STD_LOGIC;
    signal s4 : STD_LOGIC;
    signal s5 : STD_LOGIC;

begin

    use_or0: entity work.hack_or
        port map ( a      => input(0),
                   b      => input(1),
                   output => s0);
                   
    use_or1: entity work.hack_or
        port map ( a      => input(2),
                   b      => input(3),
                   output => s1);
                   
    use_or2: entity work.hack_or
        port map ( a      => input(4),
                   b      => input(5),
                   output => s2);

    use_or3: entity work.hack_or
        port map ( a      => input(6),
                   b      => input(7),
                   output => s3);
                   
    use_or4: entity work.hack_or
        port map ( a      => s0,
                   b      => s1,
                   output => s4);
                   
    use_or5: entity work.hack_or
        port map ( a      => s2,
                   b      => s3,
                   output => s5);
                   
    use_or6: entity work.hack_or
        port map ( a      => s4,
                   b      => s5,
                   output => output);

end gate_level;