----------------------------------------------------------------------------------
-- Design Name: Or16
-- Module Name: hack_or16 - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: For i=0..15 output[i]=Or(a[i],b[i]).
-- 
-- Dependencies: hack_or
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_or16 is
    Port ( a      : in  STD_LOGIC_VECTOR (15 downto 0);
           b      : in  STD_LOGIC_VECTOR (15 downto 0);
           output : out STD_LOGIC_VECTOR (15 downto 0));
end hack_or16;

architecture gate_level of hack_or16 is

begin

    generate_or16:
    for i in 0 to 15 generate
        use_or: entity work.hack_or
            port map ( a      => a(i),
                       b      => b(i),
                       output => output(i));
    end generate;

end gate_level;