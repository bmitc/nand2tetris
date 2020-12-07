----------------------------------------------------------------------------------
-- Design Name: Mux16
-- Module Name: hack_mux16 - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If selector=0 then for i=0..15 output[i]=a[i]
--              else for i=0..15 output[i]=b[i].
-- 
-- Dependencies: hack_mux
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_mux16 is
    Port ( a        : in  STD_LOGIC_VECTOR (15 downto 0);
           b        : in  STD_LOGIC_VECTOR (15 downto 0);
           selector : in  STD_LOGIC;
           output   : out STD_LOGIC_VECTOR (15 downto 0));
end hack_mux16;

architecture gate_level of hack_mux16 is

begin

    generate_mux16:
    for i in 0 to 15 generate
        use_mux: entity work.hack_mux
            port map ( a        => a(i),
                       b        => b(i),
                       selector => selector,
                       output   => output(i));
    end generate;

end gate_level;