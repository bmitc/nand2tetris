----------------------------------------------------------------------------------
-- Design Name: DFF
-- Module Name: hack_dff - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: output(t) = input(t-1)
-- 
-- Dependencies: rising_edge
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_dff is
    port ( input  : in  STD_LOGIC;
           clock  : in  STD_LOGIC;
           output : out STD_LOGIC);
end hack_dff;

architecture gate_level of hack_dff is

begin

    process (clock)
    begin
        if rising_edge(clock) then
            output <= input;
        end if;
    end process;

end gate_level;