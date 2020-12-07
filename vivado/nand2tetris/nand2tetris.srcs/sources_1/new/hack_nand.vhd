----------------------------------------------------------------------------------
-- Design Name: Nand
-- Module Name: hack_nand - gate_level
-- Project Name: Nand2Tetris
-- Target Devices: Nexys A7-100T
-- Tool Versions: Vivado 2020.2
-- Description: If a=b=1 then output=0 else output=1.
-- 
-- Dependencies: <none>
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity hack_nand is
    port ( a      : in  STD_LOGIC;
           b      : in  STD_LOGIC;
           output : out STD_LOGIC);
end hack_nand;

architecture gate_level of hack_nand is

begin

    output <= a nand b;

end gate_level;