----------------------------------------------------------------------------------
-- COPYRIGHT 2019 Jes�s Eduardo M�ndez Rosales
--This program is free software: you can redistribute it and/or modify
--it under the terms of the GNU General Public License as published by
--the Free Software Foundation, either version 3 of the License, or
--(at your option) any later version.
--
--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU General Public License for more details.
--
--You should have received a copy of the GNU General Public License
--along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
--
--							LIBRER�A LCD
--
-- Descripci�n: Con �sta librer�a podr�s implementar c�digos para una LCD 16x2 de manera
-- f�cil y r�pida, con todas las ventajas de utilizar una FPGA.
--
-- Caracter�sticas:
-- 
--	Los comandos que puedes utilizar son los siguientes:
--
-- LCD_INI() -> Inicializa la lcd.
--		 			 NOTA: Dentro de los par�ntesis poner un vector de 2 bits para encender o apagar
--					 		 el cursor y activar o desactivar el parpadeo.
--
--		"1x" -- Cursor ON
--		"0x" -- Cursor OFF
--		"x1" -- Parpadeo ON
--		"x0" -- Parpadeo OFF
--
--   Por ejemplo: LCD_INI("10") -- Inicializar LCD con cursor encendido y sin parpadeo 
--	
--			
-- CHAR() -> Manda una letra may�scula o min�scula
--
--				 IMPORTANTE: 1) Debido a que VHDL no es sensible a may�sculas y min�sculas, si se quiere
--								    escribir una letra may�scula se debe escribir una "M" antes de la letra.
--								 2) Si se quiere escribir la letra "S" may�scula, se declara "MAS"
--											
-- 	Por ejemplo: CHAR(A)  -- Escribe en la LCD la letra "a"
--						 CHAR(MA) -- Escribe en la LCD la letra "A"	
--						 CHAR(S)	 -- Escribe en la LCD la letra "s"
--						 CHAR(MAS)	 -- Escribe en la LCD la letra "S"	
--	
--
-- POS() -> Escribir en la posici�n que se indique.
--				NOTA: Dentro de los par�ntesis se dene poner la posici�n de la LCD a la que se quiere ir, empezando
--						por el rengl�n seguido de la posici�n vertical, ambos n�meros separados por una coma.
--		
--		Por ejemplo: POS(1,2) -- Manda cursor al rengl�n 1, poscici�n 2
--						 POS(2,4) -- Manda cursor al rengl�n 2, poscici�n 4		
--
--
-- CHAR_ASCII() -> Escribe un caracter a partir de su c�digo en ASCII
--						 NOTA: Dentro de los parentesis escribir x"(n�mero hex.)"
--
--		Por ejemplo: CHAR_ASCII(x"40") -- Escribe en la LCD el caracter "@"
--
--
-- CODIGO_FIN() -> Finaliza el c�digo. 
--						 NOTA: Dentro de los par�ntesis poner cualquier n�mero: 1,2,3,4...,8,9.
--
--
-- BUCLE_INI() -> Indica el inicio de un bucle. 
--						NOTA: Dentro de los par�ntesis poner cualquier n�mero: 1,2,3,4...,8,9.
--
--
-- BUCLE_FIN() -> Indica el final del bucle.
--						NOTA: Dentro de los par�ntesis poner cualquier n�mero: 1,2,3,4...,8,9.
--
--
-- INT_NUM() -> Escribe en la LCD un n�mero entero.
--					 NOTA: Dentro de los par�ntesis poner s�lo un n�mero que vaya del 0 al 9,
--						    si se quiere escribir otro n�mero entero se tiene que volver
--							 a llamar la funci�n
--
--
-- CREAR_CHAR() -> Funci�n que crea el caracter dise�ado previamente en "CARACTERES_ESPECIALES.vhd"
--                 NOTA: Dentro de los par�ntesis poner el nombre del caracter dibujado (CHAR1,CHAR2,CHAR3,..,CHAR8)
--								 
--
-- CHAR_CREADO() -> Escribe en la LCD el caracter creado por medio de la funci�n "CREAR_CHAR()"
--						  NOTA: Dentro de los par�ntesis poner el nombre del caracter creado.
--
--     Por ejemplo: 
--
--				Dentro de CARACTERES_ESPECIALES.vhd se dibujan los caracteres personalizados utilizando los vectores 
--				"CHAR_1", "CHAR_2","CHAR_3",...,"CHAR_7","CHAR_8"
--
--								 '1' => [#] - Se activa el pixel de la matr�z.
--                       '0' => [ ] - Se desactiva el pixel de la matriz.
--
-- 			Si se quiere crear el				Entonces CHAR_1 queda de la siguiente
--				siguiente caracter:					manera:
--												
--				  1  2  3  4  5						CHAR_1 <=
--  		  1 [ ][ ][ ][ ][ ]							"00000"&			
-- 		  2 [ ][ ][ ][ ][ ]							"00000"&			  
-- 		  3 [ ][#][ ][#][ ]							"01010"&   		  
-- 		  4 [ ][ ][ ][ ][ ]		=====>			"00000"&			   
-- 		  5 [#][ ][ ][ ][#]							"10001"&          
-- 		  6 [ ][#][#][#][ ]							"01110"&			  
-- 		  7 [ ][ ][ ][ ][ ]							"00000"&			  
-- 		  8 [ ][ ][ ][ ][ ]							"00000";			
--
--		
--			Como el caracter se cre� en el vector "CHAR_1",entonces se escribe en las funci�nes como "CHAR1"
--			
--			CREAR_CHAR(CHAR1)  -- Crea el caracter personalizado (CHAR1)
--			CHAR_CREADO(CHAR1) -- Muestra en la LCD el caracter creado (CHAR1)		
--
-- 
--
-- LIMPIAR_PANTALLA() -> Manda a limpiar la LCD.
--								 NOTA: �sta funci�n se activa poniendo dentro de los par�ntesis
--										 un '1' y se desactiva con un '0'. 
--
--		Por ejemplo: LIMPIAR_PANTALLA('1') -- Limpiar pantalla est� activado.
--						 LIMPIAR_PANTALLA('0') -- Limpiar pantalla est� desactivado.
--
--
--	Con los puertos de entrada "CORD" y "CORI" se hacen corrimientos a la derecha y a la
--	izquierda respectivamente. NOTA: La velocidad del corrimiento se puede cambiar 
-- modificando la variable "DELAY_COR".
--
-- Algunas funci�nes generan un vector ("BLCD") cuando se termin� de ejecutar dicha funci�n y
--	que puede ser utilizado como una bandera, el vector solo dura un ciclo de instruccion.
--	   
--		LCD_INI() ---------- BLCD <= x"01"
--		CHAR() ------------- BLCD <= x"02"
--		POS() -------------- BLCD <= x"03"
-- 	INT_NUM() ---------- BLCD <= x"04"
--	   CHAR_ASCII() ------- BLCD <= x"05"
--	   BUCLE_INI() -------- BLCD <= x"06"
--		BUCLE_FIN() -------- BLCD <= x"07"
--		LIMPIAR_PANTALLA() - BLCD <= x"08"
--	   CREAR_CHAR() ------- BLCD <= x"09"
--	 	CHAR_CREADO() ------ BLCD <= x"0A"
--
--
--		�IMPORTANTE!
--		
--		1) Se deber� especificar el n�mero de instrucciones en la constante "NUM_INSTRUCCIONES". El valor 
--			de la �ltima instrucci�n es el que se colocar�
--		2) En caso de utilizar a la librer�a como TOP del dise�o, se deber� comentar el puerto gen�rico y 
--			descomentar la constante "FPGA_CLK" para especificar la frecuencia de reloj.
--		3) Cada funci�n se acompa�a con " INST(NUM) <= <FUNCI�N> " como lo muestra en el c�digo
-- 		demostrativo.
--
--
--                C�DIGO DEMOSTRATIVO
--
		
--
-- 	INST(0) <= LCD_INI("11"); 		-- INICIALIZAMOS LCD, CURSOR A HOME, CURSOR ON, PARPADEO ON.
-- 	INST(1) <= POS(1,1);				-- EMPEZAMOS A ESCRIBIR EN LA LINEA 1, POSICI�N 1
-- 	INST(2) <= CHAR(MH);				-- ESCRIBIMOS EN LA LCD LA LETRA "h" MAYUSCULA
-- 	INST(3) <= CHAR(O);			
-- 	INST(4) <= CHAR(L);
-- 	INST(5) <= CHAR(A);
-- 	INST(6) <= CHAR_ASCII(x"21"); -- ESCRIBIMOS EL CARACTER "!"
-- 	INST(7) <= CODIGO_FIN(1);	   -- FINALIZAMOS EL CODIGO
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE WORK.COMANDOS_LCD_REVD.ALL;
use ieee.numeric_std.all;

entity LIB_LCD_INTESC_REVD is

GENERIC(
			FPGA_CLK : INTEGER := 100_000_000;
            stable_time : INTEGER := 10
);


PORT(CLK: IN STD_LOGIC;
        
-----------------------------------------------------
------------------PUERTOS DE LA LCD------------------
	  RS 		  : OUT STD_LOGIC;							--
	  RW		  : OUT STD_LOGIC;							--
	  ENA 	  : OUT STD_LOGIC;							--
	  DATA_LCD : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);   --
-----------------------------------------------------
-----------------------------------------------------
	  
	  
-----------------------------------------------------------
--------------ABAJO ESCRIBE TUS PUERTOS--------------------	
	 upb,downb,enterb,reset : in STD_logic

-----------------------------------------------------------
-----------------------------------------------------------

	  );

end LIB_LCD_INTESC_REVD;

architecture Behavioral of LIB_LCD_INTESC_REVD is


CONSTANT NUM_INSTRUCCIONES : INTEGER := 20; 	--INDICAR EL N�MERO DE INSTRUCCIONES PARA LA LCD


--------------------------------------------------------------------------------
-------------------------SE�ALES DE LA LCD (NO BORRAR)--------------------------
																										--
component PROCESADOR_LCD_REVD is																--
																										--
GENERIC(																								--
			FPGA_CLK : INTEGER := 50_000_000;												--
			NUM_INST : INTEGER := 1																--
);																										--
																										--
PORT( CLK 				 : IN  STD_LOGIC;														--
	   VECTOR_MEM 		 : IN  STD_LOGIC_VECTOR(8  DOWNTO 0);							--
	   C1A,C2A,C3A,C4A : IN  STD_LOGIC_VECTOR(39 DOWNTO 0);							--
	   C5A,C6A,C7A,C8A : IN  STD_LOGIC_VECTOR(39 DOWNTO 0);							--
	   RS 				 : OUT STD_LOGIC;														--
	   RW 				 : OUT STD_LOGIC;														--
	   ENA 				 : OUT STD_LOGIC;														--
	   BD_LCD 			 : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);			         	--
	   DATA 				 : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);							--
	   DIR_MEM 			 : OUT INTEGER RANGE 0 TO NUM_INSTRUCCIONES					--
	);																									--
																										--
end component PROCESADOR_LCD_REVD;															--
																										--
COMPONENT CARACTERES_ESPECIALES_REVD is													--
																										--
PORT( C1,C2,C3,C4 : OUT STD_LOGIC_VECTOR(39 DOWNTO 0);								--
		C5,C6,C7,C8 : OUT STD_LOGIC_VECTOR(39 DOWNTO 0)									--
	 );																								--
																										--
end COMPONENT CARACTERES_ESPECIALES_REVD;													--
																										--
CONSTANT CHAR1 : INTEGER := 1;																--
CONSTANT CHAR2 : INTEGER := 2;																--
CONSTANT CHAR3 : INTEGER := 3;																--
CONSTANT CHAR4 : INTEGER := 4;																--
CONSTANT CHAR5 : INTEGER := 5;																--
CONSTANT CHAR6 : INTEGER := 6;																--
CONSTANT CHAR7 : INTEGER := 7;																--
CONSTANT CHAR8 : INTEGER := 8;																--
																										--
type ram is array (0 to  NUM_INSTRUCCIONES) of std_logic_vector(8 downto 0); 	--
signal INST : ram := (others => (others => '0'));										--
																										--
signal blcd 			  : std_logic_vector(7 downto 0):= (others => '0');		--																										
signal vector_mem 	  : STD_LOGIC_VECTOR(8  DOWNTO 0) := (others => '0');		--
signal c1s,c2s,c3s,c4s : std_logic_vector(39 downto 0) := (others => '0');		--
signal c5s,c6s,c7s,c8s : std_logic_vector(39 downto 0) := (others => '0'); 	--
signal dir_mem 		  : integer range 0 to NUM_INSTRUCCIONES := 0;				--
																						--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

COMPONENT debounce IS 
    GENERIC(
      clk_freq    : INTEGER;   --system clock frequency in Hz
      stable_time : INTEGER);  --time button must remain stable in ms
    PORT(
      clk     : IN  STD_LOGIC;   --input clock
      reset_n : IN  STD_LOGIC;   --asynchornous active-low reset
      button  : IN  STD_LOGIC;   --input signal to be debounced
      result  : OUT STD_LOGIC);  --debounced signal
  END COMPONENT;
--------------------------------------------------------------------------------
---------------------------AGREGA TUS SE�ALES AQU�------------------------------
-----------------------------Funciones---------------------------
--AdderSubstractor
procedure add (a: in std_logic_vector;
               b: in std_logic_vector;
               ctr: in std_logic;
               Z: out std_logic_vector;
               overflow: out std_logic;
               carry: out std_logic)is
    variable sum: std_logic_vector(a'length downto 0);
    variable carryv,bs: std_logic;

begin
   
    sum := (others => '0');
    carryv := ctr;

    for i in 0 to a'length-1 loop
        bs:= ctr xor b(i);
        sum(i) := a(i) xor bs xor carryv;
        carryv := (a(i) and bs) or (a(i) and carryv) or (bs and carryv);
    end loop;
    Z:= sum(a'length-1 downto 0);
    carry:=carryv;
    overflow:= carryv xor sum(a'length-1);
    
end procedure add;
procedure multiply(
    a : in std_logic_vector; 
    b : in std_logic_vector;
    m : out std_logic_vector) is
    variable pv : std_logic_vector(a'length+b'length-1 downto 0);
    variable bp : std_logic_vector(a'length+b'length-1 downto 0);
    variable carry : std_logic;
    variable ov : std_logic;
begin
    pv := (others => '0');
    bp := "00000000"&b;
    for i in 0 to a'length-1 loop
        if a(i) = '1' then
            add(pv, bp, '0',pv,ov,carry);
        end if;
        bp := bp(a'length+b'length-2 downto 0) & '0';
    end loop;
    m := pv;
end procedure multiply;
procedure div8(
    numer: in std_logic_vector(15 downto 0);
    denom: in std_logic_vector(7 downto 0);
    quotient: out std_logic_vector(7 downto 0);
    remainder: out std_logic_vector(7 downto 0)
) is 
variable d, n1: std_logic_vector(8 downto 0);
variable n2: std_logic_vector(7 downto 0);
variable carry: std_logic;
variable overflow: std_logic;
begin
    d := '0' & denom;
    n2 := numer(7 downto 0);
    n1 := '0' & numer(15 downto 8);
    
    for i in 0 to 7 loop
        n1 := n1(7 downto 0) & n2(7);
        n2 := n2(6 downto 0) & '0';
        
        if n1 >= d then
            add(n1, d, '1', n1, carry, overflow);
            n2(0) := '1';
        end if;
    end loop;
    
    quotient := n2;
    remainder := n1(7 downto 0);
end procedure;

procedure div16(
    a: in std_logic_vector(15 downto 0);
    b: in std_logic_vector(7 downto 0);
    d: out std_logic_vector(15 downto 0)) is
variable remh, reml, quoth, qoutl: std_logic_vector(7 downto 0);
begin
    div8("00000000" & a(15 downto 8), b, quoth, remh);
    div8(remh & a(7 downto 0), b, qoutl, reml);
    d(15 downto 8) := quoth;
    d(7 downto 0) := qoutl;
end procedure;

procedure comp1 (
    x: in std_logic;
    y: in std_logic;
    variable gin: in std_logic;
    variable lin: in std_logic;
    variable gout: out std_logic;
    variable lout: out std_logic;
    variable eout: out std_logic
) is

begin
    gout:= (x and not y) or (x and gin) or (not y and gin);
    eout:=(not x and not y and not gin and not lin)or(x and y and not gin and not lin);
    lout:=(not x and y) or (not x and lin) or (y and lin);

end procedure;

procedure comparer (
    variable a : in std_logic_vector;
    variable b : in std_logic_vector;
    signal enable: in std_logic;
    variable gt : out std_logic;
    variable eq : out std_logic;
    variable lt : out std_logic
)is
    variable g,l,e: std_logic_vector(a'length downto 0);

begin
 if(enable = '1')then
    g(0):='0';
    l(0):='0';
    for i in 0 to a'length-1 loop
        comp1(a(i),b(i),g(i),l(i),g(i+1),l(i+1),e(i+1));
    end loop;
    eq:=e(a'length);
    gt:=g(a'length);
    lt:=l(a'length);
    else
    eq:='0';
    gt:='0';
    lt:='0';
    end if;
end procedure;



procedure alu ( a: in std_logic_vector(15 downto 0); 
                b: in std_logic_vector(15 downto 0); 
                F: in std_logic_vector(3 downto 0); 
                Z: out std_logic_vector(15 downto 0); 
                carry : out std_logic;
                overflow: out std_logic;
                Zero: out std_logic; 
                Neg: out std_logic) is
    variable zfv: std_logic;
    variable Zv: std_logic_vector(15 downto 0);
    begin
        carry:='0';
        overflow:='0';
        zfv:='0';
        case F is
            when "0001" => Zv := not a;
            when "0010" => add("0000000000000000",a,'1',Zv,overflow,carry);-- complemento a 2
            when "0011" => Zv := a and b;
            when "0100" => Zv := a or b;
            when "0101" => Zv := a(14 downto 0) & '0';
            when "0110" => Zv := a(a'length-1)&a(a'length-1 downto 1);
            when "0111" => add(a,b,'0',Zv,overflow,carry);--suma
            when "1000" => add(a,b,'1',Zv,overflow,carry);--resta
            when "1001" => multiply(a(7 downto 0), b(7 downto 0),Zv);--multiplicacion 8 bits
            when "1010" => 
            div16(a,b(7 downto 0),Zv); --division 16/8 bits
         
            when others => Zv := (others=>'0');
        end case;
        for i in 0 to 15 loop
            zfv:= zfv or Zv(i);
        end loop;
        Zero:= not zfv;
        Neg:= Zv(15);
        Z := Zv;
    end procedure alu;
    
------------------Maquina de estadoos-----------------------

type ROM_MEMORY_array is array(0 to 275) of std_logic_vector(15 downto 0);
constant Content: ROM_MEMORY_array:=(
--  0 => "01100001000000000",--a,
--  1 => "01100010000000000",--b,
--  2 => "01110010000000000",--r,
--  3 => "01100000000000001",--a,
--  4 => "01100011000000000",--c,
--  5 => "01100001000000000",--a,
--  6 => "01100100000000000",--d,
--  7 => "01100001000000000",--a,
--  8 => "01100010000000000",--b,
--  9 => "01110010000000000",--r,
--  10 => "01100001000000000",-- a,
--  11 => "000000000000000000",--,
--  12 => "000000000000000000",--,
--  13 => "000000000000000000",--,
--  14 => "000000000000000000",--,
--  15 => "000000000000000000",--,
--  12 => "01100011000000000",--c,
--  13 => "01101111000000000",--o,
--  14 => "01101101000000000",--m,
--  15 => "01110000000000000",--p,
--  16 => "01110101000000000",--u,
--  17 => "01110100000000000",--t,
--  18 => "01100001000000000",--a,
--  19 => "01100100000000000",--d,
--  20 => "01101111000000000",--o,
--  21 => "01110010000000000",--r,
--  22 => "01100001000000000",--a,
--  23 => "000000000000000000",--,
--  24 => "01101000000000000",--h,
--  25 => "01101111000000000",--o,
--  26 => "01111100000000000",--l,
--  27 => "01100001000000000",--a,
  -- 28 => "00000000000000000", -- ,
  29 => "1101101000000000", -- SEND DISP "inicio" 
  30 => "1011011000000001", -- LOAD DIRECTO R3 -> 1
  31 => "0000001000000000", -- SEND R3 -> ACC
  32 => "0111000000000000", -- ADD R1 + ACC store R1
  33 => "0000010000001010", -- Save r1 in ram(10)
  34 => "1011101100001010", -- LOAD RAM (10) -> R4
  35 => "1111010000000000", -- se queda en halt hasta que se aprieta enter y guarda la letra en R1
  36 => "1111000000000000",
OTHERS => "0000000000000000"
);
type RAM_MEMORY_array is array(0 to 10) of std_logic_vector(15 downto 0);
signal ContentRAM : RAM_MEMORY_array:= (others=>(others=>'0'));
type pospalabra_array is array(0 to 10) of std_logic_vector(7 downto 0);
signal ContentPalabra : pospalabra_array:= (
    1=>"00000000",
    2=>"00001100",
    3=>"00010111"
);

type estados is (init, fetch, decode, load,load1,load2, operation,operation1,endprog,send, cmp, cmp2, intruJMP,jmp,jalr,jalr2,comp,bnc,bnz,bnv,move,bs,ret, bnz2, bs2, leds, leds_clear, cmp_leds, cmp_leds2,cmp_leds3,cmp_leds4,cmp_leds5,save_regE);
signal estado:estados;


signal letra1:std_logic_vector(7 downto 0);
signal letra2:std_logic_vector(7 downto 0);
signal letra3:std_logic_vector(7 downto 0);
signal letra4:std_logic_vector(7 downto 0);
signal letra5:std_logic_vector(7 downto 0);
signal letra6:std_logic_vector(7 downto 0);
signal letra7:std_logic_vector(7 downto 0);
signal letra8:std_logic_vector(7 downto 0);
signal letra9:std_logic_vector(7 downto 0);
signal letra10:std_logic_vector(7 downto 0);
signal letra11:std_logic_vector(7 downto 0);
signal letra12:std_logic_vector(7 downto 0);
signal letra13:std_logic_vector(7 downto 0);
signal letra14:std_logic_vector(7 downto 0);
signal letra15:std_logic_vector(7 downto 0);
signal letra16:std_logic_vector(7 downto 0);
signal letrasel: std_logic_vector(7 DOWNTO 0):="01100001";
signal upsignal,downsignal, entersignal,upbefore,downbefore,enterbefore: std_logic;
signal reset_n:std_logic;
signal vidas:std_logic_vector(7 downto 0):="00000101";
signal rg1,rg2,rg3, rg4, rg5, pcsig,ret_sig,equal,dato_cmp_1,dato_cmp_2: std_logic_vector(15 downto 0);
signal getenterinicial:std_logic:='0';
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


begin

reset_n<=not reset;
---------------------------------------------------------------
-------------------COMPONENTES PARA LCD------------------------
																				 --
u1: PROCESADOR_LCD_REVD													 --
GENERIC map( FPGA_CLK => FPGA_CLK,									 --
				 NUM_INST => NUM_INSTRUCCIONES )						 --
																				 --
PORT map( CLK,VECTOR_MEM,C1S,C2S,C3S,C4S,C5S,C6S,C7S,C8S,RS, --
			 RW,ENA,BLCD,DATA_LCD, DIR_MEM );						 --
																				 --
U2 : CARACTERES_ESPECIALES_REVD 										 --
PORT MAP( C1S,C2S,C3S,C4S,C5S,C6S,C7S,C8S );				 		 --
																				 --
VECTOR_MEM <= INST(DIR_MEM);											 --
																				 --
---------------------------------------------------------------
---------------------------------------------------------------


-------------------------------------------------------------------
---------------ESCRIBE TU C�DIGO PARA LA LCD-----------------------

 	INST(0) <= LCD_INI("11");
    INST(1)  <= LCD_INI("00"); 		-- INICIALIZAMOS LCD, CURSOR A HOME, CURSOR ON, PARPADEO ON.
 	INST(2) <= POS(1,1);				-- EMPEZAMOS A ESCRIBIR EN LA LINEA 1, POSICI�N 1
 	INST(3) <= CHAR(ML);				-- ESCRIBIMOS EN LA LCD LA LETRA "h" MAYUSCULA
 	INST(4) <= CHAR(I);			
 	INST(5) <= CHAR(F);
 	INST(6) <= CHAR(E);
 	INST(7) <= CHAR(S); -- ESCRIBIMOS EL CARACTER "!"
 	INST(8) <= CHAR_ASCII(X"3A"); -- ESCRIBIMOS EL CARACTER ":"
    INST(9) <= POS(1,11);
    INST(10) <= CHAR(S);
    INST(11) <= CHAR(e);
    INST(12) <= CHAR(l);
    INST(13) <= CHAR_ASCII(X"3A"); -- ESCRIBIMOS EL CARACTER ":"
    INST(14)   <=BUCLE_INI(1);
    INST(15) <= POS(1,15);
    INST(16) <= CHAR_ASCII(letrasel); --pinta el caracter en la posicion adecuada
    INST(17) <= POS(1,7);
    INST(18) <= INT_NUM(to_integer(unsigned(vidas)));--pinta las vidas en la posicion adecuada
    INST(19)   <=BUCLE_FIN(1);
    INST(20) <= CODIGO_FIN(1);
-------------------------------------------------------------------
-------------------------------------------------------------------




-------------------------------------------------------------------
--------------------ESCRIBE TU C�DIGO DE VHDL----------------------
debounce_up: debounce
      GENERIC MAP(clk_freq => FPGA_CLK, stable_time => stable_time)
      PORT MAP(clk => clk, reset_n => reset_n, button => upb, result =>upsignal);
debounce_down: debounce
      GENERIC MAP(clk_freq => FPGA_CLK, stable_time => stable_time)
      PORT MAP(clk => clk, reset_n => reset_n, button => downb, result =>downsignal);
debounce_enter: debounce
      GENERIC MAP(clk_freq => FPGA_CLK, stable_time => stable_time)
      PORT MAP(clk => clk, reset_n => reset_n, button => enterb, result =>entersignal);
-------------------------------------------------------------------
-------------------------------------------------------------------

select_letter: process(clk) is

    begin
        if reset='1' then
        letrasel<=x"61";
        getenterinicial<='0';
        elsif rising_edge(clk) then
           
                if upsignal = '0' and upbefore = '1' then
                    if(letrasel<x"7a") then
                        letrasel<=std_logic_vector(to_unsigned(to_integer(unsigned( letrasel )) + 1, 8));
                    else
                        letrasel<=x"61";
                    end if;
                end if;
                upbefore <= upsignal;
            
           
                 if downsignal = '0' and downbefore = '1' then
                    if(letrasel>x"61") then
                        letrasel<=std_logic_vector(to_unsigned(to_integer(unsigned( letrasel )) - 1, 8));
                    else
                        letrasel<=x"7a";
                    end if;
            end if;
                downbefore <= downsignal;
           
           
           
           if entersignal = '0' and enterbefore = '1' then
                    getenterinicial<='1';
            end if;
                enterbefore <= entersignal;
        end if;
    end process;
    
 -- Pseudoaleatory word selector
 pseudoaleatory: process (clk, reset) is
 variable count:integer:=0;
  begin
 if reset='1' then 
    count:=0;
 elsif rising_edge(clk) then
    if getenterinicial='1' then
        ContentRAM(0)<=ContentPalabra(count);
    else
        if(count<3) then
            count:=count+1;
         else
            count:=0;
         end if;
      
 end if;
    end if;

 
 end process;   
    
    
----CPU


process (dato_cmp_1,dato_cmp_2) is
    variable g,l,e: std_logic_vector(dato_cmp_1'length downto 0);
begin
 if(enable_cmp = '1')then
    g(0):='0';
    l(0):='0';
    for i in 0 to 15 loop
        comp1(dato_cmp_1(i),dato_cmp_2(i),g(i),l(i),g(i+1),l(i+1),e(i+1));
    end loop;
    eq_cmp<=e(15);
    gt_cmp<=g(15);
    lt_cmp<=l(15);
    else
    eq_cmp<='0';
    gt_cmp<='0';
    lt_cmp<='0';
    end if;
end process;



process (clk, reset) is
variable actual, sig: estados;

variable pcvar,dato1,dato2: std_logic_vector(15 downto 0);
variable mdr: std_logic_vector(15 downto 0);

variable res: std_logic_vector(15 downto 0);
variable pcreg,ret_pc: std_logic_vector(7 downto 0);
variable mar: std_logic_vector(7 downto 0);
variable resaum: std_logic_vector(11 downto 0);
variable cir: std_logic_vector(7 downto 0);
variable acc,regA,regB,regC,regD,regE,op1: std_logic_vector(15 downto 0):= "0000000000000000";
variable nu1,nu2,nu3,nu4,eq,gt,lt: std_logic;
variable carry :  std_logic;
variable overflow: std_logic;
variable zero: std_logic;
variable Neg: std_logic;
variable num: std_logic_vector(15 downto 0); 
variable validation_regE: std_logic := '0';

begin
if reset='1' then
    actual := init;
elsif rising_edge (clk) then
    
                actual:=sig;
case actual is 
     --              INIT            --
	when init=>
	pcreg:="";---Aqui inicia el programa
	if getenterinicial='0'then
        sig:=init;
    else
	   sig:=fetch;
	end if;
	--            FETCH            --
	when fetch=>
	mar:=pcreg;
	mdr:= Content(to_integer(unsigned(pcreg)));
	cir:= Content(to_integer(unsigned(pcreg)))(15 downto 8);
	alu("00000000"&pcreg,"0000000000000001","0111",pcvar,nu1,nu2,nu3,nu4); -- Aqui se suma el pcreg
	pcreg:=pcvar(7 downto 0);
	sig:=decode;
	
    --              DECODE          --
    when decode=>
    case cir(7 downto 4) is
    
    when "0000" => -- SEND ACC
    mar:=mdr(7 downto 0);
    case cir(3 downto 0) is
        when "0000"=>acc:=regA;-- SEND ACC
        when "0001"=>acc:=regB;
        when "0010"=>acc:=regC;
        when "0011"=>acc:=regD;
        when "0100"=>ContentRAM(to_integer(unsigned(mar)))<=regA;-- carga la RAM con los datos del registro
        when "0101"=>ContentRAM(to_integer(unsigned(mar)))<=regB;
        when "0110"=>ContentRAM(to_integer(unsigned(mar)))<=regC;
        when "0111"=>ContentRAM(to_integer(unsigned(mar)))<=regC;
        when others=>acc:=(others=>'0');
        end case;
    sig:=fetch;
    
    when "0001" => -- NOT A
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "0010" => -- COMP 2
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "0011" => -- AND
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "0100" => -- OR
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "0101" => -- LSR
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "0110" => -- ASR
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "0111" =>     -- ADD
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "1000" => -- SUBTRACT
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "1001" => -- MULTI
    mar:=mdr(7 downto 0);
    sig:=operation;
    
    when "1010" => -- DIVIDE
    mar:=mdr(7 downto 0);
    sig:=operation;
   
    when "1011" => -- LOAD
    mar:=mdr(7 downto 0);
    sig:=load;
    
    when "1100" => -- JUMPS
    sig:=intruJMP;
    
    when "1101" => -- SEND DISP
    mar:=mdr(7 downto 0);
--    if(validation_regE = '1')then
--        sal <= regE;
--    else
--        case cir(3 downto 0) is
--        when "0000"=>sal<=regA;
--        when "0001"=>sal<=regB;
--        when "0010"=>sal<=regC;
--        when "0011"=>sal<=regD;
--        when others=>sal<=(others=>'0');
--        end case;
--    end if;
    
    sig:=send;
    
    when "1110"=> -- COMPARADOR
    sig:=comp;
    
    when "1111" => -- END PROGRAM
    
    sig:=endprog;
    when others=>sig:=init;
    end case;
    
    --             EXECUTE            --
    when intruJMP=>
    case cir(3 downto 0) is
        when "0000"=>sig:=jmp;
        when "0001"=>sig:=jalr;
        when "0010"=>sig:=bnz;
        when "0011"=>sig:=bs;
        when "0100"=>sig:=bnc;
        when "0101"=>sig:=bnv;
        when "0110"=>sig:=ret;
        when "0111"=>sig:=leds;
        when "1000"=>sig:=leds_clear;
        when "1001"=>sig:=cmp_leds;
        when "1010"=>sig:=save_regE;
        when others=>sig:=jmp;
    end case;
    
    
    --              load specific register            ---
    
    when load=>
        mdr:= Content(to_integer(unsigned(mar)));
        sig:=load1;
    when load1=>
        
        case cir(3 downto 0) is 
        when "0000"=>regA:=mdr;--carga desde rom
        when "0001"=>regB:=mdr;
        when "0010"=>regC:=mdr;
        when "0011"=>regD:=mdr;
        when "0100"=>regA:= "00000000"&mar;--carga directa
        when "0101"=>regB:= "00000000"&mar;
        when "0110"=>regC:= "00000000"&mar;
        when "0111"=>regD:= "00000000"&mar;
        when "1000"=>regA:=ContentRAM(to_integer(unsigned(mar)));--carga desde ram
        when "1001"=>regB:=ContentRAM(to_integer(unsigned(mar)));
        when "1010"=>regC:=ContentRAM(to_integer(unsigned(mar)));
        when "1011"=>regD:=ContentRAM(to_integer(unsigned(mar)));
        when "1100"=>
            case mar(7 downto 0) is
                when "00"=>
                regA:=Content(to_integer(unsigned(regA)));--carga de rom
                when "01"=>
                regB:=Content(to_integer(unsigned(regA)));--carga de rom
                when "10"=>
                regC:=Content(to_integer(unsigned(regA)));--carga de rom
                when "11"=>
                regD:=Content(to_integer(unsigned(regA)));--carga de rom
                when others=>null;
            end case;
        
        when "1101"=>
           case mar(7 downto 0) is
                when "00"=>
                regA:=Content(to_integer(unsigned(regB)));--carga de rom
                when "01"=>
                regB:=Content(to_integer(unsigned(regB)));--carga de rom
                when "10"=>
                regC:=Content(to_integer(unsigned(regB)));--carga de rom
                when "11"=>
                regD:=Content(to_integer(unsigned(regB)));--carga de rom
                when others=>null;
            end case;
        when "1110"=>
            case mar(7 downto 0) is
                    when "00"=>
                    regA:=Content(to_integer(unsigned(regC)));--carga de rom
                    when "01"=>
                    regB:=Content(to_integer(unsigned(regC)));--carga de rom
                    when "10"=>
                    regC:=Content(to_integer(unsigned(regC)));--carga de rom
                    when "11"=>
                    regD:=Content(to_integer(unsigned(regC)));--carga de rom
                    when others=>null;
                end case;
        when "1111"=>
        case mar(7 downto 0) is
                when "00"=>
                regA:=Content(to_integer(unsigned(regD)));--carga de rom
                when "01"=>
                regB:=Content(to_integer(unsigned(regD)));--carga de rom
                when "10"=>
                regC:=Content(to_integer(unsigned(regD)));--carga de rom
                when "11"=>
                regD:=Content(to_integer(unsigned(regD)));--carga de rom
                when others=>null;
            end case;
        when others=>sig:=init;
        end case;        

    sig:=fetch;
    
  
    --                operation             --
    
    when operation=>
        mdr:= Content(to_integer(unsigned(mar)));
        case cir(3 downto 2) is
        when "00"=>op1:=regA;
        when "01"=>op1:=regB;
        when "10"=>op1:=regC;
        when "11"=>op1:=regD;
        when others=>sig:=init;
        end case;
        sig:=operation1;
    when operation1=>
        alu(op1,acc,cir(7 downto 4),res,carry,overflow,zero,Neg);
        mdr:=res;
        case cir(1 downto 0) is
        when "00"=>regA:=mdr;
        when "01"=>regB:=mdr;
        when "10"=>regC:=mdr;
        when "11"=>regD:=mdr;
        when others=>sig:=init;
        end case;
        sig:=fetch;


   --             saltos            --
  --          JUMP            --   -- FUNCIONA
    when jmp=>
        pcreg:=mdr(7 downto 0); 
        sig:=fetch;
    --          JALR           
    when jalr=>
         ret_pc:=pcreg;
         sig:=jalr2;
    when jalr2 => 
         pcreg:=mdr(7 downto 0);
         sig:=fetch;
    when ret=>
        pcreg:= ret_pc;
        --alu("00000000"&ret_pc,"0000000000000001","0111",pcvar,nu1,nu2,nu3,nu4); -- Aqui se suma el pcreg
	    --pcreg:=pcvar(7 downto 0);
        ret_pc:="00000000";
        sig:=fetch;
    --          BNZ      -- Compara registro especifico   -- FUNCIONA  
    when bnz=>
    dato1:= "0000000000000000";
    dato2:= regD;
    enable_cmp <= '1';
    dato_cmp_1 <= dato1;
    dato_cmp_2 <= dato2;
    sig:=bnz2;
    when bnz2 =>
    if(eq_cmp = '1') then 
        sig:=fetch;
    else
        pcreg:=mdr(7 downto 0); 
        sig:=fetch;
    end if;
    enable_cmp <= '0';
    --          BS            -- Compara registro especifico
    when bs=>
    dato2 := regD;--:= "1000000000000001";
    enable_cmp <= '1';
    dato_cmp_1 <= "0000000000000001";
    dato_cmp_2 <= "000000000000000"&dato2(15);
    sig:=bs2;
    when bs2=>
    if(eq_cmp = '1') then
        pcreg:=mdr(7 downto 0); 
        sig:=fetch; 
    else
        sig:=fetch;
    end if;
    enable_cmp <= '0';
     --          BNC            --  Compara la operacion de add anterior si hubo carry
    when bnc=>
    if(carry = '1') then 
        pcreg:=mdr(7 downto 0); 
        carry := '0';
    end if;
    sig:=fetch;
      --          BNV            -- Compara la operacion add anterior si hubo overflow
    when bnv=>
    if(overflow = '1') then 
        pcreg:=mdr(7 downto 0); 
        overflow := '0';
    end if;
    sig:=fetch;
   
    
    
    
    
    
    -- SAVE REG
    when save_regE=>
    if(validation_regE = '0')then
        regE:=regA;
    end if;
    validation_regE := '1';
    sig:=fetch;
    --- CMP
    when cmp=>
    case cir(1 downto 0) is
        when "00"=>dato1:=regA;
        when "01"=>dato1:=regB;
        when "10"=>dato1:=regC;
        when "11"=>dato1:=regD;
        when others=>sig:=init;
        end case;
    case cir(3 downto 2) is
        when "00"=>dato2:=regA;
        when "01"=>dato2:=regB;
        when "10"=>dato2:=regC;
        when "11"=>dato2:=regD;
        when others=>sig:=init;
        end case;
    enable_cmp <= '1';
    dato_cmp_1 <= dato1;
    dato_cmp_2 <= dato2;
    --enable_cmp <= '0';
    sig:=cmp2;
    -- cmp2 --
    when cmp2 =>
    sig := fetch;
    --              move            --
    when move=>
    sig:=fetch;
    --               sendto disp           --
    when send=>
            case cir(3 downto 0) is
                when "0100"=>
                    case mar(7 downto 0) is
                        when "00000000"=>letra1<=regA(7 downto 0);--SEND REG TO A LETRA
                        when "00000001"=>letra2<=regA(7 downto 0);
                        when "00000010"=>letra3<=regA(7 downto 0);
                        when "00000011"=>letra4<=regA(7 downto 0);
                        when "00000100"=>letra5<=regA(7 downto 0);
                        when "00000101"=>letra6<=regA(7 downto 0);
                        when "00000110"=>letra7<=regA(7 downto 0);
                        when "00000111"=>letra8<=regA(7 downto 0);
                        when "00001000"=>letra9<=regA(7 downto 0);
                        when "00001001"=>letra10<=regA(7 downto 0);
                        when "00001010"=>letra11<=regA(7 downto 0);
                        when "00001011"=>letra12<=regA(7 downto 0);
                        when "00001100"=>letra13<=regA(7 downto 0);
                        when "00001101"=>letra14<=regA(7 downto 0);
                        when "00001111"=>letra15<=regA(7 downto 0);
                        when "00010000"=>letra16<=regA(7 downto 0);
                        when others=>sig:=init;
                    end case;
                when "0101"=>
                    case mar(7 downto 0) is
                            when "00000000"=>letra1<=regB(7 downto 0);
                            when "00000001"=>letra2<=regB(7 downto 0);
                            when "00000010"=>letra3<=regB(7 downto 0);
                            when "00000011"=>letra4<=regB(7 downto 0);
                            when "00000100"=>letra5<=regB(7 downto 0);
                            when "00000101"=>letra6<=regB(7 downto 0);
                            when "00000110"=>letra7<=regB(7 downto 0);
                            when "00000111"=>letra8<=regB(7 downto 0);
                            when "00001000"=>letra9<=regB(7 downto 0);
                            when "00001001"=>letra10<=regB(7 downto 0);
                            when "00001010"=>letra11<=regB(7 downto 0);
                            when "00001011"=>letra12<=regB(7 downto 0);
                            when "00001100"=>letra13<=regB(7 downto 0);
                            when "00001101"=>letra14<=regB(7 downto 0);
                            when "00001111"=>letra15<=regB(7 downto 0);
                            when "00010000"=>letra16<=regB(7 downto 0);
                            when others=>sig:=init;
                        end case;
                when "0110"=>
                        case mar(7 downto 0) is
                        when "00000000"=>letra1<=regC(7 downto 0);
                        when "00000001"=>letra2<=regC(7 downto 0);
                        when "00000010"=>letra3<=regC(7 downto 0);
                        when "00000011"=>letra4<=regC(7 downto 0);
                        when "00000100"=>letra5<=regC(7 downto 0);
                        when "00000101"=>letra6<=regC(7 downto 0);
                        when "00000110"=>letra7<=regC(7 downto 0);
                        when "00000111"=>letra8<=regC(7 downto 0);
                        when "00001000"=>letra9<=regC(7 downto 0);
                        when "00001001"=>letra10<=regC(7 downto 0);
                        when "00001010"=>letra11<=regC(7 downto 0);
                        when "00001011"=>letra12<=regC(7 downto 0);
                        when "00001100"=>letra13<=regC(7 downto 0);
                        when "00001101"=>letra14<=regC(7 downto 0);
                        when "00001111"=>letra15<=regC(7 downto 0);
                        when "00010000"=>letra16<=regC(7 downto 0);
                        when others=>sig:=init;
                    end case;
                when "0111"=>
                    case mar(7 downto 0) is
                        when "00000000"=>letra1<=regD(7 downto 0);
                        when "00000001"=>letra2<=regD(7 downto 0);
                        when "00000010"=>letra3<=regD(7 downto 0);
                        when "00000011"=>letra4<=regD(7 downto 0);
                        when "00000100"=>letra5<=regD(7 downto 0);
                        when "00000101"=>letra6<=regD(7 downto 0);
                        when "00000110"=>letra7<=regD(7 downto 0);
                        when "00000111"=>letra8<=regD(7 downto 0);
                        when "00001000"=>letra9<=regD(7 downto 0);
                        when "00001001"=>letra10<=regD(7 downto 0);
                        when "00001010"=>letra11<=regD(7 downto 0);
                        when "00001011"=>letra12<=regD(7 downto 0);
                        when "00001100"=>letra13<=regD(7 downto 0);
                        when "00001101"=>letra14<=regD(7 downto 0);
                        when "00001111"=>letra15<=regD(7 downto 0);
                        when "00010000"=>letra16<=regD(7 downto 0);
                        when others=>sig:=init;
                    end case;
            when "1000"=>--perdiste
                   letra1<=x"B0";
                   letra2<=x"B0";
                   letra3<=x"B0";
                   letra4<=x"B0";
                   letra5<=x"50";
                   letra6<=x"65";
                   letra7<=x"72";
                   letra8<=x"64";
                   letra9<=x"69";
                   letra10<=x"73";
                   letra11<=x"74";
                   letra12<=x"65";
                   letra13<=x"B0";
                   letra14<=x"B0";
                   letra15<=x"B0";
                   letra16<=x"B0";
            when "1001"=>--ganaste
                   letra1<=x"B0";
                   letra2<=x"B0";
                   letra3<=X"B0";
                   letra4<=x"B0";
                   letra5<=x"47";
                   letra6<=x"61";
                   letra7<=x"6E";
                   letra8<=x"61";
                   letra9<=x"73";
                   letra10<=x"74";
                   letra11<=x"65";
                   letra12<=x"B0";
                   letra13<=x"B0";
                   letra14<=x"B0";
                   letra15<=x"B0";
                   letra16<=x"B0";
            when "1010"=>--inicio
                   letra1<=x"B0";
                   letra2<=x"B0";
                   letra3<=x"B0";
                   letra4<=x"B0";
                   letra5<=x"B0";
                   letra6<=x"69";
                   letra7<=x"6E";
                   letra8<=x"69";
                   letra9<=x"63";
                   letra10<=x"69";
                   letra11<=x"6F";
                   letra12<=x"B0";
                   letra13<=x"B0";
                   letra14<=x"B0";
                   letra15<=x"B0";
                   letra16<=x"B0";
            --when "1100"=>
            when "1100"=>
            when "1101"=>
            when "1110"=>
            when "1111"=>    
        when others=>sal<=(others=>'0');
        end case;
    sig:=fetch;
        
    when endprog=>
    case cir(3 downto 0) is
        when "0000"=>
        validation_regE:= '0';
        if(sel=seld) then
        sig:=endprog;
        getsal <= '0';
        else 
        sig:=init;
        end if;  
        when "0100"=>
        if enter='1' then
        regA:="00000000"&letra;
        sig := fetch;
        else
        sig:=endprog;
        end if;
       when "0101"=>
       if enter='1' then
        regB:="00000000"&letra;
        sig := fetch;
        else
        sig:=endprog;
        end if;
       when "0110"=>
       if enter='1' then
        regC:="00000000"&letra;
        sig := fetch;
        else
        sig:=endprog;
        end if;
       when "0111"=>
       if enter='1' then
        regD:="00000000"&letra;
        sig := fetch;
        else
        sig:=endprog;
        end if;
        when others=>null;
        
        end case;
	when others=>sig:=init;
	 	
end case;
  rg1<=regA;  
  rg2<=regB;  
  rg3<=regC;  
  rg4<=regD;  
  rg5<=regE;  
  pcsig<=pcreg;
  ret_sig<=ret_pc;
  equal<=eq;
  estado<=sig;
               -- clk_30khz <= not clk_30khz;
   
end if;

end process;





    
-- initialicepc: process (signalenter,clkf) is begin
--    if(clkf'event and clkf='1') then
--    if(entersignal='0' and enterbefore='1') then
--        initialice<="00011101";
--    end if;
--    entersignal<=enterbefore;
--        end if;
--    end process;
    
--    entersignal<=enter;
    
    
   

end Behavioral;

