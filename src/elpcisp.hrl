%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    LPC definitions
%%% @end
%%% Created : 11 Sep 2012 by Tony Rogvall <tony@rogvall.se>

-ifndef(__ELPCISP_HRL__).
-define(__ELPCISP_HRL__, true).

%% Data should be <= 900 = 45*20  bytes
-define(UU_MAX_LINES, 20).
-define(UU_MAX_LINE_LENGTH, 45).  %% bytes (45/3)*4 = 60 characters

-define(LPC2109, 16#0201FF01).
-define(LPC2119, 16#0201FF12).
-define(LPC2129, 16#0201FF13).
-define(LPC2114, 16#0101FF12).
-define(LPC2124, 16#0101FF13).
-define(LPC2194, 16#0301FF13).
-define(LPC2292, 16#0401FF13).
-define(LPC2294, 16#0501FF13).
-define(LPC2131, 16#0002FF01).
-define(LPC2132, 16#0002FF11).
-define(LPC2134, 16#0002FF12).
-define(LPC2136, 16#0002FF23).
-define(LPC2138, 16#0002FF25).
-define(LPC2210, 16#0301FF12). %% and LPC2220 and LPC2210/01 

-define(sectorTable_210x(),
	[
	 8192, 8192, 8192, 8192, 8192, 8192, 8192, 8192,
	 8192, 8192, 8192, 8192, 8192, 8192, 8192
	]).

-define(sectorTable_2103(),
	[
	 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096
	]).

-define(sectorTable_2109(),
	[
	 8192, 8192, 8192, 8192, 8192, 8192, 8192, 8192
	]).

-define(sectorTable_211x(),
	[
	 8192, 8192, 8192, 8192, 8192, 8192, 8192, 8192,
	 8192, 8192, 8192, 8192, 8192, 8192, 8192
	]).

-define(sectorTable_212x(),
	[
	 8192, 8192, 8192, 8192, 8192, 8192, 8192, 8192,
	 65536, 65536, 8192, 8192, 8192, 8192, 8192, 8192, 8192
	]).

-define(sectorTable_213x(),
	[
	 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,
	 32768, 32768, 32768, 32768, 32768, 32768, 32768, 32768,
	 32768, 32768, 32768, 32768, 32768, 32768, 4096, 4096,
	 4096, 4096, 4096
	]).

-define(sectorTable_17xx(),
	[
	 4096,  4096,  4096,  4096,  4096,  4096,  4096,  4096,
	 4096,  4096,  4096,  4096,  4096,  4096,  4096,  4096,
	 32768, 32768, 32768, 32768, 32768, 32768, 32768, 32768,
	 32768, 32768, 32768, 32768, 32768, 32768
	]).

-define(LPC_RAMSTART_LPC2XXX,    16#40000000).
-define(LPC_RAMBASE_LPC2XXX,     16#40000200).

-define(LPC_RAMSTART_LPC17XX,    16#10000000).
-define(LPC_RAMBASE_LPC17XX,     16#10000200).

-define(LPC_RAMSTART_LPC13XX,    16#10000000).
-define(LPC_RAMBASE_LPC13XX,     16#10000300).

-define(LPC_RAMSTART_LPC11XX,    16#10000000).
-define(LPC_RAMBASE_LPC11XX,     16#10000300).

-record(device_type,
	{
	  id         :: non_neg_integer(),  %% product id
	  product    :: string(),           %% product name
	  flashSize  :: non_neg_integer(),  %% in kiB, for informational purposes only
	  ramSize    :: non_neg_integer(),  %% in kiB, for informational purposes only
	  flashSectors :: non_neg_integer(), %% Total number of sectors 
	  maxCopySize :: non_neg_integer(),  %% maximum size that can be copied to Flash 
	  %% in a single command 
	  sectorTable :: [non_neg_integer()],  %% list of block sizes
	  variant     :: atom()               %% chip variane
	}).

-define(DEV(ID,PROD,FLASH,RAM,Sectors,MaxCopy,Table,Variant),
	#device_type {
	     id = (ID), 
	     product = (PROD), 
	     flashSize = (FLASH),
	     ramSize   = (RAM),
	     flashSectors = (Sectors),
	     maxCopySize = (MaxCopy),
	     sectorTable = (Table),
	     variant = (Variant)
	    }).

-endif.




