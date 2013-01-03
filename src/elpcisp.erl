%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     elpcisp functions
%%% @end
%%% Created : 31 Aug 2012 by Tony Rogvall <tony@rogvall.se>

-module(elpcisp).

-export([open/1, open/2]).
-export([sync/1, sync/2, sync/3]).
-export([flush/1]).
-export([enter/1]).
-export([unlock/1]).
-export([echo/2]).
-export([go/2,go/3]).
-export([copy/4]).
-export([compare/4]).
-export([prepare_sector/3]).
-export([erase_sector/3]).
-export([blank_check_sector/3]).
-export([read_device_type/1]).
-export([read_partid/1]).
-export([read_version/1]).
-export([write_memory/3]).
-export([read_memory/3]).
-export([flash/2]).
-export([patch_segment/3]).
-export([block_list/2]).
-export([flash_block/4]).


%% -compile(export_all).

-import(lists, [reverse/1]).

-define(SP, $\s).
-define(is_addr(A), is_integer(A),((A) >= 0),((A) =< 16#ffffffff)).
-define(i2l(X), integer_to_list((X))).

-define(dbg(F,A), io:format((F)++"\n",(A))).
%% -define(dbg(F,A), ok).

-include("elpcisp.hrl").

%% @doc
%%    Open a LPCx decide for flashing
%% @end
-spec open(Device::string()) ->
		  {ok,uart:uart()} | {error,term()}.
open(Device) ->
    open(Device,38400).

%% @doc
%%    Open a LPCx decide for flashing
%% @end
-spec open(Device::string(),Baud::integer()) ->
		  {ok,uart:uart()} | {error,term()}.
open(Device,Baud) ->
    uart:open(Device, [{baud,Baud},{active,false},{packet,0},{mode,binary}]).

%% @doc
%%   Sync will (try to) set the LPC circuit in programming mode.
%% @end
-spec sync(U::uart:uart()) -> ok | {error,term()}.
sync(U) -> 
    sync_(U, -1, 500, "12000").

-spec sync(U::uart:uart(),Retries::integer()) ->
		  ok | {error,term()}.
sync(U, N) when is_integer(N), N>0 -> 
    sync_(U, N, 500, "12000").

-spec sync(U::uart:uart(),Retries::integer(),Tmo::timeout()) ->
		  ok | {error,term()}.

sync(U, N, Tmo) when is_integer(N), N>0, is_integer(Tmo), Tmo>0 ->
    sync_(U, N, Tmo, "12000").

sync_(U, N, Tmo, Osc) ->
    uart:setopts(U, [{active,true},{packet,0}]),
    flush(U),
    case sync__(U,N,Tmo) of
	ok ->
	    uart:setopts(U, [{packet,line}]),
	    {ok,_} = command(U, "Synchronized"),
	    command(U, Osc);
	Error ->
	    Error
    end.

flush(U) ->
    receive
	{uart,U,_} ->
	    flush(U)
    after 0 ->
	    ok
    end.


sync__(_U, 0, _Tmo) -> 
    io:format("\n"),
    {error,no_sync};
sync__(U, I, Tmo) ->
    enter(U),
    io:format("."),
    uart:send(U, "?"),
    wait_sync__(U,I,Tmo,Tmo,<<>>).

wait_sync__(U,I,Tmo,Tmo0,Acc) ->
    receive
	{uart,U,<<"?">>} ->
	    wait_sync__(U, I, Tmo,Tmo0,Acc);
	{uart,U,Data} ->
	    wait_sync__(U,I,0,Tmo0,<<Acc/binary,Data/binary>>);
	What ->
	    ?dbg("What=~p", [What]),
	    wait_sync__(U, I,Tmo,Tmo0,Acc)
    after Tmo ->
	    if Tmo =:= 0 ->
		    if Acc =:= <<"Synchronized\r\n">> ->
			    io:format("\n"),
			    ok;
		       true ->
			    sync__(U, I-1, Tmo0)
		    end;
	       true ->
		    sync__(U, I-1, Tmo0)
	    end
    end.

%% @doc
%%    Enter programming mode
%% @end
-spec enter(uart:uart()) -> ok.
		   
enter(U) ->
    uart:set_modem(U, [dtr,rts]),
    timer:sleep(100),
    %% clear buffers?
    timer:sleep(100),
    uart:clear_modem(U, [dtr]),
    timer:sleep(500),
    uart:clear_modem(U, [rts]),
    ok.

%% @doc
%%    Turn echo on or off
%% @end
-spec echo(uart:uart(), boolean()) -> ok.

echo(U, true) ->
    command(U, [$A,?SP,?i2l(1)]);
echo(U, false) ->
    command(U, [$A,?SP,?i2l(0)]).

%% @doc
%%    Prepare the all sectors from A to and including B to be 
%%    erased or written to.
%% @end
-spec prepare_sector(U::uart:uart(),A::integer(),B::integer()) ->
			    {ok,binary()} | {error,term()}.
prepare_sector(U, A, B) when is_integer(A), is_integer(B), A >= 0, A =< B ->
    command(U, [$P,?SP,?i2l(A),?SP,?i2l(B)]).

%% @doc
%%    Copy N bytes of data from RAM address Dst 
%%    to the flash starting at address Src. This is the
%%    main flashing routine.
%% @end
-spec copy(U::uart:uart(),Dst::integer(),Src::integer(),N::integer()) ->
		  {ok,binary()} | {error,term()}.
copy(U, Dst, Src, N) 
  when ?is_addr(Dst),(Dst band 16#ff) =:= 0,  ?is_addr(Src),
       ((N =:= 256) 
	orelse (N =:= 512) 
	orelse (N =:= 1024) 
	orelse (N =:= 4096)) ->
    command(U, [$C,?SP,?i2l(Dst),?SP,?i2l(Src),?SP,?i2l(N)]).

%% @doc
%%    Run the program found at address Addr
%% @end

-spec go(U::uart:uart(),Addr::integer(), arm|thumb) ->
		{ok,binary()} | {error,term()}.

go(U,Addr,arm) when ?is_addr(Addr) ->
    command(U, [$G,?SP,?i2l(Addr),?SP,$A]);
go(U,Addr,thumb) when ?is_addr(Addr) ->
    command(U, [$G,?SP,?i2l(Addr),?SP,$T]).

-spec go(U::uart:uart(),Addr::integer()) ->
		{ok,binary()} | {error,term()}.

go(U,Addr) ->
    go(U,Addr,arm).

%% @doc
%%    Erase data on sectors from A to and including sector B
%% @end
-spec erase_sector(U::uart:uart(),A::integer(),B::integer()) ->
			  {ok,binary()} | {error,term()}.

erase_sector(U, A, B) when is_integer(A), is_integer(B), A >= 0, A =< B ->
    command(U, [$E,?SP,?i2l(A),?SP,?i2l(B)]).

%% @doc
%%    Check that all sectors from A to and including B are erased
%% @end
-spec blank_check_sector(U::uart:uart(),A::integer(),B::integer()) ->
				{ok,binary()} | {error,term()}.

blank_check_sector(U, A, B) when is_integer(A), is_integer(B), A >= 0, A =< B ->
    command(U, [$I,?SP,?i2l(A),?SP,?i2l(B)]).

%% @doc
%%    Get the device type record
%% @end
-spec read_device_type(U::uart:uart()) -> {ok,#device_type{}} | {error,term()}.

read_device_type(U) ->
    case read_partid(U) of
	{ok, ID} ->
	    case lists:keysearch(ID, #device_type.id, lpc_types()) of
		false -> {error, {not_such_id, ID}};
		{value,DevType} -> {ok,DevType}
	    end;
	Error -> Error
    end.

%% @doc
%%    Read the circuit model number 
%% @end
-spec read_partid(uart:uart()) -> {ok,integer()} | {error,term()}.

read_partid(U) ->
    case command(U, [$J]) of
	{ok, [PartID]} ->
	    {ok,list_to_integer(binary_to_list(PartID))};
	Error ->
	    Error
    end.

%% @doc
%%    Read the boot program version number 
%% @end
-spec read_version(uart:uart()) -> {ok,integer()} | {error,term()}.

read_version(U) ->
    command(U, [$K]).

compare(U, A1, A2, N) when ?is_addr(A1), ?is_addr(A2), 
			   A1 band 3 =:= 0, A2 band 3 =:= 0,
			   is_integer(N), N band 3 =:= 0 ->
    command(U, [$M,?SP,?i2l(A1),?SP,?i2l(A2),?SP,?i2l(N)]).


%% @doc
%%    Unlock flash memory for erase and write operations.
%% @end
-spec unlock(uart:uart()) -> {ok,binary()} | {error,term()}.

unlock(U) ->
    command(U, [$U,?SP,?i2l(23130)]).

%% @doc
%%    Write data to RAM address Addr
%% @end
-spec write_memory(U::uart:uart(), Addr::integer(), Data::binary()) ->
			  ok | {error,term()}.

write_memory(U, Addr, Data) 
  when ?is_addr(Addr), Addr band 3 =:= 0,
       byte_size(Data) band 3 =:= 0,
       byte_size(Data) =< ?UU_MAX_LINES*?UU_MAX_LINE_LENGTH ->
    N = byte_size(Data),
    case command(U, [$W,?SP,?i2l(Addr),?SP,?i2l(N)]) of
	{ok,[]} ->
	    write_data(U, elpcisp_uu:encode_csum(Data), 1000, 3);
	Error ->
	    Error
    end.

write_data(U, Lines, Timeout, Resend) ->
    write_data(U, Lines, Lines, Timeout, Resend).

write_data(U, [Line|Lines], Lines0, Timeout, Resend) ->
    Line1 = <<Line/binary,$\r,$\n>>,
    uart:send(U, Line1),
    case wait_echo(U, Line1, Timeout) of
	ok ->
	    write_data(U, Lines, Lines0, Timeout, Resend);
	Error ->
	    Error
    end;
write_data(U, [], Lines0, Timeout, Resend) ->
    receive 
	{uart, U, <<"OK\r\n">>} ->
	    ok;
	{uart, U, <<"RESEND\r\n">>} ->
	    if Resend =:= 0 ->
		    {error, transmission_failed};
	       true ->
		    write_data(U, Lines0, Lines0, Timeout, Resend-1)
	    end
    after Timeout ->
	    {error, timeout}
    end.

%% @doc
%%   Read data from RAM or flash memeory.
%%   Address Addr must be a multiple of 4 and 
%%   byte count N must be a multiple of 4 and not more than 900 bytes
%% @end
-spec read_memory(U::uart:uart(), Addr::integer(), N::non_neg_integer()) ->
			 {ok,binary()} | {error,term()}.
read_memory(U, Addr, N) 
  when ?is_addr(Addr), Addr band 3 =:= 0, N band 3 =:= 0,
       N =< ?UU_MAX_LINES*?UU_MAX_LINE_LENGTH ->
    case command(U, [$R,?SP,?i2l(Addr),?SP,?i2l(N)]) of
	{ok, Lines} ->
	    case elpcisp_uu:decode_csum(Lines) of
		{ok,Data} ->
		    Ack = <<"OK\r\n">>,
		    uart:send(U, Ack),
		    case wait_echo(U, Ack, 1000) of
			ok -> {ok,Data};
			Error -> Error
		    end;
		Error ->
		    %% Do not resend here, let application do that
		    Ack = <<"OK\r\n">>,
		    uart:send(U, Ack),
		    case wait_echo(U, Ack, 1000) of
			ok -> Error;
			Error2 -> Error2
		    end
	    end;
	Error ->
	    Error
    end.

-spec command(U::uart:uart(), iolist()) -> ok | {error,term()}.

command(U, Cmd) ->
    Cmd1 = erlang:iolist_to_binary([Cmd,$\r,$\n]),
    ?dbg("Command: [~s]", [Cmd]),
    uart:send(U, Cmd1),
    case response(U, Cmd1) of
	{ok,Response} ->
	    decode_result(Response);
	Error ->
	    Error
    end.

wait_echo(U, Cmd, Timeout) ->
    receive 
	{uart,U,Cmd} ->
	    ok
    after Timeout ->
	    {error,timeout}
    end.

response(U, Cmd) ->
    response(U, Cmd, 5000).

response(U,Cmd,Timeout) ->
    receive
	{uart,U,Cmd} ->  %% ignore command echo
	    response1(U,Timeout,[]);
	{uart,U,Resp} ->
	    response1(U,Timeout,[trim_nl(Resp)]);
	{uart_closed, U} ->
	    {error, closed};
	{uart_error, U, Error} ->
	    {error, Error}
    after Timeout ->
	    {error, timeout}
    end.

response1(U,Timeout,Acc) ->
    receive
	{uart,U,Result} ->
	    response1(U, 50, [trim_nl(Result)|Acc]);
	{uart_closed, U} ->
	    {error, closed};
	{uart_error, U, Error} ->
	    {error, Error}
    after Timeout ->
	    if Timeout =:= 50 ->
		    {ok,reverse(Acc)};
	       true ->
		    {error, timeout}
	    end
    end.

trim_nl(Bin) ->
    Sz = byte_size(Bin),
    Sz1 = Sz-1,
    Sz2 = Sz-2,
    case Bin of
	<<Bin1:Sz2/binary,$\r,$\n>> -> Bin1;
	<<Bin1:Sz1/binary,$\n>> -> Bin1;
	_ -> Bin
    end.


-define(IAP_CMD_SUCCESS,           0).
-define(IAP_INVALID_COMMAND,       1).
-define(IAP_SRC_ADDR_ERROR,        2).
-define(IAP_DST_ADDR_ERROR,        3).
-define(IAP_SRC_ADDR_NOT_MAPPED,   4).
-define(IAP_DST_ADDR_NOT_MAPPED,   5).
-define(IAP_COUNT_ERROR,           6).
-define(IAP_INVALID_SECTOR,        7).
-define(IAP_SECTOR_NOT_BLANK,      8).
-define(IAP_SECTOR_NOT_PREPARED,   9).
-define(IAP_COMPARE_ERROR,         10).
-define(IAP_BUSY,                  11).
-define(IAP_PARAM_ERROR,           12).
-define(IAP_ADDR_ERROR,            13).
-define(IAP_ADDR_NOT_MAPPED,       14).
-define(IAP_CMD_LOCKED,            15).
-define(IAP_INVALID_CODE,          16).
-define(IAP_INVALID_BAUDRATE,      17).
-define(IAP_INVALID_STOP_BIT,      18).
-define(IAP_CODE_PROTECTION,       19).

decode_result([R|Rs]) ->
    Ts = tokenize([R]),
    case Ts of
	["OK"] -> {ok,Rs};
	["0"]  -> {ok,Rs};
	["0"|Result] -> {ok,[Result|Rs]};  %%     0 = CMD_SUCCESS
	["1"|_] -> {error,invalid_command};
	["2"|_] -> {error,src_addr_error};
	["3"|_] -> {error,dst_addr_error};
	["4"|_] -> {error,src_addr_not_mapped};
	["5"|_] -> {error,dst_addr_not_mapped};
	["6"|_] -> {error,count_error};
	["7"|_] -> {error,invalid_sector};
	["8"|_] -> {error,sector_not_blank};
	["9"|_] -> {error,sector_not_prepared_for_write_operation};
	["10"|_] -> {error,compare_error};
	["11"|_] -> {error,busy};
	["12"|_] -> {error,param_error};
	["13"|_] -> {error,addr_error};
	["14"|_] -> {error,addr_not_mapped};
	["15"|_] -> {error,cmd_locked};
	["16"|_] -> {error,invalid_code};
	["17"|_] -> {error,invalid_baud_rate};
	["18"|_] -> {error,invalid_stop_bit};
	["19"|_] -> {error,code_read_protection_enabled};
	_ -> {error,Ts}
    end.

tokenize(Rs) -> ts(Rs,[]).

ts([R|Rs],Acc) ->
    ts(binary:split(R, <<" ">>, [global]),Acc,Rs);
ts([],Acc) ->
    reverse(Acc).

ts([<<>>|Ts],Acc,Rs) ->  ts(Ts,Acc,Rs);
ts([T|Ts],Acc,Rs) -> ts(Ts,[binary_to_list(T)|Acc],Rs);
ts([],Acc,Rs) -> ts(Rs,Acc).


lpc_types() ->
    [
     ?DEV( 16#2500102B, "1102",         32,  8,  8, 4096, ?sectorTable_17xx(), lpc11xx ),

     ?DEV( 16#041E502B, "1111.../101",   8,  2,  2, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2516D02B, "1111.../102",   8,  2,  2, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0416502B, "1111.../201",   8,  4,  2, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2516902B, "1111.../202",   8,  4,  2, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#042D502B, "1112.../101",  16,  2,  4, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2524D02B, "1112.../102",  16,  2,  4, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0425502B, "1112.../201",  16,  4,  4, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2524902B, "1112.../202",  16,  4,  4, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0434502B, "1113.../201",  24,  4,  6, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2532902B, "1113.../202",  24,  4,  6, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0434102B, "1113.../301",  24,  8,  6, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2532102B, "1113.../302",  24,  8,  6, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0444502B, "1114.../201",  32,  4,  8, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2540902B, "1114.../202",  32,  4,  8, 1024, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0444102B, "1114.../301",  32,  8,  8, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#2540102B, "1114.../302",  32,  8,  8, 4096, ?sectorTable_17xx(), lpc11xx ),

     ?DEV( 16#1421102B, "11C12.../301",  16,  8,  4, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#1440102B, "11C14.../301",  32,  8,  8, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#1431102B, "11C22.../301",  16,  8,  4, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#1430102B, "11C24.../301",  32,  8,  8, 4096, ?sectorTable_17xx(), lpc11xx ),
     
     ?DEV( 16#0364002B, "1224.../101",  32,  8,  4, 2048, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0364202B, "1224.../121",  48, 12, 32, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0365002B, "1225.../301",  64, 16, 32, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0365202B, "1225.../321",  80, 20, 32, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0366002B, "1226",         96, 24, 32, 4096, ?sectorTable_17xx(), lpc11xx ),
     ?DEV( 16#0367002B, "1227",        128, 32, 32, 4096, ?sectorTable_17xx(), lpc11xx ),
     
     ?DEV( 16#2C42502B, "1311",          8,  4,  2, 1024, ?sectorTable_17xx(), lpc13xx ),
     ?DEV( 16#1816902B, "1311/01",       8,  4,  2, 1024, ?sectorTable_17xx(), lpc13xx ),
     ?DEV( 16#2C40102B, "1313",         32,  8,  8, 4096, ?sectorTable_17xx(), lpc13xx ),
     ?DEV( 16#1830102B, "1313/01",      32,  8,  8, 4096, ?sectorTable_17xx(), lpc13xx ),
     ?DEV( 16#3D01402B, "1342",         16,  4,  4, 1024, ?sectorTable_17xx(), lpc13xx ),
     ?DEV( 16#3D00002B, "1343",         32,  8,  8, 4096, ?sectorTable_17xx(), lpc13xx ),
     
     ?DEV( 16#25001118, "1751",         32,  8,  8, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#25001121, "1752",         64, 16, 16, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#25011722, "1754",        128, 32, 18, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#25011723, "1756",        256, 32, 22, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#25013F37, "1758",        512, 64, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#25113737, "1759",        512, 64, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#26011922, "1764",        128, 32, 18, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#26013733, "1765",        256, 64, 22, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#26013F33, "1766",        256, 64, 22, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#26012837, "1767",        512, 64, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#26013F37, "1768",        512, 64, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#26113F37, "1769",        512, 64, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     
     ?DEV( 16#27011132, "1774",        128, 40, 18, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#27191F43, "1776",        256, 80, 22, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#27193747, "1777",        512, 96, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#27193F47, "1778",        512, 96, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#281D1743, "1785",        256, 80, 22, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#281D1F43, "1786",        256, 80, 22, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#281D3747, "1787",        512, 96, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     ?DEV( 16#281D3F47, "1788",        512, 96, 30, 4096, ?sectorTable_17xx(), lpc17xx ),
     
     ?DEV( 16#0004FF11, "2103",         32,  8,  8, 4096, ?sectorTable_2103(), lpc2xxx ),
     ?DEV( 16#FFF0FF12, "2104",        128, 16, 15, 8192, ?sectorTable_210x(), lpc2xxx ),
     ?DEV( 16#FFF0FF22, "2105",        128, 32, 15, 8192, ?sectorTable_210x(), lpc2xxx ),
     ?DEV( 16#FFF0FF32, "2106",        128, 64, 15, 8192, ?sectorTable_210x(), lpc2xxx ),
     ?DEV( 16#0201FF01, "2109",         64,  8,  8, 4096, ?sectorTable_2109(), lpc2xxx ),
     ?DEV( 16#0101FF12, "2114",        128, 16, 15, 8192, ?sectorTable_211x(), lpc2xxx ),
     ?DEV( 16#0201FF12, "2119",        128, 16, 15, 8192, ?sectorTable_211x(), lpc2xxx ),
     ?DEV( 16#0101FF13, "2124",        256, 16, 17, 8192, ?sectorTable_212x(), lpc2xxx ),
     ?DEV( 16#0201FF13, "2129",        256, 16, 17, 8192, ?sectorTable_212x(), lpc2xxx ),
     ?DEV( 16#0002FF01, "2131",         32,  8,  8, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0002FF11, "2132",         64, 16,  9, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0002FF12, "2134",        128, 16, 11, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0002FF23, "2136",        256, 32, 15, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0002FF25, "2138",        512, 32, 27, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0402FF01, "2141",         32,  8,  8, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0402FF11, "2142",         64, 16,  9, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0402FF12, "2144",        128, 16, 11, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0402FF23, "2146",        256, 40, 15, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0402FF25, "2148",        512, 40, 27, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0301FF13, "2194",        256, 16, 17, 8192, ?sectorTable_212x(), lpc2xxx ),
     ?DEV( 16#0301FF12, "2210",          0, 16,  0, 8192, ?sectorTable_211x(), lpc2xxx ),
     ?DEV( 16#0401FF12, "2212",        128, 16, 15, 8192, ?sectorTable_211x(), lpc2xxx ),
     ?DEV( 16#0601FF13, "2214",        256, 16, 17, 8192, ?sectorTable_212x(), lpc2xxx ),
     %% "2290"; same id as the LPC2210
     ?DEV( 16#0401FF13, "2292",        256, 16, 17, 8192, ?sectorTable_212x(), lpc2xxx ),
     ?DEV( 16#0501FF13, "2294",        256, 16, 17, 8192, ?sectorTable_212x(), lpc2xxx ),
     ?DEV( 16#00000000, "2361",        128, 34, 11, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#00000000, "2362",        128, 34, 11, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0603FB02, "2364",        128, 34, 11, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600F902, "2364",        128, 34, 11, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600E823, "2365",        256, 58, 15, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0603FB23, "2366",        256, 58, 15, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600F923, "2366",        256, 58, 15, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600E825, "2367",        512, 58, 15, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0603FB25, "2368",        512, 58, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600F925, "2368",        512, 58, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1700E825, "2377",        512, 58, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#0703FF25, "2378",        512, 58, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600FD25, "2378",        512, 58, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1700FD25, "2378",        512, 58, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1700FF35, "2387",        512, 98, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1800F935, "2387",        512, 98, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1800FF35, "2388",        512, 98, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1500FF35, "2458",        512, 98, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600FF30, "2460",          0, 98,  0, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1600FF35, "2468",        512, 98, 28, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1701FF30, "2470",          0, 98,  0, 4096, ?sectorTable_213x(), lpc2xxx ),
     ?DEV( 16#1701FF35, "2478",        512, 98, 28, 4096, ?sectorTable_213x(), lpc2xxx )
].


rambase(DevType) ->    
    case DevType#device_type.variant of
	lpc2xxx -> ?LPC_RAMBASE_LPC2XXX;
	lpc17xx -> ?LPC_RAMBASE_LPC17XX;
	lpc13xx -> ?LPC_RAMBASE_LPC13XX;
	lpc11xx -> ?LPC_RAMBASE_LPC11XX
    end.

-ifdef(__not_used__).
ramstart(DevType) ->    
    case DevType#device_type.variant of
	lpc2xxx -> ?LPC_RAMSTART_LPC2XXX;
	lpc17xx -> ?LPC_RAMSTART_LPC17XX;
	lpc13xx -> ?LPC_RAMSTART_LPC13XX;
	lpc11xx -> ?LPC_RAMSTART_LPC11XX
    end.
-endif.


%% @doc
%%    Flash a firmware ihex-image from file onto a device
%% @end
-spec flash(Device::string()|port(), File::string()) -> ok | {error,term()}.

flash(Device, File) when is_list(Device), is_list(File) ->
    case open(Device) of
	{ok,U} ->
	    case sync(U, 30) of
		{ok,_} ->
		    case unlock(U) of
			{ok,_} ->
			    try flash(U, File) of
				ok -> go(U, 0);
				Result -> Result
			    catch
				error:Error -> {error,Error}
			    after
				uart:close(U)
			    end;
			Error -> 
			    Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end;
flash(U, File) when is_port(U), is_list(File) ->
    %% U must be synced and unlocked
    case filename:extension(File) of
	".ihex" ->
	    case elpcisp_ihex:load(File) of
		{ok,AddrLines} ->
		    flash_data(U, AddrLines);
		Error ->
		    Error
	    end;
	Ext ->
	    {error,{unsupported,Ext}}
    end.

flash_data(U, AddrLines) ->
    case read_device_type(U) of
	{ok,DevType} ->
	    BlockList = block_list(AddrLines, DevType),
	    flash_block_list(U, DevType, BlockList);
	Error ->
	    Error
    end.


flash_block_list(U, DevType, [{Start,StartBlock,EndBlock,Data}|Bs]) ->
    {ok,_} = prepare_sector(U, StartBlock, EndBlock),
    {ok,_} = erase_sector(U, StartBlock, EndBlock),
    flash_block(U, DevType, Start, Data),
    flash_block_list(U, DevType, Bs);
flash_block_list(_U, _DevType, []) ->
    ok.

flash_block(_U, _DevType, _Addr, <<>>) ->
    ok;
flash_block(U, DevType, Addr, Data) ->
    {Segment,Data1} = get_segment(Data, 512),
    Segment1 = patch_segment(Addr, DevType, Segment),
    {Block,BlockSize} = find_block(Addr, DevType#device_type.sectorTable),
    ?dbg("Write: ~8.16.0B [~w:~w]", [Addr,Block,BlockSize]),
    Base = rambase(DevType),
    ok = write_memory(U, Base, Segment1),
    {ok,_} = prepare_sector(U, Block, Block),
    {ok,_} = copy(U, Addr, Base, 512),
    flash_block(U, DevType, Addr+512, Data1).

patch_segment(0, DevType, Segment) -> %% lpc2xxx
    if DevType#device_type.variant  =:= lpc2xxx ->
	    <<V0:32/little, V1:32/little, V2:32/little, V3:32/little, 
	      V4:32/little, _V5:32/little, V6:32/little, V7:32/little,Rest/binary>> = Segment,
	    Sum = V0+V1+V2+V3+V4+V6+V7,
	    <<V0:32/little, V1:32/little, V2:32/little, V3:32/little, 
	      V4:32/little, (0 - Sum):32/little, V6:32/little, V7:32/little,Rest/binary>>;
       DevType#device_type.variant  =:= lpc17xx;
       DevType#device_type.variant  =:= lpc13xx;
       DevType#device_type.variant  =:= lpc11xx ->
	    <<V0:32/little, V1:32/little, V2:32/little, V3:32/little, 
	      V4:32/little, V5:32/little, V6:32/little, _V7:32/little,Rest/binary>> = Segment,
	    Sum = V0+V1+V2+V3+V4+V5+V6,
	    <<V0:32/little, V1:32/little, V2:32/little, V3:32/little, 
	      V4:32/little, V5:32/little, V6:32/little, (0 - Sum):32/little,Rest/binary>>
    end;
patch_segment(_Addr,_DevType,Segment) -> 
    Segment.

    
get_segment(Data, Len) ->
    case Data of
	<<Segment:Len/binary, Data1/binary>> ->
	    {Segment,Data1};
	Segment ->
	    Pad = (512 - byte_size(Segment)),
	    {<<Segment/binary, 0:Pad/unit:8>>, <<>>}
    end.
%%
%% given a sorted [{Addr,Data::binary()}] sorted on Addr
%% return a list of blocks that the Data will occupy.
%%
block_list(AddrDataList, ID) when is_integer(ID) ->
    case lists:keysearch(ID, #device_type.id, lpc_types()) of
	false -> {error, {not_such_id, ID}};
	{value,DevType} -> block_list(AddrDataList, DevType)
    end;
block_list([{Start,Data}|AddrDataList], DevType) when
      is_record(DevType,device_type) ->
    End = Start + byte_size(Data) - 1,
    {StartBlock,_} = find_block(Start, DevType#device_type.sectorTable),
    {EndBlock,_}   = find_block(End, DevType#device_type.sectorTable),
    [{Start,StartBlock,EndBlock,Data} | block_list(AddrDataList, DevType)];
block_list([], _DevType) ->
    [].

%% Find block and size 
find_block(Addr, SectorTable) ->
    find_block(Addr, 0, SectorTable).

find_block(Addr, I, [Size|_]) when Addr < Size -> {I,Size};
find_block(Addr, I, [Size|More]) -> find_block(Addr-Size, I+1, More).

