%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Read intel hex format
%%% @end
%%% Created : 11 Sep 2012 by Tony Rogvall <tony@rogvall.se>

-module(elpcisp_ihex).

-compile(export_all).
-import(lists, [reverse/1]).

load(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    {_Start,Lines} = decode(Bin),
	    {ok,merge(Lines)};
	Error ->
	    Error
    end.

merge(Lines) ->
    case lists:keysort(1, Lines) of
	[{Addr0,Data}|Rest] ->
	    merge(Addr0,Data,Rest,[]);
	[] ->
	    []
    end.

merge(Addr,Data,[{Addr1,Data1}|Rest],Acc) when Addr1 =:= Addr+size(Data) ->
    merge(Addr,<<Data/binary,Data1/binary>>,Rest,Acc);
merge(Addr,Data,[{Addr1,Data1}|Rest],Acc) ->
    merge(Addr1,Data1,Rest,[{Addr,Data}|Acc]);
merge(Addr,Data,[],Acc) ->
    reverse([{Addr,Data}|Acc]).

decode(Bin) ->
    decode(Bin, 0, 0, 0, []).

decode(<<$\n,Bin/binary>>, Start, ExtBase, SegBase, Acc) ->
    decode(Bin, Start, ExtBase, SegBase, Acc);
decode(<<$\r,Bin/binary>>, Start, ExtBase, SegBase, Acc) ->  
    decode(Bin, Start, ExtBase, SegBase, Acc);
decode(<<$:,L1,L2,A1,A2,A3,A4,T1,T2,Bin/binary>>,Start,ExtBase,SegBase,Acc) ->
    Len = uint8(L1,L2),
    Addr = uint16(A1,A2,A3,A4),
    Type = uint8(T1,T2),
    {Rec,<<C1,C2,Bin2/binary>>} = split_binary(Bin,Len*2),
    %% io:format("Record: type=~p, addr=~p, len=~p, data=~p\n", [Type,Addr,Len,Rec]),
    CheckSum = uint8(C1,C2),
    case checksum(<<L1,L2,A1,A2,A3,A4,T1,T2,Rec/binary>>,0) of
	CheckSum ->
	    decode_rec(Type,Addr,Rec,Start,ExtBase,SegBase,Bin2,Acc);
	_ ->
	    erlang:error(checksum_error)
    end;
decode(<<>>, Start, _ExtBase, _SegBase, Acc) ->
    {Start, reverse(Acc)}.

decode_rec(0,Addr,Rec,Start,ExtBase,SegBase,Bin,Acc) ->
    %% Data record
    Data = decode_data(Rec,[]),
    decode(Bin, Start, ExtBase, SegBase, [{ExtBase+SegBase+Addr,Data}|Acc]);
decode_rec(1,Addr,_Rec,Start,_ExtBase,_SegBase,_Bin,Acc) ->
    %% An end record
    if Start =:= 0 ->
	    {Addr,reverse(Acc)};
       true ->
	    {Start,reverse(Acc)}
    end;
decode_rec(2,_Addr,<<A1,A2,A3,A4>>,Start,ExtBase,_SegBase,Bin,Acc) ->
    %% An extended address record.  */
    SegBase1 = uint16(A1,A2,A3,A4) bsl 4,
    decode(Bin,Start,ExtBase, SegBase1, Acc);
decode_rec(3,_Addr,<<A1,A2,A3,A4,B1,B2,B3,B4>>,Start,ExtBase,SegBase,Bin,Acc) ->
    %% An extended start address record.
    Start1 = Start + (uint16(A1,A2,A3,A4) bsl 4) + uint16(B1,B2,B3,B4),
    decode(Bin,Start1,ExtBase, SegBase, Acc);
decode_rec(4,_Addr,<<A1,A2,A3,A4>>,Start,_ExtBase,SegBase,Bin,Acc) ->
    %% An extended linear address record.
    ExtBase1 = uint16(A1,A2,A3,A4) bsl 16,
    decode(Bin,Start,ExtBase1, SegBase, Acc);
decode_rec(5,_Addr,<<A1,A2,A3,A4>>,Start,ExtBase,SegBase,Bin,Acc) ->
    %% An extended linear start address record.
    Start1 = Start + (uint16(A1,A2,A3,A4) bsl 16),
    decode(Bin,Start1,ExtBase, SegBase, Acc); 
decode_rec(5,_Addr,<<A1,A2,A3,A4,B1,B2,B3,B4>>,_Start,ExtBase,SegBase,Bin,Acc) ->
    %% An extended linear start address record.
    Start1 = (uint16(A1,A2,A3,A4) bsl 16) + uint16(B1,B2,B3,B4),
    decode(Bin,Start1,ExtBase, SegBase, Acc).

checksum(<<X1,X2,Bin/binary>>, Csum) ->
    X = uint8(X1,X2),
    checksum(Bin, Csum+X);
checksum(<<>>, Csum) ->
    (-Csum) band 16#ff.

decode_data(<<>>, Acc) ->
    list_to_binary(reverse(Acc));
decode_data(<<X1,X2,Bin/binary>>, Acc) ->
    decode_data(Bin, [uint8(X1,X2) | Acc]).


uint16(X1,X2,X3,X4) ->
    (uint8(X1,X2) bsl 8) + uint8(X3,X4).

uint8(X1,X2) ->
    H0 = X1-$0,
    H1 = if H0 > 9 -> (H0 band 16#1f) - 7; true -> H0 end,
    L0 = X2-$0,
    L1 = if L0 > 9 -> (L0 band 16#1f) - 7; true -> L0 end,
    (H1 bsl 4)+L1.



