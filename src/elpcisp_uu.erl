%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    UU encode/decode
%%% @end
%%% Created : 10 Oct 2012 by Tony Rogvall <tony@rogvall.se>

-module(elpcisp_uu).

-include("elpcisp.hrl").

-export([encode/1, encode_csum/1]).
-export([decode/1, decode_csum/1]).
-import(lists, [reverse/1]).

-define(i2l(X), integer_to_list((X))).
%%
%% uuencode. Encode binary into uu encoding.
%%
-spec encode(Data::binary) -> [binary()].

encode(Data) when byte_size(Data) =< ?UU_MAX_LINES*?UU_MAX_LINE_LENGTH ->
    encode_(Data, 0, 0, [], []).

encode_csum(Data) when byte_size(Data) =< ?UU_MAX_LINES*?UU_MAX_LINE_LENGTH ->
    {Sum,Lines} = encode_(Data, 0, 0, [], []),
    CheckLine = list_to_binary(?i2l(Sum band 16#ffffffff)),
    Lines ++ [CheckLine].

encode_(Data, Sum, ?UU_MAX_LINE_LENGTH, Acc, Lines) ->
    Line = uue_line(?UU_MAX_LINE_LENGTH, Acc),
    encode_(Data, Sum, 0, [], [Line | Lines]);
encode_(<<X1,X2,X3,Data/binary>>,Sum,N,Acc,Lines) ->
    <<Y1:6,Y2:6,Y3:6,Y4:6>> = <<X1,X2,X3>>,
    encode_(Data,X1+X2+X3+Sum,N+3,uue_cons(Y4,Y3,Y2,Y1,Acc),Lines);
encode_(<<X1,X2>>,Sum,N,Acc,Lines) ->
    <<Y1:6,Y2:6,Y3:6,Y4:6>> = <<X1,X2,0>>,
    Line = uue_line(N+2, uue_cons(Y4,Y3,Y2,Y1,Acc)),
    uue_final(Line,Lines,X1+X2+Sum);
encode_(<<X1>>,Sum,N,Acc,Lines) ->
    <<Y1:6,Y2:6,Y3:6,Y4:6>> = <<X1,0,0>>,
    Line = uue_line(N+1,uue_cons(Y4,Y3,Y2,Y1,Acc)),
    uue_final(Line,Lines,X1+Sum);
encode_(<<>>,Sum,0,[],Lines) ->
    CheckLine = list_to_binary(?i2l(Sum band 16#ffffffff)),
    reverse([CheckLine|Lines]);
encode_(<<>>,Sum,N,Acc,Lines) ->
    Line = uue_line(N, Acc),
    uue_final(Line,Lines,Sum).

uue_final(Line,Lines,Sum) ->
    {Sum, reverse([Line|Lines])}.

%% encode a single byte
-define(uue(I), if (I) =:= 0 -> 96; (I) < 64 -> (I)+32	end).

uue_cons(A,B,C,D,Acc) ->
    [?uue(A),?uue(B),?uue(C),?uue(D)|Acc].
    
uue_line(Len, RCs) ->
    list_to_binary([?uue(Len)|reverse(RCs)]).


%% uudecode with checksum
-spec decode_csum([Line::binary]) -> {ok,binary()} | {error,bad_checksum}.

decode_csum(Lines) ->
    [ISum|Lines1] = reverse(Lines),
    CSum = list_to_integer(binary_to_list(ISum)),
    {Sum,Data} = decode(reverse(Lines1)),
    if CSum =:= (Sum band 16#ffffffff) ->
	    {ok,Data};
       true ->
	    {error,bad_checksum}
    end.

%% plain uudecode
-spec decode([Line::binary]) -> {CheckSum::integer(),binary()}.

decode(Lines) ->
    decode_(Lines, 0, []).

-define(uud(I), if (I) =:= 96 -> 0;
		   (I) > 32, I < 96 -> (I)-32
		end).

decode_([], Sum, Acc) ->
    {Sum, list_to_binary(reverse(Acc))};
decode_([<<L,Data/binary>> | Lines], Sum, Acc) ->
    decode_(Data, Sum, ?uud(L), Lines, Acc).

decode_(<<Y1,Y2,Y3,Y4,Data/binary>>,Sum,L,Lines,Acc) ->
    <<X1,X2,X3>> = <<(?uud(Y1)):6,(?uud(Y2)):6,(?uud(Y3)):6,(?uud(Y4)):6>>,
    case L of
	1 -> decode_(Lines,Sum+X1,[X1|Acc]);
	2 -> decode_(Lines,Sum+X1+X2,[X2,X1|Acc]);
	_ -> decode_(Data, X1+X2+X3+Sum, L-3, Lines, [X3,X2,X1|Acc])
    end;
decode_(<<>>,Sum,_L,Lines,Acc) ->
    decode_(Lines,Sum,Acc).

