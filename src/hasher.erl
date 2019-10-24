-module(hasher).
-author("erikknaake").

-export([sha512/0]).

% export all functions when we are running in testmode, this makes it easy to unit test smaller units of work
-ifdef(TEST).
-export([padd/1,
  addBit/1,
  paddZeroes/2,
  numberOfZeroesToAdd/1,
  lengthPadd/2,
  parse/1,
  preprocess/1,
  splitToNByteBlocks/2,
  rotateLeft/2,
  rotateRight/2,
  shiftRight/2,
  shiftLeft/2,
  ch/3,
  maj/3,
  sum0/1,
  sum1/1,
  sigma0/1,
  sigma1/1,
  %calculateW/3,
  %calculateWForBlock/3
  calculateWt/3,
  calculateFullW/3
]).
-endif.

sha512() ->
  calculateWt(preprocess(<<"Hello">>), 1, []).
%hashRound(preprocess(Message), initialHashValue(), nil, 79).

%%hashRound(I, Message) when I =< length(Message) ->
%%  W = calculateW().
%%
%%hashBlocks([]) ->
%%  ok.
%%
%%hashBlocks([H|T]) ->
%%  ok.

%%-spec extend(list(binary())) -> binary().
%%extend([], _) ->
%%  terminate;
%%extend(Message, I) when I == 1 ->
%%  calculateWt(lists:nth(I, Message), ).

-spec digest(list(binary()), integer(), list(binary())) -> binary().
digest(Message, I, Workers) when I == length(Message) ->
  Workers;
digest([MessageBlock | Message], I, Workers) ->
  calculateFullW(MessageBlock, [], 0),
  digest(Message, I + 1, Workers).

-spec calculateFullW(list(binary()), list(binary()), integer()) -> list(binary()).
calculateFullW(MessageBlock, W, T) when T =< 80 ->
  NextW = [W | calculateWt(MessageBlock, T, W)],
  calculateFullW(MessageBlock, NextW, T + 1);
calculateFullW(_, W, _) ->
  W.

-spec calculateWt(binary(), integer(), list(binary())) -> binary().
calculateWt(MessageBlock, T, _) when T =< 15 ->
  lists:nth(T, MessageBlock);
calculateWt(_, T, W) ->
  %% Note erlang lists are indexed from 1, zo instead of -2, -7, -15 and -16 we have to use one less
  <<WMinus7:64>> = lists:nth(T - 6, W),
  <<WMinus16:64>> = lists:nth(T - 15, W),
  io:format("Sigma1(W(T - 2)): ~p~n: ", [sigma1(lists:nth(T - 1, W))]),
  io:format("Sigma0(W(T - 15)): ~p~n: ", [sigma0(lists:nth(T - 14, W))]),
  io:format("W(T - 7): ~p~n: ", [WMinus7]),
  io:format("W(T - 16): ~p~n: ", [WMinus16]),
  (sigma1(lists:nth(T - 1, W)) +
    WMinus7 +
    sigma0(lists:nth(T - 14, W)) +
    WMinus16).


%%
%%calculateW(Message, I, W) when I =< length(Message) ->
%%  io:format("Message: ~p~nI: ~p~nW: ~p~n", [Message, I, W]),
%%  calculateW(Message, I + 1, [W | calculateWForBlock(lists:nth(I, Message), 1, [])]).
%%
%%calculateWForBlock(MessageBlock, 80, W) ->
%%  io:format("MessageBlock: ~p~nT: ~p~nW: ~p~n", [MessageBlock, 80, W]),
%%  W;
%%
%%calculateWForBlock(MessageBlock, T, W) when T =< 15 ->
%%  io:format("MessageBlock: ~p~nT: ~p~nW: ~p~n", [MessageBlock, T, W]),
%%  calculateWForBlock(MessageBlock,
%%    T + 1,
%%    W ++
%%      lists:nth(T, MessageBlock)
%%    );
%%
%%calculateWForBlock(MessageBlock, T, W) when T > 15 ->
%%  io:format("MessageBlock: ~p~nT: ~p~nW: ~p~n", [MessageBlock, T, W]),
%%  calculateWForBlock(MessageBlock,
%%    T + 1,
%%    W ++
%%      sigma1(lists:nth(T - 2, W)) +
%%        lists:nth(T - 7, W) +
%%        sigma0(lists:nth(T - 15, W) +
%%          lists:nth(T - 16, W))
%%    ). %TODO: check whether or not nth is 0 based

%%% Final hash round
%%hashRound(PreProcessedMessage, Workers, PreviousWorkers, 79) ->
%%  lists:zipwith(fun(X, Y) -> X + Y end, Workers, PreviousWorkers).
%%
%%hashRound(PreProcessedMessage, Workers, PreviousWorkers, Count) ->
%%  T1 = nth(1, PreviousWorkers) +
%%    sum1(nth(5, PreviousWorkers)) +
%%    ch(nth(5, PreviousWorkers), nth(6, PreviousWorkers), nth(7, PreviousWorkers)) +
%%    nth(Count, kConstants()) +
%%    wT(Count),
%%  T2 = sum0(nth(1, PreviousWorkers)) +
%%    maj(nth(1, PreviousWorkers), nth(2, PreviousWorkers), nth(3, PreviousWorkers)),
%%
%%  hashRound(PreProcessedMessage, nil, Workers, Count + 1).

initialHashValue() ->
  [16#6a09e667f3bcc908, 16#bb67ae8584caa73b,
    16#3c6ef372fe94f82b, 16#a54ff53a5f1d36f1,
    16#510e527fade682d1, 16#9b05688c2b3e6c1f,
    16#1f83d9abfb41bd6b, 16#5be0cd19137e2179].

-spec rotateLeft(binary(), integer()) -> integer().
rotateLeft(WordToRotate, RotateAmount) ->
  shiftLeft(WordToRotate, RotateAmount) bor shiftRight(WordToRotate, bit_size(<<WordToRotate>>) - RotateAmount).

rotateRight(V, Count) ->
  Rest = 64 - Count,
  <<Top:Rest/unsigned, Bottom:Count/unsigned>> = <<V:64/big-unsigned>>,
  <<New:64/big-unsigned>> = <<Bottom:Count/unsigned, Top:Rest/unsigned>>,
  New.

-spec shiftRight(binary(), integer()) -> binary().
shiftRight(WordToShift, ShiftAmount) ->
  WordToShift bsr ShiftAmount.

-spec shiftLeft(binary(), integer()) -> binary().
shiftLeft(WordToShift, ShiftAmount) ->
  <<Result>> = <<(WordToShift bsl ShiftAmount):(bit_size(<<WordToShift>>))>>,
  Result.

ch(X, Y, Z) ->
  (X band Y) bxor (bnot X band Z).

maj(X, Y, Z) ->
  (X band Y) bxor (X band Z) bxor (Y band Z).

-spec sum0(binary()) -> integer().
sum0(X) ->
  <<Y:64/big-unsigned>> = X,
  (rotateRight(Y, 28) bxor rotateRight(Y, 34)) bxor rotateRight(Y, 39).

-spec sum1(binary()) -> integer().
sum1(X) ->
  <<Y:64>> = X,
  rotateRight(Y, 14) bxor rotateRight(Y, 18) bxor rotateRight(Y, 41).

-spec sigma0(binary()) -> integer().
sigma0(X) ->
  <<Y:64>> = X,
  rotateRight(Y, 1) bxor rotateRight(Y, 8) bxor shiftRight(Y, 7).

-spec sigma1(binary()) -> integer().
sigma1(X) ->
  <<Y:64>> = X,
  rotateRight(Y, 19) bxor rotateRight(Y, 61) bxor shiftRight(Y, 6).

preprocess(<<Message/binary-unsigned-big>>) ->
  parse(padd(Message)).

% Parses the padded message into blocks of 1024 bits (128 bytes) made up of blocks of 64 bits (8 bytes)
parse(<<PaddedMessage/binary-unsigned-big>>) ->
  [splitToNByteBlocks(X, 8) || X <- splitToNByteBlocks(PaddedMessage, 128)].

splitToNByteBlocks(<<Bin/binary-unsigned-big>>, LenPart) ->
  lists:reverse(splitToNByteBlocksInternal(Bin, LenPart, [])).

splitToNByteBlocksInternal(<<Bin/binary-unsigned-big>>, LenPart, Acc) when byte_size(Bin) =< LenPart ->
  [Bin | Acc];
splitToNByteBlocksInternal(Bin, LenPart, Acc) ->
  <<Part:LenPart/binary, Rest/binary>> = Bin,
  splitToNByteBlocksInternal(Rest, LenPart, [Part | Acc]).

% Padds the message
padd(<<MessageToPadd/binary-unsigned-big>>) ->
  MessageLength = bit_size(MessageToPadd),
  lengthPadd(
    paddZeroes(
      addBit(MessageToPadd),
      MessageLength),
    MessageLength).

% Adds a single 1 bit to the bitstring to ensure at least some padding will be done
addBit(<<MessageToAppend/binary-unsigned-big>>) ->
  <<MessageToAppend/bitstring-unsigned-big, <<1:1>>/bitstring-unsigned-big>>.

% Adds the zeroes until the bitstring is 896 bits
paddZeroes(<<UnpaddedMessage/bitstring-unsigned-big>>, MessageLength) ->
  <<UnpaddedMessage/bitstring-unsigned-big, <<0:(numberOfZeroesToAdd(MessageLength))>>/bitstring-unsigned-big>>.

% Calculates the number of zeroes to pad until the message pre-length padded is 896
numberOfZeroesToAdd(MessageLength) ->
  mod(896 - (MessageLength + 1), 1024). % 896 magic constant from chapter 5.1.2, + 1 because message is padded with a bit

% Adds the 128 bit block that is all zeroes until the message length makes it fit
lengthPadd(<<ZeroPaddedMessage/bitstring-unsigned-big>>, MessageLength) ->
  <<ZeroPaddedMessage/bitstring-unsigned-big, <<MessageLength:128>>/bitstring-unsigned-big>>.

mod(X, Y) when X > 0 ->
  X rem Y;
mod(X, Y) when X < 0 ->
  Y + X rem Y;
mod(0, _) ->
  0.

% parseToMessageBlocks() ->
% 	.

% initialHashValue() ->
% 	.

% % Constaints defined in standard https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf Chapter 4.2.3
kConstants() ->
  [
    16#428a2f98d728ae22, 16#7137449123ef65cd, 16#b5c0fbcfec4d3b2f, 16#e9b5dba58189dbbc,
    16#d807aa98a3030242, 16#12835b0145706fbe, 16#243185be4ee4b28c, 16#550c7dc3d5ffb4e2,
    16#3956c25bf348b538, 16#59f111f1b605d019, 16#923f82a4af194f9b, 16#ab1c5ed5da6d8118,
    16#72be5d74f27b896f, 16#80deb1fe3b1696b1, 16#9bdc06a725c71235, 16#c19bf174cf692694,
    16#e49b69c19ef14ad2, 16#efbe4786384f25e3, 16#0fc19dc68b8cd5b5, 16#240ca1cc77ac9c65,
    16#2de92c6f592b0275, 16#4a7484aa6ea6e483, 16#5cb0a9dcbd41fbd4, 16#76f988da831153b5,
    16#983e5152ee66dfab, 16#a831c66d2db43210, 16#b00327c898fb213f, 16#bf597fc7beef0ee4,
    16#c6e00bf33da88fc2, 16#d5a79147930aa725, 16#06ca6351e003826f, 16#142929670a0e6e70,
    16#27b70a8546d22ffc, 16#2e1b21385c26c926, 16#4d2c6dfc5ac42aed, 16#53380d139d95b3df,
    16#650a73548baf63de, 16#766a0abb3c77b2a8, 16#81c2c92e47edaee6, 16#92722c851482353b,
    16#a2bfe8a14cf10364, 16#a81a664bbc423001, 16#c24b8b70d0f89791, 16#c76c51a30654be30,
    16#d192e819d6ef5218, 16#d69906245565a910, 16#f40e35855771202a, 16#106aa07032bbd1b8,
    16#19a4c116b8d2d0c8, 16#1e376c085141ab53, 16#2748774cdf8eeb99, 16#34b0bcb5e19b48a8,
    16#391c0cb3c5c95a63, 16#4ed8aa4ae3418acb, 16#5b9cca4f7763e373, 16#682e6ff3d6b2b8a3,
    16#748f82ee5defb2fc, 16#78a5636f43172f60, 16#84c87814a1f0ab72, 16#8cc702081a6439ec,
    16#90befffa23631e28, 16#a4506cebde82bde9, 16#bef9a3f7b2c67915, 16#c67178f2e372532b,
    16#ca273eceea26619c, 16#d186b8c721c0c207, 16#eada7dd6cde0eb1e, 16#f57d4f7fee6ed178,
    16#06f067aa72176fba, 16#0a637dc5a2c898a6, 16#113f9804bef90dae, 16#1b710b35131c471b,
    16#28db77f523047d84, 16#32caab7b40c72493, 16#3c9ebe0a15c9bebc, 16#431d67c49c100d4c,
    16#4cc5d4becb3e42b6, 16#597f299cfc657e2a, 16#5fcb6fab3ad6faec, 16#6c44198c4a475817].
