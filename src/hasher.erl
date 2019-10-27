-module(hasher).
-author("erikknaake").

-export([sha512/1]).

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
  calculateWt/3,
  calculateFullW/3,
  digest/2,
  calculateWorkers/4,
  initialWorkers/0,
  kConstants/0,
  calculateNextWorkers/4,
  compress/1,
  appendBits/2,
  hash/1,
  hash_block/2,
  binaryListToIntegerList/1,
  calculateIntermediateHashValue/2]).
-endif.

-spec sha512(binary()) -> binary().
sha512(Message) ->
  A = compress(hash(Message)),
  io:format("A: ~p~n", [A]),
  A.

-spec hash(binary()) -> list(binary()).
hash(Message) ->
  digest(preprocess(Message), initialWorkers()).

-spec digest(list(list(binary())), list(binary())) -> binary().
digest(Message, InitialWorkers) ->
      lists:foldl(fun hash_block/2, InitialWorkers, Message).

-spec compress(list(integer())) -> integer().
compress(Workers) ->
  lists:foldl(fun appendBits/2, <<>>, Workers).

-spec appendBits(integer(), binary()) -> binary().
appendBits(Value, Accumulator) ->
  appendBits(Value, Accumulator, 64).

-spec appendBits(integer(), binary(), integer()) -> binary().
appendBits(Value, Accumulator, BitSize) ->
  BinaryValue = <<Value:BitSize>>,
  <<Accumulator/big-binary, BinaryValue/big-binary>>.

-spec hash_block(list(binary()), list(binary())) -> list(binary()).
hash_block(MessageBlock, Workers) ->
  io:format("hashblock: ~p~n", [Workers]),
  calculateWorkers(Workers, [], calculateFullW(MessageBlock, [], 1), 1).

calculateIntermediateHashValue([A, B, C, D, E, F, G, H], [H0, H1, H2, H3, H4, H5, H6, H7]) ->
  [A + H0, B + H1, C + H2, D + H3, E + H4, F + H5, G + H6, H + H7].

-spec calculateWorkers(list(binary()), list(binary()), list(integer()), integer()) -> list(binary()).
calculateWorkers(Workers, PreviousWorkers, _, 81) ->
  calculateIntermediateHashValue(Workers, PreviousWorkers);
calculateWorkers(Workers, _, W, T) ->
 calculateWorkers(calculateNextWorkers(Workers, kConstants(), W, T), Workers, W, T + 1).

-spec calculateNextWorkers(list(integer()), list(integer()), list(integer()), integer()) -> list().
calculateNextWorkers([A, B, C, D, E, F, G, H], K, W, T) ->
  T1 = H + sum1(E) + ch(E, F, G) + lists:nth(T, K) + lists:nth(T, W),
  T2 = sum0(A) + maj(A, B, C),
  [
    T1 + T2,
    A,
    B,
    C,
    D + T1,
    E,
    F,
    G
  ].

-spec binaryListToIntegerList(list(binary())) -> list(integer()).
binaryListToIntegerList(BinaryList) ->
  lists:map(
    fun(Binary) ->
      <<Integer:64>> = Binary,
      Integer
    end, BinaryList).

-spec calculateFullW(list(binary()), list(binary()), integer()) -> list(integer()).
calculateFullW(_, W, 81) ->
  binaryListToIntegerList(W);
calculateFullW(MessageBlock, W, T) ->
  calculateFullW(MessageBlock,
    W ++ [calculateWt(MessageBlock, T, W)],
    T + 1
  ).

-spec calculateWt(list(binary()), integer(), list(binary())) -> binary().
calculateWt(MessageBlock, T, _) when T =< 15 ->
  lists:nth(T, MessageBlock);
calculateWt(_, T, W) ->
  %% Note erlang lists are indexed from 1, zo instead of -2, -7, -15 and -16 we have to use one less
  <<WMinus2:64>> = lists:nth(T - 1, W),
  <<WMinus7:64>> = lists:nth(T - 6, W),
  <<WMinus15:64>> = lists:nth(T - 14, W),
  <<WMinus16:64>> = lists:nth(T - 15, W),
  <<(sigma1(WMinus2) +
    WMinus7 +
    sigma0(WMinus15) +
    WMinus16):64>>.

-spec rotateLeft(binary(), integer()) -> integer().
rotateLeft(WordToRotate, RotateAmount) ->
  shiftLeft(WordToRotate, RotateAmount) bor shiftRight(WordToRotate, bit_size(<<WordToRotate>>) - RotateAmount).

-spec rotateRight(integer(), integer()) -> integer().
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

-spec sum0(integer()) -> integer().
sum0(Y) ->
  (rotateRight(Y, 28) bxor rotateRight(Y, 34)) bxor rotateRight(Y, 39).

-spec sum1(integer()) -> integer().
sum1(Y) ->
  rotateRight(Y, 14) bxor rotateRight(Y, 18) bxor rotateRight(Y, 41).

-spec sigma0(integer()) -> integer().
sigma0(Y) ->
  rotateRight(Y, 1) bxor rotateRight(Y, 8) bxor shiftRight(Y, 7).

-spec sigma1(integer()) -> integer().
sigma1(Y) ->
  rotateRight(Y, 19) bxor rotateRight(Y, 61) bxor shiftRight(Y, 6).

preprocess(<<Message/binary-unsigned-big>>) ->
  parse(padd(Message)).

% Parses the padded message into blocks of 1024 bits (128 bytes) made up of 16 blocks of 64 bits (8 bytes)
-spec parse(binary()) -> list(list(binary())).
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

% implements the modulo operation
-spec mod(integer(), integer()) -> integer().
mod(X, Y) when X > 0 ->
  X rem Y;
mod(X, Y) when X < 0 ->
  Y + X rem Y;
mod(0, _) ->
  0.

% Constants defined in chapter 5.3.5
-spec initialWorkers() -> list(binary()).
initialWorkers() ->
  [
    16#6a09e667f3bcc908,
    16#bb67ae8584caa73b,
    16#3c6ef372fe94f82b,
    16#a54ff53a5f1d36f1,
    16#510e527fade682d1,
    16#9b05688c2b3e6c1f,
    16#1f83d9abfb41bd6b,
    16#5be0cd19137e2179].

% Constaints defined in standard https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf Chapter 4.2.3
-spec kConstants() -> list(integer()).
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
