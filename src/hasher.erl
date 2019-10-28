-module(hasher).
-author("erikknaake").

-export([sha512/1, printBinaryAsHex/2, sha512AndPrint/1]).

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
  rotateRight/2,
  shiftRight/2,
  ch/3,
  maj/3,
  sum0/1,
  sum1/1,
  sigma0/1,
  sigma1/1,
  calculateMessageSchedulePart/3,
  calculateMessageSchedule/3,
  digest/2,
  calculateWorkers/2,
  initialWorkers/0,
  kConstants/0,
  calculateNextWorkers/4,
  compress/1,
  appendBits/2,
  hash/1,
  hash_block/2,
  binaryListToIntegerList/1,
  calculateIntermediateHashValue/2,
  calculateMessageSchedule/1,
  calculateWorkers/2, binaryListToBinary/1]).
-endif.

-define(K512, <<16#428A2F98D728AE22:64/big-unsigned,
  16#7137449123EF65CD:64/big-unsigned,
  16#B5C0FBCFEC4D3B2F:64/big-unsigned,
  16#E9B5DBA58189DBBC:64/big-unsigned,
  16#3956C25BF348B538:64/big-unsigned,
  16#59F111F1B605D019:64/big-unsigned,
  16#923F82A4AF194F9B:64/big-unsigned,
  16#AB1C5ED5DA6D8118:64/big-unsigned,
  16#D807AA98A3030242:64/big-unsigned,
  16#12835B0145706FBE:64/big-unsigned,
  16#243185BE4EE4B28C:64/big-unsigned,
  16#550C7DC3D5FFB4E2:64/big-unsigned,
  16#72BE5D74F27B896F:64/big-unsigned,
  16#80DEB1FE3B1696B1:64/big-unsigned,
  16#9BDC06A725C71235:64/big-unsigned,
  16#C19BF174CF692694:64/big-unsigned,
  16#E49B69C19EF14AD2:64/big-unsigned,
  16#EFBE4786384F25E3:64/big-unsigned,
  16#0FC19DC68B8CD5B5:64/big-unsigned,
  16#240CA1CC77AC9C65:64/big-unsigned,
  16#2DE92C6F592B0275:64/big-unsigned,
  16#4A7484AA6EA6E483:64/big-unsigned,
  16#5CB0A9DCBD41FBD4:64/big-unsigned,
  16#76F988DA831153B5:64/big-unsigned,
  16#983E5152EE66DFAB:64/big-unsigned,
  16#A831C66D2DB43210:64/big-unsigned,
  16#B00327C898FB213F:64/big-unsigned,
  16#BF597FC7BEEF0EE4:64/big-unsigned,
  16#C6E00BF33DA88FC2:64/big-unsigned,
  16#D5A79147930AA725:64/big-unsigned,
  16#06CA6351E003826F:64/big-unsigned,
  16#142929670A0E6E70:64/big-unsigned,
  16#27B70A8546D22FFC:64/big-unsigned,
  16#2E1B21385C26C926:64/big-unsigned,
  16#4D2C6DFC5AC42AED:64/big-unsigned,
  16#53380D139D95B3DF:64/big-unsigned,
  16#650A73548BAF63DE:64/big-unsigned,
  16#766A0ABB3C77B2A8:64/big-unsigned,
  16#81C2C92E47EDAEE6:64/big-unsigned,
  16#92722C851482353B:64/big-unsigned,
  16#A2BFE8A14CF10364:64/big-unsigned,
  16#A81A664BBC423001:64/big-unsigned,
  16#C24B8B70D0F89791:64/big-unsigned,
  16#C76C51A30654BE30:64/big-unsigned,
  16#D192E819D6EF5218:64/big-unsigned,
  16#D69906245565A910:64/big-unsigned,
  16#F40E35855771202A:64/big-unsigned,
  16#106AA07032BBD1B8:64/big-unsigned,
  16#19A4C116B8D2D0C8:64/big-unsigned,
  16#1E376C085141AB53:64/big-unsigned,
  16#2748774CDF8EEB99:64/big-unsigned,
  16#34B0BCB5E19B48A8:64/big-unsigned,
  16#391C0CB3C5C95A63:64/big-unsigned,
  16#4ED8AA4AE3418ACB:64/big-unsigned,
  16#5B9CCA4F7763E373:64/big-unsigned,
  16#682E6FF3D6B2B8A3:64/big-unsigned,
  16#748F82EE5DEFB2FC:64/big-unsigned,
  16#78A5636F43172F60:64/big-unsigned,
  16#84C87814A1F0AB72:64/big-unsigned,
  16#8CC702081A6439EC:64/big-unsigned,
  16#90BEFFFA23631E28:64/big-unsigned,
  16#A4506CEBDE82BDE9:64/big-unsigned,
  16#BEF9A3F7B2C67915:64/big-unsigned,
  16#C67178F2E372532B:64/big-unsigned,
  16#CA273ECEEA26619C:64/big-unsigned,
  16#D186B8C721C0C207:64/big-unsigned,
  16#EADA7DD6CDE0EB1E:64/big-unsigned,
  16#F57D4F7FEE6ED178:64/big-unsigned,
  16#06F067AA72176FBA:64/big-unsigned,
  16#0A637DC5A2C898A6:64/big-unsigned,
  16#113F9804BEF90DAE:64/big-unsigned,
  16#1B710B35131C471B:64/big-unsigned,
  16#28DB77F523047D84:64/big-unsigned,
  16#32CAAB7B40C72493:64/big-unsigned,
  16#3C9EBE0A15C9BEBC:64/big-unsigned,
  16#431D67C49C100D4C:64/big-unsigned,
  16#4CC5D4BECB3E42B6:64/big-unsigned,
  16#597F299CFC657E2A:64/big-unsigned,
  16#5FCB6FAB3AD6FAEC:64/big-unsigned,
  16#6C44198C4A475817:64/big-unsigned>>).


% Makes sure additions are done mod 2^64
add64(X, Y) ->
  (X + Y) band 16#FFFFFFFFFFFFFFFF.
-define(ADD64(X, Y), (X + Y) band 16#FFFFFFFFFFFFFFFF).

-spec sha512(binary()) -> binary().
sha512(Message) ->
  A = compress(hash(Message)),
  io:format("A: ~p~n", [A]),
  A.

-spec printBinaryAsHex(binary(), integer()) -> atom().
printBinaryAsHex(Binary, BitSize) ->
  <<Integer:BitSize>> = Binary,
  io:format("~.16#~n", [Integer]).

-spec sha512AndPrint(binary()) -> atom().
sha512AndPrint(Message) ->
  printBinaryAsHex(sha512(Message), 512).

-spec hash(binary()) -> list(binary()).
hash(Message) ->
  digest(preprocess(Message), initialWorkers()).

-spec digest(list(list(binary())), list(integer())) -> binary().
digest(Message, InitialWorkers) ->
      lists:foldl(
        fun(MessageBlock, PreviousWorkers) ->
%%          io:format("PreviousWorkers: ~p~n", [integerListToBinaryList(PreviousWorkers)]),
%%          io:format("Messageblock: ~p~n", [MessageBlock]),
          hash_block(MessageBlock, PreviousWorkers)
        end, InitialWorkers, Message).

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
hash_block(MessageBlock, PreviousWorkers) ->
  calculateWorkers(PreviousWorkers, calculateMessageSchedule(MessageBlock)).

-spec calculateIntermediateHashValue(list(integer()), list(integer())) -> list(integer()).
calculateIntermediateHashValue(Workers, HashValues) ->
  lists:map(fun({HashValue, Worker}) -> add64(HashValue, Worker) end, lists:zip(HashValues, Workers)).

%%-spec calculateWorkers(list(binary()), list(integer())) -> list(binary()).
calculateWorkers(InitialWorkers, MessageSchedule) ->
  sha512_loop(MessageSchedule, InitialWorkers, InitialWorkers, 0).
sha512_loop(_W, Hashes, Next, 80) ->
  lists:map(fun({X, Y}) -> ?ADD64(X, Y) end, lists:zip(Hashes, Next));
sha512_loop(W, Hashes, [A, B, C, D, E, F, G, H], Count) ->
%%  io:format("Loop W: ~p~n", [W]),
  S0 = rotateRight(A, 28) bxor rotateRight(A, 34) bxor rotateRight(A, 39),
  Maj = (A band B) bxor (A band C) bxor (B band C),
  T2 = ?ADD64(S0, Maj),
  S1 = rotateRight(E, 14) bxor rotateRight(E, 18) bxor rotateRight(E, 41),
  Ch = (E band F) bxor (((bnot E) + 1 + 16#FFFFFFFFFFFFFFFF) band G),
  Offset = Count * 8,
  <<_:Offset/binary, K:64/big-unsigned, _/binary>> = ?K512,
  <<_:Offset/binary, Wval:64/big-unsigned, _/binary>> = <<W/binary>>,
  io:format("Loop A: ~p, B: ~p, C: ~p, D: ~p, E: ~p, F: ~p, G: ~p, H: ~p~n", [A, B, C, D, E, F, G, H]),
  T1 = (H + S1 + Ch + K + Wval) band 16#FFFFFFFFFFFFFFFF,
  sha512_loop(W, Hashes, [?ADD64(T1, T2), A, B, C, ?ADD64(D, T1), E, F, G],
    Count+1).
%%-spec calculateWorkers(list(binary()), list(binary()), list(integer()), integer()) -> list(binary()).
%%calculateWorkers(Workers, PreviousWorkers, _, 80) ->
%%%%  io:format("Final Workers: ~p~nPreviousWorkers: ~p~n", [Workers, PreviousWorkers]),
%%  calculateIntermediateHashValue(Workers, PreviousWorkers);
%%calculateWorkers(Workers, PrevWorkers, MessageSchedule, T) ->
%%%%  io:format("Workers: ~p~nPreviousWorkers: ~p~n", [Workers, PrevWorkers]),
%% calculateWorkers(calculateNextWorkers(Workers, kConstants(), MessageSchedule, T), Workers, MessageSchedule, T + 1).

-spec calculateNextWorkers(list(integer()), list(integer()), list(integer()), integer()) -> list().
calculateNextWorkers([A, B, C, D, E, F, G, H], K, W, Count) ->
%%  Offset = T * 8,
%%  <<_:Offset/binary, Wt:64/big-unsigned, _/binary>> = <<W/binary>>,
%%  Kt = lists:nth(T, K),
%%  T1 = add64(add64(add64(add64(H, sum1(E)), ch(E, F, G)), Kt), Wt),
%%  T2 = add64(sum0(A), maj(A, B, C)),
%%  [
%%    add64(T1, T2),
%%    A,
%%    B,
%%    C,
%%    add64(D, T1),
%%    E,
%%    F,
%%    G
%%  ].
%%  io:format("Loop W: ~p~n", [W]),
  S0 = rotateRight(A, 28) bxor rotateRight(A, 34) bxor rotateRight(A, 39),
  Maj = (A band B) bxor (A band C) bxor (B band C),
  T2 = add64(S0, Maj),
  S1 = rotateRight(E, 14) bxor rotateRight(E, 18) bxor rotateRight(E, 41),
  Ch = (E band F) bxor (((bnot E) + 1 + 16#FFFFFFFFFFFFFFFF) band G),
  Offset = Count * 8,
  KVal = lists:nth(Count, K),
  <<_:Offset/binary, Wval:64/big-unsigned, _/binary>> = <<W/binary>>,
  io:format("Loop A: ~p, B: ~p, C: ~p, D: ~p, E: ~p, F: ~p, G: ~p, H: ~p~n", [A, B, C, D, E, F, G, H]),
  T1 = (H + S1 + Ch + KVal + Wval) band 16#FFFFFFFFFFFFFFFF,
  [
    add64(T1, T2),
    A,
    B,
    C,
    add64(D, T1),
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

-spec integerListToBinaryList(list(integer())) -> list(binary()).
integerListToBinaryList(IntegerList) ->
  lists:map(
    fun(Integer) ->
      Binary = <<Integer:64>>,
      Binary
    end, IntegerList).


-spec calculateMessageSchedule(list(binary())) -> list(integer()).
calculateMessageSchedule(MessageBlock) ->
  calculateMessageSchedule(MessageBlock, [], 1).
-spec calculateMessageSchedule(list(binary()), list(binary()), integer()) -> list(integer()).
calculateMessageSchedule(_, W, 81) ->
  binaryListToIntegerList(W);
calculateMessageSchedule(MessageBlock, _, _) ->
%%  calculateMessageSchedule(MessageBlock,
%%  io:format("Message block schedule: ~p~n", [MessageBlock]),
%%  io:format("Message block schedule as binary: ~p~n", [binaryListToBinary(MessageBlock)]),
  calculateMessageSchedulePart(MessageBlock, 16, binaryListToBinary(MessageBlock)).
%%    T + 1
%%  ).

-spec binaryListToBinary(list(binary())) -> binary().
binaryListToBinary(MessageBlock) ->
  lists:foldl(
    fun(Binary, Result) ->
      concatBinary(Result, Binary)
    end, <<>>, MessageBlock).

-spec concatBinary(binary(), binary()) -> binary().
concatBinary(Bin1, Bin2) ->
  <<Bin1/binary, Bin2/binary>>.

%%first16(_, W, 16) ->
%%  W;
%%first16(MessageBlock, W, T) ->
%%  first16(MessageBlock, W ++ lists:nth(T, MessageBlock), T + 1).
-spec calculateMessageSchedulePart(list(binary()), integer(), list(binary())) -> binary().
%%calculateMessageSchedulePart(MessageBlock, T, _) when T =< 16 ->
%%  lists:nth(T, MessageBlock);
calculateMessageSchedulePart(_, T, W) ->
  %% Note erlang lists are indexed from 1, zo instead of -2, -7, -15 and -16 we have to use one less
  sha512_extend(W, T).
%%  <<WMinus2:64>> = lists:nth(T - 1, W),
%%  <<WMinus7:64>> = lists:nth(T - 6, W),
%%  <<WMinus15:64>> = lists:nth(T - 14, W),
%%  <<WMinus16:64>> = lists:nth(T - 15, W),
%%  S0 = rotateRight(WMinus2, 1) bxor rotateRight(WMinus2, 8) bxor (WMinus2 bsr 7),
%%  S1 = rotateRight(WMinus15, 19) bxor rotateRight(WMinus15, 61) bxor (WMinus15 bsr 6),
%%  Next = (WMinus16 + S0 + WMinus7 + S1) band 16#FFFFFFFFFFFFFFFF,
%%  <<Next:64>>.
%%%%  <<(add64(
%%%%      add64(
%%%%        add64(
%%%%          sigma1(WMinus2),
%%%%          WMinus7),
%%%%        sigma0(WMinus15)),
%%%%      WMinus16)):64>>.

sha512_extend(W, 80) ->
  W;
sha512_extend(W, Count) ->
  Off1 = (Count - 15) * 8,
  Off2 = (Count - 2) * 8 - Off1 - 8,
  <<_:Off1/binary, Word1:64/big-unsigned,
    _:Off2/binary, Word2:64/big-unsigned, _/binary>> = <<W/binary>>,
  S0 = rotateRight(Word1, 1) bxor rotateRight(Word1, 8) bxor (Word1 bsr 7),
  S1 = rotateRight(Word2, 19) bxor rotateRight(Word2, 61) bxor (Word2 bsr 6),
  Off3 = (Count - 16) * 8,
  Off4 = (Count - 7) * 8 - Off3 - 8,
  <<_:Off3/binary, W16:64/big-unsigned,
    _:Off4/binary, W7:64/big-unsigned, _/binary>> = <<W/binary>>,
  Next = (W16 + S0 + W7 + S1) band 16#FFFFFFFFFFFFFFFF,
  sha512_extend(<<W/binary, Next:64/big-unsigned>>, Count+1).


-spec rotateRight(integer(), integer()) -> integer().
rotateRight(V, Count) ->
  Rest = 64 - Count,
  <<Top:Rest/unsigned, Bottom:Count/unsigned>> = <<V:64/big-unsigned>>,
  <<New:64/big-unsigned>> = <<Bottom:Count/unsigned, Top:Rest/unsigned>>,
  New.

-spec shiftRight(binary(), integer()) -> binary().
shiftRight(WordToShift, ShiftAmount) ->
  WordToShift bsr ShiftAmount.

-spec ch(integer(), integer(), integer()) -> integer().
ch(X, Y, Z) ->
  (X band Y) bxor ((bnot X) band Z).

-spec maj(integer(), integer(), integer()) -> integer().
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
-spec initialWorkers() -> list(integer()).
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