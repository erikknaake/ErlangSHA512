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
  compress/1,
  appendBits/2,
  hash/1,
  hash_block/2,
  binaryListToIntegerList/1,
  calculateIntermediateHashValue/2,
  calculateMessageSchedule/1,
  binaryListToBinary/1]).
-endif.

% Makes sure additions are done mod 2^64
add64(X, Y) ->
  (X + Y) band 16#FFFFFFFFFFFFFFFF.

-spec sha512(binary()) -> binary().
sha512(Message) ->
  compress(hash(Message)).

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
          hash_block(MessageBlock, PreviousWorkers)
        end,
        InitialWorkers,
        Message).

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

-spec calculateWorkers(list(integer()), binary()) -> list(integer()).
calculateWorkers(InitialWorkers, MessageSchedule) ->
  sha512_loop(MessageSchedule, InitialWorkers, InitialWorkers, 0).

-spec sha512_loop(binary(), list(integer()), list(integer()), integer()) -> list(integer()).
sha512_loop(_, Hashes, Next, 80) ->
  calculateIntermediateHashValue(Hashes, Next);
sha512_loop(W, Hashes, [A, B, C, D, E, F, G, H], Count) ->
  S0 = rotateRight(A, 28) bxor rotateRight(A, 34) bxor rotateRight(A, 39),
  Maj = (A band B) bxor (A band C) bxor (B band C),
  T2 = add64(S0, Maj),
  S1 = rotateRight(E, 14) bxor rotateRight(E, 18) bxor rotateRight(E, 41),
  Ch = (E band F) bxor (((bnot E) + 1 + 16#FFFFFFFFFFFFFFFF) band G),
  Offset = Count * 8,
  <<_:Offset/binary, K:64/big-unsigned, _/binary>> = kConstants(),
  <<_:Offset/binary, Wval:64/big-unsigned, _/binary>> = <<W/binary>>,
  T1 = (H + S1 + Ch + K + Wval) band 16#FFFFFFFFFFFFFFFF,
  sha512_loop(W, Hashes, [add64(T1, T2), A, B, C, add64(D, T1), E, F, G],
    Count+1).

-spec binaryListToIntegerList(list(binary())) -> list(integer()).
binaryListToIntegerList(BinaryList) ->
  lists:map(
    fun(Binary) ->
      <<Integer:64>> = Binary,
      Integer
    end, BinaryList).

-spec calculateMessageSchedule(list(binary())) -> list(integer()).
calculateMessageSchedule(MessageBlock) ->
  calculateMessageSchedule(MessageBlock, [], 1).
-spec calculateMessageSchedule(list(binary()), list(binary()), integer()) -> list(integer()).
calculateMessageSchedule(_, W, 81) ->
  binaryListToIntegerList(W);
calculateMessageSchedule(MessageBlock, _, _) ->
  calculateMessageSchedulePart(MessageBlock, 16, binaryListToBinary(MessageBlock)).
-spec binaryListToBinary(list(binary())) -> binary().
binaryListToBinary(MessageBlock) ->
  lists:foldl(
    fun(Binary, Result) ->
      concatBinary(Result, Binary)
    end, <<>>, MessageBlock).

-spec concatBinary(binary(), binary()) -> binary().
concatBinary(Bin1, Bin2) ->
  <<Bin1/binary, Bin2/binary>>.

-spec calculateMessageSchedulePart(list(binary()), integer(), list(binary())) -> binary().
calculateMessageSchedulePart(_, T, W) ->
  sha512_extend(W, T).

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

% Constants defined in standard https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf Chapter 4.2.3
-spec kConstants() -> binary().
kConstants() ->
  <<16#428A2F98D728AE22:64/big-unsigned,
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
    16#6C44198C4A475817:64/big-unsigned>>.