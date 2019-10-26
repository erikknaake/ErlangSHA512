%%%-------------------------------------------------------------------
%%% @author erikknaake
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2019 13:06
%%%-------------------------------------------------------------------
-module(hasher_test).
-author("erikknaake").

-include_lib("eunit/include/eunit.hrl").

addBit_test() ->
  [
    ?assertEqual(<<1:1>>, hasher:addBit(<<>>)),
    ?assertEqual(1, bit_size(hasher:addBit(<<>>))), % empty, so no 8 bits to start with

    ?assertEqual(<<1, 1:1>>, hasher:addBit(<<1>>)),
    ?assertEqual(9, bit_size(hasher:addBit(<<1>>))), % 8 bits plus added bit

    ?assertEqual(<<2, 1:1>>, hasher:addBit(<<2>>)),
    ?assertEqual(9, bit_size(hasher:addBit(<<2>>))),

    ?assertEqual(<<3, 1:1>>, hasher:addBit(<<3>>)),
    ?assertEqual(9, bit_size(hasher:addBit(<<3>>))),

    ?assertEqual(<<2#00000111, 2#00001111, 1:1>>, hasher:addBit(<<2#00000111, 2#00001111>>)),
    ?assertEqual(17, bit_size(hasher:addBit(<<2#0000111, 2#00001111>>))),

    ?assertEqual(<<2#00001111, 2#00001111, 1:1>>, hasher:addBit(<<2#00001111, 2#00001111>>)),
    ?assertEqual(17, bit_size(hasher:addBit(<<2#00001111, 2#00001111>>)))
  ].


numberOfZeroesToAdd_test() ->
  [
    ?assertEqual(1023, hasher:numberOfZeroesToAdd(896)),
    ?assertEqual(0, hasher:numberOfZeroesToAdd(895)),
    ?assertEqual(1022, hasher:numberOfZeroesToAdd(897)),
    ?assertEqual(500, hasher:numberOfZeroesToAdd(395))
  ].

paddZeroesOutputLength_test() ->
  [
    ?assertEqual(896, bit_size(hasher:paddZeroes(<<10:8, 1:1>>, 8))),
    ?assertEqual(1920, bit_size(hasher:paddZeroes(<<999:896, 1:1>>, 896))), % one bit more then a padding multiple
    ?assertEqual(896, bit_size(hasher:paddZeroes(<<999:895, 1:1>>, 895))), % exact padding multiple (notice the padded bit)
    ?assertEqual(896, bit_size(hasher:paddZeroes(<<999:894, 1:1>>, 894))) % one bit less then a padding multiple
  ].

paddZeroesArePaddingsZero_test() ->
  [
    ?assertEqual(<<10:8, 1:1, 0:887>>, hasher:paddZeroes(<<10:8, 1:1>>, 8)),
    ?assertEqual(<<999:896, 1:1, 0:1023>>, hasher:paddZeroes(<<999:896, 1:1>>, 896)), % one bit more then a padding multiple
    ?assertEqual(<<999:895, 1:1>>, hasher:paddZeroes(<<999:895, 1:1>>, 895)), % exact padding multiple (notice the padded bit)
    ?assertEqual(<<999:894, 1:1, 0:1>>, hasher:paddZeroes(<<999:894, 1:1>>, 894)) % one bit less then a padding multiple
  ].

lengthPadShouldAdd128Bits_test() ->
  [
    ?assertEqual(137, bit_size(hasher:lengthPadd(<<10:8, 1:1>>, 8))), % 8 bit message + padding bit + 128 byte length padding = 137
    ?assertEqual(129, bit_size(hasher:lengthPadd(<<1:1>>, 0))), % 0 bit message + padding bit + 128 bit length padding = 129
    ?assertEqual(1024, bit_size(hasher:lengthPadd(<<999:895, 1:1>>, 895))), % 895 bit message + padding bit + 128 bit length padding = 1024
    ?assertEqual(1920, bit_size(hasher:lengthPadd(<<999:896, 1:1, 0:895>>, 896))) % 896 bit message + padding bit + 895 bit zero padding + 128 bit length padding = 1920
  ].

lengthPadShouldAddZeroesAndMessageLength_test() ->
  [
    ?assertEqual(<<10:8, 1:1, 0:124, 8:4>>, hasher:lengthPadd(<<10:8, 1:1>>, 8)), % 124 zeroes and 4 bits to place the 8 in
    ?assertEqual(<<1:1, 0:128>>, hasher:lengthPadd(<<1:1>>, 0)), % 0 bit message + padding bit + 128 bit length padding = 129
    ?assertEqual(<<999:895, 1:1, 0:118, 895:10>>, hasher:lengthPadd(<<999:895, 1:1>>, 895)), % 0 bit message + padding bit + 128 bit length padding = 129
    ?assertEqual(<<999:896, 1:1, 0:895, 0:118, 896:10>>, hasher:lengthPadd(<<999:896, 1:1, 0:895>>, 896)) % 896 bit message + padding bit + 895 bit zero padding + 128 bit length padding = 1920
  ].

padding_test() ->
  [
    ?assertEqual(<<2#01100001, 2#01100010, 2#01100011, 1:1, 0:871, 0:123, 24:5>>, hasher:padd(<<2#01100001, 2#01100010, 2#01100011>>)), % example from chapter 5.1.2
    ?assertEqual(<<"abc", 1:1, 0:871, 0:123, 24:5>>, hasher:padd(<<"abc">>)), % check if Erlang has the same character representation as specification
    ?assertEqual(<<"abc", 1:1, 0:871, 0:123, 24:5>>, hasher:padd(<<2#01100001, 2#01100010, 2#01100011>>)), % check if Erlang has the same character representation as specification
    ?assertEqual(<<2#01100001, 2#01100010, 2#01100011, 1:1, 0:871, 0:123, 24:5>>, hasher:padd(<<"abc">>)) % check if Erlang has the same character representation as specification
  ].

paddingShouldBeMultipleOf1024_test() ->
  [
    ?assertEqual(1024, bit_size(hasher:padd(<<"abc">>))),
    ?assertEqual(1024, bit_size(hasher:padd(<<1:888>>))),
    ?assertEqual(2048, bit_size(hasher:padd(<<1:1912>>))),
    ?assertEqual(3072, bit_size(hasher:padd(<<1:1920>>))),
    ?assertEqual(2048, bit_size(hasher:padd(<<1:1024>>))),
    ?assertEqual(2048, bit_size(hasher:padd(<<1:1016>>))),
    ?assertEqual(2048, bit_size(hasher:padd(<<1:896>>)))
  ].

splitToNByteBlocks_test() ->
  [
    ?assertEqual([<<2#11110000, 2#10101010>>, <<2#00000000, 2#11111111>>], hasher:splitToNByteBlocks(<<2#11110000, 2#10101010, 2#00000000, 2#11111111>>, 2)),
    ?assertEqual([<<2#1111000010101010:16>>, <<2#0000000011111111:16>>], hasher:splitToNByteBlocks(<<2#11110000101010100000000011111111:32>>, 2)),
    ?assertEqual([<<2#11110000>>, <<2#10101010>>, <<2#00000000>>, <<2#11111111>>], hasher:splitToNByteBlocks(<<2#11110000101010100000000011111111:32>>, 1))
  ].

parse_length_test() ->
  [
    ?assertEqual(2, length(hasher:parse(<<100:2048>>))), % Validate number of blocks
    ?assertEqual(16, length(lists:nth(1, hasher:parse(<<100:2048>>)))), % validate number of words inside each block
    ?assertEqual(64, bit_size(lists:nth(1, lists:nth(1, hasher:parse(<<100:2048>>))))) % validate number of bits inside each word
  ].

parse_test() ->
  [
    ?assertEqual([
      [
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>
      ],
      [
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<0:64>>,
        <<100:64>>
      ]
    ], hasher:parse(<<100:2048>>))
  ].

bitshiftRight_test() ->
  [
    ?assertEqual(2#00101010, hasher:shiftRight(2#01010101, 1)),
    ?assertEqual(2#00010101, hasher:shiftRight(2#01010101, 2)),
    ?assertEqual(2#00001010, hasher:shiftRight(2#01010101, 3)),
    ?assertEqual(2#00000111, hasher:shiftRight(2#00001111, 1)),
    ?assertEqual(2#00000011, hasher:shiftRight(2#00001111, 2)),
    ?assertEqual(2#00000001, hasher:shiftRight(2#00001111, 3)),
    ?assertEqual(2#00000000, hasher:shiftRight(2#00001111, 4))
  ].

bitshiftLeft_test() ->
  [
    ?assertEqual(2#10101010, hasher:shiftLeft(2#01010101, 1)),
    ?assertEqual(2#01010100, hasher:shiftLeft(2#01010101, 2)),
    ?assertEqual(2#10101000, hasher:shiftLeft(2#01010101, 3)),
    ?assertEqual(2#00011110, hasher:shiftLeft(2#00001111, 1)),
    ?assertEqual(2#00111100, hasher:shiftLeft(2#00001111, 2)),
    ?assertEqual(2#01111000, hasher:shiftLeft(2#00001111, 3)),
    ?assertEqual(2#11110000, hasher:shiftLeft(2#00001111, 4))
  ].

rotateRight_test() ->
  [
    ?assertEqual(2#1000000000000000000000000000000000000000000000000000000000101010, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000001010101, 1)),
    ?assertEqual(2#0100000000000000000000000000000000000000000000000000000000010101, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000001010101, 2)),
    ?assertEqual(2#1010000000000000000000000000000000000000000000000000000000001010, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000001010101, 3)),
    ?assertEqual(2#0101000000000000000000000000000000000000000000000000000000000101, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000001010101, 4)),
    ?assertEqual(2#1000000000000000000000000000000000000000000000000000000000000111, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000000001111, 1)),
    ?assertEqual(2#1100000000000000000000000000000000000000000000000000000000000011, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000000001111, 2)),
    ?assertEqual(2#1110000000000000000000000000000000000000000000000000000000000001, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000000001111, 3)),
    ?assertEqual(2#1111000000000000000000000000000000000000000000000000000000000000, hasher:rotateRight(2#0000000000000000000000000000000000000000000000000000000000001111, 4))
  ].

rotateLeft_test() ->
  [
    ?assertEqual(2#10101010, hasher:rotateLeft(2#01010101, 1)),
    ?assertEqual(2#01010101, hasher:rotateLeft(2#01010101, 2)),
    ?assertEqual(2#10101010, hasher:rotateLeft(2#01010101, 3)),
    ?assertEqual(2#01010101, hasher:rotateLeft(2#01010101, 4)),
    ?assertEqual(2#00011110, hasher:rotateLeft(2#00001111, 1)),
    ?assertEqual(2#00111100, hasher:rotateLeft(2#00001111, 2)),
    ?assertEqual(2#01111000, hasher:rotateLeft(2#00001111, 3)),
    ?assertEqual(2#11110000, hasher:rotateLeft(2#00001111, 4))
  ].

ch_test() ->
  [
    ?assertEqual(2#11101100, hasher:ch(2#00011100, 2#11001100, 2#11110000)),
    ?assertEqual(2#00011100, hasher:ch(2#11110000, 2#00011100, 2#11001100)),
    ?assertEqual(2#11010000, hasher:ch(2#11001100, 2#11110000, 2#00011100))
  ].

maj_test() ->
  [
    ?assertEqual(2#11011100, hasher:maj(2#00011100, 2#11001100, 2#11110000)),
    ?assertEqual(2#11011100, hasher:maj(2#11110000, 2#00011100, 2#11001100)),
    ?assertEqual(2#11011100, hasher:maj(2#11001100, 2#11110000, 2#00011100))
  ].

sum0_test() ->
  [
    ?assertEqual(2#1101001011010010110100101101001011010010110100101101001011010010, hasher:sum0(2#1111000011110000111100001111000011110000111100001111000011110000))
  ].

sum1_test() ->
  [
    ?assertEqual(2#1000011110000111100001111000011110000111100001111000011110000111, hasher:sum1(2#1111000011110000111100001111000011110000111100001111000011110000))
  ].

sigma0_test() ->
  [
    ?assertEqual(2#1000100101101001011010010110100101101001011010010110100101101001, hasher:sigma0(2#1111000011110000111100001111000011110000111100001111000011110000))
  ].

sigma1_test() ->
  [
    ?assertEqual(2#1001101001011010010110100101101001011010010110100101101001011010, hasher:sigma1(2#1111000011110000111100001111000011110000111100001111000011110000))
  ].

wt_first16rounds_test() ->
  [
    ?assertEqual(<<40:64>>, hasher:calculateWt([<<40:64>>], 1, [])),
    ?assertEqual(<<42:64>>, hasher:calculateWt([<<40:64>>, <<42:64>>], 2, [])),
    ?assertEqual(<<44:64>>, hasher:calculateWt([<<40:64>>, <<44:64>>, <<42:64>>], 2, []))
  ].

wt_after16Rounds_test() ->
  [
    ?assertEqual(<<721631471541944654:64>>,
      hasher:calculateWt(
        [<<40:64>>, <<42:64>>],
        16,
        [<<54:64>>, % - 16
          <<10:64>>, % - 15
          <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<35:64>>, % -7
          <<>>, <<>>, <<>>, <<>>, <<30:64>>, % -2
          <<>>, <<>>]
      ))
  ].

fullW_length_test() ->
  [
    ?assertEqual(80,
      length(hasher:calculateFullW(
        [
          <<1:64>>,
          <<2:64>>,
          <<3:64>>,
          <<4:64>>,
          <<5:64>>,
          <<6:64>>,
          <<7:64>>,
          <<8:64>>,
          <<9:64>>,
          <<10:64>>,
          <<11:64>>,
          <<12:64>>,
          <<13:64>>,
          <<14:64>>,
          <<15:64>>,
          <<16:64>>
        ],
        [],
        1)
    ))
  ].

calculate_workers_last_iter_test() ->
  [
    ?assertEqual([16, 31], hasher:calculateWorkers([16, 31], hasher:kConstants(), 80))
  ].

calculate_next_workers_test() ->
  [
    ?assertEqual([4800678499904433735, 1, 2, 3, 4800678430077660744, 5, 6, 7], hasher:calculateNextWorkers([1, 2, 3, 4, 5, 6, 7, 8], hasher:kConstants(), [20], 1))
  ].