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

preprocess_test() ->
  [
    ?assertEqual([[<<97,98,99,128,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,0>>,
      <<0,0,0,0,0,0,0,24>>]], hasher:preprocess(<<2#01100001, 2#01100010, 2#01100011>>))
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
    ?assertEqual(<<40:64>>, hasher:calculateMessageSchedulePart([<<40:64>>], 1, [])),
    ?assertEqual(<<42:64>>, hasher:calculateMessageSchedulePart([<<40:64>>, <<42:64>>], 2, [])),
    ?assertEqual(<<44:64>>, hasher:calculateMessageSchedulePart([<<40:64>>, <<44:64>>, <<42:64>>], 2, [])),
    ?assertEqual(<<16:64>>, hasher:calculateMessageSchedulePart([
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
    ], 16, []))
  ].

wt_after16Rounds_test() ->
  [
    ?assertEqual(<<721631471541944654:64>>,
      hasher:calculateMessageSchedulePart(
        [<<40:64>>, <<42:64>>],
        17,
        [<<>>, <<54:64>>, % - 16
          <<10:64>>, % - 15
          <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<35:64>>, % -7
          <<>>, <<>>, <<>>, <<>>, <<30:64>>, % -2
          <<>>, <<>>]
      ))
  ].

fullW_length_test() ->
  [
    ?assertEqual(80,
      length(hasher:calculateMessageSchedule(
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
        ])
    ))
  ].

fullW_type_test() ->
  [
    ?assertEqual(true,
      is_integer(lists:nth(1, hasher:calculateMessageSchedule(
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
      ])
    )))
  ].

fullW_test() ->
  [
    ?assertEqual([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
      29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,
      54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,
      13909860030343021338,5560114995615766918],
      hasher:calculateMessageSchedule(
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
        [
          <<1:64>>, <<2:64>>, <<3:64>>, <<4:64>>, <<5:64>>, <<6:64>>, <<7:64>>, <<8:64>>, <<9:64>>,
          <<10:64>>, <<11:64>>, <<12:64>>, <<13:64>>, <<14:64>>, <<15:64>>, <<16:64>>, <<17:64>>, <<18:64>>, <<19:64>>,
          <<20:64>>, <<21:64>>, <<22:64>>, <<23:64>>, <<24:64>>, <<25:64>>, <<26:64>>, <<27:64>>, <<28:64>>, <<29:64>>,
          <<30:64>>, <<31:64>>, <<32:64>>, <<33:64>>, <<34:64>>, <<35:64>>, <<36:64>>, <<37:64>>, <<38:64>>, <<39:64>>,
          <<40:64>>, <<41:64>>, <<42:64>>, <<43:64>>, <<44:64>>, <<45:64>>, <<46:64>>, <<47:64>>, <<48:64>>, <<49:64>>,
          <<50:64>>, <<51:64>>, <<52:64>>, <<53:64>>, <<54:64>>, <<55:64>>, <<56:64>>, <<57:64>>, <<58:64>>, <<59:64>>,
          <<60:64>>, <<61:64>>, <<62:64>>, <<63:64>>, <<64:64>>, <<65:64>>, <<66:64>>, <<67:64>>, <<68:64>>, <<69:64>>,
          <<70:64>>, <<71:64>>, <<72:64>>, <<73:64>>, <<74:64>>, <<75:64>>, <<76:64>>, <<77:64>>, <<78:64>>
        ],
        79)
      )
  ].

calculate_workers_last_iter_test() ->
  [
    ?assertEqual([2, 4, 6, 8, 10, 12, 14, 16], hasher:calculateWorkers([1, 2, 3, 4, 5, 6, 7, 8], [1, 2, 3, 4, 5, 6, 7, 8], hasher:kConstants(), 81))
  ].

calculate_workers_test() ->
  [
    ?assertEqual([31048365790629015328, 6908715048216492877, 3, 5, 24399742188481158686, 6908714978389719890, 11, 13],
      hasher:calculateWorkers([1, 2, 3, 4, 5, 6, 7, 8], [1, 2, 3, 4, 5, 6, 7, 8],
      [
        1, 2, 3, 4, 5, 6, 7, 8, 9,
        10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
        30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
        40, 41,42, 43, 44, 45, 46, 47, 48, 49,
        50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
        60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
        70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
        80
      ],
      79))
  ].


calculate_next_workers_test() ->
  [
    ?assertEqual([4800678499904433735, 1, 2, 3, 4800678430077660744, 5, 6, 7], hasher:calculateNextWorkers([1, 2, 3, 4, 5, 6, 7, 8], hasher:kConstants(), [20], 1))
  ].

appendBits_test() ->
  [
    ?assertEqual(<<2:64, 1:64>>, hasher:appendBits(1, <<2:64>>))
  ].

appendBits_to_empty_test() ->
  [
    ?assertEqual(<<2:64>>, hasher:appendBits(2, <<>>))
  ].

compress_test() ->
  [
    ?assertEqual(<<4800678499904433735:64,
      1:64,
      2:64,
      3:64,
      4800678430077660744:64,
      5:64,
      6:64,
      7:64>>,
      hasher:compress([4800678499904433735, 1, 2, 3, 4800678430077660744, 5, 6, 7]))
  ].

binaryListToIntegerList_test() ->
  [
    ?assertEqual([1, 2, 3], hasher:binaryListToIntegerList([<<1:64>>, <<2:64>>, <<3:64>>]))
  ].

binaryListToIntegerListType_test() ->
  [
    ?assertEqual(true, is_integer(lists:nth(1, hasher:binaryListToIntegerList([<<1:64>>, <<2:64>>, <<3:64>>]))))
  ].

% Test if binary string are equal to string as you would expect them to be
hex_and_binary_strings_should_equal_test() ->
  [
    ?assertEqual(<<16#48656c6c6f:40>>, <<"Hello">>)
  ].

calculate_intermediate_hashresult_test() ->
  [
    ?assertEqual([2, 4, 9, 2, 4, 6, 8, 10], hasher:calculateIntermediateHashValue([1, 2, 3, 1, 2, 3, 4, 5], [1, 2, 6, 1, 2, 3, 4, 5]))
  ].

sha512_length_test() ->
  [
    ?assertEqual(512, bit_size(hasher:sha512(<<"Hello">>)))
  ].

sha512_test() ->
  [
    ?assertEqual(<<16#3615F80C9D293ED7402687F94B22D58E529B8CC7916F8FAC7FDDF7FBD5AF4CF777D3D795A7A00A16BF7E7F3FB9561EE9BAAE480DA9FE7A18769E71886B03F315:512>>, hasher:sha512(<<"Hello">>))
  ].