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

calculateMessageSchedule_test() ->
  [
    ?assertEqual( <<72,101,108,108,111,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      40,72,101,108,108,111,128,0,0,0,5,0,0,0,0,1,64,66,10,255,222,96,51,
      141,242,0,0,20,0,160,0,10,5,96,225,157,77,39,231,109,163,1,64,0,85,
      0,0,68,0,235,59,0,0,56,13,84,121,74,234,110,237,195,136,129,16,241,
      0,209,102,160,138,146,223,136,101,215,103,70,203,32,233,217,153,
      138,81,36,250,114,170,133,243,186,184,57,150,48,185,131,63,111,232,
      82,10,156,3,214,216,43,149,147,14,46,181,187,98,97,136,160,106,65,
      81,134,55,113,194,239,75,18,124,162,162,133,131,36,45,217,17,255,
      78,174,87,23,145,232,157,249,187,87,46,104,247,223,51,91,173,103,
      210,107,194,39,46,110,55,180,37,206,238,171,33,227,35,111,87,119,
      253,191,167,76,125,205,193,21,163,154,122,193,131,105,148,115,52,
      22,55,46,138,150,55,211,189,199,160,215,163,87,129,182,50,39,94,
      123,217,89,238,117,164,108,77,177,246,80,201,248,49,242,44,24,38,
      98,101,129,2,158,65,100,3,88,239,158,253,209,69,78,19,86,49,150,
      142,243,225,18,27,19,232,253,12,3,176,141,203,54,244,102,59,220,
      250,90,230,95,254,253,226,127,17,198,221,182,251,43,31,171,189,164,
      56,154,219,85,4,218,166,234,113,89,8,178,225,155,156,70,34,6,118,
      174,165,138,26,152,108,163,138,54,165,66,32,135,209,104,147,122,
      114,43,247,39,91,2,65,52,229,79,79,8,204,199,93,157,9,166,10,40,
      231,194,75,52,107,187,203,220,39,78,79,31,232,123,179,134,218,250,
      199,167,75,45,103,48,136,48,33,154,149,157,214,224,128,176,146,212,
      30,233,20,46,35,68,40,182,107,128,171,45,217,29,182,126,242,5,141,
      228,171,169,130,68,65,237,148,199,195,54,139,32,98,179,77,46,151,
      83,51,155,38,27,8,192,67,238,215,99,93,217,180,75,163,92,46,5,7,
      245,77,182,25,144,171,48,129,156,253,53,12,183,229,78,66,47,25,156,
      17,56,130,188,38,142,96,28,237,114,160,118,170,148,61,231,44,119,
      233,251,58,154,225,247,240,210,231,185,108,166,152,10,55,9,172,76,
      253,149,4,116,141,228,237,248,103,3,244,53,33,64,82,125,59,108,236,
      43,147,1,23,32,208,96,115>>,
      hasher:calculateMessageSchedule(lists:nth(1, hasher:preprocess(<<"Hello">>))))
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

sha512_length_test() ->
  [
    ?assertEqual(512, bit_size(hasher:sha512(<<"Hello">>)))
  ].

% Expected output generated with https://emn178.github.io/online-tools/sha512.html
sha512_test() ->
  [
    ?assertEqual(<<16#3615F80C9D293ED7402687F94B22D58E529B8CC7916F8FAC7FDDF7FBD5AF4CF777D3D795A7A00A16BF7E7F3FB9561EE9BAAE480DA9FE7A18769E71886B03F315:512>>, hasher:sha512(<<"Hello">>)),
    ?assertEqual(<<16#cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e:512>>, hasher:sha512(<<"">>)),
    ?assertEqual(<<16#5040597cc773cd7723d45c9d35240e698baa4520972705aa14f0966a5ad3bf982442aa5cbc2b9e77eb0b993a16370d167bb72481f6ef7f97194d935e18dbb8d9:512>>, hasher:sha512(<<"The quick fox jumps over the lazy fox">>)),
    ?assertEqual(<<16#7f79190f9d0b58ab636ff27867e76abb220cafa7a435325e3c0a04ad2d134b9c7970521553c3978588da67b5c00fd6d9704b48b65c10b7c53b55db0bfa462648:512>>, hasher:sha512(<<"This is some very lengthy message that definitely should exceed the limit that can be stored into a single block, so that we can test if the foldl part of the code works correctly. ">>))
  ].

binaryListToBinary_test() ->
  [
    ?assertEqual(<<20:64, 21:64>>, hasher:binaryListToBinary([<<20:64>>, <<21:64>>]))
  ].