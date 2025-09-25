:- module test.

% INTERFACE

:- interface.

:- use_module io.

:- pred main(io.io::di, io.io::uo) is det.



% IMPLEMENTATION

:- implementation.

%% MODULE IMPORTS

:- import_module
  list
  ,
  generalized_kleene_plus, generalized_kleene_star, generalized_question_mark
  .

:- use_module char, string.


%% TYPE ABBREVIATIONS

:- type ta_chr  == char.char.
:- type ta_chrs == list(ta_chr).
:- type ta_str  == string.


%% DCG RULES R_A AND R_B

:- pred r_a(ta_chrs::in,ta_chrs::out) is semidet.
r_a --> ['a'].

:- pred r_b(ta_chrs::in,ta_chrs::out) is semidet.
r_b --> ['b'].


%% HELPER FUNCTION STR2CHRS

:- func str2chrs(ta_str) = ta_chrs.
str2chrs(S)              = string.to_char_list(S).


%% PREDICATE TEST

%%% TESTS 1A AND TEST 1B

:- pred r_1(ta_chrs::in,ta_chrs::out) is semidet.
r_1 --> +([r_a, +([r_b])]).

:- pred test_1a is semidet.
test_1a :- r_1(str2chrs("ababbbabc"),['c']).

:- pred test_1b is semidet.
test_1b :- not r_1(str2chrs("ababa"),str2chrs("ababa")).

%%% TESTS 2A AND TEST 2B

:- pred r_2(ta_chrs::in,ta_chrs::out) is semidet.
r_2 --> +([r_a, ?([r_b])]).

:- pred test_2a is semidet.
test_2a :- r_2(str2chrs("abaaaaaabc"),['c']).

:- pred test_2b is semidet.
test_2b :- not r_2(str2chrs("aba"),str2chrs("aba")).

%%% TESTS 3A AND TEST 3B

:- pred r_3(ta_chrs::in,ta_chrs::out) is semidet.
r_3 --> ?([r_a, *([r_b])]).

:- pred test_3a is semidet.
test_3a :- r_3(str2chrs("abbbbbbbc"),['c']).

:- pred test_3b is semidet.
test_3b :- r_3(str2chrs("cab"),str2chrs("cab")).

%%% THE PREDICATE

:- pred test(io.io::di, io.io::uo) is det.
test(!IO) :- (
  (
    if not test_1a then
      io.write_string("test 1a failed\n",!IO)
    else
      true
  ),
  (
    if not test_1b then
      io.write_string("test 1b failed\n",!IO)
    else
      true
  ),
  (
    if not test_2a then
      io.write_string("test 2a failed\n",!IO)
    else
      true
  ),
  (
    if not test_2b then
      io.write_string("test 2b failed\n",!IO)
    else
      true
  ),
  (
    if not test_3a then
      io.write_string("test 3a failed\n",!IO)
    else
      true
  ),
  (
    if not test_3b then
      io.write_string("test 3b failed\n",!IO)
    else
      true
  )
).

%% MAIN

main(!IO) :- (
  generalized_kleene_plus.test(!IO),
  generalized_question_mark.test(!IO),
  generalized_kleene_star.test(!IO),
  test.test(!IO)
).
