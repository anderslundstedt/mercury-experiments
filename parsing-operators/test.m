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


%% DCG RULES R_CHR_EXCEPT_FOR

:- pred r_chr_except_for(ta_chr,               ta_chr, ta_chrs, ta_chrs).
:- mode r_chr_except_for(in,                   out,    in,      out) is semidet.

:- pred r_chr_except_for(ta_chr,ta_chr,        ta_chr, ta_chrs, ta_chrs).
:- mode r_chr_except_for(in,    in,            out,    in,      out) is semidet.

:- pred r_chr_except_for(ta_chr,ta_chr,ta_chr, ta_chr, ta_chrs, ta_chrs).
:- mode r_chr_except_for(in,    in,    in,     out,    in,      out) is semidet.

r_chr_except_for(        C1,    C2,    C3,     C_OUT) --> (
  [C_OUT],
  {not C_OUT = C1, not C_OUT = C2, not C_OUT = C3}
).
r_chr_except_for(        C1,    C2,            C_OUT) --> (
  [C_OUT],
  {not C_OUT = C1, not C_OUT = C2}
).
r_chr_except_for(        C1,                   C_OUT) --> (
  [C_OUT],
  {not C_OUT = C1}
).


%% HELPER FUNCTION STR2CHRS

:- func str2chrs(ta_str) = ta_chrs.
str2chrs(S)              = string.to_char_list(S).


%% PREDICATE TEST

%%% TESTS 1A AND TEST 1B

:- pred r_1(ta_chrs::in,ta_chrs::out) is semidet.
r_1 --> +([
  r_a,
  +([r_b])
]).

:- pred test_1a is semidet.
test_1a :- r_1(str2chrs("ababbbabc"),['c']).

:- pred test_1b is semidet.
test_1b :- not r_1(str2chrs("ababa"),str2chrs("ababa")).

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
  )
).

%% MAIN

main(!IO) :- (
  generalized_kleene_plus.test(!IO),
  generalized_question_mark.test(!IO),
  generalized_kleene_star.test(!IO),
  test.test(!IO)
).
