:- module generalized_question_mark.

% INTERFACE

:- interface.


%% MODULE IMPORTS

:- import_module char, list, string.

:- use_module io, maybe.


%% TYPE ABBREVIATIONS

:- type ta_chr      == char.
:- type ta_chrs     == list(ta_chr).
:- type ta_str      == string.
:- type ta_strs     == list(string).
:- type ta_maybe(T) == maybe.maybe(T).


%% GENERALIZED DCG ? OPERATOR

% Given a DCG rule
%   r(T::out, TKNS::in, TKNS::out),
% then
%   r'(MAYBE_T) --> ?(MAYBE_T,r)
% tries r(T) and:
% - if r(T) succeeds, then MAYBE_T = maybe.yes(T);
% - if r(T) fails, then MAYBE_T = maybe.no(T).
:- pred ?(ta_maybe(T), pred(T,   TKNS, TKNS),           TKNS,  TKNS).
:- mode ?(out,         pred(out, in,   out) is semidet, in,    out) is semidet.


%% MAIN

:- pred main(io.io::di,io.io::uo) is det.



% IMPLEMENTATION

:- implementation.


%% GENERALIZED DCG ? OPERATOR

?(X,P) --> (
  P(X_) -> {X = maybe.yes(X_)};
           {X = maybe.no}
).


%% DCG RULES R_DIGIT

:- pred r_digit(ta_chr::out, ta_chrs::in, ta_chrs::out) is semidet.
r_digit(C) -->
  [C], {list.member(C,['0','1','2','3','4','5','6','7','8','9'])}.


%% TESTS

:- pred p_test_1 is semidet.
p_test_1 :-
  ?(CHR_READ,r_digit)(string.to_char_list("123abc"),CHRS_REMAINING),
  CHR_READ       = maybe.yes('1'),
  CHRS_REMAINING = string.to_char_list("23abc").

:- pred p_test_2 is semidet.
p_test_2 :-
  ?(CHR_READ,r_digit)(string.to_char_list("12"),CHRS_REMAINING),
  CHR_READ       = maybe.yes('1'),
  CHRS_REMAINING = ['2'].

:- pred p_test_3 is semidet.
p_test_3 :-
  ?(CHR_READ,r_digit)(string.to_char_list("1abc"),CHRS_REMAINING),
  CHR_READ       = maybe.yes('1'),
  CHRS_REMAINING = ['a','b','c'].

:- pred p_test_4 is semidet.
p_test_4 :-
  ?(CHR_READ,r_digit)(string.to_char_list("1"),CHRS_REMAINING),
  CHR_READ       = maybe.yes('1'),
  CHRS_REMAINING = [].

:- pred p_test_5 is semidet.
p_test_5 :-
  ?(CHR_READ,r_digit)(string.to_char_list("a1"),CHRS_REMAINING),
  CHR_READ       = maybe.no,
  CHRS_REMAINING = ['a','1'].

:- pred p_test_6 is semidet.
p_test_6 :-
  ?(CHR_READ,r_digit)(string.to_char_list(""),CHRS_REMAINING),
  CHR_READ       = maybe.no,
  CHRS_REMAINING = [].


%% MAIN

main(!IO) :- (
  (
    not p_test_1 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_1 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_2 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_2 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_3 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_3 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_4 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_4 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_5 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_5 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_6 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_6 failed\n",!IO)
    );
    true
  )
).
