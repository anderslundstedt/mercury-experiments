:- module generalized_question_mark.

% INTERFACE

:- interface.


%% MODULE IMPORTS

:- use_module io, maybe.


%% TYPE ABBREVIATIONS

:- type ta_maybe(T) == maybe.maybe(T).


%% GENERALIZED DCG ? OPERATOR

% - Suppose we have a DCG rule
%     r(TKNS::in, TKNS::out).
%   Then
%     r' --> ?(r)
%   has the expected behaviour.
%
% - Suppose we have a DCG rule
%     r(T::out, TKNS::in, TKNS::out).
%   Then
%     r'(MAYBE_T) --> ?(MAYBE_T,r)
%   tries r(T) and:
%   - if r(T) succeeds, then MAYBE_T = maybe.yes(T);
%   - if r(T) fails,    then MAYBE_T = maybe.no(T).
%
% - Suppose we have a DCG rule
%     r(T1::in, T::out, TKNS::in, TKNS::out).
%   Then
%     r'(MAYBE_T) --> ?(MAYBE_T,r,t1)
%   tries r(t1,T) and:
%   - if r(t1,T) succeeds, then MAYBE_T = maybe.yes(T);
%   - if r(t1,T) fails,    then MAYBE_T = maybe.no(T).
%
% - Suppose we have a DCG rule
%     r(T1::in, T2::in, T::out, TKNS::in, TKNS::out).
%   Then
%     r'(MAYBE_T) --> ?(MAYBE_T,r,t1,t2)
%   tries r(t1,t2,T) and:
%   - if r(t1,t2,T) succeeds, then MAYBE_T = maybe.yes(T);
%   - if r(t1,t2,T) fails,    then MAYBE_T = maybe.no(T).
%
% - Suppose we have a DCG rule
%     r(T1::in, T2::in, T3::in, T::out, TKNS::in, TKNS::out).
%   Then
%     r'(MAYBE_T) --> ?(MAYBE_T,r,t1,t2,t3)
%   tries r(t1,t2,t3,T) and:
%   - if r(t1,t2,t3,T) succeeds, then MAYBE_T = maybe.yes(T);
%   - if r(t1,t2,t3,T) fails,    then MAYBE_T = maybe.no(T).
%
% - For rules with more than 3 inputs one may resort to currying (or one may
%   extend the predicate list below). Given a DCG rule
%     r(T1::in, T2::in, T3::in, T4::in, T::out, TKNS::in, TKNS::out),
%   Then
%     r'(MAYBE_T) --> {R = r(t1,t2,t3,t4}, ?(MAYBE_T,R)
%   tries r(t1,t2,t3,t4,T) and:
%   - if r(t1,t2,t3,t4,T) succeeds, then MAYBE_T = maybe.yes(T);
%   - if r(t1,t2,t3,t4,T) fails,    then MAYBE_T = maybe.no(T).

:- pred ?(
  ta_maybe(T), pred(T1,T2,T3, T,   TKNS, TKNS),           T1,T2,T3, TKNS, TKNS
).
:- mode ?(
  out,         pred(in,in,in, out, in,   out) is semidet, in,in,in, in,   out
) is semidet.

:- pred ?(
  ta_maybe(T), pred(T1,T2,    T,   TKNS, TKNS),           T1,T2,    TKNS, TKNS
).
:- mode ?(
  out,         pred(in,in,    out, in,   out) is semidet, in,in,    in,   out
) is semidet.

:- pred ?(
  ta_maybe(T), pred(T1,       T,   TKNS, TKNS),           T1,       TKNS, TKNS
).
:- mode ?(
  out,         pred(in,       out, in,   out) is semidet, in,       in,   out
) is semidet.

:- pred ?(
  ta_maybe(T), pred(          T,   TKNS, TKNS),                     TKNS, TKNS
).
:- mode ?(
  out,         pred(          out, in,   out) is semidet,           in,   out
) is semidet.

:- pred ?(
               pred(               TKNS, TKNS),                     TKNS, TKNS
).
:- mode ?(
               pred(               in,   out) is semidet,           in,   out
) is semidet.


%% MAIN

:- pred main(io.io::di, io.io::uo) is det.



% IMPLEMENTATION

:- implementation.


%% MODULE IMPORTS

:- import_module list, string.

:- use_module char.


%% TYPE ABBREVIATIONS

:- type ta_chr  == char.char.
:- type ta_chrs == list(ta_chr).
:- type ta_str  == string.


%% GENERALIZED DCG ? OPERATOR

?(P) --> (
  P -> {true};
       {true}
).

?(X,P) --> (
  P(X_) -> {X = maybe.yes(X_)};
           {X = maybe.no}
).

?(X,P,Y) --> (
  P(Y,X_) -> {X = maybe.yes(X_)};
             {X = maybe.no}
).

?(X,P,Y,Z) --> (
  P(Y,Z,X_) -> {X = maybe.yes(X_)};
               {X = maybe.no}
).

?(X,P,U,V,W) --> (
  P(U,V,W,X_) -> {X = maybe.yes(X_)};
                 {X = maybe.no}
).

%% TEST PREDICATES

%%% DCG RULE R_A

:- pred r_a(ta_chrs::in, ta_chrs::out) is semidet.
r_a --> ['a'].

%%% DCG RULES R_CHR_EXCEPT_FOR

:- pred r_chr_except_for(ta_chr,                      ta_chr, ta_chrs, ta_chrs).
:- mode r_chr_except_for(
                         in,                          out,    in,      out
) is semidet.

:- pred r_chr_except_for(ta_chr,ta_chr,               ta_chr, ta_chrs, ta_chrs).
:- mode r_chr_except_for(
                         in,    in,                   out,    in,      out)
is semidet.

:- pred r_chr_except_for(ta_chr,ta_chr,ta_chr,        ta_chr, ta_chrs, ta_chrs).
:- mode r_chr_except_for(
                         in,    in,    in,            out,    in,      out)
is semidet.

:- pred r_chr_except_for(ta_chr,ta_chr,ta_chr,ta_chr, ta_chr, ta_chrs, ta_chrs).
:- mode r_chr_except_for(
                         in,    in,    in,    in,     out,    in,      out
) is semidet.

r_chr_except_for(        C1,    C2,    C3,    C4,      C_OUT) --> (
  [C_OUT],
  {not C_OUT = C1, not C_OUT = C2, not C_OUT = C3, not C_OUT = C4}
).

r_chr_except_for(        C1,    C2,    C3,            C_OUT) --> (
  [C_OUT],
  {not C_OUT = C1, not C_OUT = C2, not C_OUT = C3}
).
r_chr_except_for(        C1,    C2,                   C_OUT) --> (
  [C_OUT],
  {not C_OUT = C1, not C_OUT = C2}
).
r_chr_except_for(        C1,                          C_OUT) --> (
  [C_OUT],
  {not C_OUT = C1}
).

%%% THE TEST PREDICATES

:- func str2chrs(ta_str) = ta_chrs.
str2chrs(S)              = string.to_char_list(S).

:- pred p_test_1 is semidet.
p_test_1 :-
  ?(r_a, str2chrs("abc"), str2chrs("bc")).

:- pred p_test_2 is semidet.
p_test_2 :-
  ?(r_a, str2chrs("bc"), str2chrs("bc")).

:- pred r_test_3(ta_chrs::in, ta_chrs::out) is semidet.
r_test_3 --> ?(r_a).
:- pred p_test_3 is semidet.
p_test_3 :- r_test_3(str2chrs("abc"),str2chrs("bc")).

:- pred r_test_4(ta_chrs::in, ta_chrs::out) is semidet.
r_test_4 --> ?(r_a).
:- pred p_test_4 is semidet.
p_test_4 :- r_test_4(str2chrs("bc"),str2chrs("bc")).

:- pred p_test_5 is semidet.
p_test_5 :-
  ?(maybe.yes('a'), r_chr_except_for, 'c', str2chrs("abc"), str2chrs("bc")).

:- pred p_test_6 is semidet.
p_test_6 :-
  ?(maybe.no, r_chr_except_for, 'a', str2chrs("abc"), str2chrs("abc")).

:- pred r_test_7(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_7(C) --> ?(C,r_chr_except_for,'b').
:- pred p_test_7 is semidet.
p_test_7 :- r_test_7(maybe.yes('a'),str2chrs("abc"),str2chrs("bc")).

:- pred r_test_8(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_8(C) --> ?(C,r_chr_except_for,'a').
:- pred p_test_8 is semidet.
p_test_8 :- r_test_8(maybe.no,str2chrs("abc"),str2chrs("abc")).

:- pred p_test_9 is semidet.
p_test_9 :-
  ?(maybe.yes('a'), r_chr_except_for, 'b', 'c', str2chrs("ab"), str2chrs("b")).

:- pred p_test_10 is semidet.
p_test_10 :-
  ?(maybe.no, r_chr_except_for, 'b', 'a', str2chrs("abc"), str2chrs("abc")).

:- pred r_test_11(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_11(C) --> ?(C,r_chr_except_for,'b','c').
:- pred p_test_11 is semidet.
p_test_11 :- r_test_11(maybe.yes('a'),str2chrs("abc"),str2chrs("bc")).

:- pred r_test_12(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_12(C) --> ?(C,r_chr_except_for,'b','a').
:- pred p_test_12 is semidet.
p_test_12 :- r_test_12(maybe.no,str2chrs("abc"),str2chrs("abc")).

:- pred p_test_13 is semidet.
p_test_13 :-
  ?(maybe.yes('a'),r_chr_except_for, 'b','c','d', str2chrs("a"), []).

:- pred p_test_14 is semidet.
p_test_14 :-
  ?(maybe.no, r_chr_except_for, 'b', 'a', 'd', str2chrs("ab"), str2chrs("ab")).

:- pred r_test_15(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_15(C) --> ?(C,r_chr_except_for,'b','c','d').
:- pred p_test_15 is semidet.
p_test_15 :- r_test_15(maybe.yes('a'),str2chrs("abc"),str2chrs("bc")).

:- pred r_test_16(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_16(C) --> ?(C,r_chr_except_for,'b','a','d').
:- pred p_test_16 is semidet.
p_test_16 :- r_test_16(maybe.no,str2chrs("abc"),str2chrs("abc")).

:- pred r_test_17(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_17(C) --> {R = r_chr_except_for('b','c','d','e')}, ?(C,R).
:- pred p_test_17 is semidet.
p_test_17 :- r_test_15(maybe.yes('a'),str2chrs("abc"),str2chrs("bc")).

:- pred r_test_18(ta_maybe(ta_chr)::out, ta_chrs::in, ta_chrs::out) is semidet.
r_test_18(C) --> {R = r_chr_except_for('b','c','d','a')}, ?(C,R).
:- pred p_test_18 is semidet.
p_test_18 :- r_test_18(maybe.no,str2chrs("abc"),str2chrs("abc")).


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
  ),
  (
    not p_test_7 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_7 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_8 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_8 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_9 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_9 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_10 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_10 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_11 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_11 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_12 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_12 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_13 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_13 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_14 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_14 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_15 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_15 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_16 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_16 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_17 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_17 failed\n",!IO)
    );
    true
  ),
  (
    not p_test_18 -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_18 failed\n",!IO)
    );
    true
  )
).
