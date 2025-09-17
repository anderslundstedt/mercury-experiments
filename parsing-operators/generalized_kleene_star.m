:- module generalized_kleene_star.

% INTERFACE

:- interface.


%% MODULE IMPORTS

:- import_module list.

:- use_module io.


%% GENERALIZED KLEENE STAR

% - Suppose we have a DCG rule
%     r(TKNS::in, TKNS::out).
%   Then
%     r' --> *(r)
%   has the expected behaviour.
%
% - Suppose we have a DCG rule
%     r(T::out, TKNS::in, TKNS::out).
%   Then
%     r'(TS) --> *(TS,r)
%   tries r(–) repeatedly until failure, always succeeding. If
%     r(T₁), r(T₂), ..., r(Tₙ)
%   are the successes of r(–) before failure, then
%     TS = [T₁,T₂,...,Tₙ].
%
% - Suppose we have a DCG rule
%     r(T1::in, T::out, TKNS::in, TKNS::out).
%   Then
%     r'(TS) --> *(TS,r,t1)
%   tries r(t1,–) repeatedly until failure, always succeeding. If
%     r(t1,T₁), r(t1,T₂), ..., r(t1,Tₙ)
%   are the successes of r(t1,–) before failure, then
%     TS = [T₁,T₂,...,Tₙ].
%
% - Suppose we have a DCG rule
%     r(T1::in, T2::in, T::out, TKNS::in, TKNS::out).
%   Then
%     r'(TS) --> *(TS,r,t1,t2)
%   tries r(t1,t2,–) repeatedly until failure, always succeeding. If
%     r(t1,t2,T₁), r(t1,t2,T₂), ..., r(t1,t2,Tₙ)
%   are the successes of r(t1,t2,–) before failure, then
%     TS = [T₁,T₂,...,Tₙ].
%
% - Suppose we have a DCG rule
%     r(T1::in, T2::in, T3::in, T::out, TKNS::in, TKNS::out).
%   Then
%     r'(TS) --> *(TS,r,t1,t2,t3)
%   tries r(t1,t2,t3,–) repeatedly until failure, always succeeding. If
%     r(t1,t2,t3,T₁), r(t1,t2,t3,T₂), ..., r(t1,t2,t3,Tₙ)
%   are the successes of r(t1,t2,t3,–) before failure, then
%     TS = [T₁,T₂,...,Tₙ].
%
% - For rules with more than 3 inputs one may resort to currying (or one may
%   extend the predicate list below). Given a DCG rule
%     r(T1::in, T2::in, T3::in, T4::in, T::out, TKNS::in, TKNS::out),
%   Then
%     r'(TS) --> {R = r(t1,t2,t3,t4}, *(TS,R)
%   tries r(t1,t2,t3,t4,–) repeatedly until failure, always succeeding. If
%     r(t1,t2,t3,t4,T₁), r(t1,t2,t3,t4,T₂), ..., r(t1,t2,t3,t4,Tₙ)
%   are the successes of r(t1,t2,t3,t4,–) before failure, then
%     TS = [T₁,T₂,...,Tₙ].

:- pred *(
  list(T), pred(T1,T2,T3, T,   TKNS, TKNS),           T1,T2,T3, TKNS, TKNS
).
:- mode *(
  out,     pred(in,in,in, out, in,   out) is semidet, in,in,in, in,   out
) is det.

:- pred *(
  list(T), pred(T1,T2,    T,   TKNS, TKNS),           T1,T2,    TKNS, TKNS
).
:- mode *(
  out,     pred(in,in,    out, in,   out) is semidet, in,in,    in,   out
) is det.

:- pred *(
  list(T), pred(T1,       T,   TKNS, TKNS),           T1,       TKNS, TKNS
).
:- mode *(
  out,     pred(in,       out, in,   out) is semidet, in,       in,   out
) is det.

:- pred *(
  list(T), pred(          T,   TKNS, TKNS),                     TKNS, TKNS
).
:- mode *(
  out,     pred(          out, in,   out) is semidet,           in,   out
) is det.

:- pred *(
           pred(               TKNS, TKNS),                     TKNS, TKNS
).
:- mode *(
           pred(               in,   out) is semidet,           in,   out
) is det.



%% MAIN

:- pred main(io.io::di, io.io::uo) is det.



% IMPLEMENTATION

:- implementation.


%% MODULE IMPORTS

:- import_module string.

:- use_module char.


%% TYPE ABBREVIATIONS

:- type ta_chr  == char.char.
:- type ta_chrs == list(ta_chr).
:- type ta_str  == string.


%% GENERALIZED KLEENE STAR

*(P) --> (
  P, *(P) -> {true};
             {true}
).

*(XS,P) --> (
  P(X), *(XS_,P) -> {XS = [X|XS_]};
                    {XS = []}
).

*(XS,P,X1) --> (
  P(X1,X), *(XS_,P,X1) -> {XS = [X|XS_]};
                          {XS = []}
).

*(XS,P,X1,X2) --> (
  P(X1,X2,X), *(XS_,P,X1,X2) -> {XS = [X|XS_]};
                                {XS = []}
).

*(XS,P,X1,X2,X3) --> (
  P(X1,X2,X3,X), *(XS_,P,X1,X2,X3) -> {XS = [X|XS_]};
                                      {XS = []}
).

%% TEST PREDICATES

%%% DCG RULE R_A

:- pred r_a(ta_chrs::in, ta_chrs::out) is semidet.
r_a --> ['a'].

%%% DCG RULES R_CHR_EXCEPT_FOR

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

%%% THE TEST PREDICATES

:- func str2chrs(ta_str) = ta_chrs.
str2chrs(S)              = string.to_char_list(S).

:- pred p_test_1 is semidet.
p_test_1 :-
  *(r_a, str2chrs("abc"), str2chrs("bc")).

:- pred p_test_2 is semidet.
p_test_2 :-
  *(r_a, str2chrs("aaabc"), str2chrs("bc")).

:- pred p_test_3 is semidet.
p_test_3 :-
  *(r_a, str2chrs("bc"), str2chrs("bc")).

:- pred r_test_4(ta_chrs::in, ta_chrs::out) is det.
r_test_4 --> *(r_a).
:- pred p_test_4a is semidet.
p_test_4a :- r_test_4(str2chrs("aaaabc"),str2chrs("bc")).
:- pred p_test_4b is semidet.
p_test_4b :- r_test_4(str2chrs("bc"),str2chrs("bc")).

:- pred p_test_5 is semidet.
p_test_5 :-
  *(['a'], r_chr_except_for, 'b', str2chrs("abc"), str2chrs("bc")).

:- pred p_test_6 is semidet.
p_test_6 :-
  *([], r_chr_except_for, 'a', str2chrs("abc"), str2chrs("abc")).

:- pred p_test_7 is semidet.
p_test_7 :-
  *(str2chrs("abc"), r_chr_except_for, 'd', str2chrs("abcde"), str2chrs("de")).

:- pred r_test_8(ta_chrs::out, ta_chrs::in, ta_chrs::out) is det.
r_test_8(CS) --> {R = r_chr_except_for('d')}, *(CS,R).
:- pred p_test_8a is semidet.
p_test_8a :- r_test_8(str2chrs("aaaa"),str2chrs("aaaadc"),str2chrs("dc")).
:- pred p_test_8b is semidet.
p_test_8b :- r_test_8([], str2chrs("da"), str2chrs("da")).

:- pred p_test_9 is semidet.
p_test_9 :-
  *(['a','a'], r_chr_except_for, 'b', 'c', str2chrs("aabc"), str2chrs("bc")).

:- pred p_test_10 is semidet.
p_test_10 :-
  *([], r_chr_except_for, 'a', 'b', str2chrs("abc"), str2chrs("abc")).

:- pred p_test_11 is semidet.
p_test_11 :-
  *(
    str2chrs("abc"),
    r_chr_except_for,
    'e',
    'd',
    str2chrs("abcde"),
    str2chrs("de")
  ).

:- pred r_test_12(ta_chrs::out, ta_chrs::in, ta_chrs::out) is det.
r_test_12(CS) --> *(CS,r_chr_except_for,'d','e').
:- pred p_test_12a is semidet.
p_test_12a :- r_test_12(str2chrs("aaaa"),str2chrs("aaaadc"),str2chrs("dc")).
:- pred p_test_12b is semidet.
p_test_12b :- r_test_12([], str2chrs("ea"), str2chrs("ea")).

:- pred p_test_13 is semidet.
p_test_13 :-
  *(['a'], r_chr_except_for, 'b', 'c', 'd', str2chrs("abc"), str2chrs("bc")).

:- pred p_test_14 is semidet.
p_test_14 :- *([], r_chr_except_for, 'a','b','d', str2chrs("a"), str2chrs("a")).

:- pred p_test_15 is semidet.
p_test_15 :-
  *(
    str2chrs("abc"),
    r_chr_except_for,
    'd',
    'e',
    'f',
    str2chrs("abcde"),
    str2chrs("de")
).

:- pred r_test_16(ta_chrs::out, ta_chrs::in, ta_chrs::out) is det.
r_test_16(CS) --> *(CS,r_chr_except_for,'d','e','f').
:- pred p_test_16a is semidet.
p_test_16a :- r_test_16(str2chrs("aaaa"),str2chrs("aaaadc"),str2chrs("dc")).
:- pred p_test_16b is semidet.
p_test_16b :- r_test_16([], str2chrs("da"),str2chrs("da")).


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
    not p_test_4a -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_4a failed\n",!IO)
    );
    true
  ),
  (
    not p_test_4b -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_4b failed\n",!IO)
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
    not p_test_8a -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_8a failed\n",!IO)
    );
    true
  ),
  (
    not p_test_8b -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_8b failed\n",!IO)
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
    not p_test_12a -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_12a failed\n",!IO)
    );
    true
  ),
  (
    not p_test_12b -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_12b failed\n",!IO)
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
    not p_test_16a -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_16a failed\n",!IO)
    );
    true
  ),
  (
    not p_test_16b -> (
      io.set_exit_status(1,!IO),
      io.write_string(io.stderr_stream,"p_test_16b failed\n",!IO)
    );
    true
  )
).
