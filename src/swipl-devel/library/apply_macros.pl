/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2023, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(apply_macros,
          [ expand_phrase/2,            % :PhraseGoal, -Goal
            expand_phrase/4,            % :PhraseGoal, +Pos0, -Goal, -Pos
            apply_macros_sentinel/0
          ]).
% maplist expansion uses maplist.  Do not autoload.
:- use_module(library(apply), [maplist/2, maplist/3, maplist/4]).
:- use_module(library(yall), [is_lambda/1, lambda_calls/3]).
% these may be autoloaded
:- autoload(library(lists),[append/3]).
:- autoload(library(prolog_code), [mkconj/3, extend_goal/3]).

/** <module> Goal expansion rules to avoid meta-calling

This module defines goal_expansion/2 rules to   deal with commonly used,
but fundamentally slow meta-predicates. Notable   maplist/2... defines a
useful set of predicates, but its  execution is considerable slower than
a traditional Prolog loop. Using this  library calls to maplist/2... are
translated into an call  to  a  generated  auxiliary  predicate  that is
compiled using compile_aux_clauses/1. Currently this module supports:

        * maplist/2..
        * forall/2
        * once/1
        * ignore/1
        * phrase/2
        * phrase/3
        * call_dcg/2
        * call_dcg/3

The idea for this library originates from ECLiPSe and came to SWI-Prolog
through YAP.

@tbd    Support more predicates
@author Jan Wielemaker
*/

:- create_prolog_flag(optimise_apply, default,
                      [ keep(true),
                        type(oneof([default,false,true]))
                      ]).
:- create_prolog_flag(apply_macros_scope, global,
                      [ keep(true),
                        type(oneof([global,imported]))
                      ]).

:- dynamic
    user:goal_expansion/2.
:- multifile
    user:goal_expansion/2.

%!  expand_maplist(+Callable, +Lists, -Goal) is det.
%
%   Macro expansion for maplist/2 and  higher   arity.  The first clause
%   deals with code using maplist on fixed  lists to reduce typing. Note
%   that we only expand if all  lists   have  fixed length. In theory we
%   only need at least one of fixed length,   but  in that case the goal
%   expansion instantiates variables in the  clause, causing issues with
%   the remainder of the clause expansion mechanism.

expand_maplist(Callable, Lists, Goal) :-
    maplist(is_list, Lists),
    maplist(length, Lists, Lens),
    (   sort(Lens, [Len])
    ->  Len < 10,
        unfold_maplist(Lists, Callable, Goal),
        !
    ;   Maplist =.. [maplist,Callable|Lists],
        print_message(warning, maplist(inconsistent_length(Maplist, Lens))),
        fail
    ).
expand_maplist(Callable0, Lists, Goal) :-
    length(Lists, N),
    expand_closure_no_fail(Callable0, N, Callable1),
    (   Callable1 = _:_
    ->  strip_module(Callable1, M, Callable),
        NextGoal = M:NextCall,
        QPred = M:Pred
    ;   Callable = Callable1,
        NextGoal = NextCall,
        QPred = Pred
    ),
    Callable =.. [Pred|Args],
    length(Args, Argc),
    length(Argv, Argc),
    length(Vars, N),
    MapArity is N + 1,
    format(atom(AuxName), '__aux_maplist/~d_~w+~d', [MapArity, QPred, Argc]),
    append(Lists, Args, AuxArgs),
    Goal =.. [AuxName|AuxArgs],

    AuxArity is N+Argc,
    prolog_load_context(module, Module),
    functor(NextCall, Pred, AuxArity),
    \+ predicate_property(Module:NextGoal, transparent),
    (   predicate_property(Module:Goal, defined)
    ->  true
    ;   empty_lists(N, BaseLists),
        length(Anon, Argc),
        append(BaseLists, Anon, BaseArgs),
        BaseClause =.. [AuxName|BaseArgs],

        heads_and_tails(N, NextArgs, Vars, Tails),
        append(NextArgs, Argv, AllNextArgs),
        NextHead =.. [AuxName|AllNextArgs],
        append(Argv, Vars, PredArgs),
        NextCall =.. [Pred|PredArgs],
        append(Tails, Argv, IttArgs),
        NextIterate =.. [AuxName|IttArgs],
        NextClause = (NextHead :- NextGoal, NextIterate),
        compile_aux_clauses([BaseClause, NextClause])
    ).

unfold_maplist(Lists, Callable, Goal) :-
    maplist(cons, Lists, Heads, Tails),
    !,
    maplist_extend_goal(Callable, Heads, G1),
    unfold_maplist(Tails, Callable, G2),
    mkconj(G1, G2, Goal).
unfold_maplist(_, _, true).

cons([H|T], H, T).

%!  maplist_extend_goal(+Closure, +Args, -Goal) is semidet.
%
%   Extend the maplist Closure with Args.   This  can be tricky. Notably
%   library(yall) lambda expressions may instantiate   the Closure while
%   the  real  execution  does  not.  We    can   solve  that  by  using
%   lambda_calls/3. The expand_goal_no_instantiate/2 ensures   safe goal
%   expansion.

maplist_extend_goal(Closure, Args, Goal) :-
    is_lambda(Closure),
    !,
    lambda_calls(Closure, Args, Goal1),
    expand_goal_no_instantiate(Goal1, Goal).
maplist_extend_goal(Closure, Args, Goal) :-
    extend_goal(Closure, Args, Goal1),
    expand_goal_no_instantiate(Goal1, Goal).

% using is_most_general_term/1 is an alternative, but fails
% if the goal variables have attributes.

expand_goal_no_instantiate(Goal0, Goal) :-
    term_variables(Goal0, Vars0),
    expand_goal(Goal0, Goal),
    term_variables(Goal0, Vars1),
    Vars0 == Vars1.

%!  expand_closure_no_fail(+Goal, +Extra:integer, -GoalExt) is det.
%
%   Add Extra additional arguments to Goal.

expand_closure_no_fail(Callable0, N, Callable1) :-
    '$expand_closure'(Callable0, N, Callable1),
    !.
expand_closure_no_fail(Callable, _, Callable).

empty_lists(0, []) :- !.
empty_lists(N, [[]|T]) :-
    N2 is N - 1,
    empty_lists(N2, T).

heads_and_tails(0, [], [], []).
heads_and_tails(N, [[H|T]|L1], [H|L2], [T|L3]) :-
    N2 is N - 1,
    heads_and_tails(N2, L1, L2, L3).


%!  expand_apply(+GoalIn:callable, -GoalOut) is semidet.
%
%   Macro expansion for `apply' predicates.

expand_apply(Maplist, Goal) :-
    compound(Maplist),
    compound_name_arity(Maplist, maplist, N),
    N >= 2,
    Maplist =.. [maplist, Callable|Lists],
    qcall_instantiated(Callable),
    !,
    expand_maplist(Callable, Lists, Goal).

%!  expand_apply(+GoalIn:callable, -GoalOut, +PosIn, -PosOut) is semidet.
%
%   Translation  of  simple  meta  calls    to   inline  code  while
%   maintaining position information. Note that once(Goal) cannot be
%   translated  to  `(Goal->true)`  because  this   will  break  the
%   compilation of `(once(X) ; Y)`.  A   correct  translation  is to
%   `(Goal->true;fail)`.       Abramo       Bagnara        suggested
%   `((Goal->true),true)`, which is both faster   and avoids warning
%   if style_check(+var_branches) is used.

expand_apply(forall(Cond, Action), Pos0, Goal, Pos) :-
    Goal = \+((Cond, \+(Action))),
    (   nonvar(Pos0),
        Pos0 = term_position(_,_,_,_,[PosCond,PosAct])
    ->  Pos = term_position(0,0,0,0, % \+
                            [ term_position(0,0,0,0, % ,/2
                                            [ PosCond,
                                              term_position(0,0,0,0, % \+
                                                            [PosAct])
                                            ])
                            ])
    ;   true
    ).
expand_apply(once(Once), Pos0, Goal, Pos) :-
    Goal = (Once->true),
    (   nonvar(Pos0),
        Pos0 = term_position(_,_,_,_,[OncePos]),
        compound(OncePos)
    ->  Pos = term_position(0,0,0,0,        % ->/2
                            [ OncePos,
                              F-T           % true
                            ]),
        arg(2, OncePos, F),         % highlight true/false on ")"
        T is F+1
    ;   true
    ).
expand_apply(ignore(Ignore), Pos0, Goal, Pos) :-
    Goal = (Ignore->true;true),
    (   nonvar(Pos0),
        Pos0 = term_position(_,_,_,_,[IgnorePos]),
        compound(IgnorePos)
    ->  Pos = term_position(0,0,0,0,                        % ;/2
                            [ term_position(0,0,0,0,        % ->/2
                                            [ IgnorePos,
                                              F-T           % true
                                            ]),
                              F-T                           % true
                            ]),
        arg(2, IgnorePos, F),       % highlight true/false on ")"
        T is F+1
    ;   true
    ).
expand_apply(Phrase, Pos0, Expanded, Pos) :-
    expand_phrase(Phrase, Pos0, Expanded, Pos),
    !.


%!  expand_phrase(+PhraseGoal, -Goal) is semidet.
%!  expand_phrase(+PhraseGoal, +Pos0, -Goal, -Pos) is semidet.
%
%   Provide goal-expansion for  PhraseGoal.   PhraseGoal  is  either
%   phrase/2,3  or  call_dcg/2,3.  The  current   version  does  not
%   translate control structures, but  only   simple  terminals  and
%   non-terminals.
%
%   For example:
%
%     ==
%     ?- expand_phrase(phrase(("ab", rule)), List), Goal).
%     Goal = (List=[97, 98|_G121], rule(_G121, [])).
%     ==
%
%   @throws Re-throws errors from dcg_translate_rule/2

expand_phrase(Phrase, Goal) :-
    expand_phrase(Phrase, _, Goal, _).

expand_phrase(phrase(NT,Xs), Pos0, NTXsNil, Pos) :-
    !,
    extend_pos(Pos0, 1, Pos1),
    expand_phrase(phrase(NT,Xs,[]), Pos1, NTXsNil, Pos).
expand_phrase(Goal, Pos0, NewGoal, Pos) :-
    dcg_goal(Goal, NT, Xs0, Xs),
    nonvar(NT),
    nt_pos(Pos0, NTPos),
    dcg_extend(NT, NTPos, NewGoal, Pos, Xs0, Xs).

dcg_goal(phrase(NT,Xs0,Xs), NT, Xs0, Xs).
dcg_goal(call_dcg(NT,Xs0,Xs), NT, Xs0, Xs).

%!  dcg_extend(+Callable, +Pos0, -Goal, -Pos, +Xs0, ?Xs) is semidet.

dcg_extend(Terminal, Pos0, Xs0 = DList, Pos, Xs0, Xs) :-
    terminal(Terminal, DList, Xs),
    !,
    t_pos(Pos0, Pos).
dcg_extend(Q0, Pos0, M:Q, Pos, Xs0, Xs) :-
    nonvar(Q0), Q0 = M:Q1,
    !,
    '$expand':f2_pos(Pos0, MPos, APos0, Pos, MPos, APos),
    dcg_extend(Q1, APos0, Q, APos, Xs0, Xs).
dcg_extend(Control, _, _, _, _, _) :-
    dcg_control(Control),
    !,
    fail.
dcg_extend(Compound0, Pos0, Compound, Pos, Xs0, Xs) :-
    compound(Compound0),
    !,
    extend_pos(Pos0, 2, Pos),
    compound_name_arguments(Compound0, Name, Args0),
    append(Args0, [Xs0,Xs], Args),
    compound_name_arguments(Compound, Name, Args).
dcg_extend(Name, Pos0, Compound, Pos, Xs0, Xs) :-
    atom(Name),
    !,
    extend_pos(Pos0, 2, Pos),
    compound_name_arguments(Compound, Name, [Xs0,Xs]).

dcg_control(!).
dcg_control([]).
dcg_control([_|_]).
dcg_control({_}).
dcg_control((_,_)).
dcg_control((_;_)).
dcg_control((_->_)).
dcg_control((_*->_)).

terminal([], DList, Tail) =>
    DList = Tail.
terminal(String, DList, Tail), string(String) =>
    string(String),
    string_codes(String, List),
    append(List, Tail, DList).
terminal(List, DList, Tail), is_list(List) =>
    append(List, Tail, DList).
terminal(_, _, _) =>
    fail.

extend_pos(Var, _, Var) :-
    var(Var),
    !.
extend_pos(term_position(F,T,FF,FT,ArgPos0), Extra,
           term_position(F,T,FF,FT,ArgPos)) :-
    !,
    extra_pos(Extra, T, ExtraPos),
    append(ArgPos0, ExtraPos, ArgPos).
extend_pos(FF-FT, Extra,
           term_position(FF,FT,FF,FT,ArgPos)) :-
    !,
    extra_pos(Extra, FT, ArgPos).

extra_pos(1, T, [T-T]).
extra_pos(2, T, [T-T,T-T]).

nt_pos(PhrasePos, _NTPos) :-
    var(PhrasePos),
    !.
nt_pos(term_position(_,_,_,_,[NTPos|_]), NTPos).

t_pos(Pos0, term_position(F,T,F,T,[F-T,F-T])) :-
    compound(Pos0),
    !,
    arg(1, Pos0, F),
    arg(2, Pos0, T).
t_pos(_, _).


%!  qcall_instantiated(@Term) is semidet.
%
%   True if Term is instantiated sufficiently to call it.
%
%   @tbd    Shouldn't this be callable straight away?

qcall_instantiated(Var) :-
    var(Var),
    !,
    fail.
qcall_instantiated(M:C) :-
    !,
    atom(M),
    callable(C).
qcall_instantiated(C) :-
    callable(C).


                 /*******************************
                 *            DEBUGGER          *
                 *******************************/

:- multifile
    prolog_clause:unify_goal/5.

prolog_clause:unify_goal(Maplist, Expanded, _Module, Pos0, Pos) :-
    is_maplist(Maplist),
    maplist_expansion(Expanded),
    Pos0 = term_position(F,T,FF,FT,[_MapPos|ArgsPos]),
    Pos  = term_position(F,T,FF,FT,ArgsPos).

is_maplist(Goal) :-
    compound(Goal),
    compound_name_arity(Goal, maplist, A),
    A >= 2.

maplist_expansion(Expanded) :-
    compound(Expanded),
    compound_name_arity(Expanded, Name, _),
    sub_atom(Name, 0, _, _, '__aux_maplist/').


                 /*******************************
                 *          XREF/COLOUR         *
                 *******************************/

:- multifile
    prolog_colour:vararg_goal_classification/3.

prolog_colour:vararg_goal_classification(maplist, Arity, expanded) :-
    Arity >= 2.


                 /*******************************
                 *           ACTIVATE           *
                 *******************************/

:- multifile
    system:goal_expansion/2,
    system:goal_expansion/4.

%!  apply_macros_sentinel
%
%   Used to detect that library(apply_macros) is loaded into the current
%   context  explicitly.  This  test  is  used    if   the  Prolog  flag
%   `apply_macros` is set to `imported`.

apply_macros_sentinel.

optimise_apply :-
    (   current_prolog_flag(optimise_apply, true)
    ->  true
    ;   current_prolog_flag(optimise_apply, default),
        current_prolog_flag(optimise, true)
    ->  true
    ).

apply_macros :-
    current_prolog_flag(xref, true),
    !,
    fail.
apply_macros :-
    optimise_apply,
    current_prolog_flag(apply_macros_scope, Scope),
    apply_macros(Scope).

apply_macros(global) =>
    true.
apply_macros(imported) =>
    prolog_load_context(module, M),
    predicate_property(M:apply_macros_sentinel, imported_from(apply_macros)),
    !.

system:goal_expansion(GoalIn, GoalOut) :-
    apply_macros,
    expand_apply(GoalIn, GoalOut).
system:goal_expansion(GoalIn, PosIn, GoalOut, PosOut) :-
    apply_macros,
    expand_apply(GoalIn, PosIn, GoalOut, PosOut).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(maplist(inconsistent_length(Maplist, Lens))) -->
    { functor(Maplist, _, N) },
    [ 'maplist/~d called with proper lists of different lengths (~p) always fails'
      -[N, Lens] ].
