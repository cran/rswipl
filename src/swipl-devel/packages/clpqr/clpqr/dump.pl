/*  Part of CLP(Q,R) (Constraint Logic Programming over Rationals and Reals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(clpqr_dump,
	  [ dump/3
	  ]).
:- use_module(class, [class_allvars/2]).
:- use_module(geler, [collect_nonlin/3]).
:- use_module(library(assoc), [empty_assoc/1, put_assoc/4, assoc_to_list/2]).
:- use_module(itf, [dump_linear/3, dump_nonzero/3]).
:- use_module(project, [project_attributes/2]).
:- use_module(ordering, [intern_vars/1]).
:- use_module(library(error), [must_be/2]).

%!  dump(+Target,-NewVars,-Constraints) is det.
%
%   Returns in Constraints,  the  constraints   that  currently  hold on
%   Target where all variables in Target are  copied to new variables in
%   NewVars and the constraints are  given   on  these new variables. In
%   short, you can safely  manipulate   NewVars  and Constraints without
%   changing the constraints on Target.

dump([],[],[]) :- !.
dump(Target,NewVars,Constraints) :-
	must_be(list(var), Target),
	copy_term_clpq(Target, NewVars, Constraints).

copy_term_clpq(Term,Copy,Constraints) :-
	copy_term_clpq(Term,Copy,Constraints,_Pending).

% copy_term_clpq(Term,Copy,Constraints,Pending)
%
% Pending are the goals of delayed optimisations (see wait_linear/3) that
% are waiting for their expression to become linear.  They are *not*
% constraints -- calling them inside {}/1 raises a type error -- so they
% are kept apart from Constraints.  attribute_goals//1 emits them as
% ordinary goals; dump/3 ignores them.

copy_term_clpq(Term,Copy,Constraints,Pending) :-
	State = state(-),
	(   copy_term_clpq_(Term, NV, Cs, Ps),
	    nb_setarg(1, State, NV/Cs/Ps),
	    fail
	;   arg(1, State, Copy/Constraints/Pending)
	).

copy_term_clpq_(Term, Copy, Constraints, Pending) :-
	term_variables(Term,Target),		 % get all variables in Term
	intern_vars(Target),			 % make them reachable from the store
	related_linear_vars(Target,All),	 % get all variables of the classes of the variables in Term
	nonlin_crux(All,Nonlin,Goals),		 % get a list of all the nonlinear goals of these variables
	project_attributes(Target,All),
	related_linear_vars(Target,Again),	 % project drops/adds vars
	all_attribute_goals(Again,Gs,Nonlin),
	copy_term_nat(Term/Gs/Goals,Copy/Constraints/Pending). % strip constraints

% related_linear_vars(Vs,All)
%
% Generates a list of all variables that are in the classes of the variables in
% Vs.

related_linear_vars(Vs,All) :-
	empty_assoc(S0),
	related_linear_sys(Vs,S0,Sys),
	related_linear_vars(Sys,All,[]).

% related_linear_sys(Vars,Assoc,List)
%
% Generates in List, a list of all to classes to which variables in Vars
% belong.
% Assoc should be an empty association list and is used internally.
% List contains elements of the form C-C where C is a class and both C's are
% equal.

related_linear_sys([],S0,L0) :- assoc_to_list(S0,L0).
related_linear_sys([V|Vs],S0,S2) :-
	(   get_attr(V,clpqr_itf,Att),
	    arg(6,Att,class(C))
	->  put_assoc(C,S0,C,S1)
	;   S1 = S0
	),
	related_linear_sys(Vs,S1,S2).

% related_linear_vars(Classes,[Vars|VarsTail],VarsTail)
%
% Generates a difference list of all variables in the classes in Classes.
% Classes contains elements of the form C-C where C is a class and both C's are
% equal.

related_linear_vars([]) --> [].
related_linear_vars([S-_|Ss]) -->
	{
	    class_allvars(S,Otl)
	},
	cpvars(Otl),
	related_linear_vars(Ss).

% cpvars(Vars,Out,OutTail)
%
% Makes a new difference list of the difference list Vars.
% All nonvars are removed.

cpvars(Xs) --> {var(Xs)}, !.
cpvars([X|Xs]) -->
	(   { var(X) }
	->  [X]
	;   []
	),
	cpvars(Xs).

% nonlin_crux(All,Constraints,Goals)
%
% Collects all pending non-linear constraints of variables in All.
% This marks all nonlinear goals of the variables as run and cannot
% be reversed manually.

nonlin_crux(All,Constraints,Goals) :-
	collect_nonlin(All,Gs,[]),	% collect the nonlinear goals of variables All
					% this marks the goals as run and cannot be reversed manually
	nonlin_strip(Gs,Constraints,Goals).

% nonlin_strip(Gs,Constraints,Goals)
%
% Splits the module qualified goals Gs into the constraints, with the {}/1
% and the module qualification removed, and the remaining goals.  The
% latter are the continuations of delayed optimisations; they keep their
% qualification so that they remain callable.

nonlin_strip([],[],[]).
nonlin_strip([M:What|Gs],Cs,Goals) :-
	(   What = {G}
	->  Cs = [G|Cst],
	    Goals = Goalst
	;   Cs = Cst,
	    Goals = [M:What|Goalst]
	),
	nonlin_strip(Gs,Cst,Goalst).

all_attribute_goals([]) --> [].
all_attribute_goals([V|Vs]) -->
	dump_linear(V),
	dump_nonzero(V),
	all_attribute_goals(Vs).

%%	attribute_goals(@V)// is det.
%
%	Translate  attributes  back  into  goals.    This   is  used  by
%	copy_term/3, which also determines  the   toplevel  printing  of
%	residual constraints.

clpqr_itf:attribute_goals(V) -->
	(   { term_attvars(V, Vs),
	      Vs \== [],
	      user_vars(Vs, Target),
	      Target \== [],
	      copy_term_clpq(Target, NTarget, List, Pending),
	      ( List \== [] ; Pending \== [] ),
	      NTarget = Target,
	      del_solver_atts(Vs)
	    }
	->  constraint_goal(List),
	      Pending			% delayed optimisations, see above
	;   []
	).

% user_vars(+Vars, -Target)
%
% Target are the variables of Vars that the user can actually see, i.e.
% all but the ones the solver introduced for itself (see
% var_with_def_intern/4 in bv_q.pl).  Projecting onto Target rather than
% onto Vars is what turns the raw content of the tableau, slack variables
% and all, into the constraints the user wrote down.
%
% term_attvars/2 walks through attributes, so Vars is the whole connected
% component regardless of how little of it the copied term mentions.  The
% answer is therefore not projected onto that term; it is projected onto
% the user-level variables of its component, which is the most that this
% interface allows.

user_vars([], []).
user_vars([V|Vs], Target) :-
	(   get_attr(V, clpqr_itf, Att),
	    arg(7, Att, aux)
	->  Target = Target1
	;   Target = [V|Target1]
	),
	user_vars(Vs, Target1).

constraint_goal([]) --> !.
constraint_goal(List) -->
	{ list_to_conj(List, Conj) },
	[ {}(Conj) ].

clpqr_class:attribute_goals(_) --> [].

clpqr_geler:attribute_goals(V) --> clpqr_itf:attribute_goals(V).

% del_solver_atts(Vars)
%
% dump/3 above reports the constraints on all of Vars at once, so the
% solver attributes are removed from all of them to stop attribute_goals//1
% reporting the same conjunction again for the next variable.  Both
% attributes must go: a variable that only carries a delayed non-linear
% goal has no clpqr_itf attribute at all, and removing only that one made
% such constraints appear once per variable.

del_solver_atts([]).
del_solver_atts([H|T]) :-
	del_attr(H, clpqr_itf),
	del_attr(H, clpqr_geler),
	del_solver_atts(T).


list_to_conj([], true) :- !.
list_to_conj([X], X) :- !.
list_to_conj([H|T0], (H,T)) :-
	list_to_conj(T0, T).

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(clpqr_dump:dump(_,_,_)).
