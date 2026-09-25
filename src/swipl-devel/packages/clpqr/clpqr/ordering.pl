/*  Part of CLP(Q) (Constraint Logic Programming over Rationals)

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


:- module(clpqr_ordering,
	  [ combine/3,
	    ordering/1,
	    ordering/2,
	    intern_vars/1,
	    arrangement/2
	  ]).
:- use_module(class,
	[
	    class_get_prio/2,
	    class_put_prio/2
	]).
:- use_module(itf,
	[
	    clp_type/2
	]).
:- autoload(library(ugraphs),
	[
	    add_edges/3,
	    add_vertices/3,
	    top_sort/2,
	    ugraph_union/3
	]).
:- autoload(library(lists),
	[
	    append/3
	]).

% ordering(Spec) and ordering(CLP,Spec)
%
% Records the requested variable ordering in the priority graph of the
% class the variables belong to.  A variable that comes first in the
% ordering is the one that should be *defined* by the answer constraint,
% i.e. appear on its left hand side; see arrange_pivot/1 in project.pl.
%
% CLP is the solver to intern variables into that the solver does not know
% yet.  This is what makes it legal to state an ordering before the
% constraints it talks about.  ordering/1 has no such information and
% silently ignores an ordering over variables it cannot place.

ordering(Spec) :-
	ordering(_CLP,Spec).

ordering(_,X) :-
	var(X),
	!,
	fail.
ordering(CLP,A>B) :-
	!,
	ordering(CLP,B<A).
ordering(CLP,A<B) :-
	join_class(CLP,[A,B],Class),
	class_get_prio(Class,Ga),
	!,
	add_edges([],[A-B],Gb),
	combine(Ga,Gb,Gc),
	check_acyclic(Gc,A<B),
	class_put_prio(Class,Gc).
ordering(CLP,Pb) :-
	Pb = [_|Xs],
	join_class(CLP,Pb,Class),
	class_get_prio(Class,Ga),
	!,
	(   Xs == []
	->  add_vertices([],Pb,Gb)
	;   gen_edges(Pb,Es,[]),
	    add_edges([],Es,Gb)
	),
	combine(Ga,Gb,Gc),
	check_acyclic(Gc,Pb),
	class_put_prio(Class,Gc).
ordering(_,_).

% check_acyclic(Graph,Spec)
%
% Raises an error if Spec has made the priority graph cyclic, i.e. if the
% orderings stated so far contradict each other.  Reporting this here
% rather than when the answer is projected lets us name the culprit.

check_acyclic(G,Spec) :-
	(   normalize(G,Gn),
	    top_sort(Gn,_)
	->  true
	;   throw(error(cyclic_ordering(Spec),_))
	).

arrangement(Class,Arr) :-
	class_get_prio(Class,G),
	normalize(G,Gn),
	top_sort(Gn,Arr),
	!.
% The orderings of two classes can each be acyclic and still be cyclic
% after the classes are merged, so this has to be checked here as well.
% In that case there is no single culprit to report.
arrangement(_,_) :-
	throw(error(cyclic_ordering(_),_)).

% intern_vars(Vars)
%
% Puts every variable of Vars that the solver knows about into one and the
% same class, merging the classes they belonged to before.  Anything else --
% a nonvar, a variable the solver has never seen, or one of the auxiliary
% variables that carry a clpqr_class attribute -- is skipped.
%
% dump/3 uses this to make its target variables reachable from the linear
% store before projecting.  Unlike join_class/2 it never fails, because a
% target that carries no constraints at all is perfectly legal.

intern_vars(Vars) :-
	intern_vars(Vars,_).

intern_vars([],_).
intern_vars([X|Xs],Class) :-
	(   var(X),
	    clp_type(X,CLP)
	->  (   CLP == clpr
	    ->  bv_r:var_intern(X,Class)
	    ;   bv_q:var_intern(X,Class)
	    )
	;   true
	),
	intern_vars(Xs,Class).

% join_class(CLP,Vars,Class)
%
% Puts all variables of Vars in the class Class.  A variable the solver
% does not know yet is interned into CLP; if CLP is unbound as well there
% is no way to tell whether it belongs to CLP(Q) or CLP(R) and this fails,
% which makes ordering/2 fall through to its catch-all clause.

join_class(_,[],_).
join_class(CLP,[X|Xs],Class) :-
	(   var(X)
	->  (   clp_type(X,Type)
	    ->  true
	    ;   Type = CLP
	    ),
	    var_intern(Type,X,Class)
	;   true
	),
	join_class(CLP,Xs,Class).

var_intern(clpr,X,Class) :-
	!,
	bv_r:var_intern(X,Class).
var_intern(clpq,X,Class) :-
	bv_q:var_intern(X,Class).

% combine(Ga,Gb,Gc)
%
% Combines the vertices of Ga and Gb into Gc.

combine(Ga,Gb,Gc) :-
	normalize(Ga,Gan),
	normalize(Gb,Gbn),
	ugraph_union(Gan,Gbn,Gc).

%
% both Ga and Gb might have their internal ordering invalidated
% because of bindings and aliasings
%

normalize([],[]) :- !.
normalize(G,Gsgn) :-
	G = [_|_],
	keysort(G,Gs),	% sort vertices on key
	group(Gs,Gsg),	% concatenate vertices with the same key
	normalize_vertices(Gsg,Gsgn).	% normalize

normalize_vertices([],[]).
normalize_vertices([X-Xnb|Xs],Res) :-
	(   normalize_vertex(X,Xnb,Xnorm)
	->  Res = [Xnorm|Xsn],
	    normalize_vertices(Xs,Xsn)
	;   normalize_vertices(Xs,Res)
	).

% normalize_vertex(X,Nbs,X-Nbss)
%
% Normalizes a vertex X-Nbs into X-Nbss by sorting Nbs, removing duplicates (also of X)
% and removing non-vars.

normalize_vertex(X,Nbs,X-Nbsss) :-
	var(X),
	sort(Nbs,Nbss),
	strip_nonvar(Nbss,X,Nbsss).

% strip_nonvar(Nbs,X,Res)
%
% Turns vertext X-Nbs into X-Res by removing occurrences of X from Nbs and removing
% non-vars. This to normalize after bindings have occurred. See also normalize_vertex/3.

strip_nonvar([],_,[]).
strip_nonvar([X|Xs],Y,Res) :-
	(   X==Y % duplicate of Y
	->  strip_nonvar(Xs,Y,Res)
	;   var(X) % var: keep
	->  Res = [X|Stripped],
	    strip_nonvar(Xs,Y,Stripped)
	;   % nonvar: remove
	    nonvar(X),
	    Res = []	% because Vars<anything
	).

gen_edges([]) --> [].
gen_edges([X|Xs]) -->
	gen_edges(Xs,X),
	gen_edges(Xs).

gen_edges([],_) --> [].
gen_edges([Y|Ys],X) -->
	[X-Y],
	gen_edges(Ys,X).

% group(Vert,Res)
%
% Concatenates vertices with the same key.

group([],[]).
group([K-Kl|Ks],Res) :-
	group(Ks,K,Kl,Res).

group([],K,Kl,[K-Kl]).
group([L-Ll|Ls],K,Kl,Res) :-
	(   K==L
	->  append(Kl,Ll,KLl),
	    group(Ls,K,KLl,Res)
	;   Res = [K-Kl|Tail],
	    group(Ls,L,Ll,Tail)
	).


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:error_message//1.

prolog:error_message(cyclic_ordering(Spec)) -->
	[ 'CLP(Q,R): the requested variable ordering is cyclic'-[] ],
	cyclic_culprit(Spec).

cyclic_culprit(Spec) -->
	{ var(Spec) },
	!.
cyclic_culprit(Spec) -->
	[ ' (on adding ~p)'-[Spec] ].

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(clpqr_ordering:ordering(_)).
sandbox:safe_primitive(clpqr_ordering:ordering(_,_)).
