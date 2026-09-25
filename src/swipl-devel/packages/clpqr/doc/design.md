# CLP(Q,R) — design and implementation notes

This document describes the design of the SWI-Prolog `library(clpq)` and
`library(clpr)` solvers as they are implemented in this package.  It was
written by reading the sources together with the literature the code is
derived from, and by probing the running system.  Its purpose is to make the
package maintainable again: the libraries have had no maintainer for a long
time and were, until the test suite that accompanies this document, almost
completely untested.

Writing it turned up a number of defects, which are listed in [§14](#14-defects)
together with what was done about them.

* [1. Provenance and literature](#1-provenance-and-literature)
* [2. User-visible interface](#2-user-visible-interface)
* [3. Module map](#3-module-map)
* [4. Data representation](#4-data-representation)
* [5. Constraint processing pipeline](#5-constraint-processing-pipeline)
* [6. The linear equality solver](#6-the-linear-equality-solver)
* [7. The inequality solver](#7-the-inequality-solver)
* [8. Disequations](#8-disequations)
* [9. Non-linear constraints](#9-non-linear-constraints)
* [10. Optimisation](#10-optimisation)
* [11. Branch and bound](#11-branch-and-bound)
* [12. Projection and answer presentation](#12-projection-and-answer-presentation)
* [13. CLP(Q) versus CLP(R)](#13-clpq-versus-clpr)
* [14. Defects](#14-defects)
* [15. Testing](#15-testing)
* [16. Working on this code](#16-working-on-this-code)


## 1. Provenance and literature

The code is a port of Christian Holzbaur's SICStus `clp(q,r)`, made by Leslie
De Koninck at K.U. Leuven as part of his master's thesis (supervised by Bart
Demoen, daily advisor Tom Schrijvers).  The primary reference for the intended
*behaviour* is

> Holzbaur, C.  *OFAI clp(q,r) Manual, Edition 1.3.3.*  Austrian Research
> Institute for Artificial Intelligence, Vienna, TR-95-09, 1995.
> <https://ofai.at/papers/oefai-tr-95-09.pdf>

That manual describes the implementation as three components, and the code
still has exactly that shape:

1. "A polynomial normal form expression simplification mechanism" — `nf_q.pl`
   / `nf_r.pl`.
2. "A solver for linear equations" — `bv_q.pl` / `bv_r.pl`, after

   > Holzbaur, C.  *A Specialized, Incremental Solved Form Algorithm for
   > Systems of Linear Inequalities.*  1992.

3. "A simplex algorithm to decide linear inequalities" — `ineq_q.pl` /
   `ineq_r.pl` plus the pivoting machinery in `bv_*.pl`, after Holzbaur 1994.

The background for the rest:

* Jaffar, J., Michaylov, S., Stuckey, P.J., Yap, R.H.C.  *The CLP(R) Language
  and System.*  ACM TOPLAS 14(3), 1992.  Source of the *isolation axioms*
  used to wake delayed non-linear constraints, and of the "Monash" example
  suite and `#(Const)` symbolic constants that `nf_r.pl` still supports.
* The bounded-variable simplex (each variable carries its own lower/upper
  bound instead of being represented by a slack row) is the textbook
  *bounded-variable* form, e.g. Chvátal, *Linear Programming*, 1983.
* Answer projection uses Fourier–Motzkin elimination with redundancy removal;
  see Imbert, J-L.  *Fourier's Elimination: Which to Choose?*, PPCP 1993, and
  Lassez, J-L., Lassez, C.  *Quantifier Elimination for Conjunctions of Linear
  Constraints via a Convex Hull Algorithm*, 1992.  The manual states plainly:
  "The current clp(Q,R) version uses a Fourier-Motzkin algorithm for the
  projection of linear inequalities."
* De Koninck, L., Schrijvers, T., Demoen, B.  *INCLP(R) — Interval-based
  nonlinear constraint solving over the reals*, 2006, is the follow-up work
  that handles what this solver deliberately does not.

Two design decisions from the manual are worth repeating because they explain
a lot of the code:

* **Only linear constraints are solved.**  "The clp(Q,R) system is restricted
  to deal with linear constraints because the decision algorithms for general
  nonlinear constraints are prohibitively expensive to run."  Non-linear
  constraints are *collected faithfully* and retried whenever they might have
  become linear.
* **Q and R are the same algorithm over a different number field.**  CLP(Q)
  computes with unbounded rationals and is exact; CLP(R) computes with IEEE
  doubles and replaces every exact comparison with a comparison against a
  fixed epsilon of `1.0e-10`.


## 2. User-visible interface

Both libraries export the same predicates, except that `bb_inf/4` exists only
in CLP(Q) and `bb_inf/5` only in CLP(R).

| Predicate | Meaning |
| --- | --- |
| `{}/1` | Add constraints to the store |
| `entailed/1` | Constraint follows from the store |
| `inf/2`, `inf/4` | Infimum of an expression (`/4` also returns a vertex) |
| `sup/2`, `sup/4` | Supremum of an expression |
| `minimize/1`, `maximize/1` | Optimise *and* bind the expression to the optimum |
| `bb_inf/3`, `bb_inf/4` (Q), `bb_inf/5` (R) | Mixed integer infimum |
| `dump/3` | Project the store onto a list of variables |
| `ordering/1` | Influence the shape of the projected answer |
| `clp_type/2` | Which solver owns a variable |

`library(clpq)` and `library(clpr)` export the same predicate names, so a
single module can only import one of them; use module qualification
(`clpq:{...}`) to mix them in one file.  Mixing CLP(Q) and CLP(R) constraints
on the *same variable* raises a `permission_error`.

### Constraint syntax

```
Constraints ::= Constraint | Constraint , Constraints | Constraint ; Constraints
Constraint  ::= Expr < Expr | Expr > Expr | Expr =< Expr | <=(Expr,Expr)
              | Expr >= Expr | Expr =\= Expr | Expr =:= Expr | Expr = Expr
Expr        ::= Var | Number | +Expr | -Expr | Expr+Expr | Expr-Expr
              | Expr*Expr | Expr/Expr | abs(Expr) | sin(Expr) | cos(Expr)
              | tan(Expr) | exp(Expr,Expr) | pow(Expr,Expr) | Expr^Expr
              | min(Expr,Expr) | max(Expr,Expr)
              | #(Const)                                   % CLP(R) only
```

Note what is *not* accepted, because it is a frequent source of surprise:

* `**` is not an operator of the language — only `^`, `pow/2` and `exp/2`.
* `rdiv/2` is not accepted by CLP(Q); write `1/3`, which CLP(Q) evaluates
  exactly to `1r3`.
* Anything else raises `type_error(clpq_expression, T)` resp.
  `type_error(clpr_expression, T)`.
* `sqrt/1`, `log/1`, `exp/1` (unary) are not part of the language.

Plain unification is equivalent to an equality constraint: `X = Y`,
`{X = Y}` and `{X =:= Y}` all post the same constraint, because the solver
installs an `attr_unify_hook/2`.

### Semantics of the less obvious predicates

`entailed(C)` is implemented as `negate(C, Cn), \+ {Cn}`: the constraint is
entailed exactly when adding its negation makes the store unsatisfiable.  It
therefore never changes the store.

`inf/2` and `sup/2` do not change the store either — the extremal vertex is
reached by pivoting, the value is captured in a mutable term and the pivots
are then undone by backtracking.  `minimize/1`/`maximize/1` exist separately
(rather than as `inf(E,E)`) precisely because they *do* want to stay at the
optimal vertex; see the long comment above `minimize/1` in `bv_q.pl`.

`dump(+Target, +NewVars, -Constraints)` projects the store onto `Target`,
which must be a list of *unbound* variables, and renames them to `NewVars`.
The store is not modified: `dump/3` runs the destructive projection inside a
failure-driven loop and recovers the result with `nb_setarg/3`.


## 3. Module map

```
clpq.pl / clpr.pl        user entry point, toplevel residual printing
├── clpq/nf_q.pl         {}/1, entailed/1, polynomial normal form
├── clpq/store_q.pl      sparse linear-form arithmetic
├── clpq/bv_q.pl         equality solver, simplex pivoting, optimisation, dump
├── clpq/ineq_q.pl       inequality solver (bound updates)
├── clpq/fourmotz_q.pl   Fourier–Motzkin elimination (projection)
├── clpq/bb_q.pl         branch and bound (bb_inf/3,4)
├── clpq/itf_q.pl        attr_unify_hook back end (type/bound checks)
└── clpqr/…              solver-independent parts, shared by Q and R
    ├── itf.pl           clpqr_itf attribute + attr_unify_hook/2
    ├── class.pl         clpqr_class attribute (connected component)
    ├── geler.pl         clpqr_geler attribute (delayed non-linear goals)
    ├── ordering.pl      ordering/1, priority graph, topological sort
    ├── project.pl       project_attributes/2 driver
    ├── redund.pl        redundant-bound elimination
    ├── dump.pl          dump/3, attribute_goals//1
    └── highlight.pl     library(prolog_colour) support
```

`clpr/` mirrors `clpq/` file for file.  The two trees are *not* generated from
a common source; they are independent copies that have drifted apart (see
[§13](#13-clpq-versus-clpr)).

The shared `clpqr/` modules dispatch on the solver by inspecting the first
argument of the `clpqr_itf` attribute and calling the right module explicitly:

```prolog
fm_elim(clpq,Avs,Tvs,Pivots) :- fourmotz_q:fm_elim(Avs,Tvs,Pivots).
fm_elim(clpr,Avs,Tvs,Pivots) :- fourmotz_r:fm_elim(Avs,Tvs,Pivots).
```

This is why `clpqr/*.pl` can be loaded once while both solvers are active.


## 4. Data representation

### 4.1 Polynomial normal form (`nf`)

`nf/2` in `nf_*.pl` turns a user expression into an ordered sum of monomials:

```
Nf   ::= list of v(K, Powers)          % implicit sum, ordered by Powers
Powers ::= list of Base^Exp            % implicit product, ordered by Base
```

* `v(K,[])` is the scalar `K`; the empty list `[]` is the number 0.
* A variable `X` normalises to `[v(1,[X^1])]`.
* `Powers` is kept sorted on the standard order of `Base`, exponents of equal
  bases are added (`pmerge/3`), and a zero exponent removes the factor.
* Monomials with equal `Powers` are added (`nf_add/3`), and a zero coefficient
  removes the monomial.  The ordering `[] < [X^1] < …` makes the scalar, if
  present, the first element, which `split/3` exploits.
* A `Base` need not be a variable: `sin(Nf)`, `exp(Nf,Nf)`, `min(Nf,Nf)`,
  `abs(Nf)` and the "undigested" quotient `A/B` all appear as bases.  This is
  what makes the representation closed under the non-linear functions.

Integer powers of sums are expanded with the binomial theorem (`binom/3`),
which the header comment notes is roughly 4× faster than iterated
multiplication for `(1+X+Y+Z)^15`.

`linear/1` recognises a normal form in which every monomial is `v(_,[])` or
`v(_,[X^1])` with `X` a variable.  Only such normal forms reach the linear
solver.

### 4.2 Linear form (`lin`)

Once a normal form is known to be linear it is *dereferenced* into the solver's
working representation by `deref/2`:

```
Lin ::= [ I, R | Hom ]
Hom ::= list of l(X*K, Ord)            % ordered on Ord
```

* `I` is the inhomogeneous part (a constant).
* `R` is the *rhs* accumulator: the sum over `Hom` of `K` times the currently
  *active bound* of `X`.  `R+I` is therefore the current value of the variable
  this row defines.  Keeping `R` incrementally is what makes bound checking
  O(1) instead of O(row length) — see `rcb/3`.
* `Ord` is the variable's ordering key (§4.4).  `Hom` is sorted on it, so all
  the vector operations in `store_*.pl` (`add_linear_ff/5`, `add_linear_f1/4`,
  `add_linear_11/3`, `nf_substitute/4`, `delete_factor/4`) are linear merges.

A variable `X` that has never been seen gets the row `[0,0,l(X*1,Ord)]`, i.e.
`X = X`; it is *independent*.  A variable whose row has more than one term is
*dependent* (basic).

### 4.3 Attributes

Three attributes are used.  All three live in modules named after the
attribute, which is how `attr_unify_hook/2` and `attribute_goals//1` are
found.

**`clpqr_itf`** — the main per-variable record, an 11-argument term:

| # | Contents | Notes |
| --- | --- | --- |
| 1 | `clpq` or `clpr` | never changes; used for solver dispatch |
| 2 | `type(T)` or `n` | bound state, see §7.1 |
| 3 | `strictness(S)` or `n` | 2 bits, see §7.1 |
| 4 | `lin(Lin)` or `n` | the row for this variable |
| 5 | `order(Ord)` or `n` | ordering key |
| 6 | `class(C)` or `n` | connected component |
| 7 | — | **unused**; never read, never written |
| 8 | `nonzero` or `n` | variable is constrained `=\= 0` |
| 9 | `target` or `n` | projection: variable must survive |
| 10 | `keep_indep` or `n` | projection: do not eliminate |
| 11 | `keep` or `n` | projection: pivot partner, do not drop |

Arguments 2–6 are set to `n` together (`drop_lin_atts/1`, `drop_dep_one/1`)
when a variable becomes irrelevant to the linear store.  Arguments 9–11 are
only meaningful during projection.

**`clpqr_class`** — `class(CLP, All, AllT, Basis, Prio)` on a fresh variable
that acts as the identity of a connected component ("class") of the linear
store:

* `All`/`AllT` is an open (difference) list of every variable in the class.
* `Basis` is the list of dependent variables that currently have a bound
  (the simplex basis).
* `Prio` is the user's ordering graph for this class (§12.3).

Merging two classes is done by *unifying the class variables*: the
`attr_unify_hook/2` in `class.pl` appends the two variable lists by binding
the open tail of one to the head of the other, concatenates the bases and
combines the priority graphs.  This makes union-find essentially free.

**`clpqr_geler`** — `g(CLP, goals(Conj), Flag)` holding the delayed non-linear
goals that mention this variable (§9).  `goals(Conj)` becomes `n` when the
goals have run.  `Flag` is vestigial: it is created as `n` and nothing ever
sets it to anything else, so the branch in `attr_unify_hook/2` that tests it
is unreachable.  It is kept because the shape is pattern-matched in several
places and removing it would buy nothing.

### 4.4 Variable ordering

Every variable in the linear store carries an `order(Ord)` key, and rows are
sorted on it.  The trick is that `Ord` is normally an *unbound fresh
variable*: `compare/3` on two unbound variables is stable within a session and
reflects creation order, so ordering is free and total.  `ordering/1` and
projection can bind these variables to small integers to force a particular
arrangement, and integers sort before variables, so explicitly ordered
variables come first.

`renormalize/2` in `store_*.pl` rebuilds a row after orders have been bound
or variables have been unified, because the sort invariant can be broken by
both.


## 5. Constraint processing pipeline

`{C}` (in `nf_*.pl`) is a small dispatcher:

| Input | Action |
| --- | --- |
| `var` | `instantiation_error` |
| `(A,B)` | `{A}, {B}` |
| `(A;B)` | `{A} ; {B}` — needed by `entailed/1` |
| `L < R` | `nf(L-R,Nf), submit_lt(Nf)` |
| `L > R` | `nf(R-L,Nf), submit_lt(Nf)` |
| `L =< R`, `<=(L,R)` | `nf(L-R,Nf), submit_le(Nf)` |
| `L >= R` | `nf(R-L,Nf), submit_le(Nf)` |
| `L =:= R`, `L = R` | `nf(L-R,Nf), submit_eq(Nf)` |
| `L =\= R` | `nf(L-R,Nf), submit_ne(Nf)` |
| other | `type_error(clpq_constraint, C)` |

Every constraint is thus normalised to a comparison of a polynomial with 0.
The `submit_*` predicates then perform a case analysis on the *shape* of the
normal form, in increasing order of cost.  `submit_eq/1` is representative:

```
[]                     trivially true
[v(K,[])]              K =:= 0, i.e. fail
[v(_,[X^P])], P>0      X = 0
[v(_,[NL^1])]          NL invertible  =>  X = inv(NL)(0)
[v(I,[]),v(K,[X^P])]   P = 1: X = -I/K;  P = -1: X = -K/I
[v(I,[]),v(K,[NL^P])]  NL invertible  =>  recurse
linear                 log_deref/4 then solve/1
otherwise              geler/3 (delay)
```

`log_deref/4` converts a normal form to a linear form by divide and conquer
(halving the list) rather than left to right, so that merging `n` rows costs
`O(n log n)` merges rather than `n` merges against a growing accumulator.
The same logarithmic pattern recurs in `renormalize_log/4`, `nf_mul_log/6`,
`nf_mul_factor_log/5` and `repair_log/4`.


## 6. The linear equality solver

`solve/1` in `bv_*.pl` solves `Lin = 0`.  It implements the incremental
solved-form algorithm of Holzbaur 1992: the store is kept in *reduced row
echelon form*, i.e. every dependent variable is defined by a row over
independent variables only.

Given a row to add, `sd/7` scans its homogeneous part and simultaneously

* collects the set of distinct classes the row touches (`ord_add_element/3`),
* collects the variables that have no class yet, and
* selects the variable to solve for, by *preference category*:

| Category | Variable | Why |
| --- | --- | --- |
| 1 | no class, no bounds | cheapest: nothing to repair |
| 2 | has class, no bounds | needs back-substitution in its class |
| 3 | no class, bounded | new basis entry, basis must be reconsidered |
| 4 | has class, bounded | both of the above |

`isolate/3` rewrites `Lin = 0` into `Selected = Lin1` and the four categories
then differ in how much work follows:

* **1** — just store `Lin1` as the row of `Selected` and attach the new
  variables to a fresh class.
* **2** — back-substitute `Selected` in every row of its class.  If the row
  touched exactly one class (`ClassesUniq = [_]`) the rank increases, and
  `bs_collect_bindings/5` can additionally harvest *implied values*: any row
  that collapses to `X = Inhom` yields a binding.
* **3**/**4** — as above, plus `deactivate_bound/2`, `basis_add/2`,
  `undet_active/1` and finally `rcbl/3` to re-establish feasibility of the
  whole basis.

Bindings are collected and applied at the very end (`export_binding/1`) rather
than during back-substitution, because binding a variable in the middle of the
traversal would trigger `attr_unify_hook/2` re-entrantly.

`solve_ord_x/3` is a variant that solves for a *given* variable; it is used
by the unification hook, which must eliminate the variable that is about to
be bound.


## 7. The inequality solver

### 7.1 Bounds and strictness

A variable's bound state is the `type(T)` field, with a capital letter marking
the *active* bound (the bound the variable currently sits on):

```
t_none               no bounds
t_l(L)   t_u(U)      inactive lower / upper bound
t_L(L)   t_U(U)      active lower / upper bound
t_lu(L,U)            two inactive bounds
t_Lu(L,U)            active lower, inactive upper
t_lU(L,U)            inactive lower, active upper
```

Only *independent* variables have an active bound; the `R` field of a
dependent variable's row is the weighted sum of the active bounds of the
independent variables it is defined over.

Strictness is two bits in `strictness(S)`: bit 1 (`S /\ 2`) marks a strict
lower bound, bit 0 (`S /\ 1`) a strict upper bound.  `ilb/3`, `iub/3` (check a
value), `llb//3`, `lub//3` (emit a constraint) and `dump_strict/4` all decode
it.

### 7.2 Adding an inequality

`ineq/4` in `ineq_*.pl` again dispatches on shape:

* `I < 0` with no variables — evaluate.
* `K*X + I < 0` — a *bound update* on a single variable.  This is the common
  case and it never introduces a slack variable.
* otherwise `ineq_more/4`:
  * if the row contains an *unconstrained* variable `U` (`unconstrained/4`
    finds one of category ≤ 2), solve the row for `U` against a fresh
    slack variable `S ≥ 0` and back-substitute.  This can never fail and
    never implies a value, because nothing constrains `U`.
  * otherwise create `S = Lin` with `S ≤ 0` (`var_with_def_intern/4`), add
    `S` to the basis, activate bounds (`determine_active_dec/1`) and call
    `reconsider/1`.

The bound-update predicates form a 4×8 table named by the mnemonic
*u(d|i)(l|u)(s?)* — **u**pdate **d**ependent/**i**ndependent
**l**ower/**u**pper, `s` for strict:

| | non-strict | strict |
| --- | --- | --- |
| dependent lower | `udl/5` | `udls/5` |
| dependent upper | `udu/5` | `udus/5` |
| independent lower | `uil/5` | `uils/5` |
| independent upper | `uiu/5` | `uius/5` |

Each has one clause per incoming `type(T)`, and each clause must decide
between: ignore the new bound (weaker), tighten it, detect that lower and
upper have met (`solve_bound/2`, which turns the pair into an equality), or
detect infeasibility.  Tightening the *active* bound of an independent
variable additionally shifts the `R` field of every row in the class by
`Delta` (`backsubst_delta/4`), or pivots first if the most constraining row
would be violated.

### 7.3 Restoring feasibility

`lb/3` and `ub/3` scan the basis for the *most constraining row* — the row
that limits how far a given independent variable may move.  `lb_inner/5` and
`ub_inner/5` compute, from a basic row and the coefficient of the variable in
it, how much the variable may move before that row's bound is violated.
Infeasible rows are deliberately excluded from the candidate set (`Lb =< 0`,
`Ub >= 0`); the comment in the source is emphatic: "We must NOT consider
infeasible rows as candidates to leave the basis!"

`rcb/3` finds a basic variable that is outside or at its bound and calls
`inc_step/2` or `dec_step/2`, which scan the row left to right for a variable
that can be moved in the desired direction.  The result is one of

* `applied` — a pivot was performed, try again;
* `optimum` — the row cannot be improved.  If the optimum *meets* the bound,
  the bound is an implied equality, which is fed back into `solve/5`;
* `unlimited(V,T)` — the row is unbounded in the needed direction, so the
  culprit can simply be pivoted out (`rcbl_unl/7`).

`rcbl/3` iterates this over the whole basis, threading the collected bindings.
The `*_cont` variants (`dec_step_cont/4`, `inc_step_cont/4`) additionally
maintain the *continuation*, i.e. the not-yet-examined part of the basis, so
that a pivot which swaps a variable into the basis updates the work list
(`replace_in_cont/4`).

Note that this is a pure feasibility (phase-I-like) procedure, not an
optimisation: no objective row exists until `inf/2` creates one.


## 8. Disequations

`X =\= Y` cannot be expressed in the simplex tableau, so `solve_=\=/1`

1. dereferences the normal form to `Lind`,
2. if it is ground, checks it is not 0,
3. otherwise creates a fresh variable `Nz` defined by `Nz = Lind` and sets
   argument 8 of its `clpqr_itf` attribute to `nonzero`.

Nothing else happens until `Nz` would be bound: `verify_nonzero/2` in
`itf_*.pl` then checks `Y =\= 0` for a number, or propagates the `nonzero`
mark to the variable `Y` is unified with.  The check is therefore *passive* —
it detects a violation at binding time, not at constraint time, which is
exactly what the OFAI manual's "Why disequations" section (the all-different
squared-rectangle tiling example) needs.

`dump_nz//3` renders the mark back as `Sum =\= I` for answer presentation.


## 9. Non-linear constraints

### 9.1 Delaying

When `submit_*` finds a normal form that is not linear it calls
`geler(CLP, Vars, Goal)`, which attaches `run(Mutex, Goal)` to the
`clpqr_geler` attribute of every variable in `Vars`.  The shared `Mutex`
ensures the goal runs *once* even though it is reachable from several
variables — `run/2` succeeds immediately if `Mutex` is already bound.

The delayed goals are one of `resubmit_eq/1`, `resubmit_lt/1`,
`resubmit_le/1`, `resubmit_ne/1` or `wait_linear_retry/3`.

### 9.2 Waking and repair

The `attr_unify_hook/2` in `geler.pl` runs the goals of the variable being
bound.  Each `resubmit_*` first calls `repair/2`, which re-normalises a stored
normal form whose bases may since have become (partly) ground:
`repair_p_one/2` re-runs the appropriate `nf` case for each non-variable base,
and `nf_power/3` re-expands integer powers.  The result is submitted again,
and may now be linear, solvable by an isolation axiom, or still delayed.

`wait_linear/3` is the entry point used by `inf/2`, `minimize/1` and
`bb_inf/3`: it delays the whole optimisation until its expression has become
linear.  A still-delayed optimisation is not a constraint, so it cannot be
reported inside a `{}/1` term.  `transg//1` reconstructs the user level goal
that created it (`pending_goal//2`) and `attribute_goals//1` emits it as a
separate goal, which keeps the answer of `copy_term/3` executable:

```
?- {Y >= 1, Y =< 5}, minimize(X*Y), copy_term(f(X,Y),C,Gs).
Gs = [{_A>=1, _A=<5}, clpq:minimize(_A*_B)].
```

`dump/3` returns only the constraints and drops such goals.

### 9.3 Isolation axioms

`nl_invertible/4` implements the axioms that let a non-linear atom be solved
once enough of it is ground:

| Constraint | Solvable when |
| --- | --- |
| `A = B*C` | `B` or `C` ground; or `A` and one of `B`,`C` ground |
| `A = B/C` | `C` ground; or `A` and `B` ground |
| `X = min(Y,Z)`, `X = max(Y,Z)` | `Y` and `Z` ground |
| `X = abs(Y)` | `Y` ground |
| `X = pow(Y,Z)` / `Y^Z` | any two of `X`,`Y`,`Z` ground |
| `X = sin(Y)`, `cos`, `tan` | `X` or `Y` ground |

The multiplication and division cases are handled structurally by the normal
form (a product of a scalar and a variable *is* linear), the rest by
`nl_invertible/4` and `nl_eval/2`.

Inverting `A = Kb^X` for `X` needs a logarithm, which is in general
irrational and therefore not representable in Q at all.  `log_q/3` in
`nf_q.pl` computes the quotient in floating point, looks for an exact
rational answer with a denominator of at most 16 (verifying it with exact
rational arithmetic), and only falls back on `rationalize/1` when there is
none.  That recovers `3` from `{1000 =:= 10^Y}` — whose float quotient is
`2.9999999999999996` — and `1r3` from `{2 =:= 8^Y}`.

Both solvers solve `I + K*X^P = 0` for a variable `X` and a numeric exponent
`P`, and both enumerate the two solutions of an even `P` on backtracking.
They differ in what counts as a solution, which is exactly the difference
between the two number fields:

```
?- clpq:{4 =:= X^2}.        X = 2 ;  X = -2.
?- clpr:{4 =:= X^2}.        X = 2.0 ;  X = -2.0.
?- clpq:{2 =:= X^2}.        false.
?- clpr:{2 =:= X^2}.        X = 1.4142135623730951 ;  X = -1.4142135623730951.
```

CLP(R) approximates; CLP(Q) must not, and an irrational root simply does not
exist over the rationals, so the constraint fails.  `exact_root/3` decides
this by asking `(**)/2` for the root and checking whether the answer came
back rational.


## 10. Optimisation

`inf(Expr, Inf)` waits for `Expr` to be linear, then:

1. creates a fresh dependent variable `Dep = Expr`;
2. `determine_active_dec/1` activates a bound on every variable of the row,
   choosing the bound that makes `R` as small as possible;
3. `iterate_dec/2` repeatedly calls `dec_step/2` until it reports `optimum`,
   at which point the infimum is `R + I`;
4. stores `[Inf|Vertex]` with `nb_setarg/3` in a term local to the call and
   **fails**, undoing every pivot;
5. the second branch reads that term back and posts `{Inf =:= Value}`.

If any variable of the row has `type(t_none)`, `determine_active_dec/1` fails
and so does `inf/2` — an unbounded expression has no infimum.  `sup/2` is
`inf/2` of the negation.

`minimize/1` performs the same pivoting but keeps the result, then constrains
`{Dep =:= Inf}`.  The source explains the duplication: going through
`inf(E,E)` would force the simplex to rediscover the optimal vertex from the
equation, but keeps no garbage in the tableau; both trade-offs are offered.

`inf/4` and `sup/4` additionally return the values of a caller-supplied vector
of variables at the optimal vertex (`vertex_value/2` in `bb_*.pl`, which reads
`R+I` of each row).


## 11. Branch and bound

`bb_inf(Ints, Expr, Inf, Vertex)` is a textbook depth-first branch and bound
over the LP relaxation:

* `bb_intern/3` checks each element of `Ints`: a number must be integral, a
  variable has its bounds tightened to the enclosing integers with
  `bb_narrow_lower/1` / `bb_narrow_upper/1` (which use `inf/2`, `sup/2` and
  `entailed/1` to get the *strict* case right);
* `bb_loop/2` re-optimises (`bb_reoptimize/2`), prunes against the incumbent
  (`bb_better_bound/1`), and finds the first variable whose value is not
  integral (`bb_first_nonint/5`);
* `bb_branch/3` is the choice point `{V =< Floor} ; {V >= Ceiling}`;
* when every integer variable is integral the incumbent is replaced
  (`nb_setval(prov_opt, …)`), and the whole search is driven by failure.

The incumbent has to survive the backtracking that drives the search but must
not survive the call, so it lives in a mutable term local to the call,
updated with `nb_setarg/3` — the same idiom `dump/3` uses.

In CLP(R) the extra `Eps` argument says how far from an integer a value may
be and still count as integral; `bb_inf/3` uses `0.001`.  CLP(Q) needs no such
parameter and uses `integer/1` directly.


## 12. Projection and answer presentation

This is the most intricate part of the system and the part users see.

### 12.1 `project_attributes/2`

Driven from `dump/3` (and from `copy_term/3` via `attribute_goals//1`), given
the target variables `Tvs` and all variables of their classes `Avs`:

0. `intern_vars/1` (from `ordering.pl`) puts every target the solver already
   knows into one class, so that the rest of the projection can reach the
   whole store from the target list.
1. `mark_target/1` sets argument 9 of the targets.
2. `project_nonlin/3` collects the delayed non-linear goals.  This *consumes*
   the mutexes, which is why the whole projection must run inside a failure
   loop or on a copy.
3. `redundancy_vars/1` removes redundant bounds (§12.2).
4. `make_target_indep/2` pivots each target variable with a non-target partner
   so that targets become independent; the partners are marked `keep`.
5. `drop_dep/1` discards rows of unbounded, non-target, non-`keep` dependent
   variables.
6. `fm_elim/4` eliminates the remaining non-target variables by
   Fourier–Motzkin (§12.3).
7. `impose_ordering/1` applies the user's `ordering/1` graph.

### 12.2 Redundancy elimination

`redund.pl` uses the *semantic* definition: a bound is redundant if adding its
negation makes the store unsatisfiable.  `redundant/3` therefore detaches the
bound, asserts the opposite through `{}/1`, and keeps the bound only if that
*succeeded* (`negate_l/4` and `negate_u/4` are written to fail when the
opposite bound is consistent, so that backtracking restores the bound).  For
two-sided types each side is tried separately.

This is expensive — it runs a full consistency check per bound — but it is
what makes projected answers readable; the OFAI manual reports 18 inequalities
reduced to 9 on its example.

### 12.3 Fourier–Motzkin elimination

`fourmotz_*.pl` eliminates one non-target variable at a time:

* `prefilter/2` drops targets and variables that occur in no bounded row.
* `best/3` chooses the next variable to eliminate by *fill-in*: for every
  candidate, `cp_card/4` counts how many inequalities the cross product would
  generate, and `Delta = New - Old` is minimised.  This is the standard
  greedy heuristic; if no candidate can be scored the elimination gives up and
  the remaining variables simply stay in the answer.
* `crossproduct//2` combines every pair of rows in which the variable occurs
  with opposite signs, producing `lez(Strict, Lin)` items; `flip/2` and
  `flip_strict/2` handle the sign case.
* `activate_crossproduct/1` turns each item into a fresh variable `V = Lin`
  with `V ≤ 0`, added to the basis.
* `fm_detach/1` then drops the bounds of the eliminated variable, and
  `reverse_pivot/1`/`unkeep/1` restore the pivots made by
  `make_target_indep/2`.

### 12.4 `ordering/1`

`ordering/1` accepts `A<B`, `A>B` or a list, and records edges in a *priority
graph* stored in the class's `clpqr_class` attribute.  At projection time
`arrangement/2` runs `top_sort/2` over the graph; a cycle raises
`unsatisfiable_ordering`.  `arrange/2` then binds the `order` keys of the
arrangement to `1,2,…`, renormalises every row (`renorm_all/1`) and finally
pivots rows into the requested shape (`arrange_pivot/1`).

The direction, quoting TR-95-09, is: *"Suppose that instead of B, you want Mp
to be the **defined** variable"* — so **a variable that comes earlier is the
one the answer defines**, i.e. the one on the left-hand side, and `A<B` "means
that A goes to the left of B".  `arrange_pivot/1` establishes this by pivoting
any dependent variable that is defined over an earlier-ordered variable.

`ordering/1` "acts like a constraint: you can put it anywhere in the
computation", so it has to work on variables the solver has not seen yet.  It
cannot tell by itself whether such a variable belongs to CLP(Q) or CLP(R), so
`clpq:ordering/1` and `clpr:ordering/1` are thin wrappers that pass their
solver to `clpqr_ordering:ordering/2`.

`dump/3` does *not* impose an ordering of its own — it only calls
`intern_vars/1` to make its targets reachable from the linear store, which is
what lets `nonlin_crux/2` find their delayed goals.  The order of the `dump/3`
target list is therefore immaterial, and an explicit `ordering/1` is never
contradicted.

### 12.5 Rendering

`dump_var//4` in `bv_*.pl` turns a row plus a bound type into readable
constraints: an equality `V = Sum` for `t_none`, and one or two inequalities
otherwise, scaled so that the first coefficient is 1.  `nf2sum/3` and
`hom2sum/3` build the term, choosing `+`/`-` and omitting unit coefficients.

`attribute_goals//1` in `dump.pl` wraps this for `copy_term/3` and for the
toplevel: it calls `dump/3` on the attributed variables of the term, wraps the
result in `{}/1`, and then deletes the `clpqr_itf` *and* `clpqr_geler`
attributes of every variable it has just reported, so that the same
conjunction is not emitted once per variable.  Both have to go, because a
variable that carries only a delayed non-linear goal has no `clpqr_itf`
attribute at all.

The set it projects *onto* is not the same as the set it reports *for*.
`user_vars/2` drops the variables the solver invented for itself — the slack
variables of `ineq_more/2`, the objective of `minimize/1`, the witness of a
disequation — all of which are marked `aux` in argument 7 of the
`clpqr_itf` record at the point where they are created
(`var_with_def_intern/4` and `var_intern/4` in `bv_*.pl`).  Those variables
are then eliminated by the ordinary Fourier–Motzkin machinery, which is what
turns the raw tableau back into the constraints the user wrote down.  The
attributes are still deleted from *all* of the variables, targets and
internals alike, since every one of them has now been accounted for.

The mark is deliberately put on the solver's own variables rather than on the
user's.  Missing a mark leaves an internal variable in the answer — ugly, but
correct.  Marking a user variable by mistake would project a real constraint
away and produce an answer that is too weak.

What this *cannot* do is project onto the term that was copied.
`term_attvars/2` walks through attributes, so the variable set SWI-Prolog
hands to `attribute_goals//1` is already the whole connected component, no
matter how little of it the copied term mentions.  `copy_term(X, C, Gs)` on
`{X+Y >= 1}` therefore still reports `{C + _ >= 1}` with a fresh variable for
`Y`, where `dump([X],[x],L)` gives `L = []`.  Code that needs a projection
onto specific variables has to call `dump/3`.


## 13. CLP(Q) versus CLP(R)

CLP(Q) computes with SWI-Prolog rationals (`rdiv`, `rationalize/1`).  Every
comparison is exact and every answer is exact.

CLP(R) computes with doubles and replaces each exact test with an
epsilon test, hard-coded as `1.0e-10` throughout:

| Exact test (Q) | Fuzzy test (R) |
| --- | --- |
| `K =:= 0` | `K >= -1.0e-10, K =< 1.0e-10` |
| `K =:= 1` | `abs(K-1.0) =< 1.0e-10` |
| `I < 0` | `I < -1.0e-10` |
| `I =< 0` | `I < 1.0e-10` |
| `K =\= 0` | `\+ (K >= -1.0e-10, K =< 1.0e-10)` |

Other systematic differences:

* **Coefficients.** CLP(R) writes `0.0`, `1.0`, `-1.0` where CLP(Q) writes
  `0`, `1`, `-1`, so rows stay float-typed.
* **Binding.** CLP(R) has `export_binding/2`, which snaps a value within
  epsilon of zero to exactly `0.0`.  CLP(Q) just unifies.
* **`numbers_only/1`.** CLP(Q) demands `rational/1` (integers are rational),
  CLP(R) demands `integer/1` or `float/1`.
* **Symbolic constants.** `#(pi)`, `#(p)`, `#(e)`, `#(zero)` exist only in
  CLP(R) ("provided for compatibility only", per the manual).  The first
  three evaluate SWI-Prolog's own `pi` and `e`.
* **Integer exponents.** CLP(R) accepts a float exponent that happens to be
  integral (`integerp/2`); CLP(Q) requires `integer/1`.
* **Root extraction.** Both solve `I + K*X^P = 0` for `X`, but CLP(Q) only
  accepts a rational root and fails otherwise (§9.3).
* **`bb_inf`.** `bb_inf/4` in Q, `bb_inf/5` with an epsilon in R; R rounds the
  returned vertex with `round/1`.

A consequence that bites in practice: because CLP(R) binds its variables to
*floats*, a clause head written with an integer no longer matches.  The
classic

```prolog
fib(0, 0).
fib(1, 1).
fib(N, F) :- {N > 1, N1 =:= N-1, N2 =:= N-2, F =:= F1+F2}, fib(N1,F1), fib(N2,F2).
```

works in CLP(Q) but *fails* in CLP(R), where the recursion reaches
`fib(0.0, F)` and `0.0` does not unify with `0`.  The base cases have to be
written as constraints (`fib(N,F) :- {N =:= 0, F =:= 0}.`).

The practical consequence, which the OFAI manual states outright, is that
"the fact that you can switch between clp(R) and clp(Q) should solve most of
your numerical problems regarding precision" — ill-conditioned problems belong
in CLP(Q).


## 14. Defects

Everything in this section was found by reading the code against TR-95-09 and
by probing the running system.  §14.1 to §14.5 are **fixed**, each in its own
commit with regression tests; §14.6 lists what is still open.

### 14.1 Dead code

The following predicates had no caller at all and have been removed, together
with the comments and the commented-out call sites that referred to them:

| Predicate | File | Note |
| --- | --- | --- |
| `'solve_='/1` | `bv_*.pl` | `nf_*.pl` calls `solve/1` directly |
| `solve_x/2`, `solve_x/6` | `bv_*.pl` | superseded by `solve_ord_x/3` |
| `iterate_inc/2` | `bv_*.pl` | only `iterate_dec/2` is used |
| `basis/2`, `basis_drop/1` | `bv_*.pl` | thin wrappers over `class.pl` |
| `pivot/2` | `bv_*.pl` | and it read the class from the order field |
| `nf_power_pos/3` | `nf_*.pl` | superseded by `binom/3` |
| `integerp/1` | `nf_r.pl` | only `integerp/2` is used |
| `l2conj/2`, `nonexhausted//1` | `clpqr/geler.pl` | |
| `class_get_clp/2` | `clpqr/class.pl` | exported, never imported |
| `projecting_assert/1`, `l2c/2` | `clpqr/dump.pl` | see below |

`projecting_assert/1` asserted a clause with the constraints on its variables
attached.  It was exported by `clpqr_dump` but commented out of the `clpq` and
`clpr` export lists, so it was reachable only as
`clpqr_dump:projecting_assert/1`; it is not in TR-95-09; and it was broken —
`( Sm = clpq ; Sm = clpr ), !` always chose `clpq` whichever solver the
clause actually belonged to.  It has been removed rather than repaired,
because nothing says what it should do for CLP(R).

`redundancy_vars/1` had an unreachable second clause behind a cut which
printed timings; that too is gone.  The `red_t_l/0`, `red_t_u/0`, `red_t_L/0`
and `red_t_U/0` facts in `clpqr/redund.pl` *are* called, from `redundant/3`;
they are deliberate no-op probes for counting redundant bounds and are kept.

Argument 7 of the `clpqr_itf` attribute is still never read or written.  It
is left in place: renumbering the other ten would touch every `arg/3` and
`setarg/3` call in the package for no gain.

Three more pieces are unreachable but kept, because in each case the
argument rests on something non-local rather than on the code in front of
you.  The annotated coverage sources (§15) confirm that none of them runs:

* The first clause of `ineq/4` and the first clause of `ineq_cases/6` in
  `ineq_*.pl`, because `submit_lt_c/3` and `submit_le_c/3` filter the
  single-variable case before the solver is entered.  With them go the
  `I =:= 0` arms of `ineq_one/4`, which nothing else reaches.
* The `Category = 3` arm of `solve/5` in `bv_*.pl` — "classless variable,
  all variables bounded".  `sd/7` never assigns that category at all: every
  variable that acquires a bound also acquires a class, so `preference/3` is
  never called with `3-X-K`.

`nf/2`'s `rational(X)` clause in `nf_q.pl` was in the same category and *has*
been removed: every rational satisfies `number/1`, so the clause above it
always won.

### 14.2 Wrong results

* `#(pi)` and `#(p)` were `3.14259265`: two digits of pi transposed, an error
  of 1.0e-3 where the solver's own epsilon is 1.0e-10.
* Inverting `A = B^X` computed `rational(log(A)) rdiv rational(log(Kb))`, the
  *exact* rational value of two floats, so `{8 =:= 2^Y}` bound `Y` to
  `18729944304496076r6243314768165359` rather than `3`.
* A store holding nothing but delayed non-linear goals was reported once per
  variable, because `attribute_goals//1` removed only the `clpqr_itf`
  attribute of the variables it had reported and such a variable has none.
* A `minimize/1`, `inf/2` or `bb_inf/3` still waiting for its expression to
  become linear put the solver's own continuation inside the `{}/1` residual,
  so the answer of `copy_term/3` raised a type error when called.
* `bb_inf/3,4,5` kept its incumbent in the global variable `prov_opt`, and
  `inf/2,4` (hence `sup/2,4` and `minimize/1`) kept its optimum in the global
  `inf`.  Global variables share one namespace with the calling program, so
  both destroyed a caller's variable of that name, and nested calls would
  have interfered.
* CLP(Q) did not implement the documented isolating axiom for `X = Y^Z`, so
  `{8 =:= Y^3}` reported success with `Y` unbound.

### 14.3 `ordering/1`

`ordering/1` used to have no effect whatsoever.  Four separate defects
conspired, all of them now fixed:

* The arrangement was discarded: `arrange_pivot/1` in `clpqr/project.pl`
  tested `arg(6,AttY,clpqr_class(Class))` where argument 6 of the `clpqr_itf`
  attribute holds `class(C)`, so the guard could never succeed and no pivot
  was ever made.
* The pivot went the wrong way round: it made the *last* variable of the
  arrangement the defined one, where TR-95-09 makes it the *first*.
* `dump/3` called `ordering(Target)` on its own target list, which imposes a
  total order over the targets and therefore swamps — or contradicts — any
  ordering the user asked for.  A contradiction threw a bare
  `unsatisfiable_ordering` term.  `dump/3` now calls `intern_vars/1`, which
  has the side effect the call was really there for (making the targets
  reachable from the linear store) without touching the priority graph.
* On variables the solver had not seen yet, `join_class/2` failed and
  `ordering/1` fell through to its catch-all clause, silently doing nothing.
  Since the manual explicitly allows stating an ordering before the
  constraints, `clpq:ordering/1` and `clpr:ordering/1` now pass their solver
  down so that such variables can be interned.

The four worked examples of the manual's "Variable Ordering" section are
reproduced as tests in `test_clpr.pl`.

### 14.4 The isolating axiom for `X = Y^Z`

CLP(R) solved `I + K*X^P = 0` for `X`; CLP(Q) did not, although the manual
documents the axiom for both.  CLP(Q) now solves it exactly, enumerating the
two roots of an even power and *failing* when the root is irrational, which
is the right answer over the rationals.  The companion axiom `n*X^P = 0 =>
X = 0` only holds for `P > 0`; a negative `P` now fails rather than leaving a
residue.

### 14.5 Robustness

* Several exceptions used to be thrown as bare terms rather than ISO
  `error/2` terms, so they printed as "Unknown message":
  `instantiation_error(Goal,Arg)` from `{}/1` (CLP(Q) only), from
  `entailed/1` (CLP(R) only) and from `bb_inf/3` (both), and the atom
  `unsatisfiable_ordering` from a contradictory `ordering/1`.  All of these
  now raise proper errors — `instantiation_error`, `type_error(var, T)` and
  `cyclic_ordering(Spec)` respectively — and Q and R agree on which.
  `context(_)` in 22 throws was corrected to `context(_,_)`.
* **Every solver call used to leave a choice point.**  Not one of `{}/1`,
  `inf/2`, `minimize/1`, `bb_inf/3`, `dump/3`, `entailed/1` or a plain
  unification was deterministic, so a program using CLP(Q,R) accumulated
  choice points it could never use and could not be last-call optimised.
  Four places leaked, each the same idiom — two clauses distinguished by a
  test that the first clause does not commit to, or a disjunction where an
  if-then-else was meant:

  | Predicate | File |
  | --- | --- |
  | `run/2` | `clpqr/geler.pl` |
  | `attr_unify_hook/2`, on `var(Y)` | `clpqr/geler.pl` |
  | `repair_p/5` | `nf_*.pl` |
  | `ordering/2`, on a one-element list | `clpqr/ordering.pl` |

  The same idiom in `bb_reoptimize/2`, `renormalize_log_one/3` and
  `pe2term/2` was latent rather than proven to leak, and was converted too.
  A sweep of forty scenarios across the whole interface is now deterministic;
  the only choice points left are the genuine second solution of an even
  root, `{X*X =:= 4}`.
* **Delayed goals that had already run stayed attached.**  When two
  variables that both carry delayed non-linear goals are unified, the goals
  of both run and the survivor's attribute is supposed to be cleared — the
  goals re-attach themselves if they are still non-linear.  `geler.pl` wrote

  ```prolog
  del_attr(Y, geler)          % the attribute is named clpqr_geler
  ```

  which deletes an attribute that does not exist, so nothing was ever
  cleared.  The spent goals stayed attached and `attach/3` prepended the new
  ones in front of them, so the conjunction grew quadratically in the number
  of such unifications.  Answers were unaffected — `run/2` skips a goal whose
  mutex is bound and `trans//1` leaves it out of the residual — but the cost
  was not:

  | Chained unifications | goals attached, before | after |
  | --- | --- | --- |
  | 50 | 1275 | 50 |
  | 100 | 5050 | 100 |
  | 200 | 20100 | 200 |
  | 400 | 80200 (2.4 s) | 400 (0.2 s) |

  This is the one place in the package that used a bare attribute name; every
  other `get_attr/3`, `put_attr/3` and `del_attr/2` says `clpqr_itf`,
  `clpqr_class` or `clpqr_geler`.
* The remaining stylistic oddity is
  `permission_error('mix CLP(Q) variables with','CLP(R) variables:',X)`,
  which spreads the message over the first two arguments of the formal
  term.  It prints correctly, so it is left alone.

### 14.6 Internal variables in answers

`{X+Y >= 1}` used to print as `{Y=1-X+_A, _A>=0}`, and `copy_term/3` returned
the same thing: the content of the tableau, slack variables and all.  The
cause was that `attribute_goals//1` projected onto *every* attributed variable
it could reach, so there was nothing left for the projection to eliminate and
`dump/3` ran as an expensive identity.  `dump/3` itself was unaffected,
because its caller states the targets; the same store dumps as `[x+y>=1]`.

The repair is the `aux` mark of §12.5.  It costs what projection costs: on a
chain of 80 `{A =< B}` constraints, `copy_term/3` went from 2 ms to 12 ms,
which is the price `dump/3` was already paying.

### 14.7 Still open

These are left alone deliberately, because fixing them is a decision about
the interface rather than a repair:

* **Division by zero fails silently.**  `nf_div/3` calls `zero_division/0`,
  which is `fail` with the author's comment `% raise_exception(_) ?`, so
  `{X =:= 1/0}` just fails where `is/2` would raise
  `evaluation_error(zero_divisor)`.  Either behaviour is defensible — "no
  solution" versus "undefined" — and changing it would break programs that
  rely on the failure.
* **`dump/3` requires unbound targets.**  `{X = 1}, dump([X],[y],L)` raises
  `uninstantiation_error(1)`, although `X` is exactly the kind of variable a
  user would want to dump.  What it should return instead (`[y = 1]`?
  `[]`?) is an interface decision.
* **`copy_term/3` does not project onto the copied term.**  It projects onto
  the user-level variables of the term's connected component, which is the
  most the `attribute_goals//1` interface allows; see §12.5.  Getting the
  projection onto a chosen list of variables requires `dump/3`.
* **`library(clpq)` and `library(clpr)` cannot both be imported into one
  module**, since they export the same names.  The manual's "It is allowed to
  use both libraries in one program" is true only with explicit module
  qualification.


## 15. Testing

The suite lives in `test_clpq.pl` and `test_clpr.pl` (one file per solver,
because the two libraries export the same predicate names and cannot be
imported into a single module).  Both are ordinary `library(plunit)` suites
registered with CMake via `test_libs(clpq clpr)`, so they run as
`ctest -R clpqr:`.

The suites are organised to follow this document:

| Unit | Covers |
| --- | --- |
| `syntax` | the constraint/expression grammar and its type errors |
| `nf` | normal-form construction, cancellation, powers |
| `equations` | `solve/1`: rank, implied values, class merging |
| `inequalities` | bound updates, strictness, the simplex |
| `disequations` | `=\=` and the `nonzero` mark |
| `nonlinear` | delaying, waking, isolation axioms |
| `entailment` | `entailed/1` |
| `optimisation` | `inf/2,4`, `sup/2,4`, `minimize/1`, `maximize/1` |
| `bb` | `bb_inf/3,4,5` |
| `projection` | `dump/3`, redundancy elimination, Fourier–Motzkin |
| `residuals` | `copy_term/3` and pending optimisations |
| `unify` | `attr_unify_hook/2`, aliasing, solver mixing |
| `internals` | paths the behavioural tests do not reach |
| `examples` | the mortgage, Fibonacci and Newton examples of the manual |
| `toplevel` | the answer-constraint printing in `clpq.pl` / `clpr.pl` |
| `known_issues` | the open items of §14.6, pinned to current behaviour |

Coverage is measured with `library(prolog_coverage)`:

```prolog
?- use_module(library(prolog_coverage)).
?- [library(clpq), library(clpr)].
?- coverage((test_clpq, test_clpr), [dir('cov'), annotate(true)]).
```

Clause coverage of the solver as of writing (299 CLP(Q) tests + 265 CLP(R)
tests):

| File | Clauses | % covered |
| --- | --- | --- |
| `clpqr/project.pl` | 36 | 94 |
| `clpq/store_q.pl`, `clpr/store_r.pl` | 38 | 92 |
| `clpq/nf_q.pl` | 206 | 90 |
| `clpqr/dump.pl` | 28 | 89 |
| `clpr/nf_r.pl` | 212 | 88 |
| `clpqr/class.pl` | 15 | 87 |
| `clpqr/itf.pl` | 14 | 86 |
| `clpr/bv_r.pl` | 186 | 84 |
| `clpr/bb_r.pl` | 25 | 84 |
| `clpq/bv_q.pl` | 185 | 84 |
| `clpq/fourmotz_q.pl` | 71 | 83 |
| `clpq/bb_q.pl` | 23 | 83 |
| `clpqr/geler.pl` | 15 | 80 |
| `clpr/fourmotz_r.pl` | 71 | 79 |
| `clpq/itf_q.pl` | 34 | 77 |
| `clpqr/ordering.pl` | 39 | 74 |
| `clpr/itf_r.pl` | 34 | 74 |
| `clpq/ineq_q.pl` | 100 | 70 |
| `clpr/ineq_r.pl` | 100 | 69 |
| `clpqr/redund.pl` | 34 | 59 |
| `clpq.pl`, `clpr.pl` | 10 | 50 |

What is left uncovered is, in decreasing order of size:

1. Bound-update clauses in `ineq_*.pl` and redundancy clauses in
   `clpqr/redund.pl` that only trigger for particular combinations of active
   bound and strictness (`t_L`, `t_Lu`, `t_lU`, `t_U`).  These are reachable
   in principle; each needs a simplex state that is awkward to construct from
   the outside.
2. `narrow_u/3`, `narrow_l/3` and the `inc_step_2*` clauses in `bv_*.pl`,
   which need a basic variable whose optimum lies strictly inside its bound.
3. The `sandbox:safe_primitive/1` facts, which are declarations rather than
   executable clauses, and the `prolog:message//1` clauses of `clpq.pl` and
   `clpr.pl` that only the interactive toplevel reaches.

### Reading the annotated sources

Clause percentages hide two things this solver is full of: branches inside a
clause that are never taken, and clauses that are entered but never exit.
`library(prolog_coverage)` annotates *call sites* as well as clause heads, so
both are visible:

```prolog
?- coverage((test_clpq, test_clpr),
            [ dir(cov), annotate(true), line_numbers(true), color(false),
              roots(['.../library/ext/clpqr'])
            ]).
```

| Mark | Meaning |
| --- | --- |
| `###` | never executed |
| `++N` | entered N times, always succeeded |
| `--N` | entered N times, **never** succeeded |
| `+N-M` | succeeded N times, failed M |
| `+N*M` | entered N times, succeeded M |
| `---` | call site never reached |

Two cautions.  The source column is *per file* — it is `6 + max annotation
width` — so align against the original file rather than assuming a fixed
offset.  And `++0` on an inlined built-in is an artifact, not a gap: in

```
 207 +140-6   submit_eq_b(v(_,[X^P])) :-
 208 +144-2   	var(X),
 209 ++0      	P > 0,
 211 +140-3   	X = 0.
```

the guard cannot really have run zero times when the body below it succeeded
140 times.  Arithmetic comparisons compile to VM instructions with no call
port.  Filter them out before drawing conclusions.

What the annotations showed, over and above the clause-level gaps:

* `add_linear_11h/6` — the coefficient-cancellation arm had never run, in 289
  entries.  Cancellation is precisely where a linear solver goes wrong, and
  reaching it needs a three-monomial constraint, so that `log_deref/4` splits
  and merges the halves.
* `nf2sum/3` and `f02t/2` — the arms that render an answer whose leading
  coefficient is not 1.
* `pmerge_case/9` — exponent cancellation, `X * (1/X)`.
* `wait_linear_retry/3` — the arm that re-delays an optimisation whose
  expression is *still* non-linear when it wakes.
* `bb_reoptimize/2` — the clause for an objective that is already ground.
* The solver-mixing guard is repeated in all eight `ineq_one_*` entry points
  and only two of them were exercised.
* `renormalize_log_one/3` — the arm for a class variable already bound to a
  number, which needs an `ordering/1` to make `arrange/2` renormalise.

Tests for all of these are in the `internals` units; they took the number of
unreached call sites, excluding inlined built-ins, from 113 down to 93.

The clauses that are *entered but never succeed* are mostly by design — the
`var(X), !, fail` guards, `negate_l/4` and `negate_u/4` in `clpqr/redund.pl`,
which are documented to fail when a bound is not redundant, and first clauses
that exist only to test their head arguments.  The ones worth knowing about
are `redundant/3` for `t_L` and `t_U`: in the whole suite an *active* bound
was never found redundant, so those two clauses have 20 entries and no
successes.


## 16. Working on this code

What the repairs in §14 taught, in the order it is likely to bite.

### Every fix is two fixes

`clpq/` and `clpr/` are not generated from a common source; they are
independent copies that have drifted for twenty years.  A defect in one is
usually, but not always, present in the other, and the *shape* of the drift
is not predictable:

* `{}/1` on an unbound argument raised a malformed exception in CLP(Q) and a
  well-formed one in CLP(R); for `entailed/1` it was the other way round.
* CLP(R) grew four `submit_eq_c1/3` clauses for root extraction long after
  the port; CLP(Q) never got them.
* `bb_better_bound/1` read its global with `nb_current/2` in Q and
  `catch(nb_getval(...),_,true)` in R, with subtly different behaviour when
  no incumbent exists yet.

So: after changing one, diff the two.  Normalising the module suffixes and
`rdiv`/`/` makes the diff readable:

```bash
sed -e 's/_q\b/_X/g; s/clpq/clpZ/g; s/ rdiv / \/ /g' clpq/bv_q.pl >/tmp/q
sed -e 's/_r\b/_X/g; s/clpr/clpZ/g'                   clpr/bv_r.pl >/tmp/r
diff -u /tmp/q /tmp/r
```

### The build caches `.qlf`, and the staleness lies to you

The library is installed into `build/home/library/ext/clpqr/` as symlinks
with `.qlf` files beside them.  When a change appears to have no effect, or
produces something impossible like

```
Warning: Local definition of clpr:ordering/1 overrides weak import from clpqr_ordering
Warning: Redefined static procedure clpqr_ordering:ordering/1
ERROR:   existence_error(procedure, clpr:ordering/1)
```

the `.qlf` files are stale.  Run `swipl -Dsource` to bypass them, or delete
them and let `ninja` rebuild.  Both of those warnings vanished the moment the
caches went; nothing was wrong with the source.

### Predicates are called for side effects unrelated to their name

`dump/3` called `ordering(Target)` on its target list.  It looks like a
presentation choice, and removing it is the obvious way to stop it
overriding the user's `ordering/1` — but it also *interned* the target
variables, which is what lets `nonlin_crux/2` reach their delayed goals.
Removing it silently emptied the residual goals of every purely non-linear
store.  The replacement, `intern_vars/1`, keeps the side effect and drops the
edges.

Expect more of this.  When removing a call here, check what it does to the
attributes, not just what its name says.

### `term_attvars/2` reaches through attributes

`attribute_goals//1` calls `term_attvars(V, Vs)`, and `Vs` contains not only
the constrained variables but the *class identity variable*, which carries
only a `clpqr_class` attribute and belongs to no solver.  Anything walking
that list has to tolerate variables it cannot classify.  That is why
`intern_vars/1` is total where `join_class/3` fails, and why `dump/3` used to
fail outright on an unconstrained target.

### The manual is the specification; use its worked examples

Reading the code cannot tell you which way round `arrange_pivot/1` should
compare, because either direction is internally consistent.  TR-95-09's
"Variable Ordering" section settles it in one sentence — *"Suppose that
instead of B, you want Mp to be the defined variable"* — and its four worked
mortgage answers, reproduced verbatim in `test_clpr.pl`, are what finally
showed that the port had the direction inverted *and* that `dump/3` was
overriding the user.  When changing anything about projection, run those
four first.

### Exact arithmetic is a feature of CLP(Q), not an obstacle

SWI-Prolog's `(**)/2` returns an exact rational whenever the root is
rational and a float otherwise, so `rational/1` on the result is a decision
procedure for "does this root exist in Q":

```prolog
exact_root(P,V,R) :-
	catch(R is V**(1 rdiv P), _, fail),
	rational(R),
	R**P =:= V.
```

That is the whole of CLP(Q)'s root extraction.  Conversely, `rational/1` on
a *float* is almost always a bug — it gives the exact value of the
approximation, which is how `{8 =:= 2^Y}` came to answer
`18729944304496076r6243314768165359`.  `rationalize/1` is nearly always what
was meant.

### State that must survive backtracking but not the call

Three places need this: `dump/3`, `inf/2` and `bb_inf/3`.  The idiom is a
mutable term local to the call, updated with `nb_setarg/3` and read after a
failure-driven loop:

```prolog
	State = state(none),
	(   ...,
	    nb_setarg(1, State, Value),
	    fail
	;   arg(1, State, Value)
	)
```

Both `bb_inf/3` and `inf/2` used global variables instead — `prov_opt` and
`inf` — which share one namespace with the calling program, so they
destroyed a caller's variable of that name.  `inf/2`'s was only noticed while
writing this section, which is a fair illustration of how the two copies and
the two optimisers hide the same defect from each other.

### Writing tests here

* **The result of a test goes in the second argument of `test/2`, not in an
  `assertion/1` in the body.**  plunit expands a bare `==`, `=`, `=@=` or
  `=:=` into a `true/1` option, `join_true_options/2` merges several of them,
  and a failure then names the variable:

  ```prolog
  test(negative_coefficients, [X == 2, Y == 1]) :-
      {-X - Y =:= -3, X - Y =:= 1}.
  ```
  ```
  wrong answer for Y (compared using ==)
      Expected: 77
      Got:      1
  ```

  Anything that is not a comparison goes in `true(Goal)` — that is how the
  CLP(R) suite uses its tolerant `near/2`.  A test mode (`fail`, `error(_)`,
  `throws(_)`, `all(_)`, `set(_)`) cannot be combined with `true/1`, so those
  tests carry no result options.
* `assertion/1` is then only for *intermediate* state — that a variable is
  still unbound before the next constraint, that something is not yet
  entailed.  Those are steps in the scenario, not the answer.
* `assertion/1` does not keep bindings — it is `\+ \+ Goal`.  So match the
  shape of an answer with plain unification in the body and check the numbers
  in the head:

  ```prolog
  test(bounds, [true(near(L, 1.0)), true(near(U, 3.0))]) :-
      {X >= 1, X =< 3},
      dump([X], [x], C),
      C = [x >= L, x =< U].
  ```
* In CLP(R), compare with a tolerance (`near/2` in `test_clpr.pl`), never
  `==`.  `{X =:= 3}` gives `3.0`, and `1/3` is not `0.3333333333333333` under
  `==`.
* The solver is deterministic, so a test that needs `[nondet]` is a finding,
  not a formality: the only legitimate case is the two roots of an even
  power.  plunit only *warns* about an unexpected choice point, so where the
  determinism is the point of the test, check it with `det_call/2` (in both
  suites) and put `Det == true` in the head — that turns the warning into a
  failure.  The `*` answer at the toplevel, or `A` in the debugger, names
  the clause that left the choice point.
* `_X` used twice in one clause draws a "singleton-marked variable appears
  more than once" warning.  Use a normal name; two occurrences are not a
  singleton.
