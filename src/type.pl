/**
  * this is a prolog file
  */

nat(o).
nat(s(N)) :- nat(N).

bool(true).
bool(false).

type(A, nat) :- nat(A), !.
type(A, bool) :- bool(A), !.
type(id, arrow(A, A)) :- !.
type(add, arrow(nat, arrow(nat, nat))) :- !.
type(not, arrow(bool, bool)) :- !.
type(const, arrow(A, arrow(_, A))).

type(app(F, P), R) :-
  type(F, arrow(A, B)),
  var(A),
  type(P, A),
  R = B,
  !.

type(app(F, P), R) :-
  type(F, arrow(A, B)),
  atom(A),
  get_type(types, A, TypeA),
  % type already exists in environment
  !,
  type(P, TypeA),
  type_in_env(B, R),
  !.

type(app(F, P), R) :-
  type(F, arrow(A, B)),
  atom(A),
  % type doesn't exist in environment
  type(P, TypeP),
  add_type(types, A, TypeP),
  type_in_env(B, R),
  !.

init_state(Name) :-
  empty_assoc(A),
  nb_setval(Name, A).

add_type(SName, TermName, Type) :-
  nb_getval(SName, State),
  get_assoc(TermName, State, Type).

type_in_env(T, T1) :- get_type(types, T, T1).
type_in_env(T, T).

type()
