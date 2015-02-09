/*
This file reads in a prolog file
and outputs a clp(constraint logic programming) file
*/
:- use_module(library(clpfd)).
%now this doesn't work inside a prolog file.

main :-
	write('plz enter file name and end with period.\n'),
	read(X), open(X, read, S1),
	open('clpfile.pl', write, S2),
	write(S2, ':- use_module(library(clpfd)).\n'),
	read_file(S1, S2, _), close(S1), close(S2).

read_file(S1, _, []) :-
	at_end_of_stream(S1).

read_file(S1, S2, [H|T]) :-
	\+ at_end_of_stream(S1),
	read_clause(S1, H, [variable_names(_)]), H =.. L,
	process(L, NL, VL), sort(VL, NVL),
	insert_def(NNL, NL, NVL), NNH =.. NNL,
	write(S2, NNH), write(S2, '.\n'),
	read_file(S1, S2, T).

/*
there are three cases
case1: variable list (vl) is empty
case2: we need to insert the vl into a rule
case3: we 'need' to insert the vl into a fact
notice: we only insert the vl into a rule
*/
insert_def(L, L, []).
insert_def([:-|NT], [:-|T], VL) :-
	insert_def2(NT, T, VL).
insert_def(L, L, _).

/*
here NH stands for the head of the rule
this predicate invokes insert_def3 which deals with the body of the rule
*/
insert_def2(NT, [H|T], VL) :-
	insert_def3(T2, T, VL),
	%write(T2), write('.\n'),
	H2 =.. T2, append([H], [H2], NT).
	%write(H2), write('.\n').

insert_def3([''|T], [H|_], VL) :-
%the trick is the '' above
%here _ is actually empty list
	compound(H), H =.. L,
	nth0(0, L, X),
	X = ',',
	L = [,|L2],
	%write('case1.\n'),
	insert_def4(T, L2, VL).

insert_def3([''|T], [H|_], VL) :-
	compound(H),
	%H =.. L,
	%nth0(0, L, X),
	%X =\= ',',
	%write('case2.\n'),
	insert_def4(T, [H], VL).

insert_def4(L, L, []).
insert_def4(NT, L, [VH|VL]) :-
	NVH =.. [in, VH, inf..sup],
	append([NVH], T, NT),
	insert_def4(T, L, VL).

process([], [], []).
process([H|T], [H|NT], [H|VT]) :-
	\+ compound(H), var(H),
	process(T, NT, VT).
process([H|T], [H1|NT], VL) :-
	\+ compound(H), nonvar(H), transfer(H, H1),
	process(T, NT, VL).
process([H|T], [NH|NT], VL) :-
	compound(H), H =.. L,
	append(VL1, VL2, VL),
	process(L, NL, VL1),
	NH =.. NL, process(T, NT, VL2).

transfer(=, #=).
transfer(<, #<).
transfer(>, #>).
transfer(\=, #\=).
transfer(=<, #=<).
transfer(>=, #>=).
transfer(=:=, #=:=).
transfer(=/=, #=/=).
transfer(is, #=).
transfer(X, X).
