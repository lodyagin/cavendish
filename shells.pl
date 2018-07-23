:- module(shells,
          [element_shells/2,
           next_element/2,
           show_all_elements/0,
           show_element/1,
           to_shells/2
          ]).

:- use_module(elements).

:- dynamic next_shell/2, element_shells/2.

% shell populating sequence
shell_seq(s(1)).  %  1s 
shell_seq(s(2)).  %  2s 2p
shell_seq(p(2)).  %  3s 3p 3d
shell_seq(s(3)).  %  4s 4p 4d 4f
shell_seq(p(3)).  %  5s 5p 5d 5f ..
shell_seq(s(4)).  %  6s 6p 6d 6f ..
shell_seq(d(3)).  %  7s 7p 7d 7f ..
shell_seq(p(4)).  %
shell_seq(s(5)).
shell_seq(d(4)).
shell_seq(p(5)).
shell_seq(s(6)).
shell_seq(f(4)).
shell_seq(d(5)).
shell_seq(p(6)).
shell_seq(s(7)).
shell_seq(f(5)).
shell_seq(d(6)).
shell_seq(p(7)).

% shell_seq/1 -> next_shell/2
build_shell_seq :-
   findall(S, shell_seq(S), SS),
   retractall(next_shell(_, _)),
   assert_shell_seq(SS).

assert_shell_seq([AP, BP|T]) :- !,
   AP =.. [A, AN],
   BP =.. [B, BN],
   assertz(next_shell(shell(A, AN), shell(B, BN))),
   assert_shell_seq([BP|T]).
assert_shell_seq([_]).

n_electrons_max(s, 2).
n_electrons_max(p, 6).
n_electrons_max(d, 10).
n_electrons_max(f, 14).

next_element([shell(ST, SN, EN)|T], Element) :-
   n_electrons_max(ST, ENMax), !,
   (  EN < ENMax
   -> succ(EN, EN1),
      Element = [shell(ST, SN, EN1)|T]
   ;
      next_shell(shell(ST, SN), shell(ST1, SN1)), !,
      Element = [shell(ST1, SN1, 1), shell(ST, SN, ENMax)|T]
   ).

   
%count_electrons(K0, [shell(SN, ST)|S1], S, K) :-
%   n_electrons(ST, NN),
                                %   K is K0 + NN,

show_all_elements :-
   forall(element_shells(_, S), show_element(S)).

show_element(E) :-
   show_element(E, 0).

show_element([shell(ST, SN, EN)|T], ElectronCnt) :- !,
   format("~d~a:~d ", [SN, ST, EN]),
   ElectronCnt1 is ElectronCnt + EN,
   show_element(T, ElectronCnt1).
show_element([], ElectronCnt) :-
   element(Element, ElectronCnt),
   format("(~a)~n", [Element]).

assert_all_elements :-
   retractall(element_shells(_, _)),
   ignore(assert_all_elements([shell(s, 1, 1)], 1)).

assert_all_elements(Shells, EN) :-
   assertz(element_shells(EN, Shells)),
   next_element(Shells, Shells1),
   succ(EN, EN1),
   assert_all_elements(Shells1, EN1).

to_shells(L, L) :-
   L = [_|_], !.
to_shells(El, L) :-
   atom(El), !,
   element(El, EN),
   element_shells(EN, L), !.
to_shells(EN, L) :-
   integer(EN), !,
   element_shells(EN, L), !.

:- initialization((build_shell_seq, assert_all_elements)).