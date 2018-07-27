:- module(shells,
          [element_shells/2,
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

werid_shell(24, transfer(s, d, 1)) :- !. % Cr
werid_shell(29, transfer(s, d, 1)) :- !. % Cu
werid_shell(41, transfer(s, d, 1)) :- !. % Nb
werid_shell(42, transfer(s, d, 1)) :- !. % Mo
werid_shell(44, transfer(s, d, 1)) :- !. % Ru
werid_shell(45, transfer(s, d, 1)) :- !. % Rh
werid_shell(46, transfer(s, d, 2)) :- !. % Pd
werid_shell(47, transfer(s, d, 1)) :- !. % Ag
werid_shell(57, transfer(f, d, 1)) :- !. % La
werid_shell(58, transfer(f, d, 1)) :- !. % Ce
werid_shell(64, transfer(f, d, 1)) :- !. % Gd
werid_shell(78, transfer(s, d, 1)) :- !. % Pt
werid_shell(79, transfer(s, d, 1)) :- !. % Au
werid_shell(89, transfer(f, d, 1)) :- !. % Ac
werid_shell(90, transfer(f, d, 2)) :- !. % Th
werid_shell(91, transfer(f, d, 1)) :- !. % Pa
werid_shell(92, transfer(f, d, 1)) :- !. % U
werid_shell(93, transfer(f, d, 1)) :- !. % Np
werid_shell(96, transfer(f, d, 1)) :- !. % Cm
werid_shell(103, transfer(p, p2, 1)) :- !. % Lr
werid_shell(110, transfer(f, p2, 1)) :- !. % Ds

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

next_element(ElementNumber, Element0, Element) :-
   succ(ElementNumber1, ElementNumber),
   werid_shells_correct(ElementNumber1, Element1, Element0),
   Element1 = [shell(ST, SN, EN)|T],
   n_electrons_max(ST, ENMax), !,
   (  EN < ENMax
   -> succ(EN, EN1),
      Element2 = [shell(ST, SN, EN1)|T]
   ;
      next_shell(shell(ST, SN), shell(ST1, SN1)), !,
      Element2 = [shell(ST1, SN1, 1), shell(ST, SN, ENMax)|T]
   ),
   werid_shells_correct(ElementNumber, Element2, Element).
 

werid_shells_correct(EN, Shell1, Shell) :-
   werid_shell(EN, transfer(s, d, N)), !,
   (  Shell1 = [shell(d, DSN, DEN1), shell(s, SSN, SEN1)|T],
      Shell  = [shell(d, DSN, DEN), shell(s, SSN, SEN)|T]
   ;  Shell1 = [shell(d, DSN, DEN1), F, shell(s, SSN, SEN1)|T],
      Shell  = [shell(d, DSN, DEN), F, shell(s, SSN, SEN)|T]
   ),
   plus(DEN1, N, DEN),
   plus(SEN, N, SEN1).
werid_shells_correct(EN,
                     [shell(f, FSN, FEN1)|T],
                     [shell(d, DSN, N), shell(f, FSN, FEN)|T]
                    ) :-
   werid_shell(EN, transfer(f, d, N)), !,
   succ(FSN, DSN),
   plus(FEN, N, FEN1).
werid_shells_correct(EN,
                     [D, F, S, shell(p, PSN, PEN1)|T],
                     [shell(p, P2SN, N), D, F, S, shell(p, PSN, PEN)|T]
                    ) :-
   werid_shell(EN, transfer(p, p2, N)), !,
   succ(PSN, P2SN),
   plus(PEN, N, PEN1).
werid_shells_correct(EN,
                     [D, shell(f, FSN, FEN1)|T],
                     [shell(p, P2SN, N), D, shell(f, FSN, FEN)|T]
                    ) :-
   werid_shell(EN, transfer(f, p2, N)), !,
   plus(FSN, 2, P2SN),
   plus(FEN, N, FEN1).
werid_shells_correct(_, Shell, Shell).

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
   succ(EN, EN1),
   next_element(EN1, Shells, Shells1),
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