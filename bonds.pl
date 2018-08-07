:- module(bonds,
          [balance/4,
           bond/3,
           lewis_compare/3,
           lewis_graph/2
          ]).

:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(valencies).

:- dynamic lewis_group/3.

:- initialization retractall(lewis_group(_, _, _)), init_lewis_groups.

%% rep(Element, Num, ElList)
rep(_, 0, []) :- !.
rep(Element, Num, [Element|PrevList]) :-
   succ(Prev, Num),
   rep(Element, Prev, PrevList).

compound_rep([], []) :- !.
compound_rep([atom(Elem, 1)|Tail], [Elem|ElList1]) :- !,
   compound_rep(Tail, ElList1).
compound_rep([atom(Elem, Num)|Tail], [Elem|ElList1]) :-
   succ(Prev, Num),
   compound_rep([atom(Elem,Prev)|Tail], ElList1).

lewis_compare(h, X, l) :- X \= h, !.
lewis_compare(El1, El2, Comp) :-
   periodic_line_group(El1, Line, Group1),
   periodic_line_group(El2, Line, Group2), !,
   (  Group1 < Group2
   -> Comp = g
   ;  Group1 > Group2
   -> Comp = l
   ;  Comp = n
   ).
lewis_compare(El1, El2, l) :-
   periodic_line_group(El1, Line1, Group1),
   periodic_line_group(El2, Line2, Group2),
   Line1 < Line2,
   Group2 =< Group1, !.
lewis_compare(El1, El2, g) :- lewis_compare(El2, El1, l), !. 
lewis_compare(_, _, n).

lewis_graph(Els, G) :-
   lewis_vertices(Els, [], V),
   vertices_edges_to_ugraph([], V, G).

lewis_vertices([], L, L) :- !.
lewis_vertices([E|T], L0, L) :-
   lewis_vertices2(E, T, L0, L1),
   lewis_vertices(T, L1, L).

lewis_vertices2(_, [], L, L) :- !.
lewis_vertices2(E0, [E1|T], L0, L) :-
   lewis_compare(E0, E1, Comp),
   (  Comp = g
   -> L1=[E0 - E1 | L0]
   ;  Comp = l
   -> L1 = [E1 - E0 | L0]
   ;  L1 = L0
   ),
   lewis_vertices2(E0, T, L1, L).

% lewis_group(+L0, -L, -Group) is nondet.
% L0 must be an ordered list of elements
% Select groups of elements from L0 forming L and Group.

%experimental
assert_lewis_group(Term, (-A)^_-(-B)^_) :- !,
   assert_lewis_group2(Term, A, B).
assert_lewis_group(Term, (-A)^_-B^_) :- !,
   assert_lewis_group2(Term, A, B).
assert_lewis_group(Term, A^_-(-B)^_) :- !,
   assert_lewis_group2(Term, A, B).
assert_lewis_group(Term, A^_-B^_) :- !,
   assert_lewis_group2(Term, A, B).
assert_lewis_group(Term, A^AC-B) :- !,
   assert_lewis_group(Term, A^AC-B^0).
assert_lewis_group(Term, A-B^BC) :- !,
   assert_lewis_group(Term, A^0-B^BC).
assert_lewis_group(Term, A-B) :- !,
   assert_lewis_group(Term, A^0-B^0).
assert_lewis_group(Term, (-A)^_) :- !,
   assert_lewis_group2(Term, A).
assert_lewis_group(Term, A^_) :- !,
   assert_lewis_group2(Term, A).
assert_lewis_group(Term, El) :-
   atom(El), !,
   assertz((lewis_group(L0, L, Term) :- selectchk(El, L0, L))).
assert_lewis_group(Term, A-B) :- !,
   assert_lewis_group(Term, A^0-B^0).
assert_lewis_group(r(N)) :-
   assertz((lewis_group(L0, L, r(N)) :- select_group(r(N), L0, L))).
assert_lewis_group(x(Group)) :-
   assert_lewis_group(x(Group), Group).

assert_lewis_group2(Term, A) :-
   assertz((lewis_group(L0, L, Term) :-
               select_group(A, L0, L))).
assert_lewis_group2(Term, A, B) :-
   assertz((lewis_group(L0, L, Term) :-
               select_group(A, L0, L1),
               select_group(B, L1, L))).

select_group(r(N), L0, L) :- % alkyl
   selectchk(h, L0, L1),
   alkyl_minus(L1, 0, L, N),
   N > 0.
select_group(El, L0, L) :-
   atom(El),
   selectchk(El, L0, L).

init_lewis_groups :-
   assert_lewis_group(r(_)), % r
   forall( % x
          member(G,
                 [h, f, cl, br, i,
                  -o-h, -o-r(_), -s-r(_), -se-r(_), (-o)^(-1), (-s)^(-1)
                 ]),
          assert_lewis_group(x(G))
         ).


% lewis_group(L0, L, x(E)) :-
%    select(E, L0, L), memberchk(E, [h, f, cl, br, i]).
% lewis_group(L0, L, x(compound([o^(-1)-h]))) :-  % -OH 
%    selectchk(o, L0, L1),
%    selectchk(h, L1, L).
% lewis_group(L0, L, x(compound([E^(-1)-r(N)]))) :- % -OR, -SR, -SeR
%    (  selectchk(o, L0, L1)
%    ;  selectchk(s, L0, L1)
%    ;  selectchk(se, L0, L1)
%    ),
%    lewis_group(L1, L, r(N)).
% lewis_group(L0, L, x(E^(-2))) :-  % -O-, -S-
%    (  selectchk(o, L0, L1)
%    ;  selectchk(s, L0, L1)
%    ).
% lewis_group(L0, L, m(E)) :-
%    select(E, L0, L), is_metal(E).
% lewis_group(L0, L, r(N)) :- % alkyl
%    selectchk(h, L0, L1),
%    alkyl_minus(L1, 0, L, N),
%    N > 0.

alkyl_minus(L0, N0, L, N) :- % removes CH2 component from L0 N times
   selectchk(c, L0, L1),
   selectchk(h, L1, L2),
   selectchk(h, L2, L3),
   succ(N0, N1),
   alkyl_minus(L3, N1, L, N).
alkyl_minus(L, N, L, N).
   
bond(E1, E2, ionic(N1*E1^V1, N2*E2^V2)) :-
   valency(E1, V1, G1),
   valency(E2, V2, G2),
   permutation([G1, G2], [group(s, _), group(p, 5)]),
   V11 is abs(V1),
   V21 is abs(V2),
   balance(V11, V21, N1, N2).

balance(V1, V2, N1, N2) :-
   lcm(V1, V2, VV),
   N1 is VV / V1,
   N2 is VV / V2.

lcm(A, B, C) :-
   C is A * B / gcd(A, B).
