:- module(bonds,
          [balance/4,
           bond/3,
           lewis_compare/3,
           lewis_graph/2
          ]).

:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(valencies).

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

lewis_group([E], m(E)) :-
   is_metal(E), !.

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
