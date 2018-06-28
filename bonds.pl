:- module(bonds,
          [balance/4,
           bond/3
          ]).

:- use_module(library(lists)).
:- use_module(valencies).

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
