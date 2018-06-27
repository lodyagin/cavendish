:- module(valencies,
          [group/3,
           valency/2
          ]).

:- use_module(shells).

valency(Element, Valency) :-
   var(Element), !,
   valency_by_shell(_, Valency, EN),
   element(Element, EN).
valency(Element, Valency) :-
   to_shells(Element, Shells),
   valency_by_shell(Shells, Valency, _).

% valency_by_shell(?Shells, ?Valency, ?EN) is nondet.
valency_by_shell(Shells, Valency, EN) :-
   group(Shells, group(s, Valency), EN).
valency_by_shell(Shells, Valency, EN) :-
   group(Shells, group(p, I), EN),
   I =:= 5,
   Valency is 6 - I.


group([shell(s, 1, 2)], 0, 2).
group(Shells, 0, EN) :-
   Shells = [shell(p, _, 6)|_],
   element_shells(EN, Shells).
group(Shells, group(T, I), EN) :-
   Shells = [shell(T, N, I)|_],
   element_shells(EN, Shells),
   \+ ((T = s, N = 1, I = 2) ; (T = p, I = 6)).

