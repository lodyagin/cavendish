:- module(valencies,
          [group/2,
           valency/3
          ]).

:- use_module(shells).

valency(Element, Valency, Group) :-
   var(Element), !,
   valency_by_shell(_, Group, Valency, EN),
   element(Element, EN).
valency(Element, Valency, Group) :-
   to_shells(Element, Shells),
   valency_by_shell(Shells, Group, Valency, _).

group(Element, Group) :-
   var(Element), !,
   group_by_shell(_, Group, EN),
   element(Element, EN).
group(Element, Group) :-
   to_shells(Element, Shells),
   group_by_shell(Shells, Group, _).

valency_by_shell(Shells, Group, Valency, EN) :-
   Group = group(s, Valency),
   group_by_shell(Shells, Group, EN).
valency_by_shell(Shells, Group, Valency, EN) :-
   Group = group(p, I),
   group_by_shell(Shells, Group, EN),
   I =:= 5,
   Valency is I - 6.

group_by_shell([shell(s, 1, 1)], h, 1).  % H
group_by_shell([shell(s, 1, 2)], 0, 2).  % Ne
group_by_shell(Shells, 0, EN) :-         % Other nobles
   Shells = [shell(p, _, 6)|_],
   element_shells(EN, Shells).
group_by_shell(Shells, group(T, I), EN) :- % The rest
   Shells = [shell(T, N, I)|_],
   element_shells(EN, Shells),
   \+ ((T = s, N = 1) ; (T = p, I = 6)).

