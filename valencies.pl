:- module(valencies,
          [group/2,
           is_metal/1,
           periodic_line_group/3,
           valency/3
          ]).

:- use_module(shells).
:- use_module(elements).

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

group_number_by_shells([shell(s, 1, 1)|_], 1) :- !.
group_number_by_shells([shell(s, 1, 2)|_], 18) :- !.
group_number_by_shells([shell(s, _, I)|_], I) :- !.
group_number_by_shells([shell(p, _, I1)|_], I) :- !,
   I is I1 + 12.
group_number_by_shells([shell(d, _, I1), shell(s, _, I2)|_], I) :- !,
   I is I1 + I2.
group_number_by_shells([shell(f, _, _)|_], 3).

line_number_by_shells([shell(p, Line, _)|_], Line) :- !.
line_number_by_shells([shell(s, Line, _)|_], Line) :- !.
line_number_by_shells([shell(d, Line1, _)|_], Line) :- !,
   succ(Line1, Line).
line_number_by_shells([shell(f, Line1, _)|_], Line) :-
   Line is Line1 + 2.

periodic_line_group(Element, Line, Group) :-
   to_shells(Element, Shells),
   group_number_by_shells(Shells, Group),
   line_number_by_shells(Shells, Line).

%last_metal(Line, Group)
last_metal(1, 0).
last_metal(2, 2).
last_metal(3, 13).
last_metal(4, 13).
last_metal(5, 14).
last_metal(6, 16).
last_metal(7, 16).

is_metal(Element) :-
   periodic_line_group(Element, Line, Group),
   last_metal(Line, LastMetalGroup), !,
   Group =< LastMetalGroup.

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

