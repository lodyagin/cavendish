go :-
   (	get_compound(C),
   	assert_compound(C),
   	calc_distances,
   	show_distances,
   	fail
   ;
        true
   ).



%%% Search element sequence with ambigousity

?- numlist(1, 118, L), random_sublist(L, [nondet, length(3)], _, L1), findall(E, (member(EN, L1), element(E, EN)), Es), atomic_list_concat(Es, A), atom_codes(A, CS), findall(Element, phrase(read_element_sequence(Element), CS), Elements), length(Elements, ELen), ELen \== 1.

%%%