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
%compound("nh3", compound(p(n, h, h, h), [])).
compound("ch4", compound(p(c, h, h, h, h), [1-2, 1-3, 1-4, 1-5])).
compound("h2so4", compound(p(h, h, s, o, o, o, o), [3=4, 3=5, 3-6, 3-7, 6-1, 7-2)).
compound("hclo4", compound(p(h, cl, o, o, o, o), [2=3, 2=4, 2=5, 2-6, 1-6])).
%compound("h3po4", compound(p(h, h, h, p, o, o, o, o), [])).
%compound("hno3", ).
compound("h2o2", compound(p(h, h, o, o), [1-3, 2-4, 3-4])).        
compound("ch3cooh", compound(p(c, h, h, h, c, o, o, h), [1-2, 1-3, 1-4, 1-5, 5=6, 5-7, 7-8])).
