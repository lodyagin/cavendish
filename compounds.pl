:- module(compounds,
          [read_compound//1
          ]).

:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(elements).

read_compound(Atoms) -->
   read_compound_part(Head),
   read_compound_tail(Head, Atoms).

% reads an elements sequence up to an index
read_compound_part(Atoms) -->
    read_element_variations(ElVars), !,
    { member(Elements, ElVars), % for each variation
      findall(atom(Element, 1), (member(Element, Elements)), Atoms)
    }.
    
read_compound_tail(Head0, Atoms) -->
   read_index(N), !,
   { append(Head1, [atom(Element, _)], Head0) },
   read_compound(Tail),
   { append(Head1, [atom(Element, N)|Tail], Atoms) }.
read_compound_tail(Atoms, Atoms) -->
   [].

read_element_variations(ElVars) -->
   read_alpha(Codes),
   { findall(Elements,
             phrase(read_element_sequence(Elements), Codes),
             ElVars)
   }.

read_element_sequence([Element|T]) -->
   read_element(Element),
   read_element_sequence(T).
read_element_sequence([]) -->
   [].

read_element(Element) -->
   read_element2(Element).
read_element(Element) -->
   read_element1(Element).

% bi is red as Bi
read_element2(Element) -->
   [C10], [C20],
   { code_type(C1, to_lower(C10)),
     code_type(C2, to_lower(C20)),
     atom_codes(Element, [C1, C2]),
     element(Element, _, _)
   }.

% bi is red as B
read_element1(Element) -->
   [C10], 
   { code_type(C1, to_lower(C10)),
     atom_codes(Element, [C1]),
     element(Element, _, _)
   }.

read_index(N) -->
   digits(Ds),
   {  Ds = [_|_],
      number_codes(N, Ds), N > 0
   }.

read_alpha([C|T]) -->
   [C],
   { code_type(C, alpha) }, !,
   read_alpha(T).
read_alpha([]) -->
   [].


