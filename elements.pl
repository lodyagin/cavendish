:- module(elements,
          [element/3,
           valency/2,
           reassert_valencies/0
          ]).

:- dynamic valency/2.

%connect_elements(E1, E2, bond(atom(1, E1, Cha1), atom(2, E1, Cha2))) :-
%   valency(E1, V1),
%   integer(V1), % no covalency yet
%   valency(E2, V2),
%   integer(V2),
%   Cha1 is V1 
                
reassert_valencies :-
   retractall(valency(_, _)),
   (  element(Element, _, ValList),
      member(Val, ValList),
      assertz(valency(Element, Val)),
      fail
   ;
      true
   ).
    
element(h, 1, [+1, -1, cov(1), hyd]).
element(he, 2, []).
element(li, 3, [+1, cov(1)]).
element(be, 4, [+2, cov(1), cov(2), cov(3), cov(4)]).
element(b, 5, [cov(1)]).
element(c, 6, [cov(4), +2]).
element(n, 7, [-3, cov(1), cov(2), cov(3), cov(4)]).
element(o, 8, [-2]).
element(f, 9, [-1]).
element(ne, 10, []).
element(na, 11, [+1, cov(1)]).
element(mg, 12, [+2]).
element(al, 13, [+3]).
element(si, 14, [cov(4), cov(5), cov(6)]).
element(p, 15, [-3]).
element(s, 16, [-2]).
element(cl, 17, [-1]).
element(ar, 18, []).
element(k, 19, [+1, cov(1)]).
element(ca, 20, [+2]).
element(sc, 21, [+3]).
element(ti, 22, [+2, +3, +4]).
element(v, 23, []).
element(cr, 24, []).
element(mn, 25, []).
element(fe, 26, [+2, +3]).
element(co, 27, []).
element(ni, 28, []).
element(cu, 29, [+1, +2, cov(1)]).
element(zn, 30, [+2, cov(1)]).
element(ga, 31, [+3, +2]).
element(ge, 32, [cov(4), cov(5), cov(6), +2]).
element(as, 33, [-3]).
element(se, 34, [-2]).
element(br, 35, [-1]).
element(kr, 36, []).
element(rb, 37, [+1, cov(1)]).
element(sr, 38, [+2]).
element(y, 39, [+3]).
element(zr, 40, [+4]).
element(nb, 41, []).
element(mo, 42, []).
element(tc, 43, []).
element(ru, 44, []).
element(rh, 45, []).
element(pd, 46, []).
element(ag, 47, [+1, +2, cov(1)]).
element(cd, 48, [+2, cov(1)]).
element(in, 49, [+3, +1, +2]).
element(sn, 50, [cov(4), cov(5), cov(6), +2]).
element(sb, 51, [-3]).
element(te, 52, [-2]).
element(i, 53, [-1]).
element(xe, 54, []).
element(cs, 55, [+1, cov(1)]).
element(ba, 56, [+2]).
element(la, 57, [+3]).
element(ce, 58, [+3, +4, +2]).
element(pr, 59, [+3]).
element(nd, 60, [+3]).
element(pm, 61, [+3]).
element(sm, 62, [+3]).
element(eu, 63, [+3, +4, +2]).
element(gd, 64, [+3]).
element(tb, 65, [+3]).
element(dy, 66, [+3]).
element(ho, 67, [+3]).
element(er, 68, [+3]).
element(tm, 69, [+3]).
element(yb, 70, [+3]).
element(lu, 71, [+3]).
element(hf, 72, [+4]).
element(ta, 73, []).
element(w, 74, []).
element(re, 75, []).
element(os, 76, []).
element(ir, 77, []).
element(pt, 78, []).
element(au, 79, [+1, +3]).
element(hg, 80, [+1, +2, cov(1)]).
element(tl, 81, [+3, +1]).
element(pb, 82, [cov(4), cov(5), cov(6), +2]).
element(bi, 83, [-3]).
element(po, 84, [-2]).
element(at, 85, [-1]).
element(rn, 86, []).
element(fr, 87, [+1]).
element(ra, 88, [+2]).
element(ac, 89, [+3]).
element(th, 90, [+4]).
element(pa, 91, []).
element(u, 92, []).
element(np, 93, []).
element(pu, 94, []).
element(am, 95, []).
element(cm, 96, []).
element(bk, 97, []).
element(cf, 98, []).
element(es, 99, []).
element(fm, 100, []).
element(md, 101, []).
element(no, 102, []).
element(lr, 103, []).
element(rf, 104, []).
element(db, 105, []).
element(sg, 106, []).
element(bh, 107, []).
element(hs, 108, []).
element(mr, 109, []).
element(ds, 110, []).
element(rg, 111, []).
element(cn, 112, []).
element(nh, 113, []).
element(fl, 114, []).
element(mc, 115, []).
element(lv, 116, []).
element(ts, 117, []).
element(og, 118, []).

:- initialization(reassert_valencies).

