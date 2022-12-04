all([a,'A',b,'B',c,'C',d,'D',e,'E',f,'F',g,'G',h,'H',i,'I',j,'J',k,'K',l,'L',m,'M',n,'N',o,'O',p,'P',q,'Q',r,'R',s,'S',t,'T',u,'U',v,'V',w,'W',x,'X',y,'Y',z,'Z']).

priority(a, 1). priority('A', 27).
priority(b, 2). priority('B', 28).
priority(c, 3). priority('C', 29).
priority(d, 4). priority('D', 30).
priority(e, 5). priority('E', 31).
priority(f, 6). priority('F', 32).
priority(g, 7). priority('G', 33).
priority(h, 8). priority('H', 34).
priority(i, 9). priority('I', 35).
priority(j, 10). priority('J', 36).
priority(k, 11). priority('K', 37).
priority(l, 12). priority('L', 38).
priority(m, 13). priority('M', 39).
priority(n, 14). priority('N', 40).
priority(o, 15). priority('O', 41).
priority(p, 16). priority('P', 42).
priority(q, 17). priority('Q', 43).
priority(r, 18). priority('R', 44).
priority(s, 19). priority('S', 45).
priority(t, 20). priority('T', 46).
priority(u, 21). priority('U', 47).
priority(v, 22). priority('V', 48).
priority(w, 23). priority('W', 49).
priority(x, 24). priority('X', 50).
priority(y, 25). priority('Y', 51).
priority(z, 26). priority('Z', 52).

input_array(X) :- see('input'), get_input(X), seen.

get_input([X|Xs]) :- read(X), X \== end_of_file, get_input(Xs).
get_input([]) :- read(X), X == end_of_file.

split(X, [Left, Right]) :- atom_chars(X, Codes), length(Codes, L), Half is L / 2, append(Left, Right, Codes), length(Left, Half).

%intersects two lists, keeps distinct occurances of commonn elements
intersection([], O, []).
intersection([O|One], Other, [O|I]) :- member(O, Other), intersection(One, Other, I), \+ member(O, I).
intersection([O|One], Other, I) :- member(O, Other), intersection(One, Other, I), member(O, I).
intersection([O|One], Other, I) :- \+ member(O, Other), intersection(One, Other, I).

% finds sum of lists of the shape [priority(Y, 1), priority(X, 2), ...]
priority_sum([], 0).
priority_sum([X|Xs], S) :- priority(X, S1), priority_sum(Xs, Sr), S is S1 + Sr.

sum([], 0).
sum([X|Xs], S) :- sum(Xs, S1), S is X + S1.

map_one(I, S) :- split(I, [L, R]), intersection(L, R, Common), priority_sum(Common, S).
map_all([], []).
map_all([X|Xs], [Xm|Xsm]) :- map_one(X, Xm), map_all(Xs, Xsm).

solution1(Result) :- input_array(X), map_all(X, M), sum(M, Result).

%------------------solution2--------------
split_into_groups([A, B, C], [[A, B, C]]).
split_into_groups([A, B, C|Rest], [[A, B, C]|RestGrouped]) :- split_into_groups(Rest, RestGrouped).

% a faster intersect
intersect_three(A, B, C, [], []).
intersect_three(A, B, C, [F|Rest], [F|R]) :- write(F), nl, member(F, A), member(F, B), member(F, C), intersect_three(A, B, C, Rest, R).
intersect_three(A, B, C, [F|Rest], R) :-
    \+ member(F, A),
    intersect_three(A, B, C, Rest, R).
intersect_three(A, B, C, [F|Rest], R) :-
    \+ member(F, B),
    intersect_three(A, B, C, Rest, R).
intersect_three(A, B, C, [F|Rest], R) :-
    \+ member(F, C),
    intersect_three(A, B, C, Rest, R).

map_one_group([A, B, C], Result) :-
    all(X),
    atom_chars(A, Ac),
    atom_chars(B, Bc),
    atom_chars(C, Cc),
    intersect_three(Ac, Bc, Cc, X, I), priority_sum(I, Result).

map_all_groups([], []).
map_all_groups([G|Gs], [Gm|Gsm]) :- map_one_group(G, Gm), map_all_groups(Gs, Gsm).

solution2(Result) :-
    input_array(X),
    split_into_groups(X, Groups),
    map_all_groups(Groups, GroupsMapped),
    sum(GroupsMapped, Result).
