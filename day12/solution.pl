%--------------------Part 2------------
next_position_reverse([X, Y], move(A, B), Map, [NextX, NextY]) :-
    NextX is X + A,
    on_grid(NextX),
    NextY is Y + B,
    on_grid(NextY),
    current_strength(Map, [X, Y], S1),
    current_strength(Map, [NextX, NextY], S2),
    can_move_to(S2, S1).

next_position_reverse([X, Y], M, Map, [X, Y]).

make_all_valid_moves_reverse(P, Map, [Pup, Pdown, Pleft, Pright]) :-
    next_position_reverse(P, move(0, -1), Map, Pup),
    next_position_reverse(P, move(0, 1), Map, Pdown),
    next_position_reverse(P, move(-1, 0), Map, Pleft),
    next_position_reverse(P, move(1, 0), Map, Pright).

solution2(Result) :-
    map(Map), count_path(Map, [[145, 20]], 0, Result, []).
%--------------------------------------
solution1(Result) :-
    map(Map), count_path(Map, [[0, 20]], 0, Result, []).

keep_distinct([], Acc, Acc).
keep_distinct([F|Rest], Acc, Result) :- \+ member(F, Acc), keep_distinct(Rest, [F|Acc], Result).
keep_distinct([F|Rest], Acc, Result) :- member(F, Acc), keep_distinct(Rest, Acc, Result).

make_all_valid_moves(P, Map, [Pup, Pdown, Pleft, Pright]) :-
    next_position(P, move(0, -1), Map, Pup),
    next_position(P, move(0, 1), Map, Pdown),
    next_position(P, move(-1, 0), Map, Pleft),
    next_position(P, move(1, 0), Map, Pright).

remove_visited([], Visited, Acc, Acc).
remove_visited([F|Rest], Visited, Acc, Result) :- \+ member(F, Visited), remove_visited(Rest, Visited, [F|Acc], Result).
remove_visited([F|Rest], Visited, Acc, Result) :- member(F, Visited), remove_visited(Rest, Visited, Acc, Result).

make_moves([], Map, Acc, AccDistinct, Visited) :- 
    keep_distinct(Acc, [], Acc1),
    remove_visited(Acc1, Visited, [], AccDistinct).
make_moves([P|Rest], Map, Acc, NextP, Visited) :-
%    make_all_valid_moves(P, Map, T), % for part 1
    make_all_valid_moves_reverse(P, Map, T), % for part 2
    append(T, Acc, Acc1),
    make_moves(Rest, Map, Acc1, NextP, Visited).

is_on_goal([F|Rest], Map) :- current_strength(Map, F, a). % for part 2
%is_on_goal([F|Rest], Map) :- current_strength(Map, F, goal). % for part 1
is_on_goal([F|Rest], Map) :- is_on_goal(Rest, Map).

count_path(Map, Positions, C, C, V) :- is_on_goal(Positions, Map).
count_path(Map, Positions, C, Result, Visited) :-
    \+ is_on_goal(Positions, Map),
    make_moves(Positions, Map, [], NextPositions, Visited),
    append(NextPositions, Visited, V),
    C1 is C + 1,
    count_path(Map, NextPositions, C1, Result, V).

before(X, Y) :- next(Y, X).
before(X, Y) :- next(X, Z), before(Z, Y).

can_move_to(X, goal) :- can_move_to(X, z).
can_move_to(X, X).
can_move_to(X, Y) :- next(X, Y).
can_move_to(X, Y) :- next(Y, X).
can_move_to(X, Y) :- before(Y, X).

elem(E, Row, S) :-
    length(Left, E),
    append(Left, [S|Right], Row).

current_strength([Row|Map], [X, 0], S) :- elem(X, Row, S).
current_strength(Map, [X, Y], S) :-
    elem(Y, Map, Row),
    elem(X, Row, S).

on_grid(X) :- X > -1, X < 167.

next_position([X, Y], move(A, B), Map, [NextX, NextY]) :-
    NextX is X + A,
    on_grid(NextX),
    NextY is Y + B,
    on_grid(NextY),
    current_strength(Map, [X, Y], S1),
    current_strength(Map, [NextX, NextY], S2),
    can_move_to(S1, S2).
next_position([X, Y], M, Map, [X, Y]).

sample([
[a,a,b,q,p,o,   n,m],
[a,b,c,r,y,x,   x,l],
[a,c,c,s,z,goal,x,k],
[a,c,c,t,u,v,   w,j],
[a,b,d,e,f,g,   h,i]
]).

next(a, b).
next(b, c).
next(c, d).
next(d, e).
next(e, f).
next(f, g).
next(g, h).
next(h, i).
next(i, j).
next(j, k).
next(k, l).
next(l, m).
next(m, n).
next(n, o).
next(o, p).
next(p, q).
next(q, r).
next(r, s).
next(s, t).
next(t, u).
next(u, v).
next(v, w).
next(w, x).
next(x, y).
next(y, z).

map([[a,b,c,c,c,a,a,a,a,a,c,c,c,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,a,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a],
[a,b,c,c,c,a,a,a,a,a,c,c,c,a,a,a,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,a,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a],
[a,b,c,c,c,c,a,a,a,a,a,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,a,a,c,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,a,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a],
[a,b,c,c,c,c,a,a,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,a,a,a,a,a,a,a,a,a,a,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a],
[a,b,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c],
[a,b,a,a,a,a,a,c,c,c,c,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,a,a,c,c,a,a,c,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,c,a,a,a,a,c,a,a,a,a,a,c,c,c,c,c,c,c,a,a,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c],
[a,b,a,a,a,a,a,c,c,c,c,a,a,a,a,a,a,a,a,a,a,c,a,a,c,c,c,a,a,a,a,a,a,c,a,c,c,a,a,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,k,k,k,l,l,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c],
[a,b,a,a,a,a,a,c,c,c,c,c,c,c,a,a,a,c,a,a,a,a,a,a,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,a,a,a,c,c,c,a,a,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,c,k,k,k,k,k,k,l,l,l,c,c,c,c,a,a,a,c,c,c,c,c,c,c],
[a,b,a,a,a,a,a,c,c,c,c,c,c,c,a,a,a,c,a,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,c,a,a,a,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,a,a,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,k,k,k,k,k,k,k,k,l,l,l,l,c,c,c,a,a,a,c,a,c,c,c,c,c],
[a,b,a,a,a,a,a,c,c,c,c,c,c,c,c,a,a,c,a,a,a,a,a,a,a,a,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,j,j,j,j,j,k,k,k,k,k,k,p,p,p,p,l,l,l,c,c,c,a,a,a,a,a,a,c,c,c,c],
[a,b,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,a,a,c,c,c,c,c,c,j,j,j,j,j,j,j,k,k,k,k,p,p,p,p,p,p,p,l,l,l,c,c,c,a,a,a,a,a,c,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,c,a,a,c,c,c,c,c,j,j,j,j,j,j,j,j,j,k,o,o,p,p,p,p,p,p,p,l,l,l,c,c,c,a,a,a,c,c,c,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,c,c,c,a,a,a,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,a,a,c,c,a,a,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,a,a,a,a,a,c,c,c,c,j,j,j,j,o,o,o,o,o,o,o,o,o,p,u,u,u,u,p,p,p,l,l,c,c,c,c,a,a,a,c,c,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,a,a,a,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,a,a,a,a,a,a,c,c,j,j,j,o,o,o,o,o,o,o,o,o,o,u,u,u,u,u,u,p,p,l,l,l,c,c,c,c,a,a,c,c,c,c,c],
[a,b,c,c,a,a,a,a,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,a,a,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,a,a,a,a,a,a,a,c,c,j,j,j,o,o,o,t,u,u,u,u,u,u,u,u,u,u,u,u,p,p,l,l,l,l,c,c,c,c,a,c,c,c,c,c],
[a,b,c,c,a,a,a,a,a,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,i,i,j,n,o,o,t,t,u,u,u,u,u,u,x,x,y,u,v,p,q,q,l,m,m,c,c,c,c,c,c,c,c,c,c],
[a,b,c,a,a,a,a,a,a,c,c,a,a,a,a,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,c,c,c,c,c,a,a,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,a,a,c,a,a,a,c,c,c,c,i,i,i,n,n,t,t,t,x,x,x,x,u,x,x,y,y,v,v,q,q,q,q,m,m,m,m,d,d,d,d,c,c,c,c],
[a,b,c,a,a,a,a,a,a,c,c,c,a,a,a,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,a,a,a,a,a,a,a,a,c,c,a,a,a,a,c,c,c,c,a,a,c,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,i,i,i,n,n,t,t,t,x,x,x,x,x,x,x,y,y,v,v,q,q,q,q,q,m,m,m,m,d,d,d,c,c,c,c],
[a,b,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,a,c,c,c,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,i,i,i,n,n,n,t,t,x,x,x,x,x,x,x,y,y,v,v,v,v,q,q,q,q,m,m,m,d,d,d,c,c,c,c],
[a,b,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,c,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,i,i,i,n,n,n,t,t,t,x,x,x,x,x,y,y,y,y,y,v,v,v,q,q,q,q,m,m,m,d,d,d,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,a,a,a,c,c,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,i,i,i,n,n,t,t,t,x,x,x,x,goal,z,y,y,y,y,y,v,v,v,q,q,q,m,m,m,d,d,d,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,a,a,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,a,a,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,i,i,i,n,n,t,t,t,x,x,x,y,y,y,y,y,y,y,v,v,v,v,q,q,q,m,m,m,d,d,d,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,i,i,i,n,n,n,t,t,x,x,y,y,y,y,y,y,y,v,v,v,v,v,q,q,q,q,m,m,m,d,d,d,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,c,a,c,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,i,i,i,n,n,t,t,t,x,x,w,y,y,y,y,y,w,w,v,v,r,r,r,q,q,m,m,m,d,d,d,c,c,c,c],
[a,b,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,h,h,n,n,n,t,t,w,w,w,w,w,w,w,y,y,y,w,v,r,r,r,r,n,n,n,n,m,d,d,d,c,c,c,c],
[a,b,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,a,a,a,a,c,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,a,a,a,c,c,c,h,h,n,m,m,t,t,s,w,w,w,w,w,w,y,w,w,w,r,r,r,r,n,n,n,n,n,e,e,e,c,c,c,c,c],
[a,b,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,a,a,c,c,a,a,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,a,a,a,c,a,h,h,h,m,m,m,s,s,s,s,s,s,s,s,w,w,w,w,w,r,r,r,n,n,n,n,e,e,e,e,c,c,c,c,c,c],
[a,b,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,a,a,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,h,h,h,m,m,m,m,s,s,s,s,s,s,s,s,w,w,w,w,r,r,n,n,n,n,e,e,e,e,a,c,c,c,c,c,c],
[a,b,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,a,a,a,a,a,a,a,c,c,a,a,a,c,c,a,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,a,a,a,a,a,a,a,h,h,h,h,m,m,m,m,s,s,s,s,s,s,s,w,w,w,w,r,r,n,n,n,e,e,e,e,a,a,c,c,c,c,c,c],
[a,b,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,a,a,a,a,a,a,a,a,a,a,c,c,c,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,c,a,a,a,a,a,c,c,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,a,c,h,h,h,m,m,m,m,m,m,m,m,m,s,s,s,r,r,r,r,r,n,n,n,e,e,e,a,a,a,a,a,c,c,c,c],
[a,b,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,c,c,c,a,a,a,a,a,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,a,c,h,h,h,h,m,m,m,m,m,m,m,o,o,o,s,s,r,r,r,o,n,n,e,e,e,a,a,a,a,a,a,c,c,c,c],
[a,b,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,c,c,c,c,a,a,a,c,c,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,h,h,h,h,h,g,g,g,g,o,o,o,o,o,r,r,r,o,o,n,n,f,e,e,a,a,a,a,a,c,c,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,c,c,c,c,a,a,a,a,a,a,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,h,h,h,h,g,g,g,g,g,g,o,o,o,o,o,o,o,o,o,f,f,e,a,a,a,a,a,c,c,c,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,a,a,c,c,a,c,c,c,c,c,a,a,a,a,a,a,a,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,a,a,a,c,c,c,c,c,c,a,a,a,a,a,a,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,g,g,g,g,g,g,g,g,g,o,o,o,o,o,o,f,f,f,f,a,a,a,a,a,a,c,c,c,c,c],
[a,b,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,a,a,c,c,a,a,a,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,g,g,g,g,f,o,o,o,o,f,f,f,f,c,c,c,c,a,a,c,c,c,c,c,c],
[a,b,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,a,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,a,a,a,a,c,c,c,a,a,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,g,g,f,f,f,f,f,f,f,f,f,c,c,c,c,c,c,c,c,c,c,c,c],
[a,b,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,c,a,a,a,a,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,c,c,c,c,a,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,a,g,f,f,f,f,f,f,f,c,c,c,c,c,c,c,c,c,c,c,c,c,a],
[a,b,a,a,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,a,a,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,c,a,a,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,a,a,c,c,c,c,a,a,c,c,a,a,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,a,a,a,c,c,f,f,f,f,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a],
[a,b,a,a,a,a,a,a,a,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,c,a,a,a,a,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,c,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,a,a,a,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,a,a,a,a,a,a,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,c,a,a],
[a,b,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,c,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a],
[a,b,a,a,a,a,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,c,a,a,c,c,c,c,c,c,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a,a,a,a,c,c,a,a,a,a,a,a,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,a,a,a,a,a]]).
