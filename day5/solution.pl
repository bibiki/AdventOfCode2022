solution1(Result) :- stack(Stack), instructions(Moves), make_moves(Moves, Stack, NewStack), show_message(NewStack, Result).
solution2(Result) :- stack(Stack), instructions(Moves), make_moves_9001(Moves, Stack, NewStack), show_message(NewStack, Result).
%--------------------part 2---------
make_move_9001(move(C, F, T), Stack, NewStack) :-
    get_(F, Stack, From),
    get_(T, Stack, To),
    append(Left, NewFrom, From),
    length(Left, C),
    append(Left, To, NewTo),
    replace(F, NewFrom, Stack, Temp),
    replace(T, NewTo, Temp, NewStack).

make_moves_9001([], Stack, Stack).
make_moves_9001([First|Moves], Stack, NewStack) :- make_move_9001(First, Stack, Temp), make_moves_9001(Moves, Temp, NewStack).
%-------------------------------------

show_message([], []).
show_message([[H|Head]|Stack], [H|R]) :- show_message(Stack, R).

get_(1, [S|Stack], S).
get_(C, [Head|Stack], S) :- C > 1, C1 is C - 1, get_(C1, Stack, S).

replace(1, New, [S|Stack], [New|Stack]).
replace(C, New, [S|Stack], [S|NewStack]) :-
    C > 1,
    C1 is C - 1,
    replace(C1, New, Stack, NewStack).

make_move(move(C, F, T), Stack, NewStack) :-
    get_(F, Stack, From),
    get_(T, Stack, To),
    move(C, From, To, NewFrom, NewTo),
    replace(F, NewFrom, Stack, Temp),
    replace(T, NewTo, Temp, NewStack).

make_moves([], Stack, Stack).
make_moves([First|Moves], Stack, NewStack) :- make_move(First, Stack, Temp), make_moves(Moves, Temp, NewStack).

move(1, [F|From], To, From, [F|To]).
move(Count, From, To, NewFrom, NewTo) :-
    Count > 1,
    move(1, From, To, FromT, ToT),
    C1 is Count - 1,
    move(C1, FromT, ToT, NewFrom, NewTo).

stack([[v, r, h, b, g, d, w],
       [f, r, c, g, n, j],
       [j, n, d, h, f, s, l],
       [v, s, d, j],
       [v, n, w, q, r, d, h, s],
       [m, c, h, g, p],
       [c, h, z, l, g, b, j, f],
       [r, j, s],
       [m, v, n, b, r, s, g, l]]).

instructions([
move(2, 2, 7),
move(8, 5, 6),
move(2, 4, 5),
move(1, 4, 5),
move(1, 5, 8),
move(5, 9, 2),
move(7, 1, 6),
move(7, 3, 8),
move(1, 4, 6),
move(2, 5, 6),
move(6, 7, 5),
move(2, 2, 4),
move(4, 5, 2),
move(10, 8, 1),
move(2, 7, 4),
move(4, 2, 8),
move(2, 9, 8),
move(1, 8, 4),
move(2, 4, 9),
move(5, 8, 2),
move(1, 4, 6),
move(1, 8, 9),
move(1, 7, 2),
move(2, 4, 2),
move(1, 7, 3),
move(13, 2, 1),
move(1, 2, 4),
move(1, 2, 3),
move(2, 5, 4),
move(17, 6, 4),
move(3, 4, 9),
move(14, 1, 4),
move(4, 6, 8),
move(1, 9, 8),
move(23, 4, 8),
move(6, 1, 7),
move(3, 1, 5),
move(1, 3, 8),
move(5, 7, 8),
move(1, 3, 4),
move(1, 5, 3),
move(1, 5, 1),
move(1, 3, 2),
move(1, 9, 4),
move(9, 4, 9),
move(1, 1, 2),
move(11, 8, 2),
move(1, 4, 5),
move(13, 2, 3),
move(7, 9, 6),
move(1, 5, 6),
move(1, 5, 2),
move(1, 9, 4),
move(1, 4, 9),
move(2, 8, 9),
move(1, 7, 8),
move(8, 9, 1),
move(8, 1, 4),
move(4, 6, 7),
move(1, 9, 4),
move(2, 3, 9),
move(1, 9, 1),
move(6, 4, 1),
move(2, 1, 3),
move(22, 8, 6),
move(1, 2, 5),
move(3, 7, 8),
move(15, 6, 4),
move(7, 3, 7),
move(4, 6, 9),
move(2, 9, 2),
move(6, 3, 5),
move(3, 9, 5),
move(5, 5, 8),
move(1, 2, 1),
move(6, 8, 2),
move(1, 1, 2),
move(3, 5, 3),
move(1, 7, 2),
move(4, 7, 8),
move(4, 6, 1),
move(1, 5, 1),
move(4, 8, 7),
move(2, 3, 2),
move(1, 1, 3),
move(15, 4, 2),
move(3, 7, 3),
move(4, 7, 2),
move(1, 4, 9),
move(5, 3, 8),
move(29, 2, 1),
move(1, 9, 5),
move(1, 2, 1),
move(11, 1, 5),
move(1, 4, 5),
move(2, 6, 3),
move(1, 3, 4),
move(16, 1, 9),
move(4, 8, 4),
move(3, 6, 9),
move(1, 3, 7),
move(1, 7, 3),
move(6, 1, 6),
move(3, 4, 3),
move(3, 8, 5),
move(3, 1, 8),
move(3, 1, 4),
move(2, 4, 9),
move(3, 6, 3),
move(15, 5, 2),
move(3, 2, 3),
move(4, 2, 7),
move(2, 5, 9),
move(10, 3, 6),
move(11, 9, 5),
move(2, 4, 9),
move(8, 9, 4),
move(1, 9, 6),
move(7, 4, 6),
move(3, 5, 8),
move(22, 6, 9),
move(4, 7, 8),
move(8, 5, 8),
move(2, 4, 3),
move(1, 8, 1),
move(17, 8, 3),
move(3, 3, 4),
move(13, 3, 9),
move(20, 9, 7),
move(2, 2, 9),
move(19, 9, 5),
move(1, 1, 4),
move(3, 2, 7),
move(4, 4, 3),
move(1, 9, 8),
move(18, 5, 1),
move(1, 9, 4),
move(1, 9, 7),
move(2, 4, 8),
move(1, 5, 4),
move(3, 2, 7),
move(3, 3, 1),
move(2, 1, 3),
move(3, 3, 8),
move(1, 4, 8),
move(6, 8, 2),
move(1, 3, 9),
move(1, 3, 9),
move(10, 1, 9),
move(7, 1, 7),
move(4, 7, 4),
move(29, 7, 3),
move(6, 2, 9),
move(25, 3, 6),
move(5, 3, 9),
move(13, 6, 9),
move(12, 6, 2),
move(1, 8, 9),
move(10, 2, 6),
move(7, 6, 5),
move(20, 9, 3),
move(11, 3, 6),
move(1, 7, 9),
move(2, 2, 9),
move(19, 9, 2),
move(14, 6, 8),
move(4, 5, 2),
move(2, 4, 6),
move(3, 5, 1),
move(13, 8, 5),
move(1, 6, 1),
move(2, 4, 2),
move(8, 2, 4),
move(6, 4, 7),
move(1, 9, 8),
move(2, 4, 7),
move(5, 2, 4),
move(4, 4, 2),
move(10, 5, 6),
move(1, 1, 7),
move(1, 5, 4),
move(1, 4, 9),
move(4, 7, 8),
move(5, 1, 7),
move(1, 9, 7),
move(7, 3, 2),
move(2, 5, 2),
move(8, 6, 9),
move(1, 4, 6),
move(3, 7, 4),
move(5, 9, 7),
move(2, 4, 3),
move(20, 2, 4),
move(2, 4, 8),
move(14, 4, 2),
move(12, 7, 4),
move(8, 2, 1),
move(10, 2, 4),
move(6, 8, 5),
move(1, 7, 8),
move(4, 4, 3),
move(1, 3, 9),
move(1, 2, 7),
move(1, 6, 8),
move(5, 3, 5),
move(1, 3, 2),
move(7, 4, 5),
move(6, 1, 7),
move(5, 7, 6),
move(1, 6, 5),
move(2, 7, 8),
move(1, 2, 6),
move(2, 8, 2),
move(5, 5, 7),
move(6, 6, 8),
move(16, 4, 9),
move(16, 9, 4),
move(11, 5, 4),
move(5, 8, 3),
move(2, 5, 2),
move(14, 4, 2),
move(1, 6, 3),
move(1, 6, 9),
move(1, 5, 3),
move(3, 8, 2),
move(10, 4, 7),
move(5, 9, 2),
move(3, 4, 7),
move(1, 1, 4),
move(3, 2, 5),
move(2, 3, 7),
move(1, 4, 2),
move(18, 2, 8),
move(3, 8, 4),
move(5, 3, 1),
move(1, 3, 9),
move(1, 9, 3),
move(8, 8, 7),
move(2, 5, 4),
move(1, 5, 6),
move(1, 2, 5),
move(1, 5, 8),
move(1, 6, 9),
move(3, 2, 7),
move(27, 7, 4),
move(2, 2, 4),
move(4, 8, 4),
move(1, 9, 8),
move(3, 1, 6),
move(1, 3, 5),
move(3, 8, 3),
move(1, 1, 4),
move(1, 8, 1),
move(3, 1, 4),
move(2, 8, 2),
move(2, 6, 2),
move(8, 4, 9),
move(1, 7, 1),
move(1, 5, 4),
move(1, 7, 3),
move(4, 2, 7),
move(1, 8, 6),
move(8, 9, 7),
move(1, 6, 3),
move(3, 3, 4),
move(37, 4, 1),
move(1, 4, 5),
move(13, 7, 8),
move(6, 8, 4),
move(5, 8, 3),
move(1, 7, 6),
move(4, 1, 5),
move(1, 6, 5),
move(2, 8, 4),
move(32, 1, 5),
move(1, 1, 4),
move(5, 3, 5),
move(1, 3, 2),
move(1, 2, 9),
move(19, 5, 2),
move(1, 9, 1),
move(16, 5, 1),
move(7, 5, 6),
move(1, 3, 1),
move(11, 1, 2),
move(18, 2, 4),
move(1, 5, 9),
move(8, 6, 1),
move(10, 2, 6),
move(7, 4, 9),
move(2, 2, 1),
move(7, 4, 2),
move(5, 4, 5),
move(2, 9, 6),
move(9, 6, 3),
move(5, 5, 3),
move(8, 4, 9),
move(7, 9, 8),
move(4, 2, 9),
move(10, 3, 1),
move(6, 8, 1),
move(2, 6, 3),
move(5, 3, 8),
move(3, 2, 7),
move(1, 9, 5),
move(1, 3, 5),
move(2, 7, 8),
move(1, 8, 9),
move(1, 6, 1),
move(23, 1, 4),
move(2, 5, 3),
move(1, 8, 2),
move(2, 8, 5),
move(2, 5, 6),
move(1, 2, 7),
move(1, 7, 5),
move(4, 9, 7),
move(1, 7, 5),
move(1, 3, 6),
move(3, 7, 4),
move(1, 3, 8),
move(1, 4, 6),
move(6, 1, 8),
move(4, 6, 4),
move(2, 9, 1),
move(1, 5, 1),
move(19, 4, 2),
move(2, 9, 3),
move(1, 9, 3),
move(9, 1, 8),
move(1, 5, 8),
move(1, 9, 3),
move(2, 3, 9),
move(3, 8, 4),
move(1, 4, 9),
move(1, 9, 5),
move(2, 3, 4),
move(6, 4, 7),
move(3, 9, 5),
move(4, 4, 7),
move(1, 5, 6),
move(18, 2, 7),
move(13, 7, 9),
move(3, 5, 1),
move(1, 2, 1),
move(1, 6, 5),
move(3, 1, 7),
move(1, 1, 5),
move(7, 9, 6),
move(8, 7, 4),
move(11, 7, 6),
move(5, 9, 2),
move(17, 6, 1),
move(2, 5, 1),
move(11, 8, 1),
move(20, 1, 2),
move(3, 8, 1),
move(1, 9, 8),
move(1, 6, 1),
move(11, 1, 7),
move(18, 2, 3),
move(12, 4, 8),
move(11, 7, 3),
move(7, 2, 3),
move(2, 1, 5),
move(1, 1, 3),
move(1, 8, 1),
move(1, 5, 9),
move(1, 9, 6),
move(1, 8, 7),
move(1, 5, 3),
move(1, 6, 7),
move(2, 8, 1),
move(8, 3, 2),
move(7, 2, 9),
move(6, 8, 6),
move(1, 9, 3),
move(2, 6, 4),
move(5, 9, 6),
move(7, 6, 2),
move(8, 2, 9),
move(2, 1, 9),
move(2, 7, 2),
move(2, 4, 8),
move(1, 2, 7),
move(25, 3, 7),
move(7, 9, 7),
move(1, 2, 5),
move(1, 1, 4),
move(3, 8, 1),
move(3, 1, 8),
move(3, 7, 8),
move(15, 7, 3),
move(10, 8, 3),
move(1, 5, 7),
move(1, 8, 5),
move(3, 9, 2),
move(1, 6, 4),
move(2, 2, 7),
move(1, 2, 5),
move(14, 7, 9),
move(1, 6, 2),
move(1, 7, 1),
move(1, 5, 4),
move(3, 4, 3),
move(1, 7, 6),
move(1, 2, 7),
move(1, 1, 2),
move(3, 9, 1),
move(1, 6, 2),
move(2, 2, 6),
move(17, 3, 6),
move(1, 8, 3),
move(1, 5, 4),
move(2, 7, 2),
move(9, 9, 8),
move(1, 9, 3),
move(16, 3, 2),
move(1, 7, 5),
move(5, 6, 5),
move(1, 1, 6),
move(1, 4, 1),
move(1, 9, 3),
move(9, 8, 6),
move(3, 1, 5),
move(1, 9, 1),
move(16, 2, 1),
move(2, 2, 7),
move(2, 3, 9),
move(2, 7, 4),
move(2, 9, 3),
move(3, 3, 5),
move(1, 4, 5),
move(1, 4, 2),
move(1, 1, 7),
move(1, 7, 1),
move(1, 3, 6),
move(2, 5, 1),
move(3, 6, 2),
move(2, 5, 8),
move(8, 5, 4),
move(1, 5, 3),
move(1, 3, 2),
move(1, 8, 3),
move(1, 3, 8),
move(4, 1, 7),
move(9, 1, 7),
move(6, 1, 8),
move(3, 7, 4),
move(7, 6, 7),
move(11, 4, 3),
move(2, 3, 8),
move(8, 3, 8),
move(4, 6, 1),
move(1, 7, 4),
move(2, 1, 2),
move(8, 7, 2),
move(1, 4, 8),
move(10, 8, 2),
move(2, 6, 1),
move(1, 1, 4),
move(1, 4, 8),
move(2, 1, 4),
move(6, 6, 5),
move(1, 1, 9),
move(2, 6, 8),
move(1, 4, 5),
move(1, 6, 9),
move(4, 8, 9),
move(1, 7, 1),
move(6, 8, 6),
move(1, 6, 1),
move(1, 4, 9),
move(2, 9, 5),
move(5, 5, 9),
move(8, 9, 5),
move(2, 8, 5),
move(3, 6, 9),
move(8, 5, 7),
move(5, 5, 6),
move(1, 9, 2),
move(1, 3, 1),
move(1, 6, 7),
move(1, 5, 6),
move(24, 2, 4),
move(3, 9, 7),
move(16, 4, 5),
move(2, 1, 3),
move(12, 5, 6),
move(1, 9, 5),
move(4, 5, 9),
move(1, 1, 6),
move(1, 5, 2),
move(2, 9, 8),
move(1, 8, 1),
move(5, 4, 5),
move(2, 3, 5),
move(1, 8, 3),
move(1, 1, 6),
move(3, 5, 7),
move(1, 9, 1),
move(1, 2, 8)]).
