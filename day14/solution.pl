% my largest x 534
% my largest y 168
solution2(Result) :-
    start_a_grid(170, 670, T),
    give_me(670, rok, Floor),
    append(T, [Floor], Cave),
    input(Lines),
    init(Lines, Cave, Initialized),
    keep_sand_flowing(Initialized, 1, Result, [500, 0]).

solution2_sample(Result) :-
    start_a_grid(11, 25, T),
    give_me(25, rok, Floor),
    append(T, [Floor], Cave),
    sample_lines(Lines),
    init(Lines, Cave, Initialized),
    keep_sand_flowing(Initialized, 0, Result, [12, 0]).

solution1(Result) :-
    start_a_grid(13, 11, Cave), sample_lines(Lines), init(Lines, Cave, Initialized), keep_sand_flowing(Initialized, 0, Result, [6, 0]).
%    start_a_grid(169, 535, Cave), input(Lines), init(Lines, Cave, Initialized), keep_sand_flowing(Initialized, 0, Result, [500, 0]).

start_a_grid(Xlength, Ylength, Result) :- give_me(Ylength, air, Row), give_me(Xlength, Row, Result).

give_me(0, V, []) :- !.
give_me(N, V, [V|Rest]) :-
    N1 is N - 1, give_me(N1, V, Rest).

init([], Cave, Cave).
init([[From, To]|Rest], Cave, Initialized) :- draw_line(From, To, Cave, Temp), init(Rest, Temp, Initialized).
init([[From, To|More]|Rest], Cave, Initialized) :- draw_line(From, To, Cave, Temp), init([[To|More]|Rest], Temp, Initialized).

draw_line([Xs, Ys], [Xs, Ye], Grid, Result) :- Ye < Ys, draw_line([Xs, Ye], [Xs, Ys], Grid, Result).
draw_line([Xs, Ys], [Xs, Ye], Grid, Result) :-
    Ys < Ye,
    update_cave(Grid, [Xs, Ys], ResultTemp), Y is Ys + 1, draw_line([Xs, Y], [Xs, Ye], ResultTemp, Result).
draw_line([Xs, Ys], [Xe, Ys], Grid, Result) :- Xe < Xs, draw_line([Xe, Ys], [Xs, Ys], Grid, Result).
draw_line([Xs, Ys], [Xe, Ys], Grid, Result) :-
    Xs < Xe,
    update_cave(Grid, [Xs, Ys], ResultTemp), X is Xs + 1, draw_line([X, Ys], [Xe, Ys], ResultTemp, Result).
draw_line([Xs, Ys], [Xs, Ys], Grid, Result) :- update_cave(Grid, [Xs, Ys], Result).

print_array([]).
print_array([X|Xs]) :- write(X), nl, print_array(Xs).

update_row(P, Xs, X, Result) :- append(Left, [Y|Right], Xs), length(Left, P), append(Left, [X|Right], Result).

update_cave([Row|Cave], [X, 0], [R|Cave]) :- update_row(X, Row, rok, R).
update_cave([Row|Cave], [X, Y], [Row|C]) :- Y1 is Y - 1, update_cave(Cave, [X, Y1], C).

elem(P, Xs, X) :- append(Left, [X|Right], Xs), length(Left, P).

cave_content(Cave, [X, Y], Content) :- elem(Y, Cave, Row), elem(X, Row, Content).

front_three(Cave, [X, Y], [air, B, C]) :- X = 0, Y1 is Y + 1, elem(Y1, Cave, Row), append([B, C|Left], Rest, Row).
front_three([C|Cave], [X, Y], [A, B, air]) :-
    length(C, W), W1 is W - 1,
    X = W1, elem(Y, Cave, Row), append(Left, [A, B|Rest], Row), X1 is X - 1, length(Left, X1).
front_three(Cave, [X, Y], [A, B, C]) :- X > 0, Y1 is Y + 1, elem(Y1, Cave, Row), append(Left, [A, B, C|Rest], Row), X1 is X - 1, length(Left, X1).

next_coords([rok, rok, rok], [SandX, SandY], [SandX, SandY]).
next_coords([A, air, C], [SandX, SandY], [SandX, Y]) :- Y is SandY + 1.
next_coords([air, rok, C], [SandX, SandY], [X, Y]) :- Y is SandY + 1, X is SandX - 1.
next_coords([rok, rok, air], [SandX, SandY], [X, Y]) :- Y is SandY + 1, X is SandX + 1.

determine_next(Cave, [SandX, SandY], [X, Y]) :- front_three(Cave, [SandX, SandY], Three), next_coords(Three, [SandX, SandY], [X, Y]).

place_sand(Cave, [SandX, SandY], Cave1) :- determine_next(Cave, [SandX, SandY], [SandX, SandY]), update_cave(Cave, [SandX, SandY], Cave1).
place_sand(Cave, [SandX, SandY], C) :-
    determine_next(Cave, [SandX, SandY], [X, Y]), place_sand(Cave, [X, Y], C).

keep_sand_flowing(Cave, Count, Num, [StartX, StartY]) :-
    Count1 is Count + 1,
    write([StartX, StartY]), nl,
    place_sand(Cave, [StartX, StartY], Cave1),
    Cave \= Cave1,
    write(Count1), nl,
    keep_sand_flowing(Cave1, Count1, Num, [StartX, StartY]).

keep_sand_flowing(Cave, Count, Count, Start).



sample([
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,rok,air,air,air,rok,rok],
[air,air,air,air,rok,air,air,air,rok,air],
[air,air,rok,rok,rok,air,air,air,rok,air],
[air,air,air,air,air,air,air,air,rok,air],
[air,air,air,air,air,air,air,air,rok,air],
[rok,rok,rok,rok,rok,rok,rok,rok,rok,air]]).

sample_cave_start([
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air],
[air,air,air,air,air,air,air,air,air,air]]).


sample_lines2([[[9,4], [9, 6], [7,6]],
              [[14,4], [13,4], [13,9], [5,9]]]).

sample_lines([[[4,4], [4,6], [2,6]],
              [[9,4], [8,4], [8,9], [0,9]]]).

input([[[506,104], [511,104]],
       [[504,96], [509,96]],
       [[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
       [[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
       [[499,35], [504,35]],
       [[511,23], [516,23]],
       [[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
       [[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
       [[503,38], [508,38]],
       [[520,104], [525,104]],
       [[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
       [[499,28], [499,29], [513,29], [513,28]],
       [[504,23], [509,23]],
       [[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[485,45], [485,47], [483,47], [483,54], [492,54], [492,47], [489,47], [489,45]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[499,28], [499,29], [513,29], [513,28]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[508,98], [513,98]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[524,107], [524,111], [522,111], [522,118], [534,118], [534,111], [528,111], [528,107]],
[[507,20], [512,20]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[506,130], [506,132], [502,132], [502,137], [516,137], [516,132], [511,132], [511,130]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[510,127], [514,127]],
[[524,107], [524,111], [522,111], [522,118], [534,118], [534,111], [528,111], [528,107]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[492,104], [497,104]],
[[492,35], [497,35]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[528,127], [532,127]],
[[498,140], [498,142], [493,142], [493,150], [504,150], [504,142], [503,142], [503,140]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[485,45], [485,47], [483,47], [483,54], [492,54], [492,47], [489,47], [489,45]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[516,127], [520,127]],
[[506,130], [506,132], [502,132], [502,137], [516,137], [516,132], [511,132], [511,130]],
[[495,32], [500,32]],
[[493,14], [505,14], [505,13]],
[[522,127], [526,127]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[493,14], [505,14], [505,13]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[525,125], [529,125]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[506,130], [506,132], [502,132], [502,137], [516,137], [516,132], [511,132], [511,130]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[513,104], [518,104]],
[[506,130], [506,132], [502,132], [502,137], [516,137], [516,132], [511,132], [511,130]],
[[485,45], [485,47], [483,47], [483,54], [492,54], [492,47], [489,47], [489,45]],
[[519,121], [523,121]],
[[498,140], [498,142], [493,142], [493,150], [504,150], [504,142], [503,142], [503,140]],
[[495,102], [500,102]],
[[500,20], [505,20]],
[[490,42], [504,42], [504,41]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[498,140], [498,142], [493,142], [493,150], [504,150], [504,142], [503,142], [503,140]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[524,107], [524,111], [522,111], [522,118], [534,118], [534,111], [528,111], [528,107]],
[[485,45], [485,47], [483,47], [483,54], [492,54], [492,47], [489,47], [489,45]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[509,102], [514,102]],
[[513,125], [517,125]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[490,42], [504,42], [504,41]],
[[498,100], [503,100]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[498,140], [498,142], [493,142], [493,150], [504,150], [504,142], [503,142], [503,140]],
[[489,38], [494,38]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[485,45], [485,47], [483,47], [483,54], [492,54], [492,47], [489,47], [489,45]],
[[506,130], [506,132], [502,132], [502,137], [516,137], [516,132], [511,132], [511,130]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[524,107], [524,111], [522,111], [522,118], [534,118], [534,111], [528,111], [528,107]],
[[498,140], [498,142], [493,142], [493,150], [504,150], [504,142], [503,142], [503,140]],
[[506,130], [506,132], [502,132], [502,137], [516,137], [516,132], [511,132], [511,130]],
[[496,38], [501,38]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,140], [498,142], [493,142], [493,150], [504,150], [504,142], [503,142], [503,140]],
[[519,125], [523,125]],
[[501,154], [501,155], [507,155]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[499,104], [504,104]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[501,154], [501,155], [507,155]],
[[503,17], [508,17]],
[[516,123], [520,123]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[505,100], [510,100]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[524,107], [524,111], [522,111], [522,118], [534,118], [534,111], [528,111], [528,107]],
[[500,93], [500,83], [500,93], [502,93], [502,90], [502,93], [504,93], [504,85], [504,93], [506,93], [506,83], [506,93]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,140], [498,142], [493,142], [493,150], [504,150], [504,142], [503,142], [503,140]],
[[506,130], [506,132], [502,132], [502,137], [516,137], [516,132], [511,132], [511,130]],
[[516,102], [521,102]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[485,45], [485,47], [483,47], [483,54], [492,54], [492,47], [489,47], [489,45]],
[[522,123], [526,123]],
[[485,45], [485,47], [483,47], [483,54], [492,54], [492,47], [489,47], [489,45]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[499,28], [499,29], [513,29], [513,28]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[512,100], [517,100]],
[[524,107], [524,111], [522,111], [522,118], [534,118], [534,111], [528,111], [528,107]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[497,23], [502,23]],
[[524,107], [524,111], [522,111], [522,118], [534,118], [534,111], [528,111], [528,107]],
[[489,67], [489,58], [489,67], [491,67], [491,61], [491,67], [493,67], [493,59], [493,67], [495,67], [495,64], [495,67]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[501,98], [506,98]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[498,168], [498,158], [498,168], [500,168], [500,162], [500,168], [502,168], [502,163], [502,168], [504,168], [504,160], [504,168], [506,168], [506,159], [506,168], [508,168], [508,163], [508,168], [510,168], [510,163], [510,168], [512,168], [512,167], [512,168], [514,168], [514,161], [514,168], [516,168], [516,167], [516,168]],
[[502,102], [507,102]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]],
[[487,80], [487,75], [487,80], [489,80], [489,72], [489,80], [491,80], [491,76], [491,80], [493,80], [493,71], [493,80], [495,80], [495,76], [495,80], [497,80], [497,75], [497,80], [499,80], [499,70], [499,80], [501,80], [501,78], [501,80], [503,80], [503,70], [503,80]]]).