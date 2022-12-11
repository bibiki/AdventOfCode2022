solution1(Result) :-
    Counter is 1,
    Cycle is 1,
    X is 1,
    input(Instructions), execute(Instructions, X, Cycle, [], S, Counter), sum(S, Result).

sum([], 0).
sum([F|Rest], S) :- sum(Rest, S1), S is S1 + F.

update_signals(Signals, X, 20, [X1|Signals]) :- X1 is X * 20.
update_signals(Signals, X, 60, [X1|Signals]) :- X1 is X * 60.
update_signals(Signals, X, 100, [X1|Signals]) :- X1 is X * 100.
update_signals(Signals, X, 140, [X1|Signals]) :- X1 is X * 140.
update_signals(Signals, X, 180, [X1|Signals]) :- X1 is X * 180.
update_signals(Signals, X, 220, [X1|Signals]) :- X1 is X * 220.
update_signals(Signals, X, Cycle, Signals).

execute([], X, Cycle, Signals, Signals, Counter).
execute([instruction(noop)|Instructions], X, Cycle, Signals, S, Counter) :-
    update_signals(Signals, X, Cycle, NewSignals),
    C1 is Cycle + 1,
    execute(Instructions, X, C1, NewSignals, S, 1).

execute([instruction(addx, N)|Instructions], X, Cycle, Signals, S, 1) :-
    update_signals(Signals, X, Cycle, NewSignals),
    C1 is Cycle + 1,
    execute([instruction(addx, N)|Instructions], X, C1, NewSignals, S, 2).
execute([instruction(addx, N)|Instructions], X, Cycle, Signals, S, 2) :-
    update_signals(Signals, X, Cycle, NewSignals),
    C1 is Cycle + 1,
    X1 is X + N,
    execute(Instructions, X1, C1, NewSignals, S, 1).


input([instruction(addx, 1),
instruction(addx, 4),
instruction(addx, 1),
instruction(noop),
instruction(addx, 4),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, 4),
instruction(addx, 1),
instruction(addx, 5),
instruction(noop),
instruction(noop),
instruction(addx, 5),
instruction(addx, -1),
instruction(addx, 3),
instruction(addx, 3),
instruction(addx, 1),
instruction(noop),
instruction(noop),
instruction(addx, 4),
instruction(addx, 1),
instruction(noop),
instruction(addx, -38),
instruction(addx, 10),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, 2),
instruction(addx, 3),
instruction(addx, -2),
instruction(addx, 2),
instruction(addx, 5),
instruction(addx, 2),
instruction(addx, -13),
instruction(addx, 14),
instruction(addx, 2),
instruction(noop),
instruction(noop),
instruction(addx, -9),
instruction(addx, 19),
instruction(addx, -2),
instruction(addx, 2),
instruction(addx, -9),
instruction(addx, -24),
instruction(addx, 1),
instruction(addx, 6),
instruction(noop),
instruction(noop),
instruction(addx, -2),
instruction(addx, 5),
instruction(noop),
instruction(noop),
instruction(addx, -12),
instruction(addx, 15),
instruction(noop),
instruction(addx, 3),
instruction(addx, 3),
instruction(addx, 1),
instruction(addx, 5),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, -24),
instruction(addx, 29),
instruction(addx, 5),
instruction(noop),
instruction(noop),
instruction(addx, -37),
instruction(noop),
instruction(addx, 26),
instruction(noop),
instruction(noop),
instruction(addx, -18),
instruction(addx, 28),
instruction(addx, -24),
instruction(addx, 17),
instruction(addx, -16),
instruction(addx, 4),
instruction(noop),
instruction(addx, 5),
instruction(addx, -2),
instruction(addx, 5),
instruction(addx, 2),
instruction(addx, -18),
instruction(addx, 24),
instruction(noop),
instruction(addx, -2),
instruction(addx, 10),
instruction(addx, -6),
instruction(addx, -12),
instruction(addx, -23),
instruction(noop),
instruction(addx, 41),
instruction(addx, -34),
instruction(addx, 30),
instruction(addx, -25),
instruction(noop),
instruction(addx, 16),
instruction(addx, -15),
instruction(addx, 2),
instruction(addx, -12),
instruction(addx, 19),
instruction(addx, 3),
instruction(noop),
instruction(addx, 2),
instruction(addx, -27),
instruction(addx, 36),
instruction(addx, -6),
instruction(noop),
instruction(noop),
instruction(addx, 7),
instruction(addx, -33),
instruction(addx, -4),
instruction(noop),
instruction(addx, 24),
instruction(noop),
instruction(addx, -17),
instruction(addx, 1),
instruction(noop),
instruction(addx, 4),
instruction(addx, 1),
instruction(addx, 14),
instruction(addx, -12),
instruction(addx, -14),
instruction(addx, 21),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, 5),
instruction(addx, -17),
instruction(addx, 1),
instruction(addx, 20),
instruction(addx, 2),
instruction(noop),
instruction(addx, 2),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop)]).

sample([
instruction(addx, 15),
instruction(addx, -11),
instruction(addx, 6),
instruction(addx, -3),
instruction(addx, 5),
instruction(addx, -1),
instruction(addx, -8),
instruction(addx, 13),
instruction(addx, 4),
instruction(noop),
instruction(addx, -1),
instruction(addx, 5),
instruction(addx, -1),
instruction(addx, 5),
instruction(addx, -1),
instruction(addx, 5),
instruction(addx, -1),
instruction(addx, 5),
instruction(addx, -1),
instruction(addx, -35),
instruction(addx, 1),
instruction(addx, 24),
instruction(addx, -19),
instruction(addx, 1),
instruction(addx, 16),
instruction(addx, -11),
instruction(noop),
instruction(noop),
instruction(addx, 21),
instruction(addx, -15),
instruction(noop),
instruction(noop),
instruction(addx, -3),
instruction(addx, 9),
instruction(addx, 1),
instruction(addx, -3),
instruction(addx, 8),
instruction(addx, 1),
instruction(addx, 5),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, -36),
instruction(noop),
instruction(addx, 1),
instruction(addx, 7),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, 2),
instruction(addx, 6),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, 1),
instruction(noop),
instruction(noop),
instruction(addx, 7),
instruction(addx, 1),
instruction(noop),
instruction(addx, -13),
instruction(addx, 13),
instruction(addx, 7),
instruction(noop),
instruction(addx, 1),
instruction(addx, -33),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, 2),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, 8),
instruction(noop),
instruction(addx, -1),
instruction(addx, 2),
instruction(addx, 1),
instruction(noop),
instruction(addx, 17),
instruction(addx, -9),
instruction(addx, 1),
instruction(addx, 1),
instruction(addx, -3),
instruction(addx, 11),
instruction(noop),
instruction(noop),
instruction(addx, 1),
instruction(noop),
instruction(addx, 1),
instruction(noop),
instruction(noop),
instruction(addx, -13),
instruction(addx, -19),
instruction(addx, 1),
instruction(addx, 3),
instruction(addx, 26),
instruction(addx, -30),
instruction(addx, 12),
instruction(addx, -1),
instruction(addx, 3),
instruction(addx, 1),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, -9),
instruction(addx, 18),
instruction(addx, 1),
instruction(addx, 2),
instruction(noop),
instruction(noop),
instruction(addx, 9),
instruction(noop),
instruction(noop),
instruction(noop),
instruction(addx, -1),
instruction(addx, 2),
instruction(addx, -37),
instruction(addx, 1),
instruction(addx, 3),
instruction(noop),
instruction(addx, 15),
instruction(addx, -21),
instruction(addx, 22),
instruction(addx, -6),
instruction(addx, 1),
instruction(noop),
instruction(addx, 2),
instruction(addx, 1),
instruction(noop),
instruction(addx, -10),
instruction(noop),
instruction(noop),
instruction(addx, 20),
instruction(addx, 1),
instruction(addx, 2),
instruction(addx, 2),
instruction(addx, -6),
instruction(addx, -11),
instruction(noop),
instruction(noop),
instruction(noop)]).
