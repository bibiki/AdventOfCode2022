solution(Z, O, T, THR, FR, FV, SX, SVN) :-
    % determine the two most active via visual inspection of monkies activities
    monkey(0, Zero), monkey(1, One), monkey(2, Two), monkey(3, Three), monkey(4, Four), monkey(5, Five), monkey(6, Six), monkey(7, Seven),
    monkey_business(Zero, One, Two, Three, Four, Five, Six, Seven, 10000, Z, O, T, THR, FR, FV, SX, SVN).

monkey(0, [0, 0, [97, 81, 57, 57, 91, 61]]).
monkey(1, [1, 0, [88, 62, 68, 90]]).
monkey(2, [2, 0, [74, 87]]).
monkey(3, [3, 0, [53, 81, 60, 87, 90, 99, 75]]).
monkey(4, [4, 0, [57]]).
monkey(5, [5, 0, [54, 84, 91, 55, 59, 72, 75, 70]]).
monkey(6, [6, 0, [95, 79, 79, 68, 78]]).
monkey(7, [7, 0, [61, 97, 67]]).


%manage_worry(New, NewItem) :- NewItem is New // 3.
rotator(9699690).
rem(N, R) :- rotator(Q), R1 is N // Q, N1 is R1 * Q, R is N - N1.
manage_worry(New, NewItem) :- rem(New, NewItem).

monkey_business(Z, O, T, THREE, F, FIVE, S, SEVEN, 0, Z, O, T, THREE, F, FIVE, S, SEVEN).

monkey_business(Z, O, T, THREE, F, FIVE, S, SEVEN, Round, Zt, Ot, Tt, THREEt, Ft, FIVEt, St, SEVENt) :-
    one_round(Z, Z1, FIVE, S, FIVE1, S1), % 0, 5, 6
    one_round(O, O1, F, T, F1, T1), %, 1, 4, 2
    one_round(T1, T2, SEVEN, F1, SEVEN1, F2), %, 2, 7, 4
    one_round(THREE, THREE1, T2, O1, T3, O2), % 3, 2, 1
    one_round(F2, F3, SEVEN1, Z1, SEVEN2, Z2), % 4, 7, 0
    one_round(FIVE1, FIVE2, S1, THREE1, S2, THREE2), % 5, 6, 3
    one_round(S2, S3, O2, THREE2, O3, THREE3), % 6, 1, 3
    one_round(SEVEN2, SEVEN3, Z2, FIVE2, Z3, FIVE3), % 7, 0, 5
    Round1 is Round - 1,
    monkey_business(Z3, O3, T3, THREE3, F3, FIVE3, S3, SEVEN3, Round1,
         Zt, Ot, Tt, THREEt, Ft, FIVEt, St, SEVENt).

one_round([Monkey, Acctivity, []], [Monkey, Acctivity, []], A, B, A, B).
one_round([Monkey, Acctivity, [Old|Rest]], M1, A, B, A1, B1) :-
    operation([Monkey, Acctivity, [Old|Rest]], A, M, At),
    one_round(M, M1, At, B, A1, B1).

one_round([Monkey, Acctivity, [Old|Rest]], M1, A, B, A1, B1) :-
    operation([Monkey, Acctivity, [Old|Rest]], B, M, Bt),
    one_round(M, M1, A, Bt, A1, B1).

is_divisible(N, Divisor) :- D is N / Divisor, D1 is N // Divisor, D = D1.

operation([0, A1, [Old|Rest]], [5, A2, Items],
          [0, A11, Rest], [5, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old * 7,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 11),
    append(Items, [NewItem], NewItems).

operation([0, A1, [Old|Rest]], [6, A2, Items],
          [0, A11, Rest], [6, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old * 7,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 11),
    append(Items, [NewItem], NewItems).

operation([1, A1, [Old|Rest]], [4, A2, Items],
          [1, A11, Rest], [4, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old * 17,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 19),
    append(Items, [NewItem], NewItems).

operation([1, A1, [Old|Rest]], [2, A2, Items],
          [1, A11, Rest], [2, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old * 17,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 19),
    append(Items, [NewItem], NewItems).

operation([2, A1, [Old|Rest]], [7, A2, Items],
          [2, A11, Rest], [7, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 2,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 5),
    append(Items, [NewItem], NewItems).

operation([2, A1, [Old|Rest]], [4, A2, Items],
          [2, A11, Rest], [4, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 2,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 5),
    append(Items, [NewItem], NewItems).

operation([3, A1, [Old|Rest]], [2, A2, Items],
          [3, A11, Rest], [2, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 1,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 2),
    append(Items, [NewItem], NewItems).

operation([3, A1, [Old|Rest]], [1, A2, Items],
          [3, A11, Rest], [1, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 1,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 2),
    append(Items, [NewItem], NewItems).

operation([4, A1, [Old|Rest]], [7, A2, Items],
          [4, A11, Rest], [7, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 6,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 13),
    append(Items, [NewItem], NewItems).

operation([4, A1, [Old|Rest]], [0, A2, Items],
          [4, A11, Rest], [0, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 6,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 13),
    append(Items, [NewItem], NewItems).

operation([5, A1, [Old|Rest]], [6, A2, Items],
          [5, A11, Rest], [6, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old * Old,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 7),
    append(Items, [NewItem], NewItems).

operation([5, A1, [Old|Rest]], [3, A2, Items],
          [5, A11, Rest], [3, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old * Old,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 7),
    append(Items, [NewItem], NewItems).

operation([6, A1, [Old|Rest]], [1, A2, Items],
          [6, A11, Rest], [1, A2, NewItems]) :-
    New is Old + 3,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 3),
    A11 is A1 + 1,
    append(Items, [NewItem], NewItems).

operation([6, A1, [Old|Rest]], [3, A2, Items],
          [6, A11, Rest], [3, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 3,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 3),
    append(Items, [NewItem], NewItems).

operation([7, A1, [Old|Rest]], [0, A2, Items],
         [7, A11, Rest], [0, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 4,
    manage_worry(New, NewItem),
    is_divisible(NewItem, 17),
    append(Items, [NewItem], NewItems).

operation([7, A1, [Old|Rest]], [5, A2, Items],
          [7, A11, Rest], [5, A2, NewItems]) :-
    A11 is A1 + 1,
    New is Old + 4,
    manage_worry(New, NewItem),
    \+ is_divisible(NewItem, 17),
    append(Items, [NewItem], NewItems).

operation([M, A, []], Other, [M, A, []], Other).
