solution1(Result) :-
%   needs lots of memory
%    count(-2, 0, Result).
    input(Sensors),
    mcount(-3526985, 2000000, 0, Result, Sensors).

abs(X, X) :- X >= 0.
abs(X, Y) :- X < 0, Y is 0 - X.
distance(A, B, C, D, Distance) :-
    sensor_beacon(A, B, C, D),
    measure_distance(A, B, C, D, Distance).

measure_distance(A, B, C, D, Distance) :-
    Hl is C - A, Vl is D - B,
    abs(Hl, H), abs(Vl, V),
    Distance is H + V.

is_in([Px, Py], [A, B], Distance) :- 
    measure_distance(Px, Py, A, B, D),
    D =< Distance.

is_near_any([X, Y]) :-
    distance(A, B, C, D, Distance),
    is_in([X, Y], [A, B], Distance).

count(X, Acc, Acc) :- X > 25, !.
count(X, Acc, Result) :-
    sensor_beacon(_, _, X, 10),
    X1 is X + 1, count(X1, Acc, Result).
count(X, Acc, Result) :-
    \+ is_near_any([X, 10]),
    X1 is X + 1, count(X1, Acc, Result).

count(X, Acc, Result) :-
    is_near_any([X, 10]),
    Acc1 is Acc + 1,
    X1 is X + 1, count(X1, Acc1, Result).

the_same(X, Y, X, Y).

is_inside(X, Y, sensor_beacon(A, B, C, D)) :-
    \+ the_same(X, Y, C, D),
    measure_distance(A, B, C, D, Radius),
    measure_distance(A, B, X, Y, Distance),
    Distance =< Radius.

is_inside_any(X, Y, [Sensor|Sensors]) :- is_inside(X, Y, Sensor), !.
is_inside_any(X, Y, [Sensor|Sensors]) :- \+ is_inside(X, Y, Sensor), is_inside_any(X, Y, Sensors).

mcount(4877672, Row, Acc, Acc, Sensors).

mcount(Col, Row, Acc, Result, Sensors) :-
    \+ is_inside_any(Col, Row, Sensors),
    Col1 is Col + 1,
    mcount(Col1, Row, Acc, Result, Sensors).

mcount(Col, Row, Acc, Result, Sensors) :-
    is_inside_any(Col, Row, Sensors),
    Acc1 is Acc + 1,
    Col1 is Col + 1,
    mcount(Col1, Row, Acc1, Result, Sensors).

sensor_beacon(2, 18,-2, 15).
sensor_beacon(9, 16,10, 16).
sensor_beacon(13, 2,15, 3).
sensor_beacon(12, 14,10, 16).
sensor_beacon(10, 20,10, 16).
sensor_beacon(14, 17,10, 16).
sensor_beacon(8, 7,2, 10).
sensor_beacon(2, 0,2, 10).
sensor_beacon(0, 11,2, 10).
sensor_beacon(20, 14,25, 17).
sensor_beacon(17, 20,21, 22).
sensor_beacon(16, 7,15, 3).
sensor_beacon(14, 3,15, 3).
sensor_beacon(20, 1,15, 3).

sample_input([sensor_beacon(2, 18,-2, 15),
       sensor_beacon(9, 16,10, 16),
       sensor_beacon(13, 2,15, 3),
       sensor_beacon(12, 14,10, 16),
       sensor_beacon(10, 20,10, 16),
       sensor_beacon(14, 17,10, 16),
       sensor_beacon(8, 7,2, 10),
       sensor_beacon(2, 0,2, 10),
       sensor_beacon(0, 11,2, 10),
       sensor_beacon(20, 14,25, 17),
       sensor_beacon(17, 20,21, 22),
       sensor_beacon(16, 7,15, 3),
       sensor_beacon(14, 3,15, 3),
       sensor_beacon(20, 1,15, 3)]).

input([sensor_beacon(3859432,2304903,3677247,3140958),
sensor_beacon(2488890,2695345,1934788,2667279),
sensor_beacon(3901948,701878,4095477,368031),
sensor_beacon(2422190,1775708,1765036,2000000),
sensor_beacon(2703846,3282799,2121069,3230302),
sensor_beacon(172003,2579074,-77667,3197309),
sensor_beacon(1813149,1311283,1765036,2000000),
sensor_beacon(1704453,2468117,1934788,2667279),
sensor_beacon(1927725,2976002,1934788,2667279),
sensor_beacon(3176646,1254463,2946873,2167634),
sensor_beacon(2149510,3722117,2121069,3230302),
sensor_beacon(3804434,251015,4095477,368031),
sensor_beacon(2613561,3932220,2121069,3230302),
sensor_beacon(3997794,3291220,3677247,3140958),
sensor_beacon(98328,3675176,-77667,3197309),
sensor_beacon(2006541,2259601,1934788,2667279),
sensor_beacon(663904,122919,1618552,-433244),
sensor_beacon(1116472,3349728,2121069,3230302),
sensor_beacon(2810797,2300748,2946873,2167634),
sensor_beacon(1760767,2024355,1765036,2000000),
sensor_beacon(3098487,2529092,2946873,2167634),
sensor_beacon(1716839,634872,1618552,-433244),
sensor_beacon(9323,979154,-245599,778791),
sensor_beacon(1737623,2032367,1765036,2000000),
sensor_beacon(26695,3049071,-77667,3197309),
sensor_beacon(3691492,3766350,3677247,3140958),
sensor_beacon(730556,1657010,1765036,2000000),
sensor_beacon(506169,3958647,-77667,3197309),
sensor_beacon(2728744,23398,1618552,-433244),
sensor_beacon(3215227,3077078,3677247,3140958),
sensor_beacon(2209379,3030851,2121069,3230302)]).
