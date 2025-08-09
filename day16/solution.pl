valve(ru, 0, [yh, id, on]).
valve(qk, 24, [pq, pp, on]).
valve(rp, 11, [rm, ba, ri, em, on]).
valve(bx, 0, [zx, vk, on]).
valve(jl, 0, [id, lc, on]).
valve(dc, 25, [st, on]).
valve(hx, 0, [dh, fe, on]).
valve(kj, 0, [zk, xn, on]).
valve(em, 0, [aw, rp, on]).
valve(xn, 7, [lh, kj, ku, ao, on]).
valve(dh, 9, [sy, cc, ql, lh, hx, on]).
valve(lh, 0, [xn, dh, on]).
valve(pp, 0, [qk, ta, on]).
valve(ao, 0, [aa, xn, on]).
valve(sy, 0, [dh, aa, on]).
valve(mz, 0, [jt, pf, on]).
valve(aa, 0, [jn, un, wg, sy, ao, on]).
valve(rm, 0, [xl, rp, on]).
valve(ba, 0, [rp, yp, on]).
valve(ad, 12, [lk, zx, aw, on]).
valve(zn, 0, [eq, hl, on]).
valve(ex, 18, [rb, on]).
valve(cr, 0, [ta, st, on]).
valve(wg, 0, [aa, ta, on]).
valve(un, 0, [wk, aa, on]).
valve(ve, 0, [ja, kw, on]).
valve(ja, 19, [pq, ve, on]).
valve(aw, 0, [ad, em, on]).
valve(xl, 0, [rm, pf, on]).
valve(od, 0, [vk, ri, on]).
valve(fe, 0, [jt, hx, on]).
valve(pq, 0, [ja, qk, on]).
valve(rb, 0, [cc, ex, on]).
valve(jt, 3, [rf, mz, zk, fe, dd, on]).
valve(yp, 0, [id, ba, on]).
valve(id, 14, [jl, ru, yp, on]).
valve(yh, 0, [ru, vk, on]).
valve(ta, 21, [wg, ku, pp, rf, cr, on]).
valve(lk, 0, [pf, ad, on]).
valve(dd, 0, [jn, jt, on]).
valve(hl, 0, [zn, dw, on]).
valve(vk, 22, [od, kw, bx, yh, on]).
valve(rf, 0, [jt, ta, on]).
valve(cc, 0, [rb, dh, on]).
valve(kw, 0, [ve, vk, on]).
valve(pf, 10, [wk, mz, ql, xl, lk, on]).
valve(zx, 0, [ad, bx, on]).
valve(jn, 0, [dd, aa, on]).
valve(st, 0, [cr, dc, on]).
valve(wk, 0, [pf, un, on]).
valve(dw, 13, [lc, hl, on]).
valve(zk, 0, [kj, jt, on]).
valve(ql, 0, [dh, pf, on]).
valve(ri, 0, [od, rp, on]).
valve(eq, 23, [zn, on]).
valve(lc, 0, [jl, dw, on]).
valve(ku, 0, [xn, ta, on]).

all_valves_on(C) :-
    append(_, [bb, on|_], C),
    append(_, [cc, on|_], C),
    append(_, [dd, on|_], C),
    append(_, [ee, on|_], C),
    append(_, [hh, on|_], C),
    append(_, [jj, on|_], C).

calculate_plan(P, S, Total, Total) :- length(P, S).
calculate_plan(Plan, Step, CurrentTotal, Total) :-
    append(Left, [C|Right], Plan),
    length(Left, Step),
    C \== on,
    NextStep is Step + 1,
    calculate_plan(Plan, NextStep, CurrentTotal, Total).
calculate_plan(Plan, Step, CurrentTotal, Total) :-
    append(Left, [on|Right], Plan),
    length(Left, Step),
    last(Left, V),
    valve(V, Release, _),
    NextTotal is CurrentTotal + (30 - Step) * Release,
    NextStep is Step + 1,
    calculate_plan(Plan, NextStep, NextTotal, Total).

move(_, [], []).
move(F, [on|To], []) :- append(Temp, [on], F).
move(F, [on|To], [T|Rest]) :- last(F, L), \+ append(Temp, [L, on], F), append(F, [on], T), move(F, To, Rest).
move(F, [T|To], [FT|Rest]) :- T \= on, \+ last(F, T), append(F, [T], FT), move(F, To, Rest).
move(F, [T|To], Rest) :- T \= on, last(F, T), move(F, To, Rest).

drop_on(B, B) :- \+append(A, [on], B).
drop_on(B, A) :- append(A, [on], B).

next_steps(C, NS) :- append(Temp, [on], C), append(Left, [R, on], C), valve(R, _, T), drop_on(T, Ts), move(C, Ts, NS).
next_steps(C, NS) :- append(Temp, [L], C), L \== on, append(Z, [L, on|_], C), valve(L, _, T), drop_on(T, Ts), move(C, Ts, NS).
next_steps(C, NS) :- append(Temp, [L], C), L \== on, \+ append(Z, [L, on|_], C), valve(L, _, T), move(C, T, NS).

next_steps_list([], []).
next_steps_list([C|R], Result) :- \+ all_valves_on(C), next_steps(C, NC), next_steps_list(R, NR), append(NC, NR, Result).
next_steps_list([C|R], Result) :- all_valves_on(C), next_steps_list(R, Result).

merge_sorted(A, [], [A]).
merge_sorted(A, [F|Rest], Sorted) :- calculate_plan(A, 0, 0, Ac), calculate_plan(F, 0, 0, Fc), Ac =< Fc, merge_sorted(A, Rest, Temp), append([F], Temp, Sorted).
merge_sorted(A, [F|Rest], Sorted) :- calculate_plan(A, 0, 0, Ac), calculate_plan(F, 0, 0, Fc), Ac > Fc, append([A, F], Rest, Sorted).

sort_plans([A], [A]).
sort_plans([A|As], Sorted) :-
    sort_plans(As, AsSorted),
    merge_sorted(A, AsSorted, Sorted).

truncate(L, Size, L) :- length(L, S), S < Size.
truncate(L, Size, NewL) :- length(L, S), S >= Size, length(NewL, Size), append(NewL, T, L).

play([P|Plans], Step, TargetSteps, Result) :- Step >= TargetSteps, calculate_plan(P, 0, 0, Result), write(P).
play(Plans, Step, TargetSteps, Result) :-
    write(Step),write('\n'),
    Step < TargetSteps,
    next_steps_list(Plans, NewP),
    sort_plans(NewP, NextP),
    truncate(NextP, 22, NewPlans),
    NextStep is Step + 1, play(NewPlans, NextStep, TargetSteps, Result).
