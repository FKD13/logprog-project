:- begin_tests('moves/_surroundings').
:- use_module('../../src/moves/_surroundings').
:- use_module('../test_utils').

test(get_surrounding_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,3/2),m(p(w,_),Old,3/4),m(p(w,_),Old,4/2),m(p(w,_),Old,4/3),m(p(w,_),Old,4/4)])]) :-
    Old = 3/3,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,3/3),m(p(w,_),Old,3/4),m(p(w,_),Old,3/5),m(p(w,_),Old,4/3),m(p(w,_),Old,4/5),m(p(w,_),Old,5/3),m(p(w,_),Old,5/4),m(p(w,_),Old,5/5)])]) :-
    Old = 4/4,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,5/4),m(p(w,_),Old,5/5),m(p(w,_),Old,5/6),m(p(w,_),Old,6/4),m(p(w,_),Old,6/6),m(p(w,_),Old,7/4),m(p(w,_),Old,7/5),m(p(w,_),Old,7/6)])]) :-
    Old = 6/5,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    Old = 1/1,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    Old = 1/8,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/7),m(p(w,_),Old,7/8),m(p(w,_),Old,8/7)])]) :-
    Old = 8/8,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/1),m(p(w,_),Old,7/2),m(p(w,_),Old,8/2)])]) :-
    Old = 8/1,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [])]) :-
    Old = 1/4,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/3),m(p(w,_),Old,7/4),m(p(w,_),Old,7/5),m(p(w,_),Old,8/3),m(p(w,_),Old,8/5)])]) :-
    Old = 8/4,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [m(p(w,_),Old,3/1),m(p(w,_),Old,3/2),m(p(w,_),Old,4/2),m(p(w,_),Old,5/1),m(p(w,_),Old,5/2)])]) :-
    Old = 4/1,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [m(p(w,_),Old,3/7),m(p(w,_),Old,3/8),m(p(w,_),Old,4/7),m(p(w,_),Old,5/7),m(p(w,_),Old,5/8)])]) :-
    Old = 4/8,
    get_surrounding_moves(SB, p(w, _), Old, X-[]).

:- end_tests('moves/_surroundings').