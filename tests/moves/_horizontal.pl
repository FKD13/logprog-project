:- begin_tests('moves/_horizontal').
:- use_module('../../src/moves/_horizontal').
:- use_module('../test_utils').

test(get_horizontal_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,3/2),m(p(w,_),Old,3/1),m(p(w,_),Old,3/4),m(p(w,_),Old,3/5),m(p(w,_),Old,3/6),m(p(w,_),Old,3/7),m(p(w,_),Old,3/8)])]) :-
    Old = 3/3,
    get_horizontal_moves(SB, p(w, _), Old, X-[]).

test(get_horizontal_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,4/6),m(p(w,_),Old,4/5),m(p(w,_),Old,4/4),m(p(w,_),Old,4/3),m(p(w,_),Old,4/2),m(p(w,_),Old,4/1),m(p(w,_),Old,4/8)])]) :-
    Old = 4/7,
    get_horizontal_moves(SB, p(w, _), Old, X-[]).

test(get_horizontal_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/2),m(p(w,_),Old,7/4)])]) :-
    Old = 7/3,
    get_horizontal_moves(SB, p(w, _), Old, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_horizontal_moves(SB, p(w, _), 1/1, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_horizontal_moves(SB, p(w, _), 1/8, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,8/7)])]) :-
    Old = 8/8,
    get_horizontal_moves(SB, p(w, _), Old, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,8/2)])]) :-
    Old = 8/1,
    get_horizontal_moves(SB, p(w, _), Old, X-[]).

:- end_tests('moves/_horizontal').