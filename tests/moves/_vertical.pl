:- begin_tests('moves/_vertical').
:- use_module('../../src/moves/_vertical').
:- use_module('../test_utils').

test(get_vertical_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,4/3),m(p(w,_),Old,5/3),m(p(w,_),Old,6/3),m(p(w,_),Old,7/3)])]) :-
    Old = 3/3,
    get_vertical_moves(SB, p(w, _), Old, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,3/7),m(p(w,_),Old,5/7),m(p(w,_),Old,6/7),m(p(w,_),Old,7/7)])]) :-
    Old = 4/7,
    get_vertical_moves(SB, p(w, _), Old, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,6/3),m(p(w,_),Old,5/3),m(p(w,_),Old,4/3),m(p(w,_),Old,3/3),m(p(w,_),Old,8/3)])]) :-
    Old = 7/3,
    get_vertical_moves(SB, p(w, _), Old, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_vertical_moves(SB, p(w, _), 1/1, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_vertical_moves(SB, p(w, _), 1/8, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/8)])]) :-
    Old = 8/8,
    get_vertical_moves(SB, p(w, _), Old, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/1)])]) :-
    Old = 8/1,
    get_vertical_moves(SB, p(w, _), Old, X-[]).

:- end_tests('moves/_vertical').