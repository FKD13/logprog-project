:- begin_tests('moves/_diagonal').
:- use_module('../../src/moves/_diagonal').
:- use_module('../test_utils').

test(get_diagonal_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,4/4),m(p(w,_),Old,5/5),m(p(w,_),Old,6/6),m(p(w,_),Old,7/7),m(p(w,_),Old,4/2),m(p(w,_),Old,5/1)])]) :-
    Old = 3/3,
    get_diagonal_moves(SB, p(w, _), Old, X-[]).

test(get_diagonal_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,3/6),m(p(w,_),Old,3/8),m(p(w,_),Old,5/8),m(p(w,_),Old,5/6),m(p(w,_),Old,6/5),m(p(w,_),Old,7/4)])]) :-
    Old = 4/7,
    get_diagonal_moves(SB, p(w, _), Old, X-[]).

test(get_diagonal_moves, [setup(start_board(SB)), true(X = [m(p(w,_),Old,6/2),m(p(w,_),Old,5/1),m(p(w,_),Old,6/4),m(p(w,_),Old,5/5),m(p(w,_),Old,4/6),m(p(w,_),Old,3/7),m(p(w,_),Old,8/4),m(p(w,_),Old,8/2)])]) :-
    Old = 7/3,
    get_diagonal_moves(SB, p(w, _), Old, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_diagonal_moves(SB, p(w, _), 1/1, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_diagonal_moves(SB, p(w, _), 1/8, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/7)])]) :-
    Old = 8/8,
    get_diagonal_moves(SB, p(w, _), Old, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [m(p(w,_),Old,7/2)])]) :-
    Old = 8/1,
    get_diagonal_moves(SB, p(w, _), Old, X-[]).

:- end_tests('moves/_diagonal').