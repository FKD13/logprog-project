:- begin_tests('moves/_diagonal').
:- use_module('../../src/moves/_diagonal').
:- use_module('../test_utils').

test(get_diagonal_moves, [setup(start_board(SB)), true(X = [4/4,5/5,6/6,7/7,4/2,5/1])]) :-
    get_diagonal_moves(SB, w, 3/3, X-[]).

test(get_diagonal_moves, [setup(start_board(SB)), true(X = [3/6,3/8,5/8,5/6,6/5,7/4])]) :-
    get_diagonal_moves(SB, w, 4/7, X-[]).

test(get_diagonal_moves, [setup(start_board(SB)), true(X = [6/2,5/1,6/4,5/5,4/6,3/7,8/4,8/2])]) :-
    get_diagonal_moves(SB, w, 7/3, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_diagonal_moves(SB, w, 1/1, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_diagonal_moves(SB, w, 1/8, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [7/7])]) :-
    get_diagonal_moves(SB, w, 8/8, X-[]).

test(get_diagonal_moves_corners, [setup(start_board(SB)), true(X = [7/2])]) :-
    get_diagonal_moves(SB, w, 8/1, X-[]).

:- end_tests('moves/_diagonal').