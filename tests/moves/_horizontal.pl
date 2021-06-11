:- begin_tests('moves/_horizontal').
:- use_module('../../src/moves/_horizontal').
:- use_module('../test_utils').

test(get_horizontal_moves, [setup(start_board(SB)), true(X = [3/2,3/1,3/4,3/5,3/6,3/7,3/8])]) :-
    get_horizontal_moves(SB, w, 3/3, X-[]).

test(get_horizontal_moves, [setup(start_board(SB)), true(X = [4/6,4/5,4/4,4/3,4/2,4/1,4/8])]) :-
    get_horizontal_moves(SB, w, 4/7, X-[]).

test(get_horizontal_moves, [setup(start_board(SB)), true(X = [7/2,7/4])]) :-
    get_horizontal_moves(SB, w, 7/3, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_horizontal_moves(SB, w, 1/1, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_horizontal_moves(SB, w, 1/8, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [8/7])]) :-
    get_horizontal_moves(SB, w, 8/8, X-[]).

test(get_horizontal_moves_corners, [setup(start_board(SB)), true(X = [8/2])]) :-
    get_horizontal_moves(SB, w, 8/1, X-[]).

:- end_tests('moves/_horizontal').