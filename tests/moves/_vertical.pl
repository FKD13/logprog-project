:- begin_tests('moves/_vertical').
:- use_module('../../src/moves/_vertical').
:- use_module('../test_utils').

test(get_vertical_moves, [setup(start_board(SB)), true(X = [4/3,5/3,6/3,7/3])]) :-
    get_vertical_moves(SB, w, 3/3, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [3/7,5/7,6/7,7/7])]) :-
    get_vertical_moves(SB, w, 4/7, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [6/3,5/3,4/3,3/3,8/3])]) :-
    get_vertical_moves(SB, w, 7/3, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_vertical_moves(SB, w, 1/1, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_vertical_moves(SB, w, 1/8, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [7/8])]) :-
    get_vertical_moves(SB, w, 8/8, X-[]).

test(get_vertical_moves_corners, [setup(start_board(SB)), true(X = [7/1])]) :-
    get_vertical_moves(SB, w, 8/1, X-[]).

:- end_tests('moves/_vertical').