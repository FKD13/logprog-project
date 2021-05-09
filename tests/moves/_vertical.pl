:- begin_tests('moves/_vertical').
:- use_module('../../src/moves/_vertical').
:- use_module('../test_utils').

test(get_vertical_moves, [setup(start_board(SB)), true(X = [2/3,4/3,5/3,6/3,7/3])]) :-
    get_vertical_moves(SB, 3/3, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [3/7,2/7,5/7,6/7,7/7])]) :-
    get_vertical_moves(SB, 4/7, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [6/3,5/3,4/3,3/3,2/3,8/3])]) :-
    get_vertical_moves(SB, 7/3, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [2/1])]) :-
    get_vertical_moves(SB, 1/1, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [2/8])]) :-
    get_vertical_moves(SB, 1/8, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [7/8])]) :-
    get_vertical_moves(SB, 8/8, X-[]).

test(get_vertical_moves, [setup(start_board(SB)), true(X = [7/1])]) :-
    get_vertical_moves(SB, 8/1, X-[]).

:- end_tests('moves/_vertical').