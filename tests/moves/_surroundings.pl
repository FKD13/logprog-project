:- begin_tests('moves/_surroundings').
:- use_module('../../src/moves/_surroundings').
:- use_module('../test_utils').

test(get_surrounding_moves, [setup(start_board(SB)), true(X = [3/2,3/4,4/2,4/3,4/4])]) :-
    get_surrounding_moves(SB, w, 3/3, X-[]).

test(get_surrounding_moves, [setup(start_board(SB)), true(X = [3/3,3/4,3/5,4/3,4/5,5/3,5/4,5/5])]) :-
    get_surrounding_moves(SB, w, 4/4, X-[]).

test(get_surrounding_moves, [setup(start_board(SB)), true(X = [5/4,5/5,5/6,6/4,6/6,7/4,7/5,7/6])]) :-
    get_surrounding_moves(SB, w, 6/5, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_surrounding_moves(SB, w, 1/1, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [])]) :-
    get_surrounding_moves(SB, w, 1/8, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [7/7,7/8,8/7])]) :-
    get_surrounding_moves(SB, w, 8/8, X-[]).

test(get_surrounding_moves_corners, [setup(start_board(SB)), true(X = [7/1,7/2,8/2])]) :-
    get_surrounding_moves(SB, w, 8/1, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [])]) :-
    get_surrounding_moves(SB, w, 1/4, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [7/3,7/4,7/5,8/3,8/5])]) :-
    get_surrounding_moves(SB, w, 8/4, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [3/1,3/2,4/2,5/1,5/2])]) :-
    get_surrounding_moves(SB, w, 4/1, X-[]).

test(get_surrounding_moves_sides, [setup(start_board(SB)), true(X = [3/7,3/8,4/7,5/7,5/8])]) :-
    get_surrounding_moves(SB, w, 4/8, X-[]).

:- end_tests('moves/_surroundings').