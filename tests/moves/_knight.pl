:- begin_tests('moves/_knight').

:- use_module('../../src/moves/_knight').
:- use_module('../../src/utils/board_utils').
:- use_module('../test_utils').

test(get_knight_moves_corner, [setup(empty_board(EB)), true(X = [m(p(_, knight), Old, 3/2),m(p(_, knight), Old, 2/3)])]) :-
    Old = 1/1,
    get_knight_moves(EB, p(_, knight), Old, X-[]).

test(get_knight_moves_corner, [setup(empty_board(EB)), true(X = [m(p(_, knight), Old, 2/6),m(p(_, knight), Old, 3/7)])]) :-
    Old = 1/8,
    get_knight_moves(EB, p(_, knight), Old, X-[]).

test(get_knight_moves_corner, [setup(empty_board(EB)), true(X = [m(p(_, knight), Old, 7/6),m(p(_, knight), Old, 6/7)])]) :-
    Old = 8/8,
    get_knight_moves(EB, p(_, knight), Old, X-[]).

test(get_knight_moves_corner, [setup(empty_board(EB)), true(X = [m(p(_, knight), Old, 6/2),m(p(_, knight), Old, 7/3)])]) :-
    Old = 8/1,
    get_knight_moves(EB, p(_, knight), Old, X-[]).

test(get_knight_moves, [setup(empty_board(EB)), true(X = [m(p(_, knight), Old, 6/5),m(p(_, knight), Old, 2/5),m(p(_, knight), Old, 5/6),m(p(_, knight), Old, 3/6),m(p(_, knight), Old, 5/2),m(p(_, knight), Old, 3/2),m(p(_, knight), Old, 6/3),m(p(_, knight), Old, 2/3)])]) :-
    Old = 4/4,
    get_knight_moves(EB, p(_, knight), Old, X-[]).

:- end_tests('moves/_knight').