:- begin_tests('search/_score').

:- use_module('../../src/search/_score').
:- use_module('../../src/utils/board_utils').
:- use_module('../test_utils').

test(get_score_empty_board_w, [setup(empty_board(EB)), true(Score = 0)]) :-
    get_score(EB, w, Score).

test(get_score_empty_board_b, [setup(empty_board(EB)), true(Score = 0)]) :-
    get_score(EB, b, Score).

test(get_score_start_board_w, [setup(start_board(SB)), true(Score = 0)]) :-
    get_score(SB, w, Score).

test(get_score_start_board_b, [setup(start_board(SB)), true(Score = 0)]) :-
    get_score(SB, b, Score).

test(get_score_start_custom_w, [setup(start_board(SB)), true('_score':score_piece(pawn, NegScore))]) :-
    set_piece_at(SB, 2/2, empty, Out),
    get_score(Out, w, Score),
    NegScore is -Score.

test(get_score_start_custom_b, [setup(start_board(SB)), true('_score':score_piece(pawn, Score))]) :-
    set_piece_at(SB, 2/2, empty, Out),
    get_score(Out, b, Score).

:- end_tests('search/_score').