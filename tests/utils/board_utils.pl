:- begin_tests('utils/board_utils').
:- use_module('../../src/utils/board_utils').
:- use_module('../test_utils').

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,rook))]) :-
    get_piece_at(SB, 1/1, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,knight))]) :-
    get_piece_at(SB, 1/2, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,bishop))]) :-
    get_piece_at(SB, 1/3, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,queen))]) :-
    get_piece_at(SB, 1/4, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,king))]) :-
    get_piece_at(SB, 1/5, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,bishop))]) :-
    get_piece_at(SB, 1/6, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,knight))]) :-
    get_piece_at(SB, 1/7, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(w,rook))]) :-
    get_piece_at(SB, 1/8, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), set(Piece = [p(w,pawn)])]) :-
    get_piece_at(SB, 2/_, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,rook))]) :-
    get_piece_at(SB, 8/1, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,knight))]) :-
    get_piece_at(SB, 8/2, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,bishop))]) :-
    get_piece_at(SB, 8/3, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,queen))]) :-
    get_piece_at(SB, 8/4, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,king))]) :-
    get_piece_at(SB, 8/5, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,bishop))]) :-
    get_piece_at(SB, 8/6, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,knight))]) :-
    get_piece_at(SB, 8/7, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), true(Piece = p(b,rook))]) :-
    get_piece_at(SB, 8/8, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), set(Piece = [p(b,pawn)])]) :-
    get_piece_at(SB, 7/_, Piece).

test(get_piece_at_start_board, [setup(start_board(SB)), set(Piece = [empty])]) :-
    member(X, [3, 4, 5, 6]),
    get_piece_at(SB, X/_, Piece).

test(set_piece_at, [setup(empty_board(EB))]) :-
    member(R, [1, 2, 3, 4, 5, 6, 7, 8]),
    member(C, [1, 2, 3, 4, 5, 6, 7, 8]),
    set_piece_at(EB, R/C, X, Out),
    get_piece_at(Out, R/C, X).

:- end_tests('utils/board_utils').