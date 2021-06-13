:- begin_tests('moves/_pawn').
:- use_module('../../src/moves/_pawn').
:- use_module('../../src/utils/board_utils').
:- use_module('../test_utils').

test(valid_position, [forall(empty_generator(Coord)), setup(start_board(SB)), true(X = [m(Piece, Old, Coord)])]) :-
    Piece = p(_, pawn),
    '_pawn':valid_position(SB, Piece, Old, Coord, X-[], yes).

invalid_position_generator(Coord) :- white_generator(Coord).
invalid_position_generator(Coord) :- black_generator(Coord).
invalid_position_generator(R/C  ) :- member(R, [0,9]), column(C).

test(invalid_position, [forall(invalid_position_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':valid_position(SB, _, _, Coord, []-[], no).

can_strike_invalid_position_generator(R/C) :- member(R, [0,9]), column(C).
can_strike_invalid_position_generator(R/C) :- member(C, [0,9]), column(R).

test(can_strike_invalid_position, [forall(can_strike_invalid_position_generator(Coord))]) :-
    '_pawn':can_strike(_, _, _, _, Coord, []-[]).

test(can_strike_en_passant, [forall(board_generator(Coord)), true(X = [m(Piece, Old, Coord)])]) :-
    Piece = p(w, pawn),
    '_pawn':can_strike(_, Piece, Old, Coord, Coord, X-[]).

test(cant_strike_empty, [forall(empty_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':can_strike(SB, p(w, pawn), _, 0/0, Coord, []-[]).

test(cant_strike_own_color_white, [forall(white_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':can_strike(SB, p(w, pawn), _, 0/0, Coord, []-[]).

test(cant_strike_own_color_black, [forall(black_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':can_strike(SB, p(b, pawn), _, 0/0, Coord, []-[]).

test(can_strike_other_color_white, [forall(black_generator(Coord)), setup(start_board(SB)), true(X = [m(Piece, Old, Coord)])]) :-
    Piece = p(w, pawn),
    '_pawn':can_strike(SB, Piece, Old, 0/0, Coord, X-[]).

test(can_strike_other_color_black, [forall(white_generator(Coord)), setup(start_board(SB)), true(X = [m(Piece, Old, Coord)])]) :-
    Piece = p(b, pawn),
    '_pawn':can_strike(SB, Piece, Old, 0/0, Coord, X-[]).

test(get_plain_pawn_moves, [forall(empty_generator(Coord)), setup(start_board(SB)), true(X = [m(p(_, pawn), Old, Coord)])]) :-
    '_pawn':get_plain_pawn_moves(SB, p(_, pawn), Old, Coord, X-[], 0/0, yes).

test(get_plain_pawn_moves, [setup(start_board(SB)), true(X = [m(p(w, pawn), Old, 3/2),m(p(w, pawn), Old, 3/1)])]) :-
    set_piece_at(SB, 3/1, p(b, _), Out1),
    set_piece_at(Out1, 3/3, p(w, _), Out2),
    '_pawn':get_plain_pawn_moves(Out2, p(w, pawn), Old, 3/2, X-[], 0/0, yes).

test(get_plain_pawn_moves, [setup(start_board(SB)), true(X = [m(p(w, pawn), Old, 3/2),m(p(w, pawn), Old, 3/3),m(p(w, pawn), Old, 3/1)])]) :-
    set_piece_at(SB, 3/1, p(b, _), Out1),
    set_piece_at(Out1, 3/3, p(b, _), Out2),
    '_pawn':get_plain_pawn_moves(Out2, p(w, pawn), Old, 3/2, X-[], 0/0, yes).

test(get_pawn_moves_w_start_positions, [forall(column(C)), setup(start_board(SB)), true(X = [m(p(w, pawn), 2/C, 3/C), m(p(w, pawn), 2/C, 4/C)])]) :-
    get_pawn_moves(SB, p(w, pawn), 2/C, X-[], 0/0).

test(get_pawn_moves_w_middle_positions, [forall((member(R, [3,4,5]),column(C))), setup((start_board(SB), RPlus is R + 1)), true(X = [m(p(w, pawn), R/C, RPlus/C)])]) :-
    get_pawn_moves(SB, p(w, pawn), R/C, X-[], 0/0).

test(get_pawn_moves_w_attack_positions, [forall(member(C, [2,3,4,5,6,7])), setup((start_board(SB), CPlus is C + 1, CMin is C - 1)), true(X = [m(p(w, pawn), 6/C, 7/CPlus), m(p(w, pawn), 6/C, 7/CMin)])]) :-
    get_pawn_moves(SB, p(w, pawn), 6/C, X-[], 0/0).

test(get_pawn_moves_b_start_positions, [forall(column(C)), setup(start_board(SB)), true(X = [m(p(b, pawn), 7/C, 6/C), m(p(b, pawn), 7/C, 5/C)])]) :-
    get_pawn_moves(SB, p(b, pawn), 7/C, X-[], 0/0).

test(get_pawn_moves_b_middle_positions, [forall((member(R, [4,5,6]),column(C))), setup((start_board(SB), RMin is R - 1)), true(X = [m(p(b, pawn), R/C, RMin/C)])]) :-
    get_pawn_moves(SB, p(b, pawn), R/C, X-[], 0/0).

test(get_pawn_moves_b_attack_positions, [forall(member(C, [2,3,4,5,6,7])), setup((start_board(SB), CPlus is C + 1, CMin is C - 1)), true(X = [m(p(b, pawn), 3/C, 2/CPlus), m(p(b, pawn), 3/C, 2/CMin)])]) :-
    get_pawn_moves(SB, p(b, pawn), 3/C, X-[], 0/0).

:- end_tests('moves/_pawn').