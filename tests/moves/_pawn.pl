:- begin_tests('moves/_pawn').
:- use_module('../../src/moves/_pawn').
:- use_module('../../src/utils/board_utils').
:- use_module('../test_utils').

test(valid_position, [forall(empty_generator(Coord)), setup(start_board(SB)), true(X = [Coord])]) :-
    '_pawn':valid_position(SB, Coord, X-[], yes).

invalid_position_generator(Coord) :- white_generator(Coord).
invalid_position_generator(Coord) :- black_generator(Coord).
invalid_position_generator(R/C  ) :- member(R, [0,9]), column(C).

test(invalid_position, [forall(invalid_position_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':valid_position(SB, Coord, []-[], no).

can_strike_invalid_position_generator(R/C) :- member(R, [0,9]), column(C).
can_strike_invalid_position_generator(R/C) :- member(C, [0,9]), column(R).

test(can_strike_invalid_position, [forall(can_strike_invalid_position_generator(Coord))]) :-
    '_pawn':can_strike(_, _, Coord, []-[]).

test(can_strike_en_passant, [forall(board_generator(Coord)), true(X = [Coord])]) :-
    '_pawn':can_strike(_, m(_, Coord), Coord, X-[]).

test(cant_strike_empty, [forall(empty_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':can_strike(SB, m(w, 0/0), Coord, []-[]).

test(cant_strike_own_color_white, [forall(white_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':can_strike(SB, m(w, 0/0), Coord, []-[]).

test(cant_strike_own_color_black, [forall(black_generator(Coord)), setup(start_board(SB))]) :-
    '_pawn':can_strike(SB, m(b, 0/0), Coord, []-[]).

test(can_strike_other_color_white, [forall(black_generator(Coord)), setup(start_board(SB)), true(X = [Coord])]) :-
    '_pawn':can_strike(SB, m(w, 0/0), Coord, X-[]).

test(can_strike_other_color_black, [forall(white_generator(Coord)), setup(start_board(SB)), true(X = [Coord])]) :-
    '_pawn':can_strike(SB, m(b, 0/0), Coord, X-[]).

test(get_plain_pawn_moves, [forall(empty_generator(Coord)), setup(start_board(SB)), true(X = [Coord])]) :-
    '_pawn':get_plain_pawn_moves(SB, m(_, 0/0), Coord, X-[], yes).

test(get_plain_pawn_moves, [setup(start_board(SB)), true(X = [3/2,3/1])]) :-
    set_piece_at(SB, 3/1, p(b, _), Out1),
    set_piece_at(Out1, 3/3, p(w, _), Out2),
    '_pawn':get_plain_pawn_moves(Out2, m(w, 0/0), 3/2, X-[], yes).

test(get_plain_pawn_moves, [setup(start_board(SB)), true(X = [3/2,3/3,3/1])]) :-
    set_piece_at(SB, 3/1, p(b, _), Out1),
    set_piece_at(Out1, 3/3, p(b, _), Out2),
    '_pawn':get_plain_pawn_moves(Out2, m(w, 0/0), 3/2, X-[], yes).

test(get_pawn_moves_w_start_positions, [forall(column(C)), setup(start_board(SB)), true(X = [3/C, 4/C])]) :-
    get_pawn_moves(SB, m(w, 0/0), 2/C, X-[]).

test(get_pawn_moves_w_middle_positions, [forall((member(R, [3,4,5]),column(C))), setup((start_board(SB), RPlus is R + 1)), true(X = [RPlus/C])]) :-
    get_pawn_moves(SB, m(w, 0/0), R/C, X-[]).

test(get_pawn_moves_w_attack_positions, [forall(member(C, [2,3,4,5,6,7])), setup((start_board(SB), CPlus is C + 1, CMin is C - 1)), true(X = [7/CPlus, 7/CMin])]) :-
    get_pawn_moves(SB, m(w, 0/0), 6/C, X-[]).

test(get_pawn_moves_b_start_positions, [forall(column(C)), setup(start_board(SB)), true(X = [6/C, 5/C])]) :-
    get_pawn_moves(SB, m(b, 0/0), 7/C, X-[]).

test(get_pawn_moves_b_middle_positions, [forall((member(R, [4,5,6]),column(C))), setup((start_board(SB), RMin is R - 1)), true(X = [RMin/C])]) :-
    get_pawn_moves(SB, m(b, 0/0), R/C, X-[]).

test(get_pawn_moves_b_attack_positions, [forall(member(C, [2,3,4,5,6,7])), setup((start_board(SB), CPlus is C + 1, CMin is C - 1)), true(X = [2/CPlus, 2/CMin])]) :-
    get_pawn_moves(SB, m(b, 0/0), 3/C, X-[]).

:- end_tests('moves/_pawn').