:- module('check', [check/2]).

:- use_module('moves').
:- use_module('../utils/board_utils').
:- use_module('../utils/color').

check(Board, Color) :-
    get_piece_at(Board, Coord, p(Color, king)),
    next_color(Color, NextColor),
    get_moves(Board, NextColor, Moves-[]),
    member(m(_, _, Coord), Moves).