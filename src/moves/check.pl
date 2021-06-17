:- module('state', [check/3]).

:- use_module('../utils/board_utils').

check(Board, Color, Moves) :-
    get_piece_at(Board, Coord, p(Color, king)),
    member(m(_, _, Coord), Moves).