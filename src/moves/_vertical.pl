:- module('_vertical', [get_vertical_moves/4]).

:- use_module('../utils/board_utils').

get_vertical_moves(Board, Color, R/C, AboveMoves-T) :-
    RPlus is R + 1,
    RMin is R - 1,
    get_above_moves(Board, Color, RMin /C, AboveMoves-LowerMoves),
    get_lower_moves(Board, Color, RPlus/C, LowerMoves-T).

get_above_moves(_, _, 0/_, T-T) :- !.
get_above_moves(Board, Color, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_above_moves(Board, _    , R/C, [ R/C | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_above_moves(Board, Color, R/C, [ R/C | Moves ]-T) :-
    RMin is R - 1,
    get_above_moves(Board, Color, RMin/C, Moves-T).

get_lower_moves(_, _, 9/_, X-X) :- !.
get_lower_moves(Board, Color, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_lower_moves(Board, _    , R/C, [ R/C | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_lower_moves(Board, Color, R/C, [ R/C | Moves ]-T) :-
    RPlus is R + 1,
    get_lower_moves(Board, Color, RPlus/C, Moves-T).