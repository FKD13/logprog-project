:- module('_vertical', [get_vertical_moves/4]).

:- use_module('../utils/board_utils').

get_vertical_moves(Board, Piece, R/C, AboveMoves-T) :-
    RPlus is R + 1,
    RMin is R - 1,
    get_above_moves(Board, Piece, R/C, RMin /C, AboveMoves-LowerMoves),
    get_lower_moves(Board, Piece, R/C, RPlus/C, LowerMoves-T).

get_above_moves(_    , _          , _, 0/_, T-T) :- !.
get_above_moves(Board, p(Color, _), _, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_above_moves(Board, Piece      , O, R/C, [ m(Piece, O, R/C) | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_above_moves(Board, Piece      , O, R/C, [ m(Piece, O, R/C) | Moves ]-T) :-
    RMin is R - 1,
    get_above_moves(Board, Piece, O, RMin/C, Moves-T).

get_lower_moves(_    , _          , _, 9/_, X-X) :- !.
get_lower_moves(Board, p(Color, _), _, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_lower_moves(Board, Piece      , O, R/C, [ m(Piece, O, R/C) | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_lower_moves(Board, Piece      , O, R/C, [ m(Piece, O, R/C) | Moves ]-T) :-
    RPlus is R + 1,
    get_lower_moves(Board, Piece, O, RPlus/C, Moves-T).