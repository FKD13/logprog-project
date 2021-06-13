:- module('_utils', [test_position/5, get_expanded_moves/6]).

:- use_module('../utils/board_utils').

outside_board(R/_) :- R < 1, !.
outside_board(R/_) :- R > 8, !.
outside_board(_/C) :- C < 1, !.
outside_board(_/C) :- C > 8, !.

test_position(_    , _          , _, Coord, T-T) :- outside_board(Coord), !.
test_position(Board, p(Color, _), _, Coord, T-T) :- get_piece_at(Board, Coord, p(Color, _)), !.
test_position(_    , Piece      , O, Coord, [ m(Piece, O, Coord) | T]-T).

get_expanded_moves(_       , _    , _          , _, Coord, X-X) :- outside_board(Coord), !.
get_expanded_moves(_       , Board, p(Color, _), _, R/C  , T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_expanded_moves(_       , Board, Piece      , O, R/C  , [ m(Piece, O, R/C) | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_expanded_moves(Expander, Board, Piece      , O, Coord, [ m(Piece, O, Coord) | Moves ]-T) :-
    call(Expander, Coord, NewCoord),
    get_expanded_moves(Expander, Board, Piece, O, NewCoord, Moves-T).