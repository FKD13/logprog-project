:- module('_utils', [test_position/5]).

:- use_module('../utils/board_utils').

test_position(_    , _          , _, R/_  , T-T) :- R < 1, !.
test_position(_    , _          , _, R/_  , T-T) :- R > 8, !.
test_position(_    , _          , _, _/C  , T-T) :- C < 1, !.
test_position(_    , _          , _, _/C  , T-T) :- C > 8, !.
test_position(Board, p(Color, _), _, Coord, T-T) :- get_piece_at(Board, Coord, p(Color, _)), !.
test_position(_    , Piece      , O, Coord, [ m(Piece, O, Coord) | T]-T).