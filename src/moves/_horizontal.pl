:- module('_horizontal', [get_horizontal_moves/4]).

:- use_module('_utils').

left( R/C, R/CMin ) :- CMin  is C - 1.
right(R/C, R/CPlus) :- CPlus is C + 1.

get_horizontal_moves(Board, Piece, R/C, LeftMoves-T) :-
    CMin is C - 1, CPlus is C + 1,
    get_expanded_moves('_horizontal':left,  Board, Piece, R/C, R/CMin , LeftMoves-RightMoves),
    get_expanded_moves('_horizontal':right, Board, Piece, R/C, R/CPlus, RightMoves-T).