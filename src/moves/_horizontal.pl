:- module('_horizontal', [get_horizontal_moves/4]).

/** <module> _horizontal - Get horizontal moves.
*/

:- use_module('_utils').


left( R/C, R/CMin ) :- CMin  is C - 1.
right(R/C, R/CPlus) :- CPlus is C + 1.


%!  get_horizontal_moves(+Board, +Piece, +Coord, -Moves)
%
%   Get all possible horizontal moves from a given position.
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
get_horizontal_moves(Board, Piece, R/C, LeftMoves-T) :-
    CMin is C - 1, CPlus is C + 1,
    get_expanded_moves('_horizontal':left,  Board, Piece, R/C, R/CMin , LeftMoves-RightMoves),
    get_expanded_moves('_horizontal':right, Board, Piece, R/C, R/CPlus, RightMoves-T).