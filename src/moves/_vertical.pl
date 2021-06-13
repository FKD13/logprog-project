:- module('_vertical', [get_vertical_moves/4]).

/** <module> _vertical - Get vertical moves.
*/

:- use_module('_utils').


up(  R/C, RPlus/C) :- RPlus is R + 1.
down(R/C, RMin/C ) :- RMin  is R - 1.


%!  get_vertical_moves(+Board, +Piece, +Coord, -Moves)
%
%   Get all possible vertical moves from a given position.
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
get_vertical_moves(Board, Piece, R/C, AboveMoves-T) :-
    RPlus is R + 1, RMin is R - 1,
    get_expanded_moves('_vertical':up  , Board, Piece, R/C, RPlus /C, AboveMoves-LowerMoves),
    get_expanded_moves('_vertical':down, Board, Piece, R/C, RMin/C, LowerMoves-T).