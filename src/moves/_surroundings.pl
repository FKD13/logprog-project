:- module('_surroundings', [get_surrounding_moves/4]).

/** <module> _surroundings - Get surrounding moves.
*/

:- use_module('../moves/_utils').


%!  get_surrounding_moves(+Board, +Piece, +Coord, -Moves)
%
%   Get all possible surrounding moves from a given position.
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
get_surrounding_moves(Board, Piece, R/C, L1-L9) :-
    RMin  is R - 1, CMin  is C - 1,
    RPlus is R + 1, CPlus is C + 1,
    test_position(Board, Piece, R/C, RMin / CMin , L1-L2),
    test_position(Board, Piece, R/C, RMin / C    , L2-L3),
    test_position(Board, Piece, R/C, RMin / CPlus, L3-L4),
    test_position(Board, Piece, R/C, R    / CMin , L4-L5),
    test_position(Board, Piece, R/C, R    / CPlus, L5-L6),
    test_position(Board, Piece, R/C, RPlus/ CMin , L6-L7),
    test_position(Board, Piece, R/C, RPlus/ C    , L7-L8),
    test_position(Board, Piece, R/C, RPlus/ CPlus, L8-L9).