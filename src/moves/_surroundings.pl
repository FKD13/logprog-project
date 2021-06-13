:- module('_surroundings', [get_surrounding_moves/4]).

:- use_module('../moves/_utils').

/** <module> Get surrounding moves.
 */

/** get_surrounding_moves(+Board, +Piece, +Coard, -Positions)
 *
 * Get sourounding moves for a certain position.
 */
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