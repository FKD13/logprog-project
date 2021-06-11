:- module('_surroundings', [get_surrounding_moves/4]).
:- use_module('../utils/board_utils').

/** <module> Get surrounding moves.
 */

/** get_surrounding_moves(+Board, +Color, +Coard, -Positions)
 *
 * Get sourounding moves for a certain position.
 */
get_surrounding_moves(Board, Color, R/C, L1-L9) :-
    RMin  is R - 1,
    RPlus is R + 1,
    CMin  is C - 1,
    CPlus is C + 1,
    test_position(Board, Color, RMin / CMin , L1-L2),
    test_position(Board, Color, RMin / C    , L2-L3),
    test_position(Board, Color, RMin / CPlus, L3-L4),
    test_position(Board, Color, R    / CMin , L4-L5),
    test_position(Board, Color, R    / CPlus, L5-L6),
    test_position(Board, Color, RPlus/ CMin , L6-L7),
    test_position(Board, Color, RPlus/ C    , L7-L8),
    test_position(Board, Color, RPlus/ CPlus, L8-L9).

test_position(_, _, _/0, T-T) :- !.
test_position(_, _, 0/_, T-T) :- !.
test_position(_, _, _/9, T-T) :- !.
test_position(_, _, 9/_, T-T) :- !.
test_position(Board, Color, Coord, T-T) :- 
    get_piece_at(Board, Coord, p(Color, _)), !.
test_position(_, _, Coord, [Coord | T]-T).