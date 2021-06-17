:- module('state', [check/3]).

/** <module> check - Is check?
*/

:- use_module('../utils/board_utils').


%!  check(+Board, +Color, +Moves)
%
%   Given the moves of the enemy player, unify if the current player is check.
%
%   @arg Board The board to check available moves on.
%   @arg Color The color of the current player.
%   @arg Moves The possible moves. This is a difference list.
check(Board, Color, Moves) :-
    get_piece_at(Board, Coord, p(Color, king)),
    member(m(_, _, Coord), Moves).