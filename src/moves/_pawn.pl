:- module('_pawn', [get_pawn_moves/5]).

/** <module> _pawn - Get pawn moves.
*/

:- use_module('../utils/board_utils').


%!  get_pawn_moves(+Board, +Piece, +Coord, -Moves, +Passant)
%
%   Get all possible pawn moves from a given position.
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
%   @arg Passant The possible location of the en-passant.
get_pawn_moves(Board, p(b, pawn), 7/C, Plain-T, Passant) :-
    get_plain_pawn_moves(Board, p(b, pawn), 7/C, 6/C, Plain-Leap, Passant, Valid),
    add_leap(Board, p(b, pawn), 7/C, 5/C, Leap-T, Valid), !.

get_pawn_moves(Board, p(w, pawn), 2/C, Plain-T, Passant) :-
    get_plain_pawn_moves(Board, p(w, pawn), 2/C, 3/C, Plain-Leap, Passant, Valid),
    add_leap(Board, p(w, pawn), 2/C, 4/C, Leap-T, Valid), !.

get_pawn_moves(Board, p(w, pawn), R/C, Moves, Passant) :-
    RPlus is R + 1,
    get_plain_pawn_moves(Board, p(w, pawn), R/C, RPlus/C, Moves, Passant, _), !.

get_pawn_moves(Board, p(b, pawn), R/C, Moves, Passant) :-
    RMin is R - 1,
    get_plain_pawn_moves(Board, p(b, pawn), R/C, RMin/C, Moves, Passant, _), !.


%!  get_plain_pawn_moves(+Board, +Piece, +OldCoord, +Coord, -Moves, +Passant, -Leap)
%
%   Get all possible pawn moves that are always possible (no far leap at the start)
%   for a given position. If the forward position is free and a Leap is possible Leap 
%   will have value yes. Otherwise it will be no.
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg OldCoord The current location of the Piece.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
%   @arg Passant The possible location of the en-passant.
%   @arg Leap A leap move is possible.
get_plain_pawn_moves(Board, Piece, OldCoord, R/C, Forward-T, Passant, Valid) :-
    CPlus is C + 1,
    CMin is C - 1,
    valid_position(Board, Piece, OldCoord, R/C, Forward-Strike1, Valid),
    can_strike(Board, Piece, OldCoord, Passant, R/CPlus, Strike1-Strike2),
    can_strike(Board, Piece, OldCoord, Passant, R/CMin, Strike2-T).


%!  valid_position(+Board, +Piece, +OldCoord, +Coord, -Moves, -Leap)
%
%   If the position at Coord is empty add Coord to the list of possible moves.
%   (Position is not blocked by any piece)
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg OldCoord The current location of the Piece.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
%   @arg Leap A leap move is possible.
valid_position(_    , _    , _, Coord, T-T  , no ) :- outside_board(Coord), !.
valid_position(Board, _    , _, Coord, T-T  , no ) :- get_piece_at(Board, Coord, p(_, _)), !.
valid_position(_    , Piece, O, Coord, Moves, yes) :- promote(Piece, O, Coord, Moves).


%!  add_leap(+Board, +Piece, +OldCoord, +Coord, -Moves, +Leap)
%
%   If the position at Coord is empty and a Leap is possible add Coord to the 
%   list of possible moves. (Position is not blocked by any piece)
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg OldCoord The current location of the Piece.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
%   @arg Leap A leap move is possible.
add_leap(Board, Piece, OldCoord, Coord, Moves, yes) :- valid_position(Board, Piece, OldCoord, Coord, Moves, _).
add_leap(_    , _    , _       , _    , T-T  , no ).

%!  can_strike(+Board, +Piece, +OldCoord, +Passant, +Coord, -Moves)
%
%   Add Coord to possible moves if an en Passant possible at this position
%   or there is an enemy piece at this position.
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg OldCoord The current location of the Piece.
%   @arg Passant The possible location of the en-passant.
%   @arg Coord The location of the piece.
%   @arg Moves The possible moves. This is a difference list.
can_strike(_    , _          , _, _   , Coord, T-T                       ) :- outside_board(Coord), !.
can_strike(_    , Piece      , O, Pass, Pass , [ m(Piece, O, Pass) | T]-T) :- !.
can_strike(Board, _          , _, _   , Coord, T-T                       ) :- get_piece_at(Board, Coord, empty      ), !.
can_strike(Board, p(Color, _), _, _   , Coord, T-T                       ) :- get_piece_at(Board, Coord, p(Color, _)), !.
can_strike(Board, Piece      , O, _   , Coord, Moves                     ) :- get_piece_at(Board, Coord, p(_    , _)), promote(Piece, O, Coord, Moves), !.

promote(p(Color, pawn), O, R/C, Moves) :-
    (R = 1; R = 8),
    Moves = [
        m(p(Color, queen) , O, R/C),
        m(p(Color, knight), O, R/C), 
        m(p(Color, bishop), O, R/C), 
        m(p(Color, rook)  , O, R/C)
        | T]-T, !.
promote(Piece, O, Coord, [m(Piece, O, Coord) | T]-T).