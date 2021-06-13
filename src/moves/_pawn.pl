:- module('_pawn', [get_pawn_moves/5]).

:- use_module('../utils/board_utils').

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


get_plain_pawn_moves(Board, Piece, OldCoord, R/C, Forward-T, Passant, Valid) :-
    CPlus is C + 1,
    CMin is C - 1,
    valid_position(Board, Piece, OldCoord, R/C, Forward-Strike1, Valid),
    can_strike(Board, Piece, OldCoord, Passant, R/CPlus, Strike1-Strike2),
    can_strike(Board, Piece, OldCoord, Passant, R/CMin, Strike2-T).


valid_position(_    , _    , _       , Coord, T-T, no ) :- outside_board(Coord), !.
valid_position(Board, _    , _       , Coord, T-T, no ) :- get_piece_at(Board, Coord, p(_, _)), !.
valid_position(_    , Piece, OldCoord, Coord, [ m(Piece, OldCoord, Coord) | T]-T, yes).

add_leap(Board, Piece, OldCoord, Coord, Moves, yes) :- valid_position(Board, Piece, OldCoord, Coord, Moves, _).
add_leap(_    , _    , _       , _    , T-T  , no ).

can_strike(_    , _          , _, _   , Coord, T-T                        ) :- outside_board(Coord), !.
can_strike(_    , Piece      , O, Pass, Pass , [ m(Piece, O, Pass) | T]-T ) :- !.
can_strike(Board, _          , _, _   , Coord, T-T                        ) :- get_piece_at(Board, Coord, empty      ), !.
can_strike(Board, p(Color, _), _, _   , Coord, T-T                        ) :- get_piece_at(Board, Coord, p(Color, _)), !.
can_strike(Board, Piece      , O, _   , Coord, [ m(Piece, O, Coord) | T]-T) :- get_piece_at(Board, Coord, p(_    , _)), !.
