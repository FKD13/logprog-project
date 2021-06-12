:- module('_pawn', [get_pawn_moves/4]).

:- use_module('../utils/board_utils').

get_pawn_moves(Board, m(b, Passant), 7/C, Plain-T) :-
    get_plain_pawn_moves(Board, m(b, Passant), 6/C, Plain-Leap, Valid),
    add_leap(Board, 5/C, Leap-T, Valid), !.

get_pawn_moves(Board, m(w, Passant), 2/C, Plain-T) :-
    get_plain_pawn_moves(Board, m(w, Passant), 3/C, Plain-Leap, Valid),
    add_leap(Board, 4/C, Leap-T, Valid), !.

get_pawn_moves(Board, m(w, Passant), R/C, Moves) :-
    RPlus is R + 1,
    get_plain_pawn_moves(Board, m(w, Passant), RPlus/C, Moves, _), !.

get_pawn_moves(Board, m(b, Passant), R/C, Moves) :-
    RMin is R - 1,
    get_plain_pawn_moves(Board, m(b, Passant), RMin/C, Moves, _), !.


get_plain_pawn_moves(Board, MetaData, R/C, Forward-T, Valid) :-
    CPlus is C + 1,
    CMin is C - 1,
    valid_poition(Board, R/C, Forward-Strike1, Valid),
    can_strike(Board, MetaData, R/CPlus, Strike1-Strike2),
    can_strike(Board, MetaData, R/CMin, Strike2-T).


valid_poition(_    , 9/_  , T-T           , no ) :- !.
valid_poition(_    , 0/_  , T-T           , no ) :- !.
valid_poition(Board, Coord, T-T           , no ) :- get_piece_at(Board, Coord, p(_, _)), !.
valid_poition(_    , Coord, [ Coord | T]-T, yes).

add_leap(Board, Coord, Moves, yes) :- valid_poition(Board, Coord, Moves, _).
add_leap(_    , _    , T-T  , no ).

can_strike(_    , _          , 9/_  , T-T           ) :- !.
can_strike(_    , _          , 0/_  , T-T           ) :- !.
can_strike(_    , _          , _/0  , T-T           ) :- !.
can_strike(_    , _          , _/9  , T-T           ) :- !.
can_strike(_    , m(_, Coord), Coord, [ Coord | T]-T) :- !.
can_strike(Board, _          , Coord, T-T           ) :- get_piece_at(Board, Coord, empty      ), !.
can_strike(Board, m(Color, _), Coord, T-T           ) :- get_piece_at(Board, Coord, p(Color, _)), !.
can_strike(Board, _          , Coord, [ Coord | T]-T) :- get_piece_at(Board, Coord, p(_    , _)), !.
