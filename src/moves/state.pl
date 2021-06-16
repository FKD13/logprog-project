:- module('state', [state/4]).

:- use_module('moves').
:- use_module('../utils/board_utils').
:- use_module('../utils/color').

state(_    , _    , []   , stalemate).
state(Board, Color, Moves, check    ) :- check(Board, Color, Moves).
state(_    , _    , _    , playing  ).

check(Board, Color, Moves) :-
    get_piece_at(Board, Coord, p(Color, king)),
    member(m(_, _, Coord), Moves).