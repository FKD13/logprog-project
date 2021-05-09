:- module('_vertical', [get_vertical_moves/3]).

get_vertical_moves(Board, R/C, AboveMoves-T) :-
    RPlus is R + 1,
    RMin is R - 1,
    get_above_moves(Board, RMin/C, AboveMoves-LowerMoves),
    get_lower_moves(Board, RPlus/C, LowerMoves-T).

get_above_moves(_, 0/_, X-X) :- !.
get_above_moves(Board, R/C, [ R/C | Moves ]-T) :- 
    arg(R, Board, Row),
    arg(C, Row, empty),
    RMin is R - 1,
    get_above_moves(Board, RMin/C, Moves-T), !.
get_above_moves(Board, R/C, [ R/C | T ]-T) :-
    arg(R, Board, Row),
    \+ arg(C, Row, empty).

get_lower_moves(_, 9/_, X-X) :- !.
get_lower_moves(Board, R/C, [ R/C | Moves ]-T) :- 
    arg(R, Board, Row),
    arg(C, Row, empty),
    RPlus is R + 1,
    get_lower_moves(Board, RPlus/C, Moves-T), !.
get_lower_moves(Board, R/C, [ R/C | T ]-T) :-
    arg(R, Board, Row),
    \+ arg(C, Row, empty).