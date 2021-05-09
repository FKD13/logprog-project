:- module('_horizontal', [get_horizontal_moves/3]).

get_horizontal_moves(Board, R/C, LeftMoves-T) :-
    arg(R, Board, Row),
    CMin is C - 1,
    CPlus is C + 1,
    get_left_moves(Row, R/CMin, LeftMoves-RightMoves),
    get_right_moves(Row, R/CPlus, RightMoves-T).
    
get_left_moves(_, _/0, X-X).
get_left_moves(Row, R/C, [ R/C | Moves ]-T) :-
    arg(C, Row, empty),
    CMin is C - 1,
    get_left_moves(Row, R/CMin, Moves-T), !.
get_left_moves(Row, R/C, [ R/C | T ]-T) :-
    \+ arg(C, Row, empty).

get_right_moves(_, _/9, X-X).
get_right_moves(Row, R/C, [ R/C | Moves ]-T) :-
    arg(C, Row, empty),
    CPlus is C + 1,
    get_right_moves(Row, R/CPlus, Moves-T), !.
get_right_moves(Row, R/C, [ R/C | T ]-T) :-
    \+ arg(C, Row, empty).