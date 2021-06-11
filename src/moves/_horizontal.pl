:- module('_horizontal', [get_horizontal_moves/4]).

get_horizontal_moves(Board, Color, R/C, LeftMoves-T) :-
    arg(R, Board, Row),
    CMin is C - 1,
    CPlus is C + 1,
    get_left_moves( Row, Color, R/CMin , LeftMoves-RightMoves),
    get_right_moves(Row, Color, R/CPlus, RightMoves-T).
    
get_left_moves(_, _, _/0, T-T) :- !.
get_left_moves(Row, Color, _/C, T-T) :- arg(C, Row, p(Color, _)), !.
get_left_moves(Row, _    , R/C, [ R/C | T ]-T) :- arg(C, Row, p(_, _)), !.
get_left_moves(Row, Color, R/C, [ R/C | Moves ]-T) :-
    CMin is C - 1,
    get_left_moves(Row, Color, R/CMin, Moves-T).

get_right_moves(_, _, _/9, T-T) :- !.
get_right_moves(Row, Color, _/C, T-T) :- arg(C, Row, p(Color, _)), !.
get_right_moves(Row, _    , R/C, [ R/C | T ]-T) :- arg(C, Row, p(_, _)), !.
get_right_moves(Row, Color, R/C, [ R/C | Moves ]-T) :-
    CPlus is C + 1,
    get_right_moves(Row, Color, R/CPlus, Moves-T), !.