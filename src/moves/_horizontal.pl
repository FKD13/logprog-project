:- module('_horizontal', [get_horizontal_moves/4]).

get_horizontal_moves(Board, Piece, R/C, LeftMoves-T) :-
    arg(R, Board, Row),
    CMin is C - 1,
    CPlus is C + 1,
    get_left_moves( Row, Piece, R/C, R/CMin , LeftMoves-RightMoves),
    get_right_moves(Row, Piece, R/C, R/CPlus, RightMoves-T).
    
get_left_moves(_  , _          , _, _/0, T-T) :- !.
get_left_moves(Row, p(Color, _), _, _/C, T-T) :- arg(C, Row, p(Color, _)), !.
get_left_moves(Row, Piece      , O, R/C, [ m(Piece, O, R/C) | T ]-T) :- arg(C, Row, p(_, _)), !.
get_left_moves(Row, Piece      , O, R/C, [ m(Piece, O, R/C) | Moves ]-T) :-
    CMin is C - 1,
    get_left_moves(Row, Piece, O, R/CMin, Moves-T).

get_right_moves(_  , _          , _, _/9, T-T) :- !.
get_right_moves(Row, p(Color, _), _, _/C, T-T) :- arg(C, Row, p(Color, _)), !.
get_right_moves(Row, Piece      , O, R/C, [ m(Piece, O, R/C) | T ]-T) :- arg(C, Row, p(_, _)), !.
get_right_moves(Row, Piece      , O, R/C, [ m(Piece, O, R/C) | Moves ]-T) :-
    CPlus is C + 1,
    get_right_moves(Row, Piece, O, R/CPlus, Moves-T), !.