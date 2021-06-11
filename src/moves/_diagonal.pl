:- module('_diagonal', [get_diagonal_moves/4]).

:- use_module('../utils/board_utils').

get_diagonal_moves(Board, Color, R/C, Nw-T) :-
    RMin is R - 1,
    RPlus is R + 1,
    CMin is C - 1,
    CPlus is C + 1,
    get_nw_moves(Board, Color, RMin/CMin  , Nw-Ne),
    get_ne_moves(Board, Color, RMin/CPlus , Ne-Se),
    get_se_moves(Board, Color, RPlus/CPlus, Se-Sw),
    get_sw_moves(Board, Color, RPlus/CMin , Sw-T).

get_nw_moves(_, _, 0/_, X-X) :- !.
get_nw_moves(_, _, _/0, X-X) :- !.
get_nw_moves(Board, Color, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_nw_moves(Board, _    , R/C, [ R/C | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_nw_moves(Board, Color, R/C, [ R/C | Moves ]-T) :-
    RMin is R - 1,
    CMin is C - 1,
    get_nw_moves(Board, Color, RMin/CMin, Moves-T).

get_ne_moves(_, _, 0/_, X-X) :- !.
get_ne_moves(_, _, _/9, X-X) :- !.
get_ne_moves(Board, Color, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_ne_moves(Board, _    , R/C, [ R/C | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_ne_moves(Board, Color, R/C, [ R/C | Moves ]-T) :-
    RMin is R - 1,
    CPlus is C + 1,
    get_ne_moves(Board, Color, RMin/CPlus, Moves-T).

get_se_moves(_, _, 9/_, X-X) :- !.
get_se_moves(_, _, _/9, X-X) :- !.
get_se_moves(Board, Color, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_se_moves(Board, _    , R/C, [ R/C | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_se_moves(Board, Color, R/C, [ R/C | Moves ]-T) :-
    RPlus is R + 1,
    CPlus is C + 1,
    get_se_moves(Board, Color, RPlus/CPlus, Moves-T).

get_sw_moves(_, _, 9/_, X-X) :- !.
get_sw_moves(_, _, _/0, X-X) :- !.
get_sw_moves(Board, Color, R/C, T-T) :- get_piece_at(Board, R/C, p(Color, _)), !.
get_sw_moves(Board, _    , R/C, [ R/C | T ]-T) :- get_piece_at(Board, R/C, p(_, _)), !.
get_sw_moves(Board, Color, R/C, [ R/C | Moves ]-T) :-
    RPlus is R + 1,
    CMin is C - 1,
    get_sw_moves(Board, Color, RPlus/CMin, Moves-T).