:- module('_diagonal', [get_diagonal_moves/3]).

:- use_module('../utils/board_utils').

get_diagonal_moves(Board, R/C, Nw-T) :-
    RMin is R - 1,
    RPlus is R + 1,
    CMin is C - 1,
    CPlus is C + 1,
    get_nw_moves(Board, RMin/CMin, Nw-Ne),
    get_ne_moves(Board, RMin/CPlus, Ne-Se),
    get_se_moves(Board, RPlus/CPlus, Se-Sw),
    get_sw_moves(Board, RPlus/CMin, Sw-T).

get_nw_moves(_, 0/_, X-X).
get_nw_moves(_, _/0, X-X).
get_nw_moves(Board, R/C, [ R/C | Moves ]-T) :-
    get_piece_at(Board, R/C, empty),
    RMin is R - 1,
    CMin is C - 1,
    get_nw_moves(Board, RMin/CMin, Moves-T), !.
get_nw_moves(Board, R/C, [ R/C | T ]-T) :-
    \+ get_piece_at(Board, R/C, empty).

get_ne_moves(_, 0/_, X-X).
get_ne_moves(_, _/9, X-X).
get_ne_moves(Board, R/C, [ R/C | Moves ]-T) :-
    get_piece_at(Board, R/C, empty),
    RMin is R - 1,
    CPlus is C + 1,
    get_ne_moves(Board, RMin/CPlus, Moves-T), !.
get_ne_moves(Board, R/C, [ R/C | T ]-T) :-
    \+ get_piece_at(Board, R/C, empty).

get_se_moves(_, 9/_, X-X).
get_se_moves(_, _/9, X-X).
get_se_moves(Board, R/C, [ R/C | Moves ]-T) :-
    get_piece_at(Board, R/C, empty),
    RPlus is R + 1,
    CPlus is C + 1,
    get_se_moves(Board, RPlus/CPlus, Moves-T), !.
get_se_moves(Board, R/C, [ R/C | T ]-T) :-
    \+ get_piece_at(Board, R/C, empty).

get_sw_moves(_, 9/_, X-X).
get_sw_moves(_, _/0, X-X).
get_sw_moves(Board, R/C, [ R/C | Moves ]-T) :-
    get_piece_at(Board, R/C, empty),
    RPlus is R + 1,
    CMin is C - 1,
    get_sw_moves(Board, RPlus/CMin, Moves-T), !.
get_sw_moves(Board, R/C, [ R/C | T ]-T) :-
    \+ get_piece_at(Board, R/C, empty).