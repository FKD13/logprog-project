:- module('_diagonal', [get_diagonal_moves/4]).

:- use_module('_utils').

nw(R/C, RMin/CMin  ) :- RMin  is R - 1, CMin  is C - 1.
ne(R/C, RMin/CPlus ) :- RMin  is R - 1, CPlus is C + 1.
se(R/C, RPlus/CPlus) :- RPlus is R + 1, CPlus is C + 1.
sw(R/C, RPlus/CMin ) :- RPlus is R + 1, CMin  is C - 1.

get_diagonal_moves(Board, Piece, R/C, Nw-T) :-
    RMin  is R - 1, CMin  is C - 1,
    RPlus is R + 1, CPlus is C + 1,
    get_expanded_moves('_diagonal':nw, Board, Piece, R/C, RMin/CMin  , Nw-Ne),
    get_expanded_moves('_diagonal':ne, Board, Piece, R/C, RMin/CPlus , Ne-Se),
    get_expanded_moves('_diagonal':se, Board, Piece, R/C, RPlus/CPlus, Se-Sw),
    get_expanded_moves('_diagonal':sw, Board, Piece, R/C, RPlus/CMin , Sw-T).