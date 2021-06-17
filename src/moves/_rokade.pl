:- module('rokade', [get_rokade_moves/4]).

:- use_module('_horizontal').
:- use_module('_vertical').
:- use_module('_diagonal').
:- use_module('_knight').

:- use_module('../utils/board_utils').
:- use_module('../utils/color').

get_rokade_moves(Board, m(Queen, King, yes, _), Color, Moves) :- 
    next_color(Color, NextColor),
    long_rokade(Queen, Board, Color, NextColor, Moves),
    short_rokade(King, Board, Color, NextColor, Moves).

long_rokade(no , _, _, _, T-T).
long_rokade(yes, _, _, _, T-T).

short_rokade(no , _, _, _, T-T).
short_rokade(yes, _, _, _, T-T).

safe(Board, R/C, Color, NextColor) :- 
    get_knight_moves(    Board, p(Color, knight), R/C, L1-L2),
    get_horizontal_moves(Board, p(Color, rook)  , R/C, L2-L3),
    get_vertical_moves(  Board, p(Color, rook)  , R/C, L3-L4),
    get_diagonal_moves(  Board, p(Color, bishop), R/C, L4-L5),
    CPlus is C + 1, CMin is C - 1,
    pawns(Board, R/CPlus, Color, L-L),
    pawns(Board, R/C    , Color, L-L),
    pawns(Board, R/CMin , Color, L-L).

pawns(_    , Coord, _, T-T) :- outside_board(Coord), !.
pawns(Board, R/C  , w, [ m(p(b, pawn), _, _) | T]-T) :- RPlus is R + 1, get_piece_at(Board, RMin/C, p(b, pawn)), !.
pawns(Board, R/C  , b, [ m(p(w, pawn), _, _) | T]-T) :- RMin  is R - 1, get_piece_at(Board, RMin/C, p(w, pawn)), !.
pawns(Board, R/C  , _, T-T).