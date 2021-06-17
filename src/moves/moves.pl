:- module('moves', [get_moves/5]).

:- use_module('_diagonal').
:- use_module('_horizontal').
:- use_module('_knight').
:- use_module('_pawn').
:- use_module('_rokade').
:- use_module('_surroundings').
:- use_module('_vertical').

:- use_module('../utils/board_utils').

get_moves(Board, MW, MB, Color, NormalMoves-T) :-
    findall(Moves,(
        get_piece_at(Board, Coord, p(Color, Type)),
        get_moves_piece(Board, p(Color, Type), Coord, Moves)
    ), MoveLists),
    foldl([L1-L2, L2-T, L1-T]>>true, MoveLists, X-X, NormalMoves-RokadeMoves),
    get_metadata(MW, MB, Color, Meta),
    get_rokade_moves(Board, Meta, Color, RokadeMoves-T).

get_moves_piece(Board, p(Color, rook), Coord, Horizontal-T) :- 
    get_horizontal_moves(Board, p(Color, rook), Coord, Horizontal-Vertical),
    get_vertical_moves(Board, p(Color, rook), Coord, Vertical-T), !.
get_moves_piece(Board, p(Color, knight), Coord, Moves) :-
    get_knight_moves(Board, p(Color, knight), Coord, Moves), !.
get_moves_piece(Board, p(Color, bishop), Coord, Moves) :- 
    get_diagonal_moves(Board, p(Color, bishop), Coord, Moves), !.
get_moves_piece(Board, p(Color, queen), Coord, Horizontal-T) :- 
    get_horizontal_moves(Board, p(Color, queen), Coord, Horizontal-Vertical),
    get_vertical_moves(Board, p(Color, queen), Coord, Vertical-Diagonal),
    get_diagonal_moves(Board, p(Color, queen), Coord, Diagonal-T), !.
get_moves_piece(Board, p(Color, king), Coord, Moves) :- 
    get_surrounding_moves(Board, p(Color, king), Coord, Moves), !. 
get_moves_piece(Board, p(Color, pawn), Coord, Moves) :- 
    get_pawn_moves(Board, p(Color, pawn), Coord, Moves, no), !.
    % TODO: pass correct en passant.

get_metadata(MW, _ , w, MW).
get_metadata(_ , MB, b, MB).