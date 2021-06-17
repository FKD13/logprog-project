:- module('moves', [get_moves/5]).

/** <module> moves - Get all moves.
*/

:- use_module('_diagonal').
:- use_module('_horizontal').
:- use_module('_knight').
:- use_module('_pawn').
:- use_module('_rokade').
:- use_module('_surroundings').
:- use_module('_vertical').

:- use_module('../utils/board_utils').


%!  get_moves(+Board, +MetaDataWhite, +MetaDataBlack, +Color, -Moves)
%
%   Get all possible moves given a board configuration and its metadata.
%   The metadata is used to determin if some special moves like the rokade and 
%   en passant should be taken.
%
%   @arg Board The board to check available moves on.
%   @arg MetaDataWhite Metadata for the white player.
%   @arg MetaDataBlack Metadata for the black player.
%   @arg Color The currently playing color.
%   @arg Moves The possible moves. This is a difference list.
get_moves(Board, MW, MB, Color, NormalMoves-T) :-
    get_metadata(MW, MB, Color, Meta),
    Meta = m(_, _, _, Passant),
    findall(Moves,(
        get_piece_at(Board, Coord, p(Color, Type)),
        get_moves_piece(Board, p(Color, Type), Coord, Passant, Moves)
    ), MoveLists),
    foldl([L1-L2, L2-T, L1-T]>>true, MoveLists, X-X, NormalMoves-RokadeMoves),
    get_rokade_moves(Board, Meta, Color, RokadeMoves-T).


%!  get_moves_piece(+Board, +Piece, +Coord, +Passant, -Moves)
%
%   Given a Piece and it's position, determin all possible moves for that piece.
%
%   @arg Board The board to check available moves on.
%   @arg Piece The piece at the coordinate location.
%   @arg Coord The location of the piece.
%   @arg Passant The possibility of a en passant.
%   @arg Moves The possible moves. This is a difference list.
get_moves_piece(Board, p(Color, rook), Coord, _, Horizontal-T) :- 
    get_horizontal_moves(Board, p(Color, rook), Coord, Horizontal-Vertical),
    get_vertical_moves(Board, p(Color, rook), Coord, Vertical-T), !.
get_moves_piece(Board, p(Color, knight), Coord, _, Moves) :-
    get_knight_moves(Board, p(Color, knight), Coord, Moves), !.
get_moves_piece(Board, p(Color, bishop), Coord, _, Moves) :- 
    get_diagonal_moves(Board, p(Color, bishop), Coord, Moves), !.
get_moves_piece(Board, p(Color, queen), Coord, _, Horizontal-T) :- 
    get_horizontal_moves(Board, p(Color, queen), Coord, Horizontal-Vertical),
    get_vertical_moves(Board, p(Color, queen), Coord, Vertical-Diagonal),
    get_diagonal_moves(Board, p(Color, queen), Coord, Diagonal-T), !.
get_moves_piece(Board, p(Color, king), Coord, _, Moves) :- 
    get_surrounding_moves(Board, p(Color, king), Coord, Moves), !. 
get_moves_piece(Board, p(Color, pawn), Coord, Passant, Moves) :- 
    get_pawn_moves(Board, p(Color, pawn), Coord, Moves, Passant), !.


%!  get_metadata(+MetaDataWhite, +MetaDataBlack, +Color, +MetaData)
%
%   Given a color, provide the corresponding metadata.
%
%   @arg MetaDataWhite Metadata for the white player.
%   @arg MetaDataBlack Metadata for the black player.
%   @arg Color The given color.
%   @arg MetaData The MetaData corresponding with the Color.
get_metadata(MW, _ , w, MW).
get_metadata(_ , MB, b, MB).