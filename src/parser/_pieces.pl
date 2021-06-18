:- module('_pieces', [parse_optional_piece/3]).

%!  parse_optional_piece(-OptionalPiece, +List, +End)
%
%   Parse an optional piece, We parse either a space or a piece.
parse_optional_piece(X) --> 
    parse_empty(X) | 
    parse_piece(X).

%!  parse_empty(-Empty, +List, +End)
%
%   Parse an empty space.
parse_empty(empty) --> " ".

%!  parse_piece(-Piece, +List, +End)
%
%   Parse a piece. To represent a Piece a compound is used.
%   This compound is the following.
%   ~~~
%   p(Color, Type)
%   ~~~
%   This is the representation used troughout the whole codebase.
parse_piece(p(w, king))   --> "\u2654".
parse_piece(p(w, queen))  --> "\u2655".
parse_piece(p(w, rook))   --> "\u2656".
parse_piece(p(w, bishop)) --> "\u2657".
parse_piece(p(w, knight)) --> "\u2658".
parse_piece(p(w, pawn))   --> "\u2659".
parse_piece(p(b, king))   --> "\u265A".
parse_piece(p(b, queen))  --> "\u265B".
parse_piece(p(b, rook))   --> "\u265C".
parse_piece(p(b, bishop)) --> "\u265D".
parse_piece(p(b, knight)) --> "\u265E".
parse_piece(p(b, pawn))   --> "\u265F".