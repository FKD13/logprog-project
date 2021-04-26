parse_optional_piece(X) --> parse_empty(X) | parse_piece(X).

parse_empty(empty) --> " ".

% White pieces
parse_piece(p(w, king))   --> "\u2654".
parse_piece(p(w, queen))  --> "\u2655".
parse_piece(p(w, rook))   --> "\u2656".
parse_piece(p(w, bishop)) --> "\u2657".
parse_piece(p(w, knight)) --> "\u2658".
parse_piece(p(w, pawn))   --> "\u2659".
% Black pieces
parse_piece(p(b, king))   --> "\u265A".
parse_piece(p(b, queen))  --> "\u265B".
parse_piece(p(b, rook))   --> "\u265C".
parse_piece(p(b, bishop)) --> "\u265D".
parse_piece(p(b, knight)) --> "\u265E".
parse_piece(p(b, pawn))   --> "\u265F".