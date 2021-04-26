:- initialization main.
:- use_module('board_parsing/chess_parser').


main:-
    read_string(user_input, _, Str),
    string_codes(Str, Codes),
    parse_board(Board, M1, M2, Codes, []),
    write(Board),
    write(M1),
    write(M2),
    halt(0).

