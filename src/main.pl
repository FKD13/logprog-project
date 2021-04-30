:- module(main, [main/0]).

%:- initialization main.
:- use_module('parser/chess_parser').

:- doc_server(5000).

/** <module> Main module

The main module defines rules used to start the chess program.

@author Francis Klinck
*/


%! main
% 
% The main rule, this starts the program.
main:-
    read_string(user_input, _, Str),
    string_codes(Str, _),
    halt(0).

