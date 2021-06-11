:- module('run_docs', []).

:- use_module('parser/chess_parser').
:- use_module('main').
:- use_module('utils/board_utils.pl').


:- doc_server(5000).

/** <module> Documentation

This module runs the documention server.

The server will be available at http://localhost:5000/pldoc/

Start by running:
==
$ make docs
==
*/