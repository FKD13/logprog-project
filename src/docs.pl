:- module('run_docs', []).

:- use_module('main').

:- doc_save(.., [recursive(true)]).

:- halt(0).

/** <module> Documentation

This module builds the documentation.

Build by running:
==
$ make docs
==
*/