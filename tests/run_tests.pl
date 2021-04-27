:- initialization run.
:- ['parser/_digits'].
:- ['parser/_pieces'].

run :-
    run_tests,
    halt(0).