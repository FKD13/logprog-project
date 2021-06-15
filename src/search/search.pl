:- module('search', [best/3, move/7]).

:- use_module('../moves/moves').
:- use_module('_score').
:- use_module('../utils/board_utils').
:- use_module('../utils/lists_extension').

best(Board, Color, Best) :-
    min_max(3, Board, Color, s(Best, _)).

min_max(0, Board, Color, s(_, Score)) :-
    get_score(Board, Color, Score), !.

min_max(Depth, Board, Color, s(Best, BestScore)) :-
    NDepth is Depth - 1,
    get_moves(Board, Color, Moves-[]),
    (Moves = [] -> % This color is out of moves -> win for other color.
        Best = _,
        BestScore = -1000
    ;
        maplist(
            [Move, s(Move, Score)]>>(
                make_move(Board, Move, NewBoard),
                next_color(Color, NColor), 
                min_max(NDepth, NewBoard, NColor, s(_, NegScore)),
                Score is -NegScore
            ),
            Moves,
            ScoredMoves
        ),
        'lists_extension':max_member([s(_, S1), s(_, S2)]>>(S1 =< S2), s(Best, BestScore), ScoredMoves)
    ).


next_color(w, b).
next_color(b, w).

move(Board, M1, M2, Move, NB, m(W1, W2, W3, W4), m(B1, B2, B3, B4)) :- 
    search:make_move(Board, Move, NB),
    rokade(NB, w,       M1, m(W1, W2, _ , _ )),
    rokade(NB, b,       M2, m(B1, B2, _ , _ )),
    next_turn(          M1, m(_ , _ , W3, _ )),
    next_turn(          M2, m(_ , _ , B3, _ )),
    en_passant(w, Move, M1, m(_ , _ , _ , W4)),
    en_passant(b, Move, M2, m(_ , _ , _ , B4)).

make_move(Board, m(Piece, OldCoord, Coord), NewBoard) :-
    set_piece_at(Board, OldCoord, empty, Tmp     ),
    set_piece_at(Tmp  , Coord   , Piece, NewBoard).

next_turn(m(A, B, yes, D), m(A, B, no , D)).
next_turn(m(A, B, no , D), m(A, B, yes, D)).

rokade(_    , _, m(no , no , C, D), m(no , no , C, D)) :- !.
rokade(Board, w, m(yes, no , C, D), m(yes, no , C, D)) :- get_piece_at(Board, 1/1 , p(w, rook)), get_piece_at(Board, 1/5 , p(w, king)), !.
rokade(_    , w, m(yes, no , C, D), m(no , no , C, D)) :- !.
rokade(Board, w, m(no , yes, C, D), m(no , yes, C, D)) :- get_piece_at(Board, 1/8 , p(w, rook)), get_piece_at(Board, 1/5 , p(w, king)), !.
rokade(_    , w, m(no , yes, C, D), m(no , no , C, D)) :- !.
rokade(Board, b, m(yes, no , C, D), m(yes, no , C, D)) :- get_piece_at(Board, 8/1 , p(b, rook)), get_piece_at(Board, 8/5 , p(b, king)), !.
rokade(_    , b, m(yes, no , C, D), m(no , no , C, D)) :- !.
rokade(Board, b, m(no , yes, C, D), m(no , yes, C, D)) :- get_piece_at(Board, 8/8 , p(b, rook)), get_piece_at(Board, 8/5 , p(b, king)), !.
rokade(_    , b, m(no , yes, C, D), m(no , no , C, D)) :- !.

rokade(Board, Color, m(yes, yes, C, D), m(A, B, C, D)) :-
    rokade(Board, Color, m(yes, no, C, D), m(A, no, C, D)),
    rokade(Board, Color, m(no, yes, C, D), m(no, B, C, D)).

en_passant(w, m(p(w, pawn), 2/C, 4/C), m(A, B, C_, _), m(A, B, C_, 3/C)) :- !.
en_passant(b, m(p(b, pawn), 7/C, 5/C), m(A, B, C_, _), m(A, B, C_, 6/C)) :- !.
en_passant(_, _                      , m(A, B, C_, _), m(A, B, C_, no) ).
