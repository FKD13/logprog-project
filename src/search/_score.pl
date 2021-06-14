:- module('_score', [get_score/3]).

:- use_module('../utils/board_utils').

get_score(Board, Color, ScoreSum) :-
    findall(Score, (
        get_piece_at(Board, _, p(C, T)),
        get_score_piece(Color, p(C, T), Score)
    ), Scores),
    sum_list(Scores, ScoreSum).


get_score_piece(Color, p(Color, Type), Score   ) :- score_piece(Type, Score), !.
get_score_piece(_    , p(_    , Type), NegScore) :- score_piece(Type, Score), NegScore is -Score, !.


%!  score_piece(Piece, Score).
score_piece(pawn  , 1).
score_piece(rook  , 3).
score_piece(knight, 3).
score_piece(bishop, 3).
score_piece(queen , 10).
score_piece(king  , 100).