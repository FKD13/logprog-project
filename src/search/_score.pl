:- module('_score', [get_score/3]).

/** <module> _score - Calculating the score of a board configuration.
*/

:- use_module('../utils/board_utils').


%!  get_score(+Board, +Color, -Score)
%
%   Calculate the score for a given Color.
%   All Pieces have a static score assigned.
%   Enemy pieces contribute negativly to the score and own pieces positivly.
%   The final score is the sum.
%
%   @arg Board The Board to get pieces from.
%   @arg Color The Color ro calculate the score for.
%   @arg Score The Score.
get_score(Board, Color, ScoreSum) :-
    findall(Score, (
        get_piece_at(Board, _, p(C, T)),
        get_score_piece(Color, p(C, T), Score)
    ), Scores),
    sum_list(Scores, ScoreSum).


%!  get_score_piece(+Color, +Piece, -Score)
%
%   Get the score for a given Piece.
%   If the Color is the same as the color of the Piece the score is positive,
%   else the negative is used.
%
%   @arg Color The Color ro calculate the score for.
%   @arg Piece The Piece to get the score for.
%   @arg Score The Score.
get_score_piece(Color, p(Color, Type), Score   ) :- score_piece(Type, Score), !.
get_score_piece(_    , p(_    , Type), NegScore) :- score_piece(Type, Score), NegScore is -Score, !.


%!  score_piece(+Piece, -Score).
%
%   Get the static score for a Piece.
%
%   @arg Piece The Piece to get the score for.
%   @arg Score The Score.
score_piece(pawn  , 1).
score_piece(rook  , 5).
score_piece(knight, 3).
score_piece(bishop, 3).
score_piece(queen , 9).
score_piece(king  , 200).