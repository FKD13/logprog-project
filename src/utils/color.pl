:- module('color', [next_color/2]).

/** <module> color - Color related utility.
*/

%!  next_color(+Color, -NextColor)
%
%   Get the next color given a color.
%   white -> black.
%   black -> white.
%
%   @arg Color The Color.
%   @arg NextColor The NextColor.
next_color(w, b).
next_color(b, w).