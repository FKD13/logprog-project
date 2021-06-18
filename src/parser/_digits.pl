:- module('_digits', [parse_digit/3]).

%!  parse_digit(-Id, +List, +End)
%
%   Parse a digit.
parse_digit(1) --> "1".
parse_digit(2) --> "2".
parse_digit(3) --> "3".
parse_digit(4) --> "4".
parse_digit(5) --> "5".
parse_digit(6) --> "6".
parse_digit(7) --> "7".
parse_digit(8) --> "8".
parse_digit(9) --> "9".
parse_digit(0) --> "0".