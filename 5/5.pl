:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
:- use_module((reif)).
% set_prolog_flag(double_quotes, chars).

solve(M,Input):-
    phrase(seat_codes(T), Input),
    max_list(T,M).

solveFromFile(M):-
    phrase_from_file(seat_codes(T), 'input'),
    max_list(T,M).

char_to_binary('0') --> "F".
char_to_binary('1') --> "B".
char_to_binary('0') --> "L".
char_to_binary('1') --> "R".

row_seat_code(Brow,Bseat,Code) :-
    bin_decimal(Brow,Drow),
    bin_decimal(Bseat,Dseat),
    Code #= Drow * 8 + Dseat.

seat_codes([Code|T]) -->
    binary(7,Brow),
    binary(3,Bseat),
    {row_seat_code(Brow,Bseat,Code)},
    blank, seat_codes(T).
seat_codes([Code]) --> 
    binary(7,Brow),
    binary(3,Bseat),
    {row_seat_code(Brow,Bseat,Code)},
    blank.

binary(C,[B|T]) --> char_to_binary(B), {C0 #= C-1}, binary(C0,T).
binary(1,[B]) --> char_to_binary(B).

bin_decimal(B,D):-
    number_codes(D,['0'|['b'|B]]).


% Part 2

solvePart2FromFile(S):-
    phrase_from_file(seat_codes(T), 'input'),
    gap_seat(S,T).

gap_seat(S,Seats):-
    Splus #= S + 1,
    Sminus #= S - 1,
    write(Splus),write(o),write(Sminus),nl,
    member(Sminus,Seats),
    \+ member(S,Seats),
    member(Splus,Seats).
