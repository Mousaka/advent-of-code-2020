:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).
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


% --- Part Two ---

solvePart2FromFile(S):-
    phrase_from_file(seat_codes(T), 'input'),
    your_seat_seats(S,T).

% Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
% It's a completely full flight, so your seat should be the only missing boarding pass in your list. However, there's a catch: some of the seats at the very front and back of the plane don't exist on this aircraft, so they'll be missing from your list as well.
% Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.
% What is the ID of your seat?

your_seat_seats(YourSeat,Seats):-    % --- This predicate describes the relationship between your seat and the list of seats
    YourSeatPlusOne #= YourSeat + 1, % --- "the seats with IDs +1 and -1 from yours
    YourSeatMinusOne #= YourSeat - 1,%
    member(YourSeatMinusOne,Seats),  %      will be in your list."
    member(YourSeatPlusOne,Seats),   % ---
    \+ member(YourSeat,Seats).       % --- "It's a completely full flight, so your seat should be the only missing boarding pass in your list.""

% Add some constraints to generate a list of seat given your seat insted like such:
% length(T,L),L #< 15,all_distinct(T),chain(T,#<), maplist(#<(0),T),maplist(#>(15),T), your_seat_seats(10,T),label(T).
% length(T,L),L #= 13,all_distinct(T),chain(T,#<), maplist(#<(0),T),maplist(#>(15),T), your_seat_seats(10,T),label(T).