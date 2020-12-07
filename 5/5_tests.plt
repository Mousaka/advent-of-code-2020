pending :-
    current_prolog_flag(argv, ['--all'|_]).
pending :-
    write('\nA TEST IS PENDING!\n'),
    fail.

:- begin_tests(seat_codes).

    test(one_seat_number, condition(true)) :-
        phrase(seat_codes([0]), `FFFFFFFLLL`).

    test(two_seat_numbers, condition(true)) :-
        phrase(seat_codes([8,8]), `FFFFFFBLLL FFFFFFBLLL`).
    
    test(two_max_seat_numbers, condition(true)) :-
        phrase(seat_codes([1023,1023]), `BBBBBBBRRR BBBBBBBRRR`).

    test(max_is_one, condition(true)) :-
        solve(1, `FFFFFFFLLL FFFFFFFLLR`).
:- end_tests(row_count).


