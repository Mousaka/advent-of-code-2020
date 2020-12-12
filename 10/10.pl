:- use_module(library(pio)).
:- use_module(library(debug)).
:- use_module(library(lists)).
% :- use_module(reif).
:- set_prolog_flag(double_quotes, chars).
% ... --> [] | [_], ... .
% Parsing stuff
% ... 
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).
% phrase_from_file(lines(L), 'input'),maplist(number_chars,Ns,L).


list_max([N|Ns], Max) :-
        foldl(list_max_, Ns, N, Max).
    
list_max_(N, Max0, Max) :-
        Max #= max(N, Max0).

list_min([N|Ns], Min) :-
        foldl(list_min_, Ns, N, Min).

list_min_(N, Min0, Min) :-
        Min #= min(N, Min0).

input([16,
        10,
        15,
        5,
        1,
        11,
        7,
        19,
        6,
        12,
        4]).

adaptify(UnsortedAdapters,OneC,ThreeC):-
        list_max(UnsortedAdapters,Max),
        Last #= Max + 3,
        sort([Last|UnsortedAdapters],Adapters),
        % chain(#<, Adapters),
        adaptify(Adapters,0,OneC,ThreeC).
adaptify([],_,0,0).
adaptify([Adapter|T],Prev,OneC,ThreeC):-
        Diff #= (Adapter - Prev),
        adaptify(T,Adapter,OneC0,ThreeC0),
        ((Diff #= 1 -> OneC #= OneC0 + 1, ThreeC #= ThreeC0);
        (Diff #= 3 -> OneC #= OneC0, ThreeC #= ThreeC0 + 1)).

solvePart1(C):-
        phrase_from_file(lines(L), 'input'),
        maplist(number_chars,Ns,L),
        adaptify(Ns,Ones,Threes),
        C #= Ones * Threes.



adaptify2(UnsortedAdapters,Comb):-
        list_max(UnsortedAdapters,Max),
        Last #= Max + 3,
        sort([Last|UnsortedAdapters],Adapters),
        % chain(#<, Adapters),
        adaptify2(Adapters,0,Comb).
adaptify2([],_,0,0).
adaptify2([Adapter|T],Prev,Comb):-
        Diff #= (Adapter - Prev),
        (Diff #= 1;
                Diff #= 3),
        Comb = Comb0
        adaptify2(T,Adapter,Comb0).
        