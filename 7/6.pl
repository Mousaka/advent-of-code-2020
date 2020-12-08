:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).

at_end --> \+ [_].

solve(T,Input):-
    phrase(group_counts(T), Input).

solveFromFile(T):-
    phrase_from_file(group_counts(T), 'input').

n_unique(N,L):-
    list_to_set(L,S),
    length(S,N).

group_counts(Count) -->
    blank,blank,
    group_counts(Count).
group_counts(Count) -->
    group(G),
    group_counts(N2),
    { n_unique(N1,G), Count #= N1 + N2 }.
group_counts(N) -->
    group(G),
    { n_unique(N,G) }. 

group([]) --> blank,at_end.
group([]),`\n\n` --> blank,blank.
group([H|T]) --> nonblank(H), group(T).
group([H|T]) --> blank, nonblank(H), group(T).

