:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input)).

at_end --> \+ [_].
blank_end --> blank, blank_end.
blank_end --> at_end.
solve(T,Input):-
    phrase(group_counts(T), Input).

solveFromFile(T):-
    phrase_from_file(group_counts(T), 'input').

range(Is, L, U) :-
    I #>= L,
    I #=< U
    , findall(I, indomain(I), Is).

all_yes_count(L,C):-
    range(Alphabet,97,123),
    elem_in_all(Alphabet,L,X),
    length(X,C).

elem_in_all(Acc,[H],X):-
    intersection(Acc, H, X).
elem_in_all(Acc, [H|L],X):-
    intersection(Acc, H, Acc0),
    elem_in_all(Acc0,L,X).

n_unique(N,L):-
    list_to_set(L,S),
    length(S,N).

group_counts(0) -->
    blank_end.
group_counts(Count) -->
    blank,blank,
    group(G),
    group_counts(N2),
    { all_yes_count(G,N1), Count #= N1 + N2 }.
% Had a bug for like 1h. The solution was to swap
% the two predicates below (:
group_counts(Count) -->
    group(G),
    group_counts(N2),
    { all_yes_count(G,N1), Count #= N1 + N2 }.
group_counts(Count) -->
    group(G),
    { all_yes_count(G,Count) }. 


group([]) --> blank_end.
group([]), `\n\n` --> blank.
group([H|T]) --> person(H), group(T).

person([H]) --> nonblank(H),blank.
person([H|T]) --> nonblank(H), person(T).
