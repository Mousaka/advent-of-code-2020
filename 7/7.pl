:- use_module(library(pio)).
:- use_module(library(charsio)).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
% :- use_module(reif).
:- dynamic(cont/2).
:- retractall(cont(_,_)).

% ... --> [] | [_], ... .
% Parsing stuff
ws --> [W], { char_type(W, white) }, ws.
ws --> [].

identifier([A|As]) --> [A], { char_type(A, alpha) }, symbol_r(As).

symbol_r([A|As]) --> [A], { char_type(A, alnum) }, symbol_r(As).
symbol_r([])     --> [].

bagName(A) -->
        identifier(X),blank,identifier(Y),blank,identifier(_),
        { string_chars(X0,X),
                string_chars(Y0,Y),
                atomics_to_string([X0,"_",  Y0], A0),
                atom_string(A,A0)
        }.

subBag(D-B) -->
        integer(D),blank,bagName(B).

subBags([]) --> `no other bags.`.
subBags([B|T]) --> subBag(B),subBagsH(T).

subBagsH([B|T]) --> ", ", subBag(B), subBagsH(T).
subBagsH([]) --> ".".

bag(cont(B,SubBags)) -->
        bagName(B),ws,"contain",ws,subBags(SubBags).

bags([H|T]) --> bag(H), bagsH(T).

bagsH([]) --> call(eos), !.
bagsH([H|T]) --> `\n`, bag(H), bagsH(T).
% Non parsing stuff
assertList([]).
assertList([H|T]):-
        assertz(H),assertList(T).


bag_has_bag(Bag,BagInBag):-
        cont(Bag, InnerBagQs),
        bag_has_bagH(InnerBagQs,BagInBag).

bag_has_bagH([_-BagInBag|_],BagInBag). % '-' means Pair which is somewhat like a tuple. (_,B) pattern matching in Prolog is written like _-B .
bag_has_bagH([_-B|T],BagInBag):-
        dif(B,BagInBag),
        (
                bag_has_bag(B,BagInBag) ->
                true
                ;
                % \+bag_has_bag(B,BagInBag),  adding this makes the results unique but it becomes way slower. if only I could use if_ here.
                bag_has_bagH(T,BagInBag)
        ).

solvePart1(N):-
        retractall(cont(_,_)),
        phrase_from_file(bags(B), 'input'),
        assertList(B), % Builds up the fact db with the terms parsed from input file
        aggregate_all(count,X,bag_has_bag(X,shiny_gold),N). % the 2nd arg 'X' makes this only count unique solutions. 208 is the correct answer.

% Part 2
:- use_module(library(clpfd)).
bag_inside_count(Bag,C0):-
        cont(Bag,SubBags),
        bag_inside_countH(SubBags,C0).

bag_inside_countH([],0).
bag_inside_countH([C0-Bag|T],C):-
        bag_inside_countH(T,C1),
        bag_inside_count(Bag,C2),
        C #= C0 + (C0 * C2) + C1.

solvePart2(N):-
        retractall(cont(_,_)),
        phrase_from_file(bags(B), 'input'),
        assertList(B),
        bag_inside_count(shiny_gold,N).     