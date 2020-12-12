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


% Part2.
:- use_module(library(ordsets)).

distance_constraint(A,B):-
        D #= B - A,
        if_(D #= 1,
                true,
        if_(D #= 3, true, false)).
distance_constraint2(A,B):-
        D #= B - A,
        (D #= 1; D = 3).

all_follow_dc([],_).
all_follow_dc([Max],Max).
all_follow_dc([A,B|T],Max):-
        distance_constraint(A,B),
        all_follow_dc([B|T],Max).

adapters_working(UnorderedAdapters,Fixed):-
        list_max(UnorderedAdapters,Max),
        % list_to_ord_set(UnorderedAdaptersUnorderedAdapters,AllAdapters),
        reverse([0|_], RSubAdapters),
        reverse([Max|RSubAdapters],Fixed),
        
        subset(Fixed, [0|UnorderedAdapters]).

        

       
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Reified predicates for use with predicates from library(reif).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#=(X, Y, T) :-
        X #= Y #<==> B,
        zo_t(B, T).

#<(X, Y, T) :-
        X #< Y #<==> B,
        zo_t(B, T).

zo_t(0, false).
zo_t(1, true).

       



% Inspired by
% https://stackoverflow.com/a/30207770


subset([X],Ys):-
      selectd(X,Ys,_).  
subset([X,X1|Xs], Ys) :-
   selectd(X, Ys, Zs),
   selectd(X1, Zs, _),
   distance_constraint(X,X1),
   subset(Xs, Zs).


equal_elementsBB(Xs,Ys) :-
        same_length(Xs,Ys),
        equal_elementsB(Xs,Ys).


equal_elementsB([], []).
equal_elementsB([X|Xs], Ys) :-
   selectd(X, Ys, Zs),
   equal_elementsB(Xs, Zs).

selectd(E,[A|As],Bs1) :-
        if_(A = E, As = Bs1, 
                   (Bs1 = [A|Bs], selectd(E,As,Bs))).