%Part one.

solvePartOne(Solution):-
    open('map.txt',read,Str),
    read_map(Str,MapWithEnd),
    delete(MapWithEnd,[e,n,d,_,o,f,_,f,i,l,e],Map),
    steps(Map,Solution),
    close(Str).

steps(Map,Count):-
    stepHelper(Map,0,0,Count).

stepHelper([],_,Count,Res):-
    Res is Count.
stepHelper([_],_,Count,Res):-
    Res is Count.
stepHelper([_,H2|T], X, Count, Res):-
    moveX(H2,X,NextX),
    % write((H2,NextX,StartCount,TreeCount)),
    countTree(H2,NextX,Count,NextCount),
    stepHelper([H2|T],NextX,NextCount,Res).

countTreeHelper(t,Count,NextCount):-
    NextCount is Count + 1.
countTreeHelper(_,Count,Count).

countTree(L,X,Count,NextCount):-
    nth0(X,L,A),
    countTreeHelper(A,Count,NextCount).

moveX(L,X,NextX):-
    length(L, Length),
    NextX is mod(X + 3, Length).


read_map(Stream,[]):-
    at_end_of_stream(Stream).
read_map(Stream,[X|L]):-
\+  at_end_of_stream(Stream),
read(Stream,Atom),
atom_chars(Atom, X),
read_map(Stream,L). 
