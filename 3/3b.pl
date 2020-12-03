%Part two.

solvePartTwo(Solution):-
    open('map.txt',read,Str),
    read_map(Str,MapWithEnd),
    delete(MapWithEnd,[e,n,d,_,o,f,_,f,i,l,e],Map),
    steps(Map,1,1,S1),
    steps(Map,3,1,S2),
    steps(Map,5,1,S3),
    steps(Map,7,1,S4),
    steps(Map,1,2,S5),
    Solution is S1 * S2 * S3 * S4 * S5,
    close(Str).

steps(Map,XSteps,YSteps,Count):-
    dropRows(Map,YSteps,NextMap),
    stepHelper(NextMap,XSteps,YSteps,0,0,Count).

stepHelper([],_,_,_,Count,Res):-
    Res is Count.
stepHelper([H|T], XSteps,YSteps,X, Count, Res):-
    moveX(H,XSteps,X,NextX),
    countTree(H,NextX,Count,NextCount),
    dropRows([H|T],YSteps,NextMap),
    stepHelper(NextMap,XSteps,YSteps,NextX,NextCount,Res).

countTreeHelper(t,Count,NextCount):-
    NextCount is Count + 1.
countTreeHelper(_,Count,Count).

countTree(L,X,Count,NextCount):-
    nth0(X,L,A),
    countTreeHelper(A,Count,NextCount).

moveX(L,XSteps,X,NextX):-
    length(L, Length),
    NextX is mod(X + XSteps, Length).

dropRows([],_,[]).
dropRows(L,0,L).
dropRows([_|T],RowsToDrop,R):-
    RowsToDrop0 is RowsToDrop - 1,
    dropRows(T,RowsToDrop0, R).


read_map(Stream,[]):-
    at_end_of_stream(Stream).
read_map(Stream,[X|L]):-
\+  at_end_of_stream(Stream),
read(Stream,Atom),
atom_chars(Atom, X),
read_map(Stream,L). 
