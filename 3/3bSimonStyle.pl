%Part two Simon style.


solvePartTwo(Solution):-
    open('map.txt',read,Str),
    read_TreeIndexes(Str,TreeIndexes, Width),
    close(Str),
    steps(Width,TreeIndexes,1,1,S1),
    steps(Width,TreeIndexes,3,1,S2),
    steps(Width,TreeIndexes,5,1,S3),
    steps(Width,TreeIndexes,7,1,S4),
    steps(Width,TreeIndexes,1,2,S5),
    Solution is S1 * S2 * S3 * S4 * S5.

% Can also be used if you "know" the Count but want to know what kind of Tree setup would give that count:
% ?- steps(3,[[0,1,2],[X,2],Y],1,1,2).
% X = 1,
% Y = [2|_1618] 
% 
% or
% ?- steps(3,TreeSetup,1,1,2).
% TreeSetup = [_4332, [1|_4364], [2|_4400]] 

steps(Width,[H|T],StepsRight,StepsDown,Count):-
    dropRows([H|T],StepsDown,NextTreeIndexes),
    stepHelper(Width,NextTreeIndexes,StepsRight,StepsDown,0,0,Count).

stepHelper(_,[],_,_,_,Count,Count).
stepHelper(Width,[H|T], StepsRight,StepsDown,Index,Count, Res):-
    moveX(Width,StepsRight,Index,NextIndex),
    countTree(H,NextIndex,AdditionalTrees),
    NextCount is Count + AdditionalTrees,
    dropRows([H|T],StepsDown,NextTreeIndexes),
    stepHelper(Width,NextTreeIndexes,StepsRight,StepsDown,NextIndex,NextCount,Res).


countTree(L,Index,1):-
    member(Index,L).
countTree(_,_,0).
moveX(Width,StepsRight,Index,NextIndex):-
    NextIndex is mod(Index + StepsRight, Width).

dropRows([],_,[]).
dropRows(L,0,L).
dropRows([_|T],RowsToDrop,R):-
    RowsToDrop0 is RowsToDrop - 1,
    dropRows(T,RowsToDrop0, R).


atom_to_tree_indexes(A,Indexes):-
    atom_chars(A,A0),
    atomToIndexesHelper(A0,0,[],Indexes).

atomToIndexesHelper([], _, Acc, Acc).
atomToIndexesHelper([t|T], I, Acc, Res):-
    I0 is I + 1,
    atomToIndexesHelper(T, I0, [I|Acc], Res).
atomToIndexesHelper([_|T], I, Acc, Res):-
    I0 is I + 1,
    atomToIndexesHelper(T, I0, Acc, Res).
    

read_TreeIndexes(Stream,[],_):-
    at_end_of_stream(Stream).
read_TreeIndexes(Stream,[Index|L],Width):-
    \+  at_end_of_stream(Stream),
    read(Stream,Atom),
    atom_length(Atom, Width),
    atom_to_tree_indexes(Atom, Index),
    read_TreeIndexes(Stream,L,Width). 
