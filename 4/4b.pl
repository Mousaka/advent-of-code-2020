:- use_module(library(pio)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

at_end --> \+ [_].

nspace, ` ` --> ` `.
nl_putback, `\n` --> `\n`.

twonl--> `\n\n`.

height --> integer(N), `cm`, { N =< 193, N >= 150}.
height --> integer(N), `in`, { N =< 76, N >= 59}.

colorChar --> [T], { member(T,`abcdef`) }. 
colorChar --> digit(_).

eyeColor --> `amb`.
eyeColor --> `blu`.
eyeColor --> `brn`.
eyeColor --> `gry`.
eyeColor --> `grn`.
eyeColor --> `hzl`.
eyeColor --> `oth`.

digit --> [T], {member(T, `0123456789`)}.

repeat(1,Pattern) --> Pattern.
repeat(N,Pattern) --> Pattern, { N0 is N-1 },repeat(N0, Pattern).


partKey(byr) --> `byr`,`:`, integer(N), { N =< 2020, N >= 1920 }.
partKey(iyr) --> `iyr`,`:`, integer(N), { N =< 2020, N >= 2010 }.
partKey(eyr) --> `eyr`,`:`, integer(N), { N =< 2030, N >= 2020 }.
partKey(hgt) --> `hgt`,`:`, height. 
partKey(hcl) --> `hcl`,`:`, `#`,repeat(6,colorChar).
partKey(ecl) --> `ecl`,`:`, eyeColor.
partKey(pid) --> `pid`,`:`, repeat(9,digit).
partKey(other) --> string(_),`:`, string(_).


part(Key) -->
    partKey(Key), (nspace;nl_putback). 

parts([]) --> twonl.
parts([]) --> at_end.
parts(L) --> blank,parts(L).
parts([P|L]) -->
    part(P),parts(L).

validP, [Count] -->
    parts(Passport),
    {
    (subset([byr,iyr,eyr,hgt,hcl,ecl,pid],Passport), Count = 1); Count=0
    }.

passports, [C] --> [C],at_end.
passports, [C] --> [C1],validP,passports,[C2], { C is C1 + C2 }.

startCount, [0,T] --> [T].

countValidPassports, [C] --> startCount, passports, [C].

solve(Solution):-
    phrase_from_file((countValidPassports,remainder([Solution])),'input.txt').
