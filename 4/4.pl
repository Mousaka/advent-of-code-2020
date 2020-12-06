:- use_module(library(pio)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

at_end --> \+ [_].

nspace, ` ` --> ` `.
nl_putback, `\n` --> `\n`.

twonl--> `\n\n`.

part(P) -->
    string(P),`:`, (string(_);`#`), (nspace;nl_putback). 

parts([]) --> twonl.
parts([]) --> at_end.
parts(L) --> blank,parts(L).
parts([P|L]) -->
    part(P),parts(L).

validP, [Count] -->
    parts(Passport),
    { (subset([`byr`,`iyr`,`eyr`,`hgt`,`hcl`,`ecl`,`pid`],Passport), Count = 1); Count=0 }.

passports, [C] --> [C],at_end.
passports, [C] --> [C1],validP,passports,[C2], { C is C1 + C2 }.

startCount, [0,T] --> [T].

countValidPassports, [C] --> startCount, passports, [C].

solve(Solution):-
    phrase_from_file((countValidPassports,remainder([Solution])),'input.txt').
